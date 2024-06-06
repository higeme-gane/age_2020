library(tidyverse)
library(tictoc)
library(profmem)

file_no <- 1
file_age <- c(
  "https://www.dropbox.com/scl/fi/wnlr9jawc9l1d7wmjfx0b/age_2020.csv?rlkey=p3bxw1vxvho13m54r2prnzi2n&st=gmcan3lb&raw=1",
  "age_2020.csv")
df_base <- read_csv(file_age[file_no])
df_master <- df_base %>%
  select(area_code, municipalities) %>%
  distinct(area_code, .keep_all = TRUE)
df_both <- df_base %>%
  group_by(area_code, age) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  mutate(sex = "both")
df_con <- df_base %>%
  select(sex, age, area_code, value) %>%
  bind_rows(df_both) %>%
  arrange(age)

tic()
piv <- map(df_master$area_code, ~{
  df_filter <- df_con %>%
    filter(area_code == .x)
  df_rep <- dplyr::tibble(
    sex = rep(df_filter$sex, df_filter$value),
    age = rep(df_filter$age, df_filter$value))
  df_rep %>%
    group_by(sex) %>%
    summarise(med = median(age), .groups = "drop") %>%
    add_row(sex = "both", med = median(df_rep$age)) %>%
    mutate(area_code = .x)
}) |> list_rbind()
toc()

tic()
piv <- map(c("both", "male", "female"), function(f_sex) {
  df_sex <- filter(.data = df_con, sex == f_sex)
  map(df_master$area_code, function(f_area) {
    df <- filter(.data = df_sex, area_code == f_area) %>%
      mutate(cumsum = cumsum(value))
    total <- tail(df$cumsum, 1)
    half_n <- (total + 1) / 2
    lower_n <- floor(half_n)
    upper_n <- ceiling(half_n)
    lower_row <- min(which(lower_n <= df$cumsum))
    upper_row <- min(which(upper_n <= df$cumsum))
    med <- (df$age[lower_row] + df$age[upper_row]) / 2
    dplyr::tibble(area_code = f_area,
                  med_age = med)
  }) |> list_rbind() %>%
    mutate(sex = f_sex)
}) |> list_rbind()
toc()

#全国中央値単体
df_all_japan <- df_con %>%
  filter(sex == "both",
         area_code == "00000")

tic()
p <- profmem({
  vec_all_med <- rep(df_all_japan$age, df_all_japan$value)
  vec_all_med <- median(vec_all_med)
})
toc()
print(p)

tic()
p_2 <- profmem({
  df_human_med <- df_all_japan %>%
    mutate(cumsum = cumsum(value))
  total <- tail(df_human_med$cumsum, 1)
  half_n <- (total + 1) / 2
  lower_n <- floor(half_n)
  upper_n <- ceiling(half_n)
  lower_row <- min(which(lower_n <= df_human_med$cumsum))
  upper_row <- min(which(upper_n <= df_human_med$cumsum))
  med <- (df_human_med$age[lower_row] + df_human_med$age[upper_row]) / 2
})
toc()
print(p_2)