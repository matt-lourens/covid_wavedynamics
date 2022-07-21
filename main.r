library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)

# === ETH Zurich data ===

repo_path <- "/home/matt/dev/repos/dailyRe-Data"
alpha_code <- "ZAF"
# Read Data
raw_data <- read.csv(str_glue("{repo_path}/{alpha_code}-estimates.csv"))
summary(raw_data)
# Process data
raw_data$date <- as.Date(raw_data$date)
# Filter data
# exclude ZAF since it's the aggregate of all regions
region_filter <- c("Eastern Cape", "Free State", "Gauteng", "KwaZulu-Natal",
"Limpopo", "Mpumalanga", "North West", "Northern Cape", "Western Cape")
# Specify estimate_type: (Cori_step, Cori_slidingWindow)
# Cori step seems to have more records
estimate_type_filter <- "Cori_step"
# Specify data_type: (Confirmed cases, Deaths)
data_type_filter <- "Deaths"
# Specify date range (start, end)
wave_range <- c(as.Date("2022/01/01"), as.Date("2022/10/01"))

# set up list of canidate waves
waves <- list(c(as.Date("2020/01/01"), as.Date("2020/07/18")),
            c(as.Date("2020/09/10"), as.Date("2021/01/06")),
            c(as.Date("2021/03/21"), as.Date("2021/07/10")),
            c(as.Date("2021/10/30"), as.Date("2022/01/01")),
            c(as.Date("2022/03/09"), as.Date("2022/05/16"))
        )

# Store each filtered canidate wave data to list
wave_data <- list()
for (i in seq_along(waves)) {
    wave <- waves[[i]]
    print(wave)
    tmp_wave_data <- raw_data %>%
                            filter(region %in% region_filter
                            & estimate_type == estimate_type_filter
                            & data_type == data_type_filter & date > wave[1]
                            & date < wave[2])
    wave_data[[i]] <- tmp_wave_data
}

# Split data per region
regions <- c("Eastern Cape", "Free State", "Gauteng", "KwaZulu-Natal",
"Limpopo", "Mpumalanga", "North West", "Northern Cape", "Western Cape")

# define function calculating wave start
get_wave_start <- function(data, ci_low = 1, ci_top = 1) {
    cirteria_dates <- data %>%
                        filter(median_R_highHPD > ci_top  & median_R_lowHPD > ci_low)
    return(min(cirteria_dates$date))
}

wave_start_df <- data.frame()
tmp <- c()
for (i in seq_along(wave_data)){
    wave_df <- wave_data[[i]]
    for (region_val in regions){
        wave_start <- get_wave_start(wave_df %>% filter(region == region_val), ci_low = 0.90, ci_top = 1)
        tmp <- c(tmp, wave_start)
        wave_start_df <- rbind(wave_start_df, data.frame(wave_n = i, region = region_val, start_date = wave_start))
        print(str_glue("wave {i} -{region_val} - {wave_start}"))
    }
}
# add ranks
wave_start_df <- wave_start_df %>%
            group_by(wave_n) %>%
            mutate(rank = order(order(start_date)))
df_cor <- cor.test(as.matrix(wave_start_df), type = "spearman")

# First province per wave
for (i in seq_along(wave_data)){
    print(str_glue(" === wave {i} === "))
    print(wave_start_df %>% filter(wave_n == i) %>% arrange(rank))
}
# Spearman for all pairwise combinations of waves
combos <- combn(1:max(wave_start_df$wave_n), 2)
cor_df <- data.frame()
for (i in 1:ncol(combos)){
    x <- (wave_start_df %>% filter(wave_n == combos[1,i]))$rank
    y <- (wave_start_df %>% filter(wave_n == combos[2,i]))$rank
    corr <- cor.test(x = x, y = y, method = "spearman")
    cor_df <-  rbind(cor_df, data.frame(from_wave = str_glue("wave_{combos[1,i]}"), to_wave = str_glue("wave_{combos[2,i]}"),
                     rho = corr$estimate, p = corr$p.value, 
                     label = str_glue("rho = {format(round(corr$estimate, 2), nsmall = 2)}\np = {format(round(corr$p.value, 2), nsmall = 2)}")))
    print(str_glue(" === wave {combos[1,i]} -> {combos[2,i]} === "))
    print(corr)
}

ggplot(cor_df, aes(x = from_wave, y = to_wave, fill = rho)) +
  geom_raster() +
  geom_text(aes(label = label)) +
  scale_fill_distiller(palette = "Spectral") +
  theme_minimal() +
  theme(panel.grid = element_blank())


# Pivot so that each wave is a variable
pivot_wave_start <- wave_start_df %>%
            pivot_wider(names_from = wave_n, values_from = rank, names_prefix = "wave_")

for (i in seq_along(wave_data)){
    print(str_glue(" === wave {i} === "))
     print(str_glue("Range: {waves[[i]][1]} - {waves[[i]][2]}"))
    print(wave_start_df %>% filter(wave_n == i) %>% arrange(rank))
    View(wave_start_df %>% filter(wave_n == i) %>% arrange(rank))
}
