library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(vistime)
library(cluster)
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
region_filter <- c(
    "Eastern Cape", "Free State", "Gauteng", "KwaZulu-Natal",
    "Limpopo", "Mpumalanga", "North West", "Northern Cape", "Western Cape"
)
# Specify estimate_type: (Cori_step, Cori_slidingWindow)
# Cori step seems to have more records
estimate_type_filter <- "Cori_slidingWindow"
# Specify data_type: (Confirmed cases, Deaths)
data_type_filter <- "Confirmed cases"
# Specify date range (start, end)
# wave_range <- c(as.Date("2022/01/01"), as.Date("2022/10/01"))

wave_data <- raw_data %>%
    filter(region %in% region_filter &
        estimate_type == estimate_type_filter &
        data_type == data_type_filter)

crit_gp <- wave_data %>% filter(median_R_mean>1 & median_R_highHPD>1 & median_R_lowHPD>1)
# vistime(crit_gp, col.event="median_R_lowHPD", col.start="date", col.end="date", col.group = "region", show_labels = FALSE)

# Can maybe do this per province
clusters <- lapply(region_filter, (\(x) kmeans((crit_gp %>% filter(region==x))$date, 5)$cluster))
clusters[[1]]

min((crit_gp %>% filter(region=="Eastern Cape"))[clusters[[1]]==1,]$date)
max((crit_gp %>% filter(region=="Eastern Cape"))[clusters[[1]]==1,]$date)
region_filter[[1]]
region_filter=c("Eastern Cape")
waves <- lapply(1:5, (\(x) c(min((crit_gp %>% filter(region=="Eastern Cape"))[clusters[[1]]==x,]$date), max((crit_gp %>% filter(region=="Eastern Cape"))[clusters[[1]]==x,]$date))))



get_wave_start <- function(data, window_size = 7, column_name = "median_R_mean", critical_value = 1) {
    min_date <- min(data$date)
    max_date <- max(data$date)
    lower_date <- min_date
    found_date <- FALSE
    while ((lower_date < max_date - window_size) && found_date == FALSE) {
        window_data <- data %>% filter(date >= lower_date & date < lower_date + window_size)
        if (sum(window_data[[column_name]] > critical_value) >= window_size) {
            found_date <- TRUE
            return(lower_date)
        }
        lower_date <- lower_date + 1
    }
    if (found_date == FALSE) {
        return(NA)
    }
}
waves[[1]]
get_wave_start((raw_data %>% filter(region=="Eastern Cape" & date > waves[[1]][1] & date < waves[[1]][2])))
# vistime(raw_data, col.event = "")
# set up list of candidate waves
# n_date_range = 7
# start_date = as.Date("2020/01/01")
# end_date = start_date + n_date_range

# raw_data %>%
#         filter(region %in% region_filter
#         & estimate_type == estimate_type_filter
#         & data_type == data_type_filter & date > start_date
#         & date < end_date
# waves <- list(
#     c(as.Date("2020/01/01"), as.Date("2020/07/18")),
#     c(as.Date("2020/09/27"), as.Date("2021/01/06")),
#     c(as.Date("2021/03/20"), as.Date("2021/07/10")),
#     c(as.Date("2021/10/30"), as.Date("2022/01/01")),
#     c(as.Date("2022/03/09"), as.Date("2022/05/16"))
# )

# Store each filtered canidate wave data to list
wave_data <- list()
for (i in seq_along(waves)) {
    wave <- waves[[i]]
    print(wave)
    tmp_wave_data <- raw_data %>%
        filter(region %in% region_filter &
            estimate_type == estimate_type_filter &
            data_type == data_type_filter & date > wave[1] &
            date < wave[2])
    wave_data[[i]] <- tmp_wave_data
}

# Split data per region
regions <- c(
    "Eastern Cape", "Free State", "Gauteng", "KwaZulu-Natal",
    "Limpopo", "Mpumalanga", "North West", "Northern Cape", "Western Cape"
)

# define function calculating wave start
get_wave_start <- function(data, window_size = 7, column_name = "median_R_mean", critical_value = 1) {
    min_date <- min(data$date)
    max_date <- max(data$date)
    lower_date <- min_date
    found_date <- FALSE
    while ((lower_date < max_date - window_size) && found_date == FALSE) {
        window_data <- data %>% filter(date >= lower_date & date < lower_date + window_size)
        if (sum(window_data[[column_name]] > critical_value) >= window_size) {
            found_date <- TRUE
            return(lower_date)
        }
        lower_date <- lower_date + 1
    }
    if (found_date == FALSE) {
        return(NA)
    }
}
data <- wave_df %>% filter(region == region_val)

wave_start_df <- data.frame()
tmp <- c()
for (i in seq_along(wave_data)) {
    wave_df <- wave_data[[i]]
    for (region_val in regions) {
        wave_start <- get_wave_start(wave_df %>% filter(region == region_val), window_size = 7, critical_value = 1, column_name = "median_R_mean") # median_R_mean median_R_lowHPD
        tmp <- c(tmp, wave_start)
        wave_start_df <- rbind(wave_start_df, data.frame(wave_n = i, region = region_val, start_date = wave_start))
        print(str_glue("wave {i} - {region_val} - {wave_start}"))
    }
}
# add ranks
wave_start_df <- wave_start_df %>%
    group_by(wave_n) %>%
    mutate(rank = order(order(start_date)))
# df_cor <- cor.test(as.matrix(wave_start_df), type = "spearman")

# First province per wave
for (i in seq_along(wave_data)) {
    print(str_glue(" === wave {i} === "))
    print(wave_start_df %>% filter(wave_n == i) %>% arrange(rank))
}
# Spearman for all pairwise combinations of waves
combos <- combn(1:max(wave_start_df$wave_n), 2)
cor_df <- data.frame()
for (i in 1:ncol(combos)) {
    x <- (wave_start_df %>% filter(wave_n == combos[1, i]))$rank
    y <- (wave_start_df %>% filter(wave_n == combos[2, i]))$rank
    corr <- cor.test(x = x, y = y, method = "spearman")
    cor_df <- rbind(cor_df, data.frame(
        from_wave = str_glue("wave_{combos[1,i]}"), to_wave = str_glue("wave_{combos[2,i]}"),
        rho = corr$estimate, p = corr$p.value,
        label = str_glue("rho = {format(round(corr$estimate, 2), nsmall = 2)}\np = {format(round(corr$p.value, 2), nsmall = 2)}")
    ))
    print(str_glue(" === wave {combos[1,i]} -> {combos[2,i]} === "))
    print(corr)
}

tmp_plot <- ggplot(cor_df, aes(x = from_wave, y = to_wave, fill = rho)) +
    geom_raster() +
    geom_text(aes(label = label)) +
    scale_fill_distiller(palette = "Spectral") +
    theme_minimal() +
    theme(panel.grid = element_blank())

# display plot
show(tmp_plot)

ggsave(
    file = str_glue("report/sa_provinces/all_regions-{estimate_type_filter}-{data_type_filter}22.svg"),
    plot = tmp_plot, width = 10, height = 8
)
# Pivot so that each wave is a variable
pivot_wave_start <- wave_start_df %>%
    pivot_wider(names_from = wave_n, values_from = rank, names_prefix = "wave_")

for (i in seq_along(wave_data)) {
    print(str_glue(" === wave {i} === "))
    print(str_glue("Range: {waves[[i]][1]} - {waves[[i]][2]}"))
    tmp_df <- wave_start_df %>%
        filter(wave_n == i) %>%
        arrange(rank)
    print(tmp_df)
    View(tmp_df)
    write.csv(tmp_df, str_glue("report/sa_provinces/all_regions-{estimate_type_filter}-{data_type_filter}-wave_{i}.csv"))
}
