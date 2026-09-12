# Temperature trends sketchbook

# Assumes project is already open
library(tidyverse)
library(lubridate)
library(trend)
library(here)

# Duplicate function from pipeline: convert UTCI from K to C
fix_utci_celsius=function(utci_daily){
  # Convert UTCI observations to Celsius from Kelvin
  return(utci_daily %>%
           mutate(utci = utci - 273.15))
}

# utci_daily <- tar_read(utci_daily)
# 
# # Test with just one district for now
# test_sd_id = "24-468"
# 
# utci_test <- utci_daily %>%
#   filter(pc11_sd_id == test_sd_id)
# 
# head(utci_test)
# 
# # Method 1: Generalized Additive Model (GAM)
# # VERY SLOW
# library(mgcv)
# library(lubridate)
# 
# utci_test <- utci_test %>% 
#   mutate(
#     year = year(date),
#     day_of_year = yday(date),
#     numeric_date = as.numeric(date)
#   )
# 
# gam_model <- gam(
#   utci ~ s(day_of_year, bs = "cc", k = 365) + s(numeric_date, k = 20),
#   data = utci_test,
#   method = "REML"
# )
# 
# # Method 2: STL Decomposition
# library(feasts)
# library(fable)
# library(tsibble)
# 
# ts_data <- utci_test %>% 
#   as_tsibble(key = pc11_sd_id, index = date) %>%
#   fill_gaps() %>%
#   mutate(utci = zoo::na.approx(utci, na.rm=F))
# 
# stl_fit <- ts_data %>%
#   model(STL(utci ~ season(period = "1 year", window = "periodic") + trend(window = 365 * 5))) %>%
#   components()
# 
# autoplot(stl_fit, trend)

# Method 3: Aggregation to annual values and fitting Mann-Kendall Trend

mk_helper <- function(x){
  mk <- mk.test(x$mean_utci)
  sens <- sens.slope(x$mean_utci)
  list_to_return = c(
    "mk_pval" = mk$pvalg,
    "tau" = mk$estimates[["tau"]],
    "slope" = sens$estimates[["Sen's slope"]],
    "slope_ci_lower" = sens$conf.int[1],
    "slope_ci_upper" = sens$conf.int[2]
  )
  return(data.frame(t(list_to_return)))
}

get_mk_trends <- function(utci_daily){
  # Get annual summaries
  annual_summaries <- utci_daily %>%
    mutate(year = year(date)) %>%
    group_by(pc11_sd_id, d_name, year) %>%
    summarize(mean_utci = mean(utci, na.rm=T), .groups = "drop")
  
  # Get three tables for Mann-Kendall trends: the full slate, pre-2000, and post-2000
  mk_trends_full <- annual_summaries %>%
    group_by(pc11_sd_id, d_name) %>%
    group_modify(~ mk_helper(.x)) %>%
    ungroup()
  
  mk_trends_pre_2000 <- annual_summaries %>%
    filter(year < 2000) %>%
    group_by(pc11_sd_id, d_name) %>%
    group_modify(~ mk_helper(.x)) %>%
    ungroup()
  
  mk_trends_post_2000 <- annual_summaries %>%
    filter(year >= 2000) %>%
    group_by(pc11_sd_id, d_name) %>%
    group_modify(~ mk_helper(.x)) %>%
    ungroup()
  
  list_to_return = list(
    "full" = mk_trends_full,
    "pre_2000" = mk_trends_pre_2000,
    "post_2000" = mk_trends_post_2000
  )
  return(list_to_return)
}

# Get all three UTCI files

get_filename_str=function(filename_here){return(as.character(filename_here))}

main_data_path <- file.path("C:","Users","jdr42","Documents","Projects","india_heat_project")
temperature_output_path <- file.path(main_data_path,"temperature_data","output")

utci_mean <- read_csv(get_filename_str(file.path(temperature_output_path, "district_daytime_mean_utci_full.csv.gz")))
utci_max <- read_csv(get_filename_str(file.path(temperature_output_path, "district_daily_max_utci_full.csv.gz")))
utci_hottest_mean <- read_csv(get_filename_str(file.path(temperature_output_path, "district_daytime_hottest_mean_utci_full.csv.gz")))

mk_trends_utci_mean <- get_mk_trends(fix_utci_celsius(utci_mean))
mk_trends_utci_max <- get_mk_trends(fix_utci_celsius(utci_max %>% rename(utci = max_utci)))
mk_trends_utci_hottest_mean <- get_mk_trends(fix_utci_celsius(utci_hottest_mean))

# Write out data
write_csv(mk_trends_utci_mean[[1]], file.path(temperature_output_path, "mk_trends_utci_mean_full.csv"))
write_csv(mk_trends_utci_mean[[2]], file.path(temperature_output_path, "mk_trends_utci_mean_pre_2000.csv"))
write_csv(mk_trends_utci_mean[[3]], file.path(temperature_output_path, "mk_trends_utci_mean_post_2000.csv"))

write_csv(mk_trends_utci_max[[1]], file.path(temperature_output_path, "mk_trends_utci_max_full.csv"))
write_csv(mk_trends_utci_max[[2]], file.path(temperature_output_path, "mk_trends_utci_max_pre_2000.csv"))
write_csv(mk_trends_utci_max[[3]], file.path(temperature_output_path, "mk_trends_utci_max_post_2000.csv"))

write_csv(mk_trends_utci_hottest_mean[[1]], file.path(temperature_output_path, "mk_trends_utci_hottest_mean_full.csv"))
write_csv(mk_trends_utci_hottest_mean[[2]], file.path(temperature_output_path, "mk_trends_utci_hottest_mean_pre_2000.csv"))
write_csv(mk_trends_utci_hottest_mean[[3]], file.path(temperature_output_path, "mk_trends_utci_hottest_mean_post_2000.csv"))

# Get a single unified table with slopes and indicators for whether it's statistically significant
list_to_join <- list(
  mk_trends_utci_mean[[1]]%>%
    rename("mean_fs" = "slope") %>%
    mutate(mfsig = (mk_pval < 0.05)) %>%
    select(c("pc11_sd_id", "d_name", "mean_fs", "mfsig")),
  mk_trends_utci_mean[[2]] %>%
    rename("mean_bs" = "slope") %>%
    mutate(mbsig = (mk_pval < 0.05)) %>%
    select(c("pc11_sd_id", "d_name","mean_bs", "mbsig")),
  mk_trends_utci_mean[[3]] %>%
    rename("mean_as" = "slope") %>%
    mutate(masig = (mk_pval < 0.05)) %>%
    select(c("pc11_sd_id", "d_name","mean_as", "masig")),
  mk_trends_utci_max[[1]] %>%
    rename("max_fs" = "slope") %>%
    mutate(xfsig = (mk_pval < 0.05)) %>%
    select(c("pc11_sd_id", "d_name", "max_fs", "xfsig")),
  mk_trends_utci_max[[2]] %>%
    rename("max_bs" = "slope") %>%
    mutate(xbsig = (mk_pval < 0.05)) %>%
    select(c("pc11_sd_id", "d_name", "max_bs", "xbsig")),
  mk_trends_utci_max[[3]] %>%
    rename("max_as" = "slope") %>%
    mutate(xasig = (mk_pval < 0.05)) %>%
    select(c("pc11_sd_id", "d_name", "max_as", "xasig")),
  mk_trends_utci_hottest_mean[[1]] %>%
    rename("hot_fs" = "slope") %>%
    mutate(hfsig = (mk_pval < 0.05)) %>%
    select(c("pc11_sd_id", "d_name", "hot_fs", "hfsig")),
  mk_trends_utci_hottest_mean[[2]] %>%
    rename("hot_bs" = "slope") %>%
    mutate(hbsig = (mk_pval < 0.05)) %>%
    select(c("pc11_sd_id", "d_name", "hot_bs", "hbsig")),
  mk_trends_utci_hottest_mean[[3]] %>%
    rename("hot_as" = "slope") %>%
    mutate(hasig = (mk_pval < 0.05)) %>%
    select(c("pc11_sd_id", "d_name", "hot_as", "hasig"))
)

full_slope_data <- list_to_join %>%
  reduce(inner_join, by = c("pc11_sd_id", "d_name"))

# Join on shape boundaries
library(sf)
geodata_path <- file.path(main_data_path,"geodata")
district_shapefile <- st_read(file.path(geodata_path, "Shape Files", "shrug-pc11dist-poly-shp", "district.shp"))

district_shapefile <- district_shapefile %>%
  mutate(pc11_sd_id = paste0(pc11_s_id, "-", pc11_d_id))

full_slope_shapefile <- district_shapefile %>%
  select(c("pc11_sd_id", "d_name", "geometry")) %>%
  left_join(full_slope_data, by = c("pc11_sd_id", "d_name"))

# Get a plot
plot_temp_trends <- function(slope_metric, title){
  g <- ggplot(full_slope_shapefile) + 
    geom_sf(aes(fill = ({{slope_metric}} * 10)), color = NA, linewidth = 0) + 
    scale_fill_viridis_c(option = "F", name = "°C / Decade") + 
    labs(
      title = title,
      caption = "Source: Copernicus CDS"
    ) + 
    theme_void() + 
    theme(
      plot.background = element_rect(fill = "white", color=NA)
    )
  return(g)
}

g_mean_full <- plot_temp_trends(mean_fs, "Daily Mean UTCI Trend, 1970-2025")
g_mean_before <- plot_temp_trends(mean_bs, "Daily Mean UTCI Trend, 1970-2000")
g_mean_after <- plot_temp_trends(mean_as, "Daily Mean UTCI Trend, 2000-2025")

g_max_full <- plot_temp_trends(max_fs, "Daily Max UTCI Trend, 1970-2025")
g_max_before <- plot_temp_trends(max_bs, "Daily Max UTCI Trend, 1970-2000")
g_max_after <- plot_temp_trends(max_as, "Daily Max UTCI Trend, 2000-2025")

g_hottest_full <- plot_temp_trends(hot_fs, "Daytime Avg. UTCI Trend, 1970-2025")
g_hottest_before <- plot_temp_trends(hot_bs, "Daytime Avg. UTCI Trend, 1970-2000")
g_hottest_after <- plot_temp_trends(hot_as, "Daytime Avg. UTCI Trend, 2000-2025")

vis_output_path <- file.path(main_data_path, "vis")
ggsave(file.path(vis_output_path, "mean_utci_trend_1970_2025.png"), g_mean_full, width = 6, height = 4, units = "in")
ggsave(file.path(vis_output_path, "mean_utci_trend_1970_2000.png"), g_mean_before, width = 6, height = 4, units = "in")
ggsave(file.path(vis_output_path, "mean_utci_trend_2000_2025.png"), g_mean_after, width = 6, height = 4, units = "in")

ggsave(file.path(vis_output_path, "max_utci_trend_1970_2025.png"), g_max_full, width = 6, height = 4, units = "in")
ggsave(file.path(vis_output_path, "max_utci_trend_1970_2000.png"), g_max_before, width = 6, height = 4, units = "in")
ggsave(file.path(vis_output_path, "max_utci_trend_2000_2025.png"), g_max_after, width = 6, height = 4, units = "in")

ggsave(file.path(vis_output_path, "mean_hottest_utci_trend_1970_2025.png"), g_hottest_full, width = 6, height = 4, units = "in")
ggsave(file.path(vis_output_path, "mean_hottest_utci_trend_1970_2000.png"), g_hottest_before, width = 6, height = 4, units = "in")
ggsave(file.path(vis_output_path, "mean_hottest_utci_trend_2000_2025.png"), g_hottest_after, width = 6, height = 4, units = "in")

# Multiplots
mean_long_slope_data <- full_slope_shapefile %>%
  select(c("pc11_sd_id", "d_name", "mean_fs", "mean_bs", "mean_as", "geometry")) %>%
  pivot_longer(
    cols = c("mean_fs", "mean_bs", "mean_as"),
    names_to = "period",
    names_prefix = "mean_",
    values_to = "slope_per_year"
  ) %>%
  mutate(
    period = case_when(
      period == "fs" ~ "Full Sample",
      period == "bs" ~ "Before 2000",
      period == "as" ~ "After 2000"
    ),
    period = factor(period, levels=c("Full Sample", "Before 2000", "After 2000")),
    slope_per_decade = (slope_per_year) * 10
  )

max_long_slope_data <- full_slope_shapefile %>%
  select(c("pc11_sd_id", "d_name", "max_fs", "max_bs", "max_as", "geometry")) %>%
  pivot_longer(
    cols = c("max_fs", "max_bs", "max_as"),
    names_to = "period",
    names_prefix = "max_",
    values_to = "slope_per_year"
  ) %>%
  mutate(
    period = case_when(
      period == "fs" ~ "Full Sample",
      period == "bs" ~ "Before 2000",
      period == "as" ~ "After 2000"
    ),
    period = factor(period, levels=c("Full Sample", "Before 2000", "After 2000")),
    slope_per_decade = (slope_per_year) * 10
  )

hottest_long_slope_data <- full_slope_shapefile %>%
  select(c("pc11_sd_id", "d_name", "hot_fs", "hot_bs", "hot_as", "geometry")) %>%
  pivot_longer(
    cols = c("hot_fs", "hot_bs", "hot_as"),
    names_to = "period",
    names_prefix = "hot_",
    values_to = "slope_per_year"
  ) %>%
  mutate(
    period = case_when(
      period == "fs" ~ "Full Sample",
      period == "bs" ~ "Before 2000",
      period == "as" ~ "After 2000"
    ),
    period = factor(period, levels=c("Full Sample", "Before 2000", "After 2000")),
    slope_per_decade = (slope_per_year) * 10
  )

get_multiplot <- function(data, title){
  multiplot <- ggplot(data) + 
    geom_sf(aes(fill = slope_per_decade), color = NA, linewidth = 0) +
    scale_fill_viridis_c(option = "F", name = "°C / Decade") + 
    facet_wrap(~ period, ncol = 3) +
    theme_void() + 
    theme(
      plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
      plot.subtitle = element_text(size = 11, hjust = 0.5, color = "gray30"),
      strip.text = element_text(size = 11, face = "bold"),
      legend.position = "bottom",
      legend.box = "horizontal",
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      plot.background = element_rect(fill = "white", color=NA)
    ) + 
    labs(
      title=title
    )
  return(multiplot)
}


mean_multiplot <- get_multiplot(mean_long_slope_data, "Daily Mean UTCI Trends, 1970-2025")
max_multiplot <- get_multiplot(max_long_slope_data, "Daily Max UTCI Trends, 1970-2025")
hottest_multiplot <- get_multiplot(hottest_long_slope_data, "Daytime Average UTCI Trends, 1970-2025")

ggsave(file.path(vis_output_path, "mean_utci_trend_multiplot.png"), mean_multiplot, width = 6, height = 4, units = "in")
ggsave(file.path(vis_output_path, "max_utci_trend_multiplot.png"), max_multiplot, width = 6, height = 4, units = "in")
ggsave(file.path(vis_output_path, "hottest_mean_utci_trend_multiplot.png"), hottest_multiplot, width = 6, height = 4, units = "in")


library(patchwork)

g_mean_full | g_mean_before | g_mean_after

# With shading for statistical significance
ggplot(full_slope_shapefile) + 
  geom_sf(aes(fill = (mean_as * 10), alpha = mfsig), color = NA, linewidth = 0) + 
  scale_fill_viridis_c(option = "F", name = "°C / Decade") + 
  scale_alpha_manual(
    values = c(`FALSE` = 0.25, `TRUE` = 1.0),
    labels = c("p ≥ 0.05", "p < 0.05"),
    name = "Significance"
  ) +
  labs(
    title = "Mean UTCI Trend, 1970-2025",
    caption = "Source: Copernicus CDS"
  ) + 
  theme_minimal()

# Exploration
test_district = "Mumbai"
trend_index = 2

mk_trends_utci_mean[[trend_index]] %>% filter(mk_pval < 0.05) %>% filter(d_name == test_district)
mk_trends_utci_max[[trend_index]] %>% filter(mk_pval < 0.05) %>% filter(d_name == test_district)
mk_trends_utci_hottest_mean[[trend_index]] %>% filter(mk_pval < 0.05) %>% filter(d_name == test_district)

# Proportions which are statistically significant
unlist(sapply(mk_trends_utci_mean, function(x){x %>% summarize(signif_pct = mean(mk_pval < 0.05))}))
unlist(sapply(mk_trends_utci_max, function(x){x %>% summarize(signif_pct = mean(mk_pval < 0.05))}))
unlist(sapply(mk_trends_utci_hottest_mean, function(x){x %>% summarize(signif_pct = mean(mk_pval < 0.05))}))

# Proportions which are statistically significant *and* have slopes above 0.1C/decade
unlist(sapply(mk_trends_utci_mean, function(x){x %>% summarize(signif_pct = mean((mk_pval < 0.05) & (slope >= 0.01)))}))
unlist(sapply(mk_trends_utci_max, function(x){x %>% summarize(signif_pct = mean((mk_pval < 0.05) & (slope >= 0.01)))}))
unlist(sapply(mk_trends_utci_hottest_mean, function(x){x %>% summarize(signif_pct = mean((mk_pval < 0.05) & (slope >= 0.01)))}))

# Proportions which are statistically significant and have negative slopes
unlist(sapply(mk_trends_utci_mean, function(x){x %>% summarize(signif_pct = mean((mk_pval < 0.05) & (slope < 0)))}))
unlist(sapply(mk_trends_utci_max, function(x){x %>% summarize(signif_pct = mean((mk_pval < 0.05) & (slope < 0)))}))
unlist(sapply(mk_trends_utci_hottest_mean, function(x){x %>% summarize(signif_pct = mean((mk_pval < 0.05) & (slope < 0)))}))


mk_trends_utci_mean[[1]] %>%
  mutate(slope_decade = (slope * 10)) %>% 
  descr(slope_decade)

mk_trends_full %>%
  filter(mk_pval < 0.05) %>%
  arrange(-slope)

mk_trends_since_2000 %>%
  filter(mk_pval < 0.05) %>%
  arrange(-slope)

mk_trends_since_2000 %>%
  filter(d_name == "Mumbai")

mk_trends_full %>%
  summarize(signif_pct = mean(mk_pval < 0.05))

# Add a ridge plot
library(ggridges)

# Observed UTCI
annual_summaries %>%
  mutate(decade = factor(floor(year / 10) * 10)) %>%
  ggplot(aes(x = mean_utci, y = decade, fill = after_stat(x))) + 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) + 
  scale_fill_viridis_c(option = "plasma", name = "Temp (°C)") +
  theme_minimal() +
  labs(x = "Mean Daily Temperature (°C)", y = "Decade", title = "Decadal Shift in Indian City Temperatures")

# Trend distribution
mk_trends_since_2000 %>%
  ggplot(aes(x = slope)) +
  geom_histogram()

mk_trends_full %>%
  ggplot(aes(x = slope)) + 
  geom_histogram()
