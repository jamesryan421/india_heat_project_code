suppressMessages({library(dplyr); library(readr); library(data.table)})
W <- commandArgs(trailingOnly=TRUE)[1]; O <- commandArgs(trailingOnly=TRUE)[2]
S <- readRDS(file.path(W, "samples.rds")); U <- readRDS(file.path(W, "utci.rds")); R <- readRDS(file.path(W, "root_cause_data.rds"))
## lookup: survey district codes -> pc11 (nsscode for NSS rounds, plfscode for PLFS)
write_csv(S$hces_distcodes %>% transmute(state, district, nsscode, plfscode, pc11_sd_id, state_name, district_name), file.path(O, "district_lookup.csv"))
## destination attributes
A <- R$alt %>% transmute(pc11_sd_id, wage_eus, wage_plfs, cons_early, cons_late, rent_hedonic, rent76_hed)
cov <- R$cov %>% transmute(pc11_sd_id, pop11, urban_pop, log_urban_pop, ec_emp_pc, manuf_share, ag_share, lit, pm25)
geo <- read_csv("/Users/tanmaydevi/Library/CloudStorage/Box-Box/Urban Economics India/india_pipeline_prereqs/geodata/district_slope_water_instruments.csv", show_col_types=FALSE) %>%
  transmute(pc11_sd_id, coast_dist = NEAR_DIST, slope = Unsuitable_Slope_Share, water = Internal_Water_Share, area = Shape_Area)
heat <- function(x, sfx) { x <- as.data.frame(x)[, c("pc11_sd_id","c26","c32","c38","mean_utci")]; names(x)[-1] <- paste0(names(x)[-1], sfx); x }
H <- heat(U$early_alt, "_9908") %>% full_join(heat(U$late, "_1222"), by="pc11_sd_id") %>% full_join(heat(U$early, "_0111"), by="pc11_sd_id")
D <- H %>% left_join(A, by="pc11_sd_id") %>% left_join(cov, by="pc11_sd_id") %>% left_join(geo, by="pc11_sd_id")
write_csv(D, file.path(O, "destination_attributes.csv"))
cat("destinations:", nrow(D), "\n"); print(colSums(!is.na(D[, c("wage_eus","wage_plfs","cons_early","cons_late","rent_hedonic","rent76_hed","log_urban_pop","pm25","coast_dist")])))
