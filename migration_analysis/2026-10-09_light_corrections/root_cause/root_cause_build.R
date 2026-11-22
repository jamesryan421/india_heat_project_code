## Build district covariates and merge with the corrected second-stage data (main and no-minimum specs)
suppressMessages({library(haven); library(dplyr); library(readr)})
W <- commandArgs(trailingOnly=TRUE)[1]
B <- "/Users/tanmaydevi/Library/CloudStorage/Box-Box/Urban Economics India"
specs <- readRDS(file.path(W, "harness_specs.rds"))
S <- readRDS(file.path(W, "samples.rds")); hdc <- S$hces_distcodes
plfs2pc11 <- hdc %>% filter(!is.na(plfscode)) %>% transmute(plfscode = as.integer(plfscode), pc11_sd_id)
## Economic Census 2013 (all non-farm establishments), pc11 district
ec <- read_dta(file.path(B, "Data/RawData/Economic Census 2013/shrug-ec13-dta/ec13_pc11dist.dta")) %>%
  transmute(pc11_sd_id = sprintf("%02d-%03d", as.integer(pc11_state_id), as.integer(pc11_district_id)),
            ec_emp = ec13_emp_all, ec_manuf = ec13_emp_manuf, ec_serv = ec13_emp_services, ec_hired = ec13_emp_hired, ec_gov = ec13_emp_gov)
## Census 2011 PCA (district totals)
pca <- read_dta(file.path(B, "Data/RawData/SHRUG CENSUS 2011/pc11_pca_clean_pc11dist.dta")) %>%
  transmute(pc11_sd_id = sprintf("%02d-%03d", as.integer(pc11_state_id), as.integer(pc11_district_id)),
            pop11 = pc11_pca_tot_p, lit = pc11_pca_p_lit / (pc11_pca_tot_p - pc11_pca_p_06),
            ag_share = (pc11_pca_main_cl_p + pc11_pca_main_al_p) / pc11_pca_mainwork_p,
            nonfarm_share = pc11_pca_main_ot_p / pc11_pca_mainwork_p)
## Urban population 2011 (plfscode keyed; sum over post-2011 children mapped to the same 2011 district)
up <- read_csv(file.path(B, "Code/Python/output/district_urban_pop_harmonized.csv"), show_col_types = FALSE) %>%
  transmute(plfscode = as.integer(district_id_2011), urban_pop_2011) %>% inner_join(plfs2pc11, by = "plfscode") %>%
  group_by(pc11_sd_id) %>% summarise(urban_pop = sum(urban_pop_2011, na.rm = TRUE), .groups = "drop")
## PM2.5, mean 2010-2022 (plfscode keyed)
pm <- read_csv(file.path(B, "Data/ProcessedData/Intermediate/pm25_annual_district.csv"), show_col_types = FALSE) %>%
  filter(year >= 2010, year <= 2022) %>% transmute(plfscode = as.integer(district_id_2011), pm25 = pm25_annual_mean) %>%
  inner_join(plfs2pc11, by = "plfscode") %>% group_by(pc11_sd_id) %>% summarise(pm25 = mean(pm25, na.rm = TRUE), .groups = "drop")
cov <- pca %>% left_join(ec, by = "pc11_sd_id") %>% left_join(up, by = "pc11_sd_id") %>% left_join(pm, by = "pc11_sd_id") %>%
  mutate(log_urban_pop = log(urban_pop), urban_share = urban_pop / pop11, ec_emp_pc = ec_emp / pop11,
         manuf_share = ec_manuf / ec_emp, hired_share = ec_hired / ec_emp, log_ec_emp = log(ec_emp))
out <- lapply(specs, function(r) r$data %>% left_join(cov, by = c("district_early" = "pc11_sd_id")))
names(out) <- sapply(specs, `[[`, "label")
## also the actual-wage panel built earlier (EUS / PLFS wage FEs, tenant hedonic rents NSS 65 / NSS 76)
alt <- readRDS(file.path(W, "alt_panel.rds")) %>% left_join(cov, by = "pc11_sd_id")
saveRDS(list(specs = out, alt = alt, cov = cov), file.path(W, "root_cause_data.rds"))
d <- out[["No tenant minimum"]]
cat("districts (no-minimum spec):", nrow(d), "; covariate coverage:\n")
print(sapply(d[, c("ec_emp_pc","manuf_share","hired_share","log_urban_pop","urban_share","ag_share","lit","pm25")], function(x) sum(!is.na(x))))
print(summary(d[, c("ec_emp_pc","manuf_share","log_urban_pop","urban_share","ag_share","pm25")]))
