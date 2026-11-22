## (1) Does the per-person consumption measure drive the wage gradient? (2) Actual wages with controls.
suppressMessages({library(dplyr); library(readr); library(haven); library(tidyr); library(purrr); library(broom); library(lubridate); library(stringr); library(tibble); library(survey); library(targets); library(fixest, lib.loc=Sys.getenv("R_LIBS_USER"))})
W <- commandArgs(trailingOnly=TRUE)[1]; ST <- commandArgs(trailingOnly=TRUE)[2]; options(width=220)
BR <- file.path(W, "../repo_clone/india_heat_project_code")
source(file.path(BR, "pipeline_config.R")); source(file.path(BR, "estimation_pipeline_functions.R"))
R <- readRDS(file.path(W, "root_cause_data.rds")); d0 <- R$specs[["No tenant minimum"]]
rd <- function(x) tar_read_raw(x, store = ST)
nss <- filter_nss(get_nss_data(rd("nss_ind"), rd("hces_distcodes"), rd("nss_heads"), "all"), opt_threshold)
late <- get_housing_data_late(do.call(get_hces_merged_emp, c(lapply(c("hces_assets","hces_distcodes","hces_consumption","hces_ident","hces_roster","hces_types","level3_raw","level9_raw"), rd), list(worker_sample="all"))))
fe <- function(fit) { f <- fixef(fit)$pc11_sd_id; data.frame(district_early = names(f), v = as.numeric(f)) }
## first-stage variants
fs <- list(
  late_pc      = feols(logepc ~ poly(age,2) + male + edu + hindu + scst | pc11_sd_id, late, weights=~mult, notes=FALSE),
  late_pc_hh   = feols(logepc ~ poly(age,2) + male + edu + hindu + scst + log(hhsize) | pc11_sd_id, late, weights=~mult, notes=FALSE),
  late_tot_hh  = feols(log(totexp) ~ poly(age,2) + male + edu + hindu + scst + log(hhsize) | pc11_sd_id, late, weights=~mult, notes=FALSE),
  early_pc     = feols(logepc ~ poly(Age,2) + male + edu + hindu + scst | pc11_sd_id, nss, weights=~Combined_multiplier, notes=FALSE),
  early_pc_hh  = feols(logepc ~ poly(Age,2) + male + edu + hindu + scst + log(num_hhmem) | pc11_sd_id, nss, weights=~Combined_multiplier, notes=FALSE))
cat("log household size coefficient in per-person consumption: late", round(coef(fs$late_pc_hh)["log(hhsize)"],3), " early", round(coef(fs$early_pc_hh)["log(num_hhmem)"],3), "\n")
## district mean household size vs heat
hh <- late %>% group_by(pc11_sd_id) %>% summarise(mean_hhsize = weighted.mean(hhsize, mult), .groups="drop")
d <- d0 %>% left_join(hh, by=c("district_early"="pc11_sd_id"))
for (nm in names(fs)) d <- d %>% left_join(fe(fs[[nm]]) %>% rename(!!nm := v), by="district_early")
est38 <- function(m, per) { v <- paste0(c("days_above_26_","days_above_32_","days_above_38_"), per); c(30*sum(coef(m)[v]), 30*sqrt(sum(vcov(m)[v,v]))) }
ctl <- function(per) paste0("hindu_",per," + scst_",per," + Age_",per," + educ_hs_",per," + educ_ps_",per," + days_above_26_",per," + days_above_32_",per," + days_above_38_",per)
g <- function(y, per) { m <- feols(as.formula(paste0(y, " ~ ", ctl(per), " | state_early")), d, weights=as.formula(paste0("~pop_",per)), vcov="hetero", notes=FALSE); e <- est38(m, per); sprintf("%7.3f (%5.3f)", e[1], e[2]) }
cat("\nEffect of 30 more days above 38 C on district differentials (state FE, district controls, pop weights), 413 districts\n")
cat("  mean household size (late):            ", g("mean_hhsize","late"), "\n")
cat("  late: per-person consumption (pipeline):", g("late_pc","late"), "\n")
cat("  late: per-person, + log household size: ", g("late_pc_hh","late"), "\n")
cat("  late: household total, + log hh size:   ", g("late_tot_hh","late"), "\n")
cat("  early: per-person consumption:          ", g("early_pc","early"), "\n")
cat("  early: per-person, + log household size:", g("early_pc_hh","early"), "\n")
## QOL with household-size-adjusted wage
d$Q_late_hh <- 0.2*d$estimate_rent_late - d$late_pc_hh
cat("  late QOL using the household-size-adjusted wage:", g("Q_late_hh","late"), "\n")
## (2) actual wages with the control blocks
A <- R$alt %>% rename(pop_w = pop11.x) %>% mutate(urban_share = ifelse(urban_share > 1, NA, urban_share), pm25 = ifelse(pm25 <= 0, NA, pm25), log_area = log(Shape_Area))
blocks <- c(base = "", geography = "+ coast_dist + Unsuitable_Slope_Share + Internal_Water_Share + log_area",
            all = "+ coast_dist + Unsuitable_Slope_Share + Internal_Water_Share + log_area + log_urban_pop + urban_share + ec_emp_pc + manuf_share + hired_share + ag_share + lit + log(pm25)")
cat("\nActual wages and tenant hedonic rents (30 more days above 38 C)\n")
for (y in c("wage_eus","wage_plfs","rent_hedonic","rent76_hed")) {
  per <- ifelse(y %in% c("wage_eus","rent_hedonic"), "e", "l"); vv <- paste0(c("c26_","c32_","c38_"), per)
  dA <- A[complete.cases(A[, c(y, "coast_dist","log_urban_pop","urban_share","ec_emp_pc","manuf_share","hired_share","ag_share","lit","pm25")]), ]
  out <- sapply(names(blocks), function(bn) { m <- feols(as.formula(paste0(y, " ~ ", paste(vv, collapse=" + "), " ", blocks[[bn]], " | state")), dA, weights=~pop_w, vcov="hetero", notes=FALSE)
    sprintf("%s %7.3f (%5.3f)", bn, 30*sum(coef(m)[vv]), 30*sqrt(sum(vcov(m)[vv,vv]))) })
  cat(sprintf("  %-13s N=%3d | %s\n", y, nrow(dA), paste(out, collapse=" | ")))
}
