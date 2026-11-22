## Point estimates (full-sample weights, robust SEs) using the corrected branch's own functions.
## Usage: Rscript harness.R <branch_dir> <targets_store_of_main_run> <out_rds>
args <- commandArgs(trailingOnly=TRUE); BR <- args[1]; STORE <- args[2]; OUT <- args[3]
suppressMessages({library(dplyr); library(readr); library(haven); library(tidyr); library(purrr); library(broom); library(lubridate)
  library(stringr); library(tibble); library(survey); library(fixest); library(AER); library(targets)})
options(survey.lonely.psu = "adjust", width=200)
source(file.path(BR, "pipeline_config.R")); source(file.path(BR, "estimation_pipeline_functions.R"))
rd <- function(x) tar_read_raw(x, store = STORE)
hces_distcodes <- rd("hces_distcodes"); nss_ind <- rd("nss_ind"); nss_heads <- rd("nss_heads")
df_hcs3 <- rd("df_hcs3"); df_hcs4 <- rd("df_hcs4"); df_hcs6 <- rd("df_hcs6")
hces_args <- lapply(c("hces_assets","hces_distcodes","hces_consumption","hces_ident","hces_roster","hces_types","level3_raw","level9_raw"), rd)
df_pop <- rd("df_pop"); utci_ewo <- rd("utci_ewo"); utci_lwo <- rd("utci_lwo")
fe_vec <- function(fit) { f <- fixef(fit)$pc11_sd_id; m <- matrix(f, ncol=1, dimnames=list(names(f), NULL)); m }
run_spec <- function(label, worker_sample="all", rent_min_n=10, late_pop_col="pop_23_proj", s=0.20, early_size=TRUE) {
  nss <- filter_nss(get_nss_data(nss_ind, hces_distcodes, nss_heads, worker_sample), opt_threshold)
  hcs <- filter_nss(get_housing_data_early(df_hcs3, df_hcs4, df_hcs6, hces_distcodes), rent_min_n)
  late <- get_housing_data_late(do.call(get_hces_merged_emp, c(hces_args, list(worker_sample=worker_sample))))
  ten <- late %>% filter(dwellingtype == 2, mrent > 0) %>% filter_nss(rent_min_n)
  we <- feols(logepc ~ poly(Age, 2) + male + edu + hindu + scst | pc11_sd_id, nss, weights=~Combined_multiplier, notes=FALSE, warn=FALSE)
  ref <- if (early_size) loghc ~ log_area + rooms + dwelling_type + pucca_walls + pucca_floor + pucca_roof + piped_water + own_latrine + elec | pc11_sd_id
         else loghc ~ dwelling_type + pucca_walls + pucca_floor + pucca_roof + piped_water + own_latrine + elec | pc11_sd_id
  re <- feols(ref, hcs, weights=~Wgt_combined, notes=FALSE, warn=FALSE)
  wl <- feols(logepc ~ poly(age, 2) + male + edu + hindu + scst | pc11_sd_id, late, weights=~mult, notes=FALSE, warn=FALSE)
  rl <- feols(loghc ~ pucca_walls + pucca_floor + pucca_roof + cooking_fuel + lighting_source + piped_water + own_latrine | pc11_sd_id, ten, weights=~mult, notes=FALSE, warn=FALSE)
  ce <- merge_pop_plot_data(get_combined_iter_coef(fe_vec(we), fe_vec(re)), df_pop, recent_data=FALSE)
  cl <- merge_pop_plot_data(get_combined_iter_coef(fe_vec(wl), fe_vec(rl)), df_pop, recent_data=TRUE, late_pop_col=late_pop_col)
  jd <- get_joined_differentials(ce, cl, s)
  des_e <- svydesign(ids=~FSU_Serial_no, strata=~nss_interaction, weights=~Combined_multiplier, data=get_nss_interaction(nss), nest=TRUE)
  des_l <- svydesign(ids=~fsu, strata=~hces_interaction, weights=~mult, data=get_hces_interaction(late), nest=TRUE)
  d <- get_bootstrap_data_redux(jd, utci_ewo, utci_lwo, get_district_controls(des_e, des_l))
  ## pipeline statistic at full sample
  v <- get_mwtp_piecewise(d, seq_len(nrow(d)), s)
  ## QOL regressions with robust SEs; cumulative band values
  band <- function(per) {
    d$Q <- s*d[[paste0("estimate_rent_",per)]] - d[[paste0("estimate_wage_",per)]]
    f <- as.formula(paste0("Q ~ hindu_",per," + scst_",per," + Age_",per," + educ_hs_",per," + educ_ps_",per,
                           " + days_above_26_",per," + days_above_32_",per," + days_above_38_",per," | state_early"))
    m <- feols(f, d, weights=as.formula(paste0("~pop_",per)), vcov="hetero", notes=FALSE)
    b <- coef(m)[grep("days", names(coef(m)))]; V <- vcov(m)[names(b), names(b)]
    L <- rbind(c(1,0,0), c(1,1,0), c(1,1,1)); est <- drop(L %*% b); se <- sqrt(diag(L %*% V %*% t(L)))
    data.frame(spec=label, period=per, band=c("26-32","32-38",">38"), mwtp=est, se=se)
  }
  grad <- function(y, per) {
    f <- as.formula(paste0(y, "_", per, " ~ hindu_",per," + scst_",per," + Age_",per," + educ_hs_",per," + educ_ps_",per,
                           " + days_above_26_",per," + days_above_32_",per," + days_above_38_",per," | state_early"))
    m <- feols(f, d, weights=as.formula(paste0("~pop_",per)), vcov="hetero", notes=FALSE)
    b <- coef(m)[grep("days", names(coef(m)))]; V <- vcov(m)[names(b), names(b)]
    L <- rbind(c(1,0,0), c(1,1,0), c(1,1,1)); est <- drop(L %*% b); se <- sqrt(diag(L %*% V %*% t(L)))
    data.frame(spec=label, outcome=y, period=per, band=c("26-32","32-38",">38"), b=est, se=se)
  }
  iv <- ivreg(delta_logpop ~ delta_netwage | Unsuitable_Slope_Share + Internal_Water_Share + bartik_shock, data=d)
  fs <- lm(delta_netwage ~ Unsuitable_Slope_Share + Internal_Water_Share + bartik_shock, data=d)
  list(label=label, n=nrow(d), n_fe=c(wage_early=length(fixef(we)$pc11_sd_id), rent_early=length(fixef(re)$pc11_sd_id),
       wage_late=length(fixef(wl)$pc11_sd_id), rent_late=length(fixef(rl)$pc11_sd_id)), samples=c(early_hh=nobs(we), early_tenants=nobs(re), late_hh=nobs(wl), late_tenants=nobs(rl)),
       pipeline_vec=v, bands=rbind(band("early"), band("late")),
       grads=rbind(grad("estimate_wage","early"), grad("estimate_rent","early"), grad("estimate_wage","late"), grad("estimate_rent","late")),
       eps=coef(iv)["delta_netwage"], eps_se=sqrt(vcov(iv)["delta_netwage","delta_netwage"]), fsF=summary(fs)$fstatistic[1],
       persist=c(wage=cor(d$estimate_wage_early, d$estimate_wage_late), rent=cor(d$estimate_rent_early, d$estimate_rent_late)),
       rent_coefs=list(early=coeftable(re)[1:4,], late=coeftable(rl)[1:4,]), data=d)
}
specs <- list(
  run_spec("Corrected (main)"),
  run_spec("Regular-wage households", worker_sample="regular_wage"),
  run_spec("No tenant minimum", rent_min_n=0),
  run_spec("WorldPop for 2023", late_pop_col="pop_23_wp"),
  run_spec("Housing share 0.15", s=0.15),
  run_spec("Early rent without size", early_size=FALSE)
)
saveRDS(specs, OUT)
for (r in specs) {
  cat("\n=====", r$label, " | second-stage districts:", r$n, " | FE counts:", paste(names(r$n_fe), r$n_fe, collapse=", "),
      " | eps =", round(r$eps,3), "(se", round(r$eps_se,3), ") first-stage F =", round(r$fsF,2),
      " | persistence wage/rent:", round(r$persist,2), "\n")
  print(r$bands %>% mutate(across(c(mwtp,se), ~round(.x,5))), row.names=FALSE)
}
cat("\nGradients (cumulative per-day effect by band) for the main spec:\n")
print(specs[[1]]$grads %>% mutate(across(c(b,se), ~round(.x,5))), row.names=FALSE)
