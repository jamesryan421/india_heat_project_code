## Root-cause analysis: what explains the negative heat gradient in wages and the positive heat "MWTP"?
suppressMessages({library(dplyr); library(readr); library(fixest, lib.loc=Sys.getenv("R_LIBS_USER"))})
W <- commandArgs(trailingOnly=TRUE)[1]; options(width=220)
R <- readRDS(file.path(W, "root_cause_data.rds"))
geo <- read_csv("/Users/tanmaydevi/Library/CloudStorage/Box-Box/Urban Economics India/india_pipeline_prereqs/geodata/district_slope_water_instruments.csv", show_col_types=FALSE) %>% select(pc11_sd_id, Shape_Area)
s <- 0.20
prep <- function(d) {
  d %>% left_join(geo, by=c("district_early"="pc11_sd_id")) %>%
    mutate(urban_share = ifelse(urban_share > 1, NA, urban_share), pm25 = ifelse(pm25 <= 0, NA, pm25), log_area = log(Shape_Area),
           Q_late = s*estimate_rent_late - estimate_wage_late, Q_early = s*estimate_rent_early - estimate_wage_early)
}
blocks <- list(
  "base"        = "",
  "+geography"  = "+ coast_dist + Unsuitable_Slope_Share + Internal_Water_Share + log_area",
  "+city size"  = "+ coast_dist + Unsuitable_Slope_Share + Internal_Water_Share + log_area + log_urban_pop + urban_share",
  "+economy"    = "+ coast_dist + Unsuitable_Slope_Share + Internal_Water_Share + log_area + log_urban_pop + urban_share + ec_emp_pc + manuf_share + hired_share + ag_share",
  "+literacy, PM2.5" = "+ coast_dist + Unsuitable_Slope_Share + Internal_Water_Share + log_area + log_urban_pop + urban_share + ec_emp_pc + manuf_share + hired_share + ag_share + lit + log(pm25)")
allc <- c("coast_dist","Unsuitable_Slope_Share","Internal_Water_Share","log_area","log_urban_pop","urban_share","ec_emp_pc","manuf_share","hired_share","ag_share","lit","pm25")
est38 <- function(m, per) {  # effect of 30 extra days above 38 C (moving days from below 26), cumulative over the three variables
  v <- paste0(c("days_above_26_","days_above_32_","days_above_38_"), per); b <- coef(m)[v]; V <- vcov(m)[v, v]
  c(est = 30*sum(b), se = 30*sqrt(sum(V)))
}
fmt <- function(x) sprintf("%7.3f (%5.3f)", x[1], x[2])
run_table <- function(d, per, label) {
  ctl <- paste0("hindu_",per," + scst_",per," + Age_",per," + educ_hs_",per," + educ_ps_",per,
                " + days_above_26_",per," + days_above_32_",per," + days_above_38_",per)
  dd <- d[complete.cases(d[, allc]), ]
  cat(sprintf("\n%s, %s period: %d districts with all covariates (effect of 30 more days above 38 C, robust SE)\n", label, per, nrow(dd)))
  cat(sprintf("%-18s %-18s %-18s %-18s\n", "controls", "log wage (cons.)", "log rent", "QOL = 0.2 rent - wage"))
  for (bn in names(blocks)) {
    f <- function(y) feols(as.formula(paste0(y, "_", per, " ~ ", ctl, " ", blocks[[bn]], " | state_early")), dd, weights=as.formula(paste0("~pop_",per)), vcov="hetero", notes=FALSE)
    cat(sprintf("%-18s %-18s %-18s %-18s\n", bn, fmt(est38(f("estimate_wage"),per)), fmt(est38(f("estimate_rent"),per)), fmt(est38(f("Q"),per))))
  }
  invisible(dd)
}
d_nomin <- prep(R$specs[["No tenant minimum"]]); d_main <- prep(R$specs[["Corrected (main)"]])
cat("=== A. How hot districts differ (state FE, pop-weighted): coefficient of 30 more days above 38 C on each covariate (late window)\n")
for (v in c("log_urban_pop","urban_share","ec_emp_pc","manuf_share","hired_share","ag_share","lit","coast_dist","pm25")) {
  m <- feols(as.formula(paste0(v, " ~ days_above_26_late + days_above_32_late + days_above_38_late | state_early")), d_nomin, weights=~pop_late, vcov="hetero", notes=FALSE)
  e <- est38(m, "late"); cat(sprintf("  %-14s mean %8.3f | 30 more days >38: %8.3f (se %.3f)\n", v, mean(d_nomin[[v]], na.rm=TRUE), e[1], e[2]))
}
cat("\n=== B. Adding controls step by step\n")
dd_late <- run_table(d_nomin, "late", "No tenant minimum")
dd_early <- run_table(d_nomin, "early", "No tenant minimum")
run_table(d_main, "late", "Main (10-tenant minimum)")

cat("\n=== C. Gelbach decomposition of the change in the wage effect (late, no-minimum sample, base -> full)\n")
per <- "late"; ctl <- paste0("hindu_",per," + scst_",per," + Age_",per," + educ_hs_",per," + educ_ps_",per," + days_above_26_",per," + days_above_32_",per," + days_above_38_",per)
dd <- dd_late %>% mutate(log_pm25 = log(pm25))
xs <- c("coast_dist","Unsuitable_Slope_Share","Internal_Water_Share","log_area","log_urban_pop","urban_share","ec_emp_pc","manuf_share","hired_share","ag_share","lit","log_pm25")
grp <- c(rep("geography",4), rep("city size",2), rep("economic structure",4), "literacy", "PM2.5")
mb <- feols(as.formula(paste0("estimate_wage_late ~ ", ctl, " | state_early")), dd, weights=~pop_late, notes=FALSE)
mf <- feols(as.formula(paste0("estimate_wage_late ~ ", ctl, " + ", paste(xs, collapse=" + "), " | state_early")), dd, weights=~pop_late, notes=FALSE)
v <- paste0(c("days_above_26_","days_above_32_","days_above_38_"), per)
contrib <- sapply(xs, function(x) {
  g <- feols(as.formula(paste0(x, " ~ ", ctl, " | state_early")), dd, weights=~pop_late, notes=FALSE)
  30*sum(coef(g)[v]) * coef(mf)[x]
})
cat(sprintf("  base effect %.3f ; full effect %.3f ; change %.3f (sum of contributions %.3f)\n", 30*sum(coef(mb)[v]), 30*sum(coef(mf)[v]), 30*sum(coef(mb)[v]) - 30*sum(coef(mf)[v]), sum(contrib)))
print(round(tapply(contrib, grp, sum), 3))
cat("  largest single contributions:\n"); print(round(sort(contrib, decreasing=TRUE)[1:5], 3))

cat("\n=== D. Concentrating the sample (late period, base controls; no-minimum sample)\n")
ctlL <- paste0("hindu_late + scst_late + Age_late + educ_hs_late + educ_ps_late + days_above_26_late + days_above_32_late + days_above_38_late")
sub <- list("all districts" = d_nomin,
            "urban pop >= 0.5m" = d_nomin %>% filter(urban_pop >= 5e5),
            "urban pop >= 1m" = d_nomin %>% filter(urban_pop >= 1e6),
            "plains (slope share < 0.1)" = d_nomin %>% filter(Unsuitable_Slope_Share < 0.1),
            "plains & urban pop >= 0.5m" = d_nomin %>% filter(Unsuitable_Slope_Share < 0.1, urban_pop >= 5e5),
            "inland (coast dist > 100)" = d_nomin %>% filter(coast_dist > 100))
cat(sprintf("%-28s %4s %-18s %-18s %-18s\n", "sample", "N", "log wage (cons.)", "log rent", "QOL"))
for (sn in names(sub)) {
  ds <- sub[[sn]]
  f <- function(y) feols(as.formula(paste0(y, " ~ ", ctlL, " | state_early")), ds, weights=~pop_late, vcov="hetero", notes=FALSE)
  cat(sprintf("%-28s %4d %-18s %-18s %-18s\n", sn, nrow(ds), fmt(est38(f("estimate_wage_late"),"late")), fmt(est38(f("estimate_rent_late"),"late")), fmt(est38(f("Q_late"),"late"))))
}

cat("\n=== E. Same steps with actual wages (PLFS pooled wage FE, late) and NSS 76 tenant hedonic rents\n")
A <- R$alt %>% mutate(urban_share = ifelse(urban_share > 1, NA, urban_share), pm25 = ifelse(pm25 <= 0, NA, pm25), log_area = log(Shape_Area))
for (y in c("wage_plfs","rent76_hed","wage_eus")) {
  per <- ifelse(y == "wage_eus", "e", "l")
  base <- paste0("c26_",per," + c32_",per," + c38_",per)
  dA <- A[complete.cases(A[, c(y, allc)]), ]
  out <- sapply(names(blocks), function(bn) {
    m <- feols(as.formula(paste0(y, " ~ ", base, " ", blocks[[bn]], " | state")), dA, weights=~pop11, vcov="hetero", notes=FALSE)
    vv <- paste0(c("c26_","c32_","c38_"), per); fmt(c(30*sum(coef(m)[vv]), 30*sqrt(sum(vcov(m)[vv,vv]))))
  })
  cat(sprintf("%-12s N=%3d | %s\n", y, nrow(dA), paste(names(blocks), out, sep=": ", collapse=" | ")))
}
