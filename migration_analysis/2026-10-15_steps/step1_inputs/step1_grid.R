## Step 1 (15 Oct 2026): value of 30 more hot days with theta fixed, before and after the input fixes.
## Variant 0 reproduces the 11 Oct grid (heat_project_summary_figures/data/theta_grid_heat_value.csv); each later variant
## adds one change, so every difference can be traced to a single fix.
## Value = 30 * heat coefficient / theta (share of income; negative = heat is a cost), Poisson with offset theta * wage,
## origin-state x survey fixed effects, destination-clustered standard errors.
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; B4F <- args[3]; OUT <- args[4]; OLDGRID <- args[5]
source("mig_build_v2.R")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
SY <- 0.20   # housing expenditure share of urban tenants (HCES 2022-23), as in pipeline_config.R
thetas <- c(0.65, 1, 2, 3, 4, 5, 8, 12.9, 17.2)
ctl_old <- " + coast_dist + slope + log(pm25) + manuf_share + ec_emp_pc"
ctl_new <- " + coast_dist + slope + lpm + pm_miss + manuf_share + ec_emp_pc"
vars_old <- c("N", "w", "heat", "log_dist", "same_state", "log_urban_pop", "coast_dist", "slope", "pm25", "manuf_share", "ec_emp_pc")
vars_new <- c("N", "w", "heat", "log_dist", "same_state", "log_urban_pop", "coast_dist", "slope", "lpm", "manuf_share", "ec_emp_pc")

run_grid <- function(G, label, ctl, vars, wexpr = "lwage", heatvar = "heat32", extra = character(0), interstate = FALSE,
                     samples = c("pool", "nss64", "plfs21")) {
  gE <- copy(G$gE)[, survey := "nss64"]; gL <- copy(G$gL)[, survey := "plfs21"]
  res <- list()
  for (sv in samples) {
    g <- if (sv == "pool") rbind(gE, gL, fill = TRUE) else if (sv == "nss64") copy(gE) else copy(gL)
    g[, `:=`(origin_s = paste(survey, origin), heat = get(heatvar))]
    g[, w := eval(parse(text = wexpr))]
    vv <- unique(c(vars, extra))
    gg <- g[complete.cases(g[, ..vv])]
    if (interstate) gg <- gg[same_state == 0]
    rhs <- paste0("heat + log_dist", if (!interstate) " + same_state" else "", " + log_urban_pop", ctl, " | origin_s")
    r0 <- fepois(as.formula(paste("N ~ w +", rhs)), gg, cluster = ~ dest, notes = FALSE, warn = FALSE)
    b <- coef(r0); V <- vcov(r0)[c("heat", "w"), c("heat", "w")]
    v30 <- 30 * b[["heat"]] / b[["w"]]; gr <- c(30 / b[["w"]], -30 * b[["heat"]] / b[["w"]]^2); s30 <- sqrt(drop(t(gr) %*% V %*% gr))
    n_used <- nobs(r0)
    res[[length(res) + 1]] <- data.table(variant = label, sample = sv, type = "free", theta = b[["w"]], theta_se = se(r0)[["w"]],
                                         value30 = v30, lo = v30 - 1.96 * s30, hi = v30 + 1.96 * s30,
                                         migrants = sum(gg$N), dest = uniqueN(gg$dest), cells_used = n_used)
    for (th in thetas) {
      gg[, off := th * w]
      r <- fepois(as.formula(paste("N ~", rhs)), gg, offset = ~ off, cluster = ~ dest, notes = FALSE, warn = FALSE)
      bh <- coef(r)[["heat"]]; sh <- se(r)[["heat"]]
      res[[length(res) + 1]] <- data.table(variant = label, sample = sv, type = "fixed", theta = th, theta_se = NA_real_,
                                           value30 = 30 * bh / th, lo = 30 * (bh - 1.96 * sh) / th, hi = 30 * (bh + 1.96 * sh) / th,
                                           migrants = sum(gg$N), dest = uniqueN(gg$dest), cells_used = nobs(r))
    }
  }
  rbindlist(res)
}

X <- load_inputs(IN, POPF, B4F)
cat("Destinations:", nrow(X$D), "| PM2.5 missing:", X$D[, sum(pm_miss)], "| PM2.5 zero:", X$D[pm25 == 0, .N], "\n")
cat("\nSample counts:\n")
G0 <- make_grids(X, drop_within = FALSE, men = FALSE)
G1 <- make_grids(X, drop_within = TRUE, men = FALSE)
G2 <- make_grids(X, drop_within = TRUE, men = TRUE)
saveRDS(G2, file.path(OUT, "grids_step1.rds"))

R <- rbindlist(list(
  run_grid(G0, "0 old (replication)", ctl_old, vars_old),
  run_grid(G1, "1 drop within-district movers", ctl_old, vars_old),
  run_grid(G2, "2 + men 15-64", ctl_old, vars_old),
  run_grid(G2, "3 + PM2.5 kept (new baseline)", ctl_new, vars_new),
  run_grid(G2, "4 rent sample, wage only", ctl_new, vars_new, extra = "lrent"),
  run_grid(G2, "5 rent sample, wage net of rent", ctl_new, vars_new, wexpr = paste0("lwage - ", SY, " * lrent"), extra = "lrent"),
  run_grid(G2, "6 baseline, heat = days above 38 C", ctl_new, vars_new, heatvar = "heat38"),
  run_grid(G2, "7 baseline, interstate movers only", ctl_new, vars_new, interstate = TRUE)
))
nc <- c("theta", "theta_se", "value30", "lo", "hi"); R[, (nc) := lapply(.SD, function(v) round(v, 4)), .SDcols = nc]
fwrite(R, file.path(OUT, "step1_grid.csv"))

## Check 1: variant 0 against the 11 Oct grid
old <- fread(OLDGRID)
new0 <- R[variant == "0 old (replication)"]
chk_fixed <- merge(new0[type == "fixed", .(sample, theta, value30_new = value30, migrants_new = migrants)],
                   old[type == "fixed", .(sample, theta = round(theta, 4), value30_old = round(value30, 4), migrants_old = migrants)],
                   by = c("sample", "theta"))
chk_free <- merge(new0[type == "free", .(sample, theta_new = theta, value30_new = value30, migrants_new = migrants)],
                  old[type == "free", .(sample, theta_old = round(theta, 4), value30_old = round(value30, 4), migrants_old = migrants)],
                  by = "sample")
cat("\nCheck 1: replication of the 11 Oct grid\n")
cat("  fixed-theta rows compared:", nrow(chk_fixed), "| max abs difference in value30:", max(abs(chk_fixed$value30_new - chk_fixed$value30_old)),
    "| migrant counts equal:", all(chk_fixed$migrants_new == chk_fixed$migrants_old), "\n")
print(chk_free)
print(chk_fixed[theta %in% c(1, 4, 12.9, 17.2)])

## Heat gradients of the district wage and rent across destinations (productivity side of formula (4)), same controls
D <- copy(X$D)
gradtab <- list()
for (per in c("early", "late")) {
  hv <- if (per == "early") "c32_9908" else "c32_1222"
  for (y in if (per == "early") c("wage_eus", "rent_hedonic") else c("wage_plfs", "rent76_hed")) {
    for (fe in c("none", "state")) {
      d <- D[!is.na(get(y))]
      f <- as.formula(paste0(y, " ~ ", hv, " + log_urban_pop + coast_dist + slope + lpm + pm_miss + manuf_share + ec_emp_pc",
                             if (fe == "state") " | st2" else ""))
      r <- feols(f, d, vcov = "hetero", notes = FALSE, warn = FALSE)
      gradtab[[length(gradtab) + 1]] <- data.table(period = per, outcome = y, state_fe = fe, n = nobs(r),
                                                   grad30 = round(30 * coef(r)[[hv]], 4), se30 = round(30 * se(r)[[hv]], 4))
    }
  }
}
GT <- rbindlist(gradtab); fwrite(GT, file.path(OUT, "step1_heat_gradients.csv"))
cat("\nHeat gradients across urban destinations (per 30 more days above 32 C; same controls; robust SEs):\n"); print(GT)

cat("\nAll variants, pooled sample:\n"); print(R[sample == "pool"], nrows = 200)
cat("\nAll variants, by survey:\n"); print(R[sample != "pool" & (type == "free" | theta %in% c(1, 4, 12.9))], nrows = 200)
