## Step 1b (15 Oct 2026): the theta grid on the corrected destination attributes (build_dest_v2.R: PM2.5 and urban
## population rebuilt on 2011 Census codes, which restores Telangana/Hyderabad and Delhi and fixes Delhi's urban population).
## Each variant changes one thing relative to the one it is compared with.
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; B4F <- args[3]; OUT <- args[4]; ATTR2 <- args[5]
source("mig_build_v2.R")
dir.create(OUT, showWarnings = FALSE, recursive = TRUE)
SY <- 0.20
thetas <- c(1, 2, 3, 4, 5, 8, 12.9, 17.2)
ctl_log <- " + coast_dist + slope + log(pm25) + manuf_share + ec_emp_pc"
ctl_new <- " + coast_dist + slope + lpm + pm_miss + manuf_share + ec_emp_pc"
ctl_nopm <- " + coast_dist + slope + manuf_share + ec_emp_pc"
v_log <- c("N", "w", "heat", "log_dist", "same_state", "log_urban_pop", "coast_dist", "slope", "pm25", "manuf_share", "ec_emp_pc")
v_new <- c("N", "w", "heat", "log_dist", "same_state", "log_urban_pop", "coast_dist", "slope", "lpm", "manuf_share", "ec_emp_pc")
run_grid <- function(G, label, ctl, vars, wexpr = "lwage", heatvar = "heat32", extra = character(0), interstate = FALSE) {
  gE <- copy(G$gE)[, survey := "nss64"]; gL <- copy(G$gL)[, survey := "plfs21"]
  res <- list()
  for (sv in c("pool", "nss64", "plfs21")) {
    g <- if (sv == "pool") rbind(gE, gL, fill = TRUE) else if (sv == "nss64") copy(gE) else copy(gL)
    g[, `:=`(origin_s = paste(survey, origin), heat = get(heatvar))]
    g[, w := eval(parse(text = wexpr))]
    gg <- g[complete.cases(g[, unique(c(vars, extra)), with = FALSE])]
    gg <- gg[is.finite(log(pmax(pm25, 0)))| !grepl("log\\(pm25\\)", ctl)]          # make the log(pm25) drop explicit
    if (interstate) gg <- gg[same_state == 0]
    rhs <- paste0("heat + log_dist", if (!interstate) " + same_state" else "", " + log_urban_pop", ctl, " | origin_s")
    r0 <- fepois(as.formula(paste("N ~ w +", rhs)), gg, cluster = ~ dest, notes = FALSE, warn = FALSE)
    b <- coef(r0); V <- vcov(r0)[c("heat", "w"), c("heat", "w")]
    v30 <- 30 * b[["heat"]] / b[["w"]]; gr <- c(30 / b[["w"]], -30 * b[["heat"]] / b[["w"]]^2); s30 <- sqrt(drop(t(gr) %*% V %*% gr))
    res[[length(res) + 1]] <- data.table(variant = label, sample = sv, type = "free", theta = b[["w"]], theta_se = se(r0)[["w"]],
                                         value30 = v30, lo = v30 - 1.96 * s30, hi = v30 + 1.96 * s30, migrants = sum(gg$N), dest = uniqueN(gg$dest),
                                         b_size = b[["log_urban_pop"]], se_size = se(r0)[["log_urban_pop"]], b_dist = b[["log_dist"]],
                                         hyd_in = "28-536" %in% gg$dest, delhi_in = "07-090" %in% gg$dest)
    for (th in thetas) {
      gg[, off := th * w]
      r <- fepois(as.formula(paste("N ~", rhs)), gg, offset = ~ off, cluster = ~ dest, notes = FALSE, warn = FALSE)
      bh <- coef(r)[["heat"]]; sh <- se(r)[["heat"]]
      res[[length(res) + 1]] <- data.table(variant = label, sample = sv, type = "fixed", theta = th, theta_se = NA_real_,
                                           value30 = 30 * bh / th, lo = 30 * (bh - 1.96 * sh) / th, hi = 30 * (bh + 1.96 * sh) / th,
                                           migrants = sum(gg$N), dest = uniqueN(gg$dest), b_size = coef(r)[["log_urban_pop"]], se_size = se(r)[["log_urban_pop"]],
                                           b_dist = coef(r)[["log_dist"]], hyd_in = "28-536" %in% gg$dest, delhi_in = "07-090" %in% gg$dest)
    }
  }
  rbindlist(res)
}
X1 <- load_inputs(IN, POPF, B4F)
X2 <- X1; X2$D <- prep_D(ATTR2)
cat("v2 attributes: PM2.5 missing", X2$D[, sum(pm_miss)], "| log_urban_pop missing", X2$D[is.na(log_urban_pop), .N], "\n")
G_old_new <- make_grids(X1, TRUE, TRUE, verbose = FALSE)
G2_old <- make_grids(X2, FALSE, FALSE, verbose = FALSE)
G2 <- make_grids(X2, TRUE, TRUE, verbose = FALSE)
saveRDS(G2, file.path(OUT, "grids_step1b.rds"))
R <- rbindlist(list(
  run_grid(G_old_new, "2  old attributes, new sample, log(pm25)", ctl_log, v_log),
  run_grid(G2_old, "8  new attributes, old sample, log(pm25)", ctl_log, v_log),
  run_grid(G2, "9  new attributes, new sample, log(pm25)", ctl_log, v_log),
  run_grid(G2, "10 new attributes, new sample, no PM2.5", ctl_nopm, v_new),
  run_grid(G2, "11 rent sample, wage only", ctl_log, v_log, extra = "lrent"),
  run_grid(G2, "12 rent sample, wage net of rent", ctl_log, v_log, wexpr = paste0("lwage - ", SY, " * lrent"), extra = "lrent"),
  run_grid(G2, "13 heat = days above 38 C", ctl_log, v_log, heatvar = "heat38"),
  run_grid(G2, "14 interstate movers only", ctl_log, v_log, interstate = TRUE)
))
nc <- c("theta", "theta_se", "value30", "lo", "hi", "b_size", "se_size", "b_dist"); R[, (nc) := lapply(.SD, function(v) round(v, 4)), .SDcols = nc]
fwrite(R, file.path(OUT, "step1b_grid.csv"))
## heat gradients of district wage and rent, new attributes
D <- copy(X2$D); gt <- list()
for (per in c("early", "late")) {
  hv <- if (per == "early") "c32_9908" else "c32_1222"
  for (y in if (per == "early") c("wage_eus", "rent_hedonic") else c("wage_plfs", "rent76_hed")) for (fe in c("none", "state")) {
    r <- feols(as.formula(paste0(y, " ~ ", hv, " + log_urban_pop + coast_dist + slope + log(pm25) + manuf_share + ec_emp_pc", if (fe == "state") " | st2" else "")),
               D[!is.na(get(y))], vcov = "hetero", notes = FALSE, warn = FALSE)
    gt[[length(gt) + 1]] <- data.table(period = per, outcome = y, state_fe = fe, n = nobs(r), grad30 = round(30 * coef(r)[[hv]], 4), se30 = round(30 * se(r)[[hv]], 4))
  }
}
GT <- rbindlist(gt); fwrite(GT, file.path(OUT, "step1b_heat_gradients.csv"))
cat("\nHeat gradients (per 30 more days above 32 C), new attributes:\n"); print(GT)
cat("\nPooled sample:\n"); print(R[sample == "pool", .(variant, type, theta, value30, lo, hi, migrants, dest, b_size, se_size, hyd_in, delhi_in)], nrows = 200)
cat("\nBy survey (free theta and theta = 4, 12.9):\n"); print(R[sample != "pool" & (type == "free" | theta %in% c(4, 12.9)), .(variant, sample, type, theta, value30, lo, hi, migrants, dest)], nrows = 200)
