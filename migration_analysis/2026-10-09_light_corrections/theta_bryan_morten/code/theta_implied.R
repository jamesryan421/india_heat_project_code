## What theta implies for heat. The destination-choice model of urban work migrants (mig_build.R grids) with the wage
## coefficient fixed at theta (Poisson with offset theta * wage); value of 30 extra days above 32 C = 30 * heat coefficient / theta
## (negative = heat is a cost). Wage = survey district wage, or the Bryan-Morten destination effect (log wage per efficiency
## unit: origin-state effects and the selection term (1/theta) log pi removed).
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; OUT <- args[3]; TOUT <- args[4]
source(file.path(IN, "mig_build.R"))
m <- readRDS(file.path(TOUT, "analysis_sample.rds"))[ret == FALSE]
thetas <- c(5, 11.7, 12.9, 17.2, 32.4)   # 5 = gravity IV range; 12.9 OLS and 17.2 IV (movers excl. return migrants), 11.7-32.4 its 95% interval
gE[, survey := "nss64"]; gL[, survey := "plfs21"]
## destination effects for a given theta, urban sector, by survey
dfe <- function(th) {
  r <- feols(I(lw + lpi / th) ~ age + age2 | dsv + osv, m, notes = FALSE, warn = FALSE)
  f <- fixef(r)[["dsv"]]
  x <- data.table(k = names(f), bm = unname(f))
  x[, `:=`(dest = sub(" .*", "", k), sector = sub("^\\S+ (\\S)_.*", "\\1", k), survey = sub(".*_", "", k))]
  x <- x[sector == "2"]
  x[, bm := bm - mean(bm), by = survey]
  x[, .(dest, survey, bm)]
}
out <- list(); grad <- list()
for (th in thetas) {
  fe <- dfe(th)
  for (sv in c("nss64", "plfs21", "pool")) {
    g <- if (sv == "pool") rbind(gE, gL, fill = TRUE) else if (sv == "nss64") copy(gE) else copy(gL)
    g <- merge(g, fe, by = c("dest", "survey"), all.x = TRUE)
    g[, `:=`(origin_s = paste(survey, origin), heat = heat32)]
    for (wv in c("lwage", "bm")) {
      vars <- c("N", wv, "heat", "log_dist", "same_state", "log_urban_pop", "coast_dist", "slope", "pm25", "manuf_share", "ec_emp_pc")
      gg <- g[complete.cases(g[, ..vars])]
      gg[, off := th * get(wv)]
      r <- fepois(as.formula(paste0("N ~ heat + log_dist + same_state + log_urban_pop", ctl, " | origin_s")), gg, offset = ~ off,
                  cluster = ~ dest, notes = FALSE, warn = FALSE)
      b <- coef(r)["heat"]; s <- se(r)["heat"]
      out[[length(out) + 1]] <- data.table(theta = th, sample = sv, wage = wv, migrants = sum(gg$N), dest = uniqueN(gg$dest),
                                           heat_coef = b, heat_se = s, value30 = 30 * b / th, value30_lo = 30 * (b - 1.96 * s) / th,
                                           value30_hi = 30 * (b + 1.96 * s) / th)
      if (sv != "pool") {
        dl <- unique(gg[, c("dest", "dstate", wv, "heat", "coast_dist", "slope", "pm25", "manuf_share", "ec_emp_pc", "log_urban_pop"), with = FALSE])
        rg <- feols(as.formula(paste0(wv, " ~ heat + log_urban_pop", ctl, " | dstate")), dl, vcov = "hetero", notes = FALSE, warn = FALSE)
        grad[[length(grad) + 1]] <- data.table(theta = th, sample = sv, wage = wv, n_dest = nobs(rg), wage_grad30 = 30 * coef(rg)["heat"], se30 = 30 * se(rg)["heat"])
      }
    }
  }
}
O <- rbindlist(out); G <- unique(rbindlist(grad)[wage == "lwage", theta := NA])
nc <- c("heat_coef", "heat_se", "value30", "value30_lo", "value30_hi"); O[, (nc) := lapply(.SD, round, 4), .SDcols = nc]
cat("\nValue of 30 extra days above 32 C with the wage coefficient fixed at theta (share of income; negative = cost):\n"); print(O, nrows = 200)
cat("\nWage gradient in heat across urban destinations, within state, with controls (per 30 days):\n"); G[, `:=`(wage_grad30 = round(wage_grad30, 4), se30 = round(se30, 4))]; print(G, nrows = 100)
fwrite(O, file.path(TOUT, "theta_implied_heat_value.csv")); fwrite(G, file.path(TOUT, "theta_implied_wage_gradients.csv"))
