## Data for the figures in the summary note.
## (A) value of 30 more days above 32 C in the destination-choice model, with the wage coefficient fixed on a grid of theta
##     (Poisson with offset theta * wage), plus the freely estimated wage coefficient for reference;
## (B) binned Bryan-Morten relationship: movers' log wage against the log share of their origin state living in the district,
##     both net of destination (district x sector x survey) and origin (state x survey) effects and age terms;
## (C) simplified district and state boundaries for the heat map.
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; OUT <- args[3]; TOUT <- args[4]
source(file.path(IN, "mig_build.R"))
suppressMessages({library(data.table); library(fixest); library(sf)})
FD <- file.path(TOUT, "figdata"); dir.create(FD, showWarnings = FALSE)
## ---------------- (A) theta grid ----------------
gE[, survey := "nss64"]; gL[, survey := "plfs21"]
vars <- c("N", "lwage", "heat32", "log_dist", "same_state", "log_urban_pop", "coast_dist", "slope", "pm25", "manuf_share", "ec_emp_pc")
prep <- function(sv) {
  g <- if (sv == "pool") rbind(gE, gL, fill = TRUE) else if (sv == "nss64") copy(gE) else copy(gL)
  g[, `:=`(origin_s = paste(survey, origin), heat = heat32)]
  g[complete.cases(g[, ..vars])]
}
rhs <- paste0("heat + log_dist + same_state + log_urban_pop", ctl, " | origin_s")
thetas <- c(0.5, 0.65, 0.8, 1, 1.25, 1.5, 2, 2.5, 3, 4, 5, 6.5, 8, 10, 12.9, 15, 17.2, 20, 25, 27.6, 32.4, 40, 50)
res <- list()
for (sv in c("pool", "nss64", "plfs21")) {
  gg <- prep(sv)
  r0 <- fepois(as.formula(paste("N ~ lwage +", rhs)), gg, cluster = ~ dest, notes = FALSE, warn = FALSE)
  b <- coef(r0); V <- vcov(r0)[c("heat", "lwage"), c("heat", "lwage")]
  v30 <- 30 * b[["heat"]] / b[["lwage"]]
  gr <- c(30 / b[["lwage"]], -30 * b[["heat"]] / b[["lwage"]]^2)
  se30 <- sqrt(drop(t(gr) %*% V %*% gr))
  res[[length(res) + 1]] <- data.table(sample = sv, type = "free", theta = b[["lwage"]], theta_se = se(r0)[["lwage"]],
                                       value30 = v30, value30_lo = v30 - 1.96 * se30, value30_hi = v30 + 1.96 * se30, migrants = sum(gg$N))
  for (th in thetas) {
    gg[, off := th * lwage]
    r <- fepois(as.formula(paste("N ~", rhs)), gg, offset = ~ off, cluster = ~ dest, notes = FALSE, warn = FALSE)
    bh <- coef(r)[["heat"]]; sh <- se(r)[["heat"]]
    res[[length(res) + 1]] <- data.table(sample = sv, type = "fixed", theta = th, theta_se = NA_real_, value30 = 30 * bh / th,
                                         value30_lo = 30 * (bh - 1.96 * sh) / th, value30_hi = 30 * (bh + 1.96 * sh) / th, migrants = sum(gg$N))
  }
}
R <- rbindlist(res)
fwrite(R, file.path(FD, "theta_grid_heat_value.csv"))
cat("\n(A) Value of 30 more days above 32 C (share of income; negative = cost), pooled:\n"); print(R[sample == "pool"], nrows = 100)
cat("\nFree wage coefficient by sample:\n"); print(R[type == "free"])
## ---------------- (B) binned Bryan-Morten relationship ----------------
m <- readRDS(file.path(TOUT, "analysis_sample.rds"))
mv <- m[inter == 1 & ret == FALSE & is.finite(lpi) & is.finite(lw) & !is.na(age)]
rw <- feols(lw ~ age + age2 | dsv + osv, mv, notes = FALSE, warn = FALSE)
rp <- feols(lpi ~ age + age2 | dsv + osv, mv, notes = FALSE, warn = FALSE)
mv[, `:=`(rwage = NA_real_, rshare = NA_real_)]
mv[obs(rw), rwage := resid(rw)]; mv[obs(rp), rshare := resid(rp)]
b <- mv[!is.na(rwage) & !is.na(rshare)]
sl <- coef(lm(rwage ~ rshare, b))[["rshare"]]
b[, bin := cut(rshare, unique(quantile(rshare, 0:20 / 20)), include.lowest = TRUE, labels = FALSE)]
B <- b[, .(x = mean(rshare), y = mean(rwage), n = .N), by = bin][order(bin)]
fwrite(B, file.path(FD, "bm_binscatter.csv"))
fwrite(data.table(n = nrow(b), slope = sl, theta = -1 / sl), file.path(FD, "bm_binscatter_slope.csv"))
cat(sprintf("\n(B) movers %d; slope of residual log wage on residual log share %.4f (theta %.1f)\n", nrow(b), sl, -1 / sl)); print(B)
## ---------------- (C) boundaries ----------------
pg <- fread(POPF, select = c("pc11_sd_id", "geometry"))
pg <- pg[!grepl("-000$", pc11_sd_id)]
sf_use_s2(FALSE)
d <- st_sf(pc11_sd_id = pg$pc11_sd_id, geometry = st_make_valid(st_as_sfc(pg$geometry, crs = 4326)))
d <- suppressWarnings(st_simplify(d, dTolerance = 0.01, preserveTopology = TRUE))
d$state <- substr(d$pc11_sd_id, 1, 2)
s <- do.call(rbind, lapply(split(d, d$state), function(x) st_sf(state = x$state[1], geometry = st_union(suppressWarnings(st_buffer(st_make_valid(x), 0))))))
s <- suppressWarnings(st_simplify(s, dTolerance = 0.01, preserveTopology = TRUE))
st_write(d, file.path(FD, "districts.geojson"), delete_dsn = TRUE, quiet = TRUE)
st_write(s, file.path(FD, "states.geojson"), delete_dsn = TRUE, quiet = TRUE)
cat(sprintf("\n(C) districts %d, states %d written\n", nrow(d), nrow(s)))
