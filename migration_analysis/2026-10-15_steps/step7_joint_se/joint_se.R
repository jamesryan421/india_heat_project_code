## Joint standard errors for formula (4) (15 Oct 2026).
## MWTP for a hot day = s_y * beta_p - beta_w + beta_N / eta. One bootstrap over destinations re-estimates every piece
## on the same draw, so their covariances are carried through:
##   beta_N: heat effect on the inflow of men who moved for work in the last 0-4 years (Census 2011, Poisson, origin FE);
##   eta:    flow elasticity, destination wage (NSS 68) instrumented by the other random half of each district's earners
##           (control function);
##   gamma:  heat coefficient in that same instrumented model (wage held fixed);
##   beta_w: heat gradient of the district wage (OLS across destinations, same controls);
##   beta_p: heat gradient of the district tenant rent (NSS 65, 2008-09; destinations with rent data).
## Two versions of the cost of 30 more hot days (percent of income, positive = cost):
##   one model:  -30 * (gamma / eta + s_y * beta_p)   (the wage term is inside gamma, with the Poisson weights)
##   four terms: -30 * (s_y * beta_p - beta_w + beta_N / eta)   (formula (4) literally; beta_w is unweighted)
## plus the one-model cost with eta fixed at 2.5 and 4 (uncertainty from gamma and beta_p only).
## Usage: Rscript joint_se.R <counts> <attributes_v2> <POPF> <nss68 halves> <out dir> <B>
suppressMessages({library(data.table); library(fixest); library(sf); library(parallel)})
a <- commandArgs(trailingOnly = TRUE); OUT <- a[5]; B <- as.integer(a[6]); SY <- 0.20
options(width = 220)
hav <- function(lon1, lat1, lon2, lat2) { r <- pi/180; x <- sin((lat2-lat1)*r/2)^2 + cos(lat1*r)*cos(lat2*r)*sin((lon2-lon1)*r/2)^2; 2*6371*asin(sqrt(x)) }
C <- fread(a[1], colClasses = c(dest = "character", o = "character"))
D <- fread(a[2]); H <- fread(a[4])
pop <- fread(a[3], select = c("pc11_sd_id", "geometry", "pop_11_proj"))
sf_use_s2(FALSE)
cen <- suppressWarnings(st_coordinates(st_point_on_surface(st_make_valid(st_as_sfc(pop$geometry, crs = 4326)))))
cent <- data.table(dest = pop$pc11_sd_id, lon = cen[, 1], lat = cen[, 2], pop11 = pop$pop_11_proj)[!grepl("-000$", dest)]
oc <- cent[, .(olon = weighted.mean(lon, pop11, na.rm = TRUE), olat = weighted.mean(lat, pop11, na.rm = TRUE)), by = .(o = substr(dest, 1, 2))]
D <- merge(D, H[, .(pc11_sd_id, w_h1, w_h2)], by = "pc11_sd_id", all.x = TRUE)
cn <- C[area == "Urban" & dur %in% c("0", "1-4"), .(N = sum(M_work)), by = .(o, dest)]
g <- merge(cn, D, by.x = "dest", by.y = "pc11_sd_id"); g <- merge(g, cent[, .(dest, lon, lat)], by = "dest"); g <- merge(g, oc, by = "o")
g[, `:=`(log_dist = log(pmax(hav(olon, olat, lon, lat), 25)), same_state = as.integer(o == substr(dest, 1, 2)), heat = c32_0111, lpm = log(pm25))]
ctl <- c("log_urban_pop", "coast_dist", "slope", "lpm", "manuf_share", "ec_emp_pc")
need <- c("N", "heat", "log_dist", "wage_eus", "w_h1", "w_h2", ctl)
g <- g[complete.cases(g[, ..need]) & is.finite(lpm) & is.finite(log_urban_pop)]
cat(sprintf("Sample: %d destinations, %.2f million men; %d destinations with rent data\n", uniqueN(g$dest), sum(g$N) / 1e6, uniqueN(g[!is.na(rent_hedonic), dest])))
rhs_ctl <- paste(ctl, collapse = " + ")
pieces <- function(gg) {
  dl <- unique(gg[, c("dest", "heat", "wage_eus", "rent_hedonic", "w_h1", "w_h2", ctl), with = FALSE], by = "dest")
  ## beta_N: reduced-form heat effect on flows
  mN <- fepois(as.formula(paste("N ~ heat + log_dist + same_state +", rhs_ctl, "| o")), gg, notes = FALSE, warn = FALSE)
  ## eta and gamma: split-half control function
  fs <- feols(as.formula(paste("w_h1 ~ w_h2 + heat +", rhs_ctl)), dl, notes = FALSE, warn = FALSE)
  dl[, vhat := w_h1 - predict(fs, newdata = dl)]
  g2 <- merge(gg, dl[, .(dest, vhat)], by = "dest")
  mC <- fepois(as.formula(paste("N ~ w_h1 + vhat + heat + log_dist + same_state +", rhs_ctl, "| o")), g2, notes = FALSE, warn = FALSE)
  ## beta_w and beta_p: destination-level gradients
  mw <- feols(as.formula(paste("wage_eus ~ heat +", rhs_ctl)), dl, notes = FALSE, warn = FALSE)
  mp <- feols(as.formula(paste("rent_hedonic ~ heat +", rhs_ctl)), dl[!is.na(rent_hedonic)], notes = FALSE, warn = FALSE)
  bN <- coef(mN)[["heat"]]; eta <- coef(mC)[["w_h1"]]; gam <- coef(mC)[["heat"]]; bw <- coef(mw)[["heat"]]; bp <- coef(mp)[["heat"]]
  ## one-model cost with eta fixed (offset)
  fixed <- sapply(c(2.5, 4), function(th) {
    gg2 <- copy(gg); gg2[, off := th * wage_eus]
    m <- fepois(as.formula(paste("N ~ heat + log_dist + same_state +", rhs_ctl, "| o")), gg2, offset = ~ off, notes = FALSE, warn = FALSE)
    -100 * 30 * (coef(m)[["heat"]] / th + SY * bp)
  })
  c(beta_N30 = 100 * 30 * bN, eta = eta, gamma30 = 100 * 30 * gam, beta_w30 = 100 * 30 * bw, beta_p30 = 100 * 30 * bp,
    cost_one = -100 * 30 * (gam / eta + SY * bp), cost_four = -100 * 30 * (SY * bp - bw + bN / eta),
    cost_eta2.5 = fixed[1], cost_eta4 = fixed[2])
}
pt <- pieces(g)
keys <- unique(g$dest); setkey(g, dest)
bs <- do.call(rbind, mclapply(seq_len(B), function(b) {
  set.seed(9000 + b); setFixest_nthreads(1); setDTthreads(1)
  draw <- sample(keys, length(keys), replace = TRUE)
  gb <- rbindlist(lapply(seq_along(draw), function(k) { y <- g[.(draw[k])]; y[, dest := paste(dest, k)]; y }))
  r <- try(pieces(gb), silent = TRUE); if (inherits(r, "try-error")) rep(NA_real_, length(pt)) else r
}, mc.cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "4"))))
colnames(bs) <- names(pt)
R <- data.table(quantity = names(pt), estimate = round(pt, 3), se = round(apply(bs, 2, sd, na.rm = TRUE), 3),
                lo = round(apply(bs, 2, quantile, .025, na.rm = TRUE), 3), hi = round(apply(bs, 2, quantile, .975, na.rm = TRUE), 3),
                median = round(apply(bs, 2, median, na.rm = TRUE), 3), B_ok = colSums(!is.na(bs)))
fwrite(R, file.path(OUT, "joint_se.csv")); fwrite(as.data.table(bs), file.path(OUT, "joint_se_draws.csv"))
cat("\nPieces of formula (4) and the cost of 30 more hot days (percent; *30 = per 30 days). Bootstrap over destinations, B =", B, "\n"); print(R)
cat("\nCorrelations of the pieces across bootstrap draws:\n")
print(round(cor(bs[, c("beta_N30", "eta", "gamma30", "beta_w30", "beta_p30")], use = "pairwise.complete.obs"), 2))
cat(sprintf("\nShare of draws with a positive cost (one model): %.2f | cost above the US figure of 2.1 percent: %.2f\n",
            mean(bs[, "cost_one"] > 0, na.rm = TRUE), mean(bs[, "cost_one"] > 2.1, na.rm = TRUE)))
