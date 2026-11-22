## Step 4 (15 Oct 2026): the flow elasticity eta from the Census counts, with the destination wage instrumented.
## Outcome: men who moved for work into the urban part of district d from state o in the last 0-4 years (Census 2011 D-3).
## Wage: NSS 68 district wage effect (wage_eus). Control function: first stage at the destination level (wage on the
## instrument, heat and the destination controls), its residual added to the Poisson; standard errors and intervals from
## a bootstrap over destinations. Instruments:
##   split  - the wage effect from a random half of each district's workers, as the instrument for the other half's
##            (removes sampling noise only);
##   plfs   - the PLFS 2017-20 wage effect for the same district;
##   asi    - ASI 2009-10 average wage per factory worker;
##   bartik - EC 2005 to 2013 shift-share employment growth.
## Usage: Rscript census_iv.R <counts> <attributes_v2> <POPF> <halves> <asi> <bartik> <out dir> <B>
suppressMessages({library(data.table); library(fixest); library(sf); library(parallel)})
a <- commandArgs(trailingOnly = TRUE); OUT <- a[7]; B <- as.integer(a[8])
options(width = 220)
hav <- function(lon1, lat1, lon2, lat2) { r <- pi/180; x <- sin((lat2-lat1)*r/2)^2 + cos(lat1*r)*cos(lat2*r)*sin((lon2-lon1)*r/2)^2; 2*6371*asin(sqrt(x)) }
C <- fread(a[1], colClasses = c(dest = "character", o = "character"))
D <- fread(a[2])
pop <- fread(a[3], select = c("pc11_sd_id", "geometry", "pop_11_proj"))
sf_use_s2(FALSE)
cen <- suppressWarnings(st_coordinates(st_point_on_surface(st_make_valid(st_as_sfc(pop$geometry, crs = 4326)))))
cent <- data.table(dest = pop$pc11_sd_id, lon = cen[, 1], lat = cen[, 2], pop11 = pop$pop_11_proj)[!grepl("-000$", dest)]
oc <- cent[, .(olon = weighted.mean(lon, pop11, na.rm = TRUE), olat = weighted.mean(lat, pop11, na.rm = TRUE)), by = .(o = substr(dest, 1, 2))]
H <- fread(a[4]); A <- fread(a[5]); BK <- fread(a[6], colClasses = "character"); setnames(BK, c("s", "d", "bartik"))
BK[, `:=`(pc11_sd_id = paste0(s, "-", d), bartik = as.numeric(bartik))]; BK[, bartik := (bartik - mean(bartik, na.rm = TRUE)) / sd(bartik, na.rm = TRUE)]
D <- merge(D, H[, .(pc11_sd_id, w_h1, w_h2)], by = "pc11_sd_id", all.x = TRUE)
D <- merge(D, A[, .(pc11_sd_id, asi_lw)], by = "pc11_sd_id", all.x = TRUE)
D <- merge(D, BK[, .(pc11_sd_id, bartik)], by = "pc11_sd_id", all.x = TRUE)
cn <- C[area == "Urban" & dur %in% c("0", "1-4"), .(N = sum(M_work)), by = .(o, dest)]
g <- merge(cn, D, by.x = "dest", by.y = "pc11_sd_id"); g <- merge(g, cent[, .(dest, lon, lat)], by = "dest"); g <- merge(g, oc, by = "o")
g[, `:=`(log_dist = log(pmax(hav(olon, olat, lon, lat), 25)), same_state = as.integer(o == substr(dest, 1, 2)), dst = substr(dest, 1, 2),
         heat = c32_0111, lpm = log(pm25))]
ctl_d <- c("heat", "log_urban_pop", "coast_dist", "slope", "lpm", "manuf_share", "ec_emp_pc")
IV <- list(split = c(x = "w_h1", z = "w_h2"), split_rev = c(x = "w_h2", z = "w_h1"), plfs = c(x = "wage_eus", z = "wage_plfs"),
           asi = c(x = "wage_eus", z = "asi_lw"), bartik = c(x = "wage_eus", z = "bartik"), none = c(x = "wage_eus", z = NA))
est_once <- function(gg, x, z, fe) {
  gg <- copy(gg); gg[, xw := get(x)]
  rhs_c <- paste0("xw + heat + log_dist", if (fe == "o") " + same_state" else "", " + log_urban_pop + coast_dist + slope + lpm + manuf_share + ec_emp_pc")
  if (is.na(z)) {
    m <- fepois(as.formula(paste("N ~", rhs_c, "|", fe)), gg, notes = FALSE, warn = FALSE)
    return(c(fs = NA, fs_t = NA, eta = coef(m)[["xw"]], heat = coef(m)[["heat"]], vhat = NA, cost30 = -30 * coef(m)[["heat"]] / coef(m)[["xw"]]))
  }
  dl <- unique(gg[, c("dest", "dst", "xw", z, ctl_d), with = FALSE], by = "dest"); setnames(dl, z, "zz")
  fs <- feols(as.formula(paste("xw ~ zz +", paste(ctl_d, collapse = " + "), if (fe == "o") "" else "| dst")), dl, notes = FALSE, warn = FALSE)
  dl[, vhat := xw - predict(fs, newdata = dl)]
  gg <- merge(gg, dl[, .(dest, vhat)], by = "dest")
  m <- fepois(as.formula(paste("N ~", rhs_c, "+ vhat |", fe)), gg, notes = FALSE, warn = FALSE)
  c(fs = coef(fs)[["zz"]], fs_t = coef(fs)[["zz"]] / se(fs, vcov = "hetero")[["zz"]], eta = coef(m)[["xw"]], heat = coef(m)[["heat"]],
    vhat = coef(m)[["vhat"]], cost30 = -30 * coef(m)[["heat"]] / coef(m)[["xw"]])
}
run <- function(name, fe) {
  x <- IV[[name]][["x"]]; z <- IV[[name]][["z"]]
  need <- c("N", x, ctl_d, "log_dist", if (!is.na(z)) z)
  gg <- g[complete.cases(g[, need, with = FALSE]) & is.finite(lpm) & is.finite(log_urban_pop)]
  pt <- est_once(gg, x, z, fe)
  keys <- unique(gg$dest); setkey(gg, dest)
  bs <- do.call(rbind, mclapply(seq_len(B), function(b) {
    set.seed(5000 + b); setFixest_nthreads(1); setDTthreads(1)
    draw <- sample(keys, length(keys), replace = TRUE)
    gb <- rbindlist(lapply(seq_along(draw), function(k) { y <- gg[.(draw[k])]; y[, dest := paste(dest, k)]; y }))
    r <- try(est_once(gb, x, z, fe), silent = TRUE); if (inherits(r, "try-error")) rep(NA_real_, 6) else r
  }, mc.cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "4"))))
  colnames(bs) <- names(pt)
  q <- function(v) quantile(bs[, v], c(.025, .975), na.rm = TRUE)
  data.table(instrument = name, fe = fe, dest = uniqueN(gg$dest), migrants = sum(gg$N), fs = pt[["fs"]], fs_F = pt[["fs_t"]]^2,
             eta = pt[["eta"]], eta_se = sd(bs[, "eta"], na.rm = TRUE), eta_lo = q("eta")[1], eta_hi = q("eta")[2],
             cost30 = pt[["cost30"]], cost_lo = q("cost30")[1], cost_hi = q("cost30")[2], vhat = pt[["vhat"]], B_ok = sum(complete.cases(bs[, c("eta", "cost30")])))
}
R <- rbindlist(lapply(c("o", "o^dst"), function(fe) rbindlist(lapply(names(IV), run, fe = fe))))
num <- names(R)[sapply(R, is.numeric)]; R[, (num) := lapply(.SD, function(v) round(v, 4)), .SDcols = num]
fwrite(R, file.path(OUT, "census_iv.csv"))
cat("Flow elasticity eta with the destination wage instrumented (control function; bootstrap over destinations, B =", B, ")\n")
cat("cost30 = cost of 30 more days above 32 C at that eta (share of income; positive = cost)\n")
print(R)
