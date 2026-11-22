## Bryan & Morten (2019) selection equation:  log wage_iod = destination FE + origin FE - (1/theta) log pi_od + controls.
## pi_od from Census 2011 D-03 (men); wages from NSS 64 and PLFS 2020-21. Instruments for log pi_od: log distance (+ contiguity),
## or the share of men from states bordering o who live in d. theta = -1/beta; its interval inverts the 95% interval for beta.
suppressMessages({library(data.table); library(fixest)})
args <- commandArgs(trailingOnly = TRUE); OUT <- args[1]
options(width = 220)
cells <- fread(file.path(OUT, "pi_cells.csv"), colClasses = c(o = "character", d = "character"))
micro <- fread(file.path(OUT, "micro.csv"), colClasses = c(o = "character", d = "character", sector = "character"))
m <- merge(micro, cells[, .(o, d, M_T, M_U, pi_m_T, pi_m_U, pi_p_T, pi_w_T, pi_nbr, dist_km, contig, same_state)], by = c("o", "d"), all.x = TRUE)
cat(sprintf("Micro rows %d; without a Census cell %d; with a zero Census count %d (movers %d)\n",
            nrow(m), m[is.na(pi_m_T), .N], m[pi_m_T == 0, .N], m[mover == TRUE & (is.na(pi_m_T) | pi_m_T == 0), .N]))
m[, `:=`(lpi = log(pi_m_T), lpi_u = log(pi_m_U), lpi_p = log(pi_p_T), lpi_w = log(pi_w_T), lpi_nbr = log(pi_nbr),
         ldist = log(pmax(dist_km, 25)), age2 = age^2 / 100, dsec = paste(d, sector), inter = as.integer(o != substr(d, 1, 2)))]
m[, ldist_i := inter * ldist]
m[, `:=`(dsv = paste(dsec, survey, sep = "_"), osv = paste(o, survey, sep = "_"))]
m[, wtn := wt / mean(wt, na.rm = TRUE), by = survey]
m <- m[is.finite(lpi)]
mv <- m[inter == 1]
cat(sprintf("Interstate movers with a Census share: %d (NSS 64 %d, PLFS %d); origin-destination cells %d; destinations %d; origins %d\n",
            nrow(mv), mv[survey == "nss64", .N], mv[survey == "plfs21", .N], uniqueN(mv[, .(o, d)]), uniqueN(mv$d), uniqueN(mv$o)))
cat(sprintf("Return migrants among movers: NSS 64 %d of %d, PLFS %d of %d\n", mv[survey == "nss64", sum(ret)], mv[survey == "nss64", .N], mv[survey == "plfs21", sum(ret)], mv[survey == "plfs21", .N]))
FE <- "dsv + osv"
res <- list(); fes <- list()
get_fs <- function(r) {
  v <- tryCatch(unlist(fitstat(r, "ivwald")), error = function(e) NULL)
  if (is.null(v)) return(NA_real_)
  as.numeric(v[grep("stat$", names(v))][1])
}
row <- function(r, spec, method, var, dat) {
  b <- unname(coef(r)[var]); s <- unname(se(r)[var])
  lo <- b - 1.96 * s; hi <- b + 1.96 * s
  data.table(spec = spec, method = method, beta = b, se = s, theta = -1 / b,
             theta_lo = ifelse(lo < 0, -1 / lo, NA), theta_hi = ifelse(hi < 0, -1 / hi, Inf),
             n = nobs(r), cells = uniqueN(dat[, .(o, d)]), dest = uniqueN(dat$d), orig = uniqueN(dat$o),
             fs_coef = if (method == "OLS") NA_real_ else unname(coef(r$iv_first_stage[[1]])[1]),
             fs_F = if (method == "OLS") NA_real_ else get_fs(r))
}
run <- function(dat, spec, pv = "lpi", ctrl = "age + age2", fe = FE, inst = "ldist", wts = NULL, ols = TRUE, keep_fe = FALSE) {
  dat <- dat[is.finite(get(pv))]
  out <- list()
  if (ols) {
    r <- feols(as.formula(sprintf("lw ~ %s + %s | %s", pv, ctrl, fe)), dat, cluster = ~ o + d, weights = wts)
    out[[1]] <- row(r, spec, "OLS", pv, dat)
  }
  for (iv in inst) {
    di <- dat[complete.cases(dat[, all.vars(as.formula(paste("~", iv))), with = FALSE])]
    r <- feols(as.formula(sprintf("lw ~ %s | %s | %s ~ %s", ctrl, fe, pv, iv)), di, cluster = ~ o + d, weights = wts)
    out[[length(out) + 1]] <- row(r, spec, paste("IV:", iv), paste0("fit_", pv), di)
    if (keep_fe) fes[[spec]] <<- data.table(dest = names(fixef(r)[[1]]), fe = unname(fixef(r)[[1]]))
  }
  rbindlist(out)
}
## ---- descriptive gravity on the Census cells (cf. Bryan-Morten Table 2, col. 1: -0.717 for Indonesia) ----
cg <- cells[same_state == 0 & M_T > 0]
cg[, `:=`(lpi = log(pi_m_T), ldist = log(pmax(dist_km, 25)))]
gr <- feols(lpi ~ ldist + contig | o + d, cg, cluster = ~ o + d)
cat(sprintf("\nCensus gravity, interstate cells with positive counts (%d): log share on log distance %.3f (%.3f), contiguous states %.3f (%.3f)\n",
            nobs(gr), coef(gr)["ldist"], se(gr)["ldist"], coef(gr)["contig"], se(gr)["contig"]))
rf <- feols(as.formula(sprintf("lw ~ ldist + age + age2 | %s", FE)), mv, cluster = ~ o + d)
cat(sprintf("Reduced form, movers: log wage on log distance %.4f (%.4f) [Bryan-Morten Indonesia: 0.029]\n\n", coef(rf)["ldist"], se(rf)["ldist"]))
## ---- main specifications ----
res[["A1"]]  <- run(mv, "A1 movers, both surveys", inst = c("ldist", "ldist + contig", "lpi_nbr"), keep_fe = TRUE)
res[["A2"]]  <- run(mv[survey == "nss64"], "A2 movers, NSS 64 only", inst = c("ldist", "ldist + contig"))
res[["A3"]]  <- run(mv[survey == "plfs21"], "A3 movers, PLFS 2020-21 only", inst = c("ldist", "ldist + contig"))
res[["A4"]]  <- run(mv[sector == "2"], "A4 movers in urban areas, urban shares", pv = "lpi_u", inst = c("ldist", "ldist + contig"))
res[["A5"]]  <- run(mv[!is.na(edu)], "A5 movers, + education dummies", ctrl = "age + age2 + i(edu)", inst = c("ldist", "ldist + contig"))
res[["A6"]]  <- run(mv[work == TRUE], "A6 movers who moved for work", inst = c("ldist", "ldist + contig"))
res[["A7"]]  <- run(mv[is.na(transfer) | transfer == FALSE], "A7 movers, excluding job transfers (NSS 64)", inst = c("ldist", "ldist + contig"))
res[["A8"]]  <- run(mv[reg == TRUE], "A8 movers in regular jobs", inst = c("ldist", "ldist + contig"))
res[["A9"]]  <- run(mv[reg == FALSE], "A9 movers in casual jobs", inst = c("ldist", "ldist + contig"))
res[["A10"]] <- run(mv, "A10 movers, survey weights", inst = c("ldist", "ldist + contig"), wts = ~ wtn)
res[["A11"]] <- run(mv, "A11 movers, shares of all persons", pv = "lpi_p", inst = c("ldist", "ldist + contig"))
res[["A12"]] <- run(mv[head == TRUE], "A12 movers who head the household (Bryan-Morten sample)", inst = c("ldist", "ldist + contig"))
res[["X1"]]  <- run(mv[ret == FALSE], "X1 movers excl. return migrants, both surveys", inst = c("ldist", "ldist + contig"), keep_fe = TRUE)
res[["X2"]]  <- run(mv[ret == FALSE & survey == "nss64"], "X2 movers excl. return migrants, NSS 64", inst = c("ldist", "ldist + contig"))
res[["X3"]]  <- run(mv[ret == FALSE & survey == "plfs21"], "X3 movers excl. return migrants, PLFS 2020-21", inst = c("ldist", "ldist + contig"))
res[["X4"]]  <- run(mv[ret == FALSE & work == TRUE], "X4 movers excl. return migrants, moved for work", inst = c("ldist", "ldist + contig"))
res[["B1"]]  <- run(m, "B1 all wage earners incl. natives", inst = c("inter + ldist_i"), keep_fe = TRUE)
res[["X5"]]  <- run(m[ret == FALSE], "X5 all wage earners incl. natives, excl. return migrants", inst = c("inter + ldist_i"))
res[["B2"]]  <- run(m[sector == "2"], "B2 all urban wage earners incl. natives, urban shares", pv = "lpi_u", inst = c("inter + ldist_i"))
## ---- cell means, as in Bryan-Morten (origin-destination cells with at least 5 movers) ----
cm <- mv[, .(lw = mean(lw), n = .N, lpi = lpi[1], ldist = ldist[1], contig = contig[1], age = mean(age), age2 = mean(age2)), by = .(o, d, survey)][n >= 5]
cm[, dsec := d]
res[["C1"]] <- run(cm, "C1 cell means, cells with 5+ movers", fe = "d^survey + o^survey", inst = c("ldist", "ldist + contig"))
R <- rbindlist(res)
num_cols <- c("beta", "se", "theta", "theta_lo", "theta_hi", "fs_coef", "fs_F")
fwrite(R, file.path(OUT, "theta_results.csv"))
P <- copy(R)[, (num_cols) := lapply(.SD, function(x) round(x, 3)), .SDcols = num_cols]
print(P[, .(spec, method, beta, se, theta, theta_lo, theta_hi, n, cells, dest, orig, fs_coef, fs_F)], nrows = 200)
for (k in names(fes)) fwrite(fes[[k]], file.path(OUT, sprintf("dest_fe_%s.csv", gsub("[^A-Za-z0-9]+", "_", k))))
saveRDS(m, file.path(OUT, "analysis_sample.rds"))
