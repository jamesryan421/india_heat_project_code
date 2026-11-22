## Cohort design (15 Oct 2026): the flow elasticity eta from changes over time within origin-destination pairs.
## Census 2011 D-3 counts of men who moved for work into the urban part of district d from state o, by time since moving:
## arrived in the last year ("recent", about 2010-11) and 1-4 years earlier ("earlier", about 2006-10). Pay at arrival:
## the NSS 68 (2011-12) district wage effect for the recent cohort and the NSS 64 (2007-08) one for the earlier cohort.
## Origin x destination fixed effects absorb everything fixed about a pair (climate and heat, distance, networks,
## language, all fixed amenities); origin x cohort effects absorb changes at the origin and the different lengths of
## the two arrival windows. eta comes only from destinations whose pay rose more than others' between 2007-08 and 2011-12.
## Wage instrumented by control function (first stage with destination and cohort effects): the other random half of each
## district's earners in the same round (removes sampling noise), or the EC 2005-13 Bartik shock interacted with the
## recent cohort. Placebos: men who moved with their household, women who moved for marriage.
## Usage: Rscript cohort_design.R <counts> <nss64 halves> <nss68 halves> <bartik> <out dir> <B>
suppressMessages({library(data.table); library(fixest); library(parallel)})
a <- commandArgs(trailingOnly = TRUE); OUT <- a[5]; B <- as.integer(a[6])
options(width = 220)
C <- fread(a[1], colClasses = c(dest = "character", o = "character"))[area == "Urban" & dur %in% c("0", "1-4")]
C[, coh := fifelse(dur == "0", "recent", "earlier")]
h64 <- fread(a[2]); h68 <- fread(a[3]); setnames(h68, c("pc11_sd_id", "w68_full", "w68_h1", "w68_h2", "n68"))
BK <- fread(a[4], colClasses = "character"); setnames(BK, c("s", "d", "bartik"))
BK[, `:=`(dest = paste0(s, "-", d), bartik = as.numeric(bartik))]; BK[, bartik := (bartik - mean(bartik, na.rm = TRUE)) / sd(bartik, na.rm = TRUE)]
W <- merge(h64, h68, by = "pc11_sd_id"); setnames(W, "pc11_sd_id", "dest")
W <- merge(W, BK[, .(dest, bartik)], by = "dest", all.x = TRUE)
Wl <- rbind(W[, .(dest, coh = "earlier", w_full = w64_full, w_h1 = w64_h1, w_h2 = w64_h2, bartik_r = 0, bartik)],
            W[, .(dest, coh = "recent", w_full = w68_full, w_h1 = w68_h1, w_h2 = w68_h2, bartik_r = bartik, bartik)])
g0 <- merge(C, Wl, by = c("dest", "coh"))
g0[, `:=`(od = paste(o, dest), oc = paste(o, coh), inter = o != substr(dest, 1, 2))]
cat(sprintf("Destinations with wages in both rounds: %d | men who moved for work: recent %.2f million, earlier %.2f million\n",
            uniqueN(g0$dest), g0[coh == "recent", sum(M_work)] / 1e6, g0[coh == "earlier", sum(M_work)] / 1e6))
cat(sprintf("Change in district pay 2007-08 to 2011-12 across these destinations: SD %.3f\n", sd(W$w68_full - W$w64_full)))
est_once <- function(gg, outcome, iv) {
  gg <- copy(gg); gg[, N := get(outcome)]
  if (iv == "none") {
    m <- fepois(N ~ w_full | od + oc, gg, notes = FALSE, warn = FALSE)
    return(c(eta = coef(m)[["w_full"]], fs_F = NA))
  }
  dl <- unique(gg[, .(dest, coh, w_full, w_h1, w_h2, bartik_r)], by = c("dest", "coh"))
  if (iv == "split") { dl[, `:=`(x = w_h1, z = w_h2)] } else if (iv == "split_rev") { dl[, `:=`(x = w_h2, z = w_h1)] } else { dl[, `:=`(x = w_full, z = bartik_r)] }
  dl <- dl[!is.na(x) & !is.na(z)]
  fs <- feols(x ~ z | dest + coh, dl, notes = FALSE, warn = FALSE)
  dl[, vhat := resid(fs)]
  gg <- merge(gg, dl[, .(dest, coh, x, vhat)], by = c("dest", "coh"))
  m <- fepois(N ~ x + vhat | od + oc, gg, notes = FALSE, warn = FALSE)
  c(eta = coef(m)[["x"]], fs_F = (coef(fs)[["z"]] / se(fs, vcov = "hetero")[["z"]])^2)
}
run <- function(outcome, iv, sub = "all") {
  gg <- if (sub == "interstate") g0[inter == TRUE] else g0
  gg <- gg[!is.na(w_full) & !is.na(w_h1) & !is.na(w_h2)]
  if (iv == "bartik") gg <- gg[!is.na(bartik)]
  pt <- est_once(gg, outcome, iv)
  keys <- unique(gg$dest); setkey(gg, dest)
  bs <- unlist(mclapply(seq_len(B), function(b) {
    set.seed(7000 + b); setFixest_nthreads(1); setDTthreads(1)
    draw <- sample(keys, length(keys), replace = TRUE)
    gb <- rbindlist(lapply(seq_along(draw), function(k) { y <- gg[.(draw[k])]; y[, `:=`(dest = paste(dest, k), od = paste(od, k))]; y }))
    r <- try(est_once(gb, outcome, iv)[["eta"]], silent = TRUE); if (inherits(r, "try-error")) NA_real_ else r
  }, mc.cores = as.integer(Sys.getenv("SLURM_CPUS_PER_TASK", "4"))))
  data.table(outcome = outcome, iv = iv, sample = sub, dest = uniqueN(gg$dest), people = sum(gg[[outcome]]), fs_F = round(pt[["fs_F"]], 1),
             eta = round(pt[["eta"]], 3), se = round(sd(bs, na.rm = TRUE), 3), lo = round(quantile(bs, .025, na.rm = TRUE), 3),
             hi = round(quantile(bs, .975, na.rm = TRUE), 3), B_ok = sum(!is.na(bs)))
}
R <- rbindlist(list(
  run("M_work", "none"), run("M_work", "split"), run("M_work", "split_rev"), run("M_work", "bartik"),
  run("M_work", "none", "interstate"), run("M_work", "split", "interstate"),
  run("M_hh", "none"), run("M_hh", "split"), run("F_mar", "none"), run("F_mar", "split")
))
fwrite(R, file.path(OUT, "cohort_design.csv"))
cat("\neta from changes within origin-destination pairs (bootstrap over destinations, B =", B, ")\n"); print(R)
