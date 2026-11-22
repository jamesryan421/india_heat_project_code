## Step 4 (15 Oct 2026): split-half district wage effects from the NSS 68 urban wage file, for a split-sample instrument.
## Same Mincer as the pipeline (ln wage on age, age^2, male, high school, university, district effects); the full-sample
## effects are checked against wage_eus in destination_attributes.csv. Districts are mapped to 2011 codes as in
## rep_06_alt.R (district_id_2011 as a four-digit PLFS-style code through the district lookup).
suppressMessages({library(haven); library(data.table)})
B <- "/Users/tanmaydevi/Library/CloudStorage/Box-Box/Urban Economics India"
d <- as.data.table(read_dta(file.path(B, "Code/Stata/Albouy_Replication_Early/output/data/nss68_eus_wage_individual.dta")))
d <- d[is.finite(ln_wage)][, `:=`(age = as.numeric(Age), age2 = as.numeric(Age)^2, dist = sprintf("%04d", as.integer(district_id_2011)))]
lk <- fread(file.path(B, "docs/audits/2026-10-09_light_corrections/migration_choice/district_lookup.csv"))
p2 <- unique(lk[!is.na(plfscode), .(dist = sprintf("%04d", plfscode), pc11_sd_id)], by = "dist")
X <- c("age", "age2", "male", "hs", "univ")
fe_of <- function(z) {
  dm <- copy(z)[, c("y", X) := lapply(.SD, function(v) v - mean(v)), by = dist, .SDcols = c("ln_wage", X)]
  b <- coef(lm(as.formula(paste("y ~ 0 +", paste(X, collapse = "+"))), dm))
  z[, r := ln_wage - as.matrix(z[, X, with = FALSE]) %*% b]
  z[, .(fe = mean(r), n = .N), by = dist]
}
set.seed(20261015); d[, half := sample(rep(1:2, length.out = .N)), by = dist]
f0 <- fe_of(copy(d))[n >= 10]; f1 <- fe_of(copy(d[half == 1])); f2 <- fe_of(copy(d[half == 2]))
w <- merge(merge(f0[, .(dist, w_full = fe, n)], f1[, .(dist, w_h1 = fe, n1 = n)], by = "dist"), f2[, .(dist, w_h2 = fe, n2 = n)], by = "dist")
w <- merge(w, p2, by = "dist")
## several survey codes can map to one 2011 district: weighted means, as in rep_06_alt.R
w <- w[, .(w_full = weighted.mean(w_full, n), w_h1 = weighted.mean(w_h1, n1), w_h2 = weighted.mean(w_h2, n2), n = sum(n)), by = pc11_sd_id]
for (v in c("w_full", "w_h1", "w_h2")) w[, (v) := get(v) - weighted.mean(get(v), n)]
D <- fread(file.path(B, "docs/audits/2026-10-09_light_corrections/migration_choice/destination_attributes.csv"))
m <- merge(w, D[, .(pc11_sd_id, wage_eus)], by = "pc11_sd_id")
cat(sprintf("Districts: %d | check: corr(full-sample effect, wage_eus) = %.4f, slope %.3f | corr(half 1, half 2) = %.3f\n",
            nrow(m), cor(m$w_full, m$wage_eus, use = "complete.obs"), coef(lm(wage_eus ~ w_full, m))[2], cor(m$w_h1, m$w_h2)))
fwrite(w, "nss68_wage_halves_pc11.csv")
