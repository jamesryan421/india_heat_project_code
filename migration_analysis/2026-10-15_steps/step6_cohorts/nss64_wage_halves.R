## Cohort design (15 Oct 2026): NSS 64 (2007-08) urban district wage effects, built like the pipeline's NSS 68 ones
## (02_build_nss_eus_wages.do): status 31/41/51 in the current week (Block 5), weekly wage earnings summed over wage
## activities, urban, ages 15-65, Mincer on age, age^2, male and two schooling dummies with district effects.
## NSS 64 codes education differently from NSS 68: the pipeline's hs (NSS 68 code >= 06, primary or more) is NSS 64
## code >= 07, and univ (NSS 68 >= 10, higher secondary or more) is NSS 64 >= 11.
## Also writes split halves, and checks the NSS 64 effects against NSS 68 and the reliability of the 2007-08 to
## 2011-12 change.
suppressMessages({library(data.table)})
B <- "/Users/tanmaydevi/Library/CloudStorage/Box-Box/Urban Economics India"
R64 <- file.path(B, "Data/RawData/NSS/NSSO_64_Sch10.2_2007-08")
b4 <- fread(file.path(R64, "Block-4-demographic-usual-activity-members-records.csv"), colClasses = "character",
            select = c("key_memb", "Sector", "state", "District", "B4_c4", "B4_c5", "B4_c7"))
b5 <- fread(file.path(R64, "Block-5-members-time-disposition-records.csv"), colClasses = "character",
            select = c("key_memb_acty", "B5_c4", "B5_c17"))
b5[, `:=`(key_memb = substr(key_memb_acty, 1, 11), st = as.integer(B5_c4), earn = as.numeric(B5_c17))]
w5 <- b5[st %in% c(31L, 41L, 51L) & earn > 0, .(wage_weekly = sum(earn)), by = key_memb]
d <- merge(b4, w5, by = "key_memb")
cat("NSS 64 wage earners with positive weekly wages:", nrow(d), "| urban:", d[Sector == "2", .N], "\n")
d <- d[Sector == "2"][, `:=`(age = as.numeric(B4_c5), male = as.numeric(B4_c4 == "1"), edu = suppressWarnings(as.integer(B4_c7)))]
d <- d[age >= 15 & age <= 65 & !is.na(edu) & B4_c4 %in% c("1", "2")]
d[, `:=`(hs = as.numeric(edu >= 7), univ = as.numeric(edu >= 11), age2 = age^2, ln_wage = log(wage_weekly),   # numeric: demeaning must not truncate
         dist = sprintf("%04d", as.integer(state) * 100L + as.integer(District)))]
cat("Urban, ages 15-65, education known:", nrow(d), "\n")
lk <- fread(file.path(B, "docs/audits/2026-10-09_light_corrections/migration_choice/district_lookup.csv"))
lkn <- unique(lk[!is.na(nsscode), .(dist = sprintf("%04d", nsscode), pc11_sd_id)], by = "dist")
X <- c("age", "age2", "male", "hs", "univ")
fe_of <- function(z) {
  dm <- copy(z)[, c("y", X) := lapply(.SD, function(v) v - mean(v)), by = dist, .SDcols = c("ln_wage", X)]
  b <- coef(lm(as.formula(paste("y ~ 0 +", paste(X, collapse = "+"))), dm))
  z[, r := ln_wage - as.matrix(z[, X, with = FALSE]) %*% b]
  list(fe = z[, .(fe = mean(r), n = .N), by = dist], b = b)
}
full <- fe_of(copy(d)); cat("Mincer coefficients:", paste(names(full$b), round(full$b, 3), collapse = ", "), "\n")
set.seed(20261016); d[, half := sample(rep(1:2, length.out = .N)), by = dist]
f0 <- full$fe[n >= 10]; f1 <- fe_of(copy(d[half == 1]))$fe; f2 <- fe_of(copy(d[half == 2]))$fe
w <- merge(merge(f0[, .(dist, w_full = fe, n)], f1[, .(dist, w_h1 = fe, n1 = n)], by = "dist"), f2[, .(dist, w_h2 = fe, n2 = n)], by = "dist")
cat("NSS 64 districts with >= 10 earners:", nrow(f0), "| mapped to 2011 districts:", w[dist %in% lkn$dist, .N], "\n")
w <- merge(w, lkn, by = "dist")
w <- w[, .(w_full = weighted.mean(w_full, n), w_h1 = weighted.mean(w_h1, n1), w_h2 = weighted.mean(w_h2, n2), n = sum(n)), by = pc11_sd_id]
for (v in c("w_full", "w_h1", "w_h2")) w[, (v) := get(v) - weighted.mean(get(v), n)]
setnames(w, c("pc11_sd_id", "w64_full", "w64_h1", "w64_h2", "n64"))
fwrite(w, "nss64_wage_halves_pc11.csv")
## checks against NSS 68
h68 <- fread("../step4_flow_elasticity/nss68_wage_halves_pc11.csv"); setnames(h68, c("pc11_sd_id", "w68_full", "w68_h1", "w68_h2", "n68"))
m <- merge(w, h68, by = "pc11_sd_id")
cat(sprintf("Districts with both rounds: %d | median earners NSS 64 %d, NSS 68 %d\n", nrow(m), as.integer(median(m$n64)), as.integer(median(m$n68))))
cat(sprintf("corr(NSS 64, NSS 68) levels %.3f | split-half corr NSS 64 %.3f, NSS 68 %.3f\n", cor(m$w64_full, m$w68_full), cor(m$w64_h1, m$w64_h2), cor(m$w68_h1, m$w68_h2)))
m[, `:=`(dA = w68_h1 - w64_h1, dB = w68_h2 - w64_h2, dF = w68_full - w64_full)]
cat(sprintf("Change 2007-08 to 2011-12: SD %.3f | corr of the change across independent halves %.3f (reliability of the full-sample change about %.2f)\n",
            sd(m$dF), cor(m$dA, m$dB), 2 * cor(m$dA, m$dB) / (1 + cor(m$dA, m$dB))))
