## Step 5c (22 Nov 2026): do older migrants avoid hot districts more than younger ones?
## Same model, sample and data as step 5b (step5b_education.R, 15 Oct), with the education split replaced by an age
## split. Survey migrants: men 15-64 who moved for work from another district (NSS 64: last five years; PLFS 2020-21:
## any duration), counts weighted by the survey multipliers. Destination x survey effects absorb every attraction of a
## place; origin x group x survey effects absorb where each group comes from. Identified: heat x group.
##
## Part 1 reruns step 5b's education split. It has to reproduce the 15 Oct result (pooled -0.0109, s.e. 0.0567;
##   5,775 and 4,577 migrants; 638 destinations), or the local inputs differ from the ones used on NOTS.
## Part 2 splits by age at 30 (about the migrants' median) and at 35. The grouping reuses make_grids(by_edu = TRUE)
##   unchanged: the education fields are overwritten with an age code ("10"/"08" = older, "01" = younger), so the
##   sample, fixed effects and clustering are exactly those of step 5b.
## Part 3 reweights to urban men aged 15-64 in PLFS 2020-21 (first visit, weighted). If older migrants' value of
##   heat differs from younger migrants' by D, the value for the target population differs from the migrants' by
##   (share older in target - share older among migrants) x D. Same for education. Assumes eta is the same in both
##   groups and that, within a group, movers and non-movers have the same taste for heat.
## Usage: Rscript step5c_age.R <IN> <POPF> <B4F> <attributes_v2> <out dir>
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; B4F <- args[3]; ATTR2 <- args[4]; OUT <- args[5]
source("mig_build_v2.R")
X <- load_inputs(IN, POPF, B4F, ATTR2)

fit <- function(G, label) {
  gE <- copy(G$gE)[, survey := "nss64"]; gL <- copy(G$gL)[, survey := "plfs21"]
  out <- list()
  for (sv in c("pool", "nss64", "plfs21")) {
    g <- if (sv == "pool") rbind(gE, gL, fill = TRUE) else if (sv == "nss64") copy(gE) else copy(gL)
    g[, `:=`(h = heat32 / 30, os = paste(survey, origin, sec), ds = paste(survey, dest))]
    g <- g[!is.na(h) & is.finite(log_dist)]
    m <- fepois(N ~ h:sec + log_dist + log_dist:sec + same_state + same_state:sec | os + ds, g, cluster = ~ dest, notes = FALSE, warn = FALSE)
    out[[sv]] <- data.table(split = label, sample = sv, coef = coef(m)[["h:sec"]], se = se(m)[["h:sec"]],
                            migrants_1 = round(sum(g[sec == 1, N])), migrants_0 = round(sum(g[sec == 0, N])), dest = uniqueN(g$dest))
  }
  rbindlist(out)
}

## Part 1: step 5b replication
R_edu <- fit(make_grids(X, TRUE, TRUE, verbose = FALSE, weighted = TRUE, by_edu = TRUE), "secondary or more")
cat("\nPart 1, step 5b replication (15 Oct pooled: coef -0.0109, se 0.0567, 5775 / 4577 migrants, 638 destinations)\n")
print(R_edu[, .(split, sample, coef = round(coef, 4), se = round(se, 4), migrants_1, migrants_0, dest)])
ok <- abs(R_edu[sample == "pool", coef] - (-0.0109)) < 5e-4 && abs(R_edu[sample == "pool", se] - 0.0567) < 5e-4 &&
      R_edu[sample == "pool", migrants_1] == 5775 && R_edu[sample == "pool", migrants_0] == 4577 && R_edu[sample == "pool", dest] == 638
cat("Replication matches the 15 Oct run:", ok, "\n")
if (!ok) stop("Step 5b not reproduced; stopping before the age split.")

## Part 2: age splits
age_grids <- function(cut) {
  Y <- X; Y$nss <- copy(X$nss); Y$plfs <- copy(X$plfs)
  Y$nss[, B4_c7 := fifelse(as.integer(B4_c5) >= cut, "10", "01")]
  Y$plfs[, gedu := fifelse(age >= cut, "08", "01")]
  make_grids(Y, TRUE, TRUE, verbose = FALSE, weighted = TRUE, by_edu = TRUE)
}
R_age <- rbind(fit(age_grids(30), "age 30 or more"), fit(age_grids(35), "age 35 or more"))
for (eta in c(2, 2.9, 4)) R_age[, paste0("extra_cost_eta", eta) := round(-100 * coef / eta, 2)]
cat("\nPart 2, heat x older: difference in the log-flow effect of 30 more hot days, older minus younger migrants\n",
    "(negative = older migrants avoid heat more); extra_cost_etaX = their extra cost of 30 hot days, percent of income\n")
print(R_age[, .(split, sample, coef = round(coef, 4), se = round(se, 4), migrants_1, migrants_0, dest,
                extra_cost_eta2, extra_cost_eta2.9, extra_cost_eta4)])

## Part 3: reweighting to urban men 15-64 (PLFS 2020-21, weighted)
Tg <- X$plfs[sector == "2" & sex == "1" & age >= 15 & age <= 64 & !is.na(wt)]
tgt <- c(`age 30 or more` = Tg[, sum(wt * (age >= 30)) / sum(wt)],
         `age 35 or more` = Tg[, sum(wt * (age >= 35)) / sum(wt)],
         `secondary or more` = Tg[!is.na(edu_sec(gedu, "plfs")), sum(wt * edu_sec(gedu, "plfs")) / sum(wt)])
cat(sprintf("\nTarget: urban men 15-64 in PLFS 2020-21, %d records\n", nrow(Tg)))
R_all <- rbind(R_edu, R_age, fill = TRUE)[sample == "pool"]
W <- R_all[, .(split, coef, se, share_migrants = migrants_1 / (migrants_1 + migrants_0))]
W[, share_target := tgt[split]]
for (eta in c(2.9, 4)) {
  W[, paste0("adjust_eta", eta) := round((share_target - share_migrants) * (-100 * coef / eta), 2)]
  W[, paste0("adjust_eta", eta, "_lo") := round((share_target - share_migrants) * (-100 * (coef + 1.96 * se) / eta), 2)]
  W[, paste0("adjust_eta", eta, "_hi") := round((share_target - share_migrants) * (-100 * (coef - 1.96 * se) / eta), 2)]
}
cat("Part 3, change in the cost of 30 hot days (percent of income) when reweighting from migrants to the target,\n",
    "one characteristic at a time; lo/hi = 95% interval from the heat x group coefficient only\n")
print(W[, lapply(.SD, function(v) if (is.numeric(v)) round(v, 3) else v)])
fwrite(rbind(R_edu, R_age, fill = TRUE), file.path(OUT, "step5c_age_splits.csv"))
fwrite(W, file.path(OUT, "step5c_reweighting.csv"))
