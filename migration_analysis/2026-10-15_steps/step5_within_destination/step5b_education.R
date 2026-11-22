## Step 5b (15 Oct 2026): do better-educated migrants avoid hot districts more?
## Survey migrants (men 15-64 who moved for work from another district; NSS 64: last five years; PLFS 2020-21: any
## duration), counts weighted by the survey multipliers, split by secondary school or more. Destination x survey fixed
## effects absorb every attraction of a place; origin x education x survey effects absorb where each group comes from.
## Identified: heat x secondary, i.e. how much more (or less) the better educated avoid heat than others choosing among
## the same destinations. Distance and same-state effects are allowed to differ by education.
## Usage: Rscript step5b_education.R <IN> <POPF> <B4F> <attributes_v2> <out dir>
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; B4F <- args[3]; ATTR2 <- args[4]; OUT <- args[5]
source("mig_build_v2.R")
X <- load_inputs(IN, POPF, B4F, ATTR2)
G <- make_grids(X, TRUE, TRUE, verbose = FALSE, weighted = TRUE, by_edu = TRUE)
gE <- copy(G$gE)[, survey := "nss64"]; gL <- copy(G$gL)[, survey := "plfs21"]
for (s in c("nss64", "plfs21")) {
  x <- if (s == "nss64") X$nss[B6_c7 == "1" & Sector == "2" & B6_c16 %in% sprintf("%02d", 1:6) & B6_c11 %in% c("3", "4", "5", "6") & B4_c4 == "1" &
                                 as.integer(B4_c5) >= 15 & as.integer(B4_c5) <= 64 & as.numeric(B6_c10) <= 5]
       else X$plfs[sector == "2" & enum_diff == "1" & reason %in% c("01", "02") & loc %in% c("3", "4", "5", "6") & sex == "1" & age >= 15 & age <= 64]
  e <- if (s == "nss64") edu_sec(x$B4_c7, "nss64") else edu_sec(x$gedu, "plfs")
  cat(sprintf("%s: migrants %d, education missing %d, secondary or more %.1f%% (unweighted)\n", s, nrow(x), sum(is.na(e)), 100 * mean(e, na.rm = TRUE)))
}
res <- list()
for (sv in c("pool", "nss64", "plfs21")) {
  g <- if (sv == "pool") rbind(gE, gL, fill = TRUE) else if (sv == "nss64") copy(gE) else copy(gL)
  g[, `:=`(h = heat32 / 30, os = paste(survey, origin, sec), ds = paste(survey, dest))]
  g <- g[!is.na(h) & is.finite(log_dist)]
  m <- fepois(N ~ h:sec + log_dist + log_dist:sec + same_state + same_state:sec | os + ds, g, cluster = ~ dest, notes = FALSE, warn = FALSE)
  k <- "h:sec"
  res[[sv]] <- data.table(sample = sv, coef = coef(m)[[k]], se = se(m)[[k]], migrants_sec = round(sum(g[sec == 1, N])), migrants_less = round(sum(g[sec == 0, N])), dest = uniqueN(g$dest))
}
R <- rbindlist(res)
for (eta in c(2, 4, 12.9)) R[, paste0("extra_cost_eta", eta) := round(-100 * coef / eta, 2)]
R[, `:=`(coef = round(coef, 4), se = round(se, 4))]
cat("\nheat x secondary: difference in the log-flow effect of 30 more hot days, secondary-educated minus others\n",
    "(negative = the better educated avoid heat more); extra_cost_etaX = their extra cost of 30 hot days in percent of income\n")
print(R)
fwrite(R, file.path(OUT, "step5b_education.csv"))
