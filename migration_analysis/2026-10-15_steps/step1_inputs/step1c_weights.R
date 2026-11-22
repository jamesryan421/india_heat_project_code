## Step 1c (15 Oct 2026): are the survey grids distorted by using unweighted sample counts? NSS and PLFS sample states
## at different rates, so unweighted counts do not show where migrants actually go. Baseline (variant 9) with and without
## survey weights, plus the destination-level correlation of each with the Census 0-4 year counts.
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; B4F <- args[3]; OUT <- args[4]; ATTR2 <- args[5]; CENS <- args[6]
source("mig_build_v2.R")
X <- load_inputs(IN, POPF, B4F, ATTR2)
G_u <- make_grids(X, TRUE, TRUE, verbose = FALSE)
G_w <- make_grids(X, TRUE, TRUE, verbose = FALSE, weighted = TRUE)
cat("Weights missing: NSS 64", X$nss[is.na(as.numeric(wgt_combined)), .N], "| PLFS", X$plfs[is.na(wt), .N], "\n")
v <- c("N", "w", "heat", "log_dist", "same_state", "log_urban_pop", "coast_dist", "slope", "pm25", "manuf_share", "ec_emp_pc")
rhs <- "heat + log_dist + same_state + log_urban_pop + coast_dist + slope + log(pm25) + manuf_share + ec_emp_pc | origin_s"
fit <- function(G, lab) {
  gE <- copy(G$gE)[, survey := "nss64"]; gL <- copy(G$gL)[, survey := "plfs21"]
  rbindlist(lapply(c("pool", "nss64", "plfs21"), function(sv) {
    g <- if (sv == "pool") rbind(gE, gL, fill = TRUE) else if (sv == "nss64") gE else gL
    g <- copy(g); g[, `:=`(origin_s = paste(survey, origin), heat = heat32, w = lwage)]
    g <- g[complete.cases(g[, ..v]) & pm25 > 0]
    r0 <- fepois(as.formula(paste("N ~ w +", rhs)), g, cluster = ~dest, notes = FALSE, warn = FALSE)
    out <- data.table(spec = lab, sample = sv, theta = "free", est = coef(r0)[["w"]], se = se(r0)[["w"]], cost30 = -30 * coef(r0)[["heat"]] / coef(r0)[["w"]], lo = NA_real_, hi = NA_real_)
    for (th in c(0, 1, 2, 4, 8, 12.9)) {
      g[, off := th * w]
      r <- fepois(as.formula(paste("N ~", rhs)), g, offset = ~off, cluster = ~dest, notes = FALSE, warn = FALSE)
      b <- coef(r)[["heat"]]; s <- se(r)[["heat"]]
      out <- rbind(out, data.table(spec = lab, sample = sv, theta = as.character(th), est = b, se = s,
                                   cost30 = if (th > 0) -30 * b / th else 30 * b, lo = if (th > 0) -30 * (b + 1.96 * s) / th else 30 * (b - 1.96 * s),
                                   hi = if (th > 0) -30 * (b - 1.96 * s) / th else 30 * (b + 1.96 * s)))
    }
    out
  }))
}
R <- rbind(fit(G_u, "unweighted"), fit(G_w, "weighted"))
R[, c("est", "se", "cost30", "lo", "hi") := lapply(.SD, function(z) round(z, 4)), .SDcols = c("est", "se", "cost30", "lo", "hi")]
cat("\nTheta = 0 rows give 30 x the heat effect on flows (not a cost). Other rows: cost of 30 more hot days (positive = cost).\n")
print(R, nrows = 100)
fwrite(R, file.path(OUT, "step1c_weights.csv"))
## destination-level agreement with the Census 0-4 year counts (men who moved for work, urban)
C <- fread(CENS, colClasses = c(dest = "character", o = "character"))[area == "Urban" & dur %in% c("0", "1-4"), .(cN = sum(M_work)), by = dest]
agree <- function(G, lab) {
  s <- rbind(G$gE[, .(sN = sum(N)), by = dest][, survey := "NSS 64"], G$gL[, .(sN = sum(N)), by = dest][, survey := "PLFS"])
  m <- merge(s, C, by = "dest")[sN > 0 & cN > 0]
  m[, .(spec = lab, destinations = .N, cor_log = round(cor(log(sN), log(cN)), 3)), by = survey]
}
cat("\nCorrelation of log counts by destination with the Census:\n"); print(rbind(agree(G_u, "unweighted"), agree(G_w, "weighted")))
