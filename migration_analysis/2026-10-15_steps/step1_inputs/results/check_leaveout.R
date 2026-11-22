source("mig_build_v2.R")
G <- readRDS("out/grids_step1b.rds")
gE <- copy(G$gE)[, survey := "nss64"]; gL <- copy(G$gL)[, survey := "plfs21"]
g <- rbind(gE, gL, fill = TRUE); g[, `:=`(origin_s = paste(survey, origin), heat = heat32, w = lwage)]
v <- c("N","w","heat","log_dist","same_state","log_urban_pop","coast_dist","slope","pm25","manuf_share","ec_emp_pc")
gg <- g[complete.cases(g[, ..v]) & pm25 > 0]
rhs <- "heat + log_dist + same_state + log_urban_pop + coast_dist + slope + log(pm25) + manuf_share + ec_emp_pc | origin_s"
fit <- function(d, lab) rbindlist(lapply(c(4, 12.9), function(th) { d <- copy(d); d[, off := th * w]
  r <- fepois(as.formula(paste("N ~", rhs)), d, offset = ~off, cluster = ~dest, notes = FALSE, warn = FALSE)
  b <- coef(r)[["heat"]]; s <- se(r)[["heat"]]
  data.table(spec = lab, theta = th, dest = uniqueN(d$dest), migrants = sum(d$N), cost30 = round(-30*b/th, 4), lo = round(-30*(b+1.96*s)/th, 4), hi = round(-30*(b-1.96*s)/th, 4)) }))
st <- substr(gg$dest, 1, 2); tel <- gg$dest %in% sprintf("28-%03d", 532:541)
metro <- c("07-090","07-091","07-092","07-093","07-095","07-096","07-097","07-098","27-518","19-342","33-603","28-536","29-572","24-474","27-521")
out <- rbindlist(list(fit(gg, "baseline (variant 9)"), fit(gg[st != "07"], "without Delhi"), fit(gg[!tel], "without Telangana"),
  fit(gg[st != "07" & !tel], "without Delhi and Telangana"), fit(gg[!(dest %in% metro)], "without the big metros"),
  fit(gg[survey == "nss64"], "NSS 64 only"), fit(gg[survey == "plfs21"], "PLFS only")))
print(out[order(theta)])
cat("\nMigrants in Delhi districts:", gg[st == "07", sum(N)], "| Telangana:", gg[tel == TRUE, sum(N)], "| big metros:", gg[dest %in% metro, sum(N)], "of", sum(gg$N), "\n")
