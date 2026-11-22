source("/scratch/td52/heat_steps_2026-10-15/step1/mig_build_v2.R")
G <- readRDS("out/grids_step1.rds")
lk <- fread("/scratch/td52/heat_light_2026-10-09/migchoice/in/district_lookup.csv")
nm <- unique(lk[!is.na(pc11_sd_id), .(dest = pc11_sd_id, state_name, district_name)], by = "dest")
gE <- copy(G$gE)[, survey := "nss64"]; gL <- copy(G$gL)[, survey := "plfs21"]
g <- rbind(gE, gL, fill = TRUE); g[, `:=`(origin_s = paste(survey, origin), heat = heat32, w = lwage)]
base <- c("N","w","heat","log_dist","same_state","log_urban_pop","coast_dist","slope","manuf_share","ec_emp_pc")
gg <- g[complete.cases(g[, ..base])]
dd <- unique(gg[, .(dest, survey, pm25, pm_miss, heat, w, N)][, .(N = sum(N), pm25 = pm25[1], pm_miss = pm_miss[1], heat = mean(heat), w = mean(w)), by = .(dest, survey)])
add <- dd[is.na(pm25) | pm25 == 0][, .(N = sum(N), heat32 = round(mean(heat)), wage = round(mean(w), 2), surveys = paste(survey, collapse = "+")), by = .(dest, pm25)]
add <- merge(add, nm, by = "dest", all.x = TRUE)[order(-N)]
cat("Destinations added by keeping PM2.5 gaps:", nrow(add), "| migrants there:", sum(add$N), "\n"); print(add, nrows = 50)
cat("\nMean heat32 (days/yr): added", round(weighted.mean(add$heat32, add$N), 1), "(migrant-weighted) vs all destinations", round(weighted.mean(dd$heat, dd$N), 1), "\n")
rhs0 <- "heat + log_dist + same_state + log_urban_pop + coast_dist + slope + manuf_share + ec_emp_pc"
fit <- function(d, rhs, th, lab) { d <- copy(d); d[, off := th * w]
  r <- fepois(as.formula(paste("N ~", rhs, "| origin_s")), d, offset = ~off, cluster = ~dest, notes = FALSE, warn = FALSE)
  b <- coef(r)[["heat"]]; s <- se(r)[["heat"]]
  data.table(spec = lab, theta = th, dest = uniqueN(d$dest), migrants = sum(d$N), value30 = round(30*b/th, 4), lo = round(30*(b-1.96*s)/th, 4), hi = round(30*(b+1.96*s)/th, 4)) }
out <- list()
for (th in c(4, 12.9)) {
  out[[length(out)+1]] <- fit(gg, paste(rhs0, "+ lpm + pm_miss"), th, "A all 586, lpm + pm_miss (= variant 3)")
  out[[length(out)+1]] <- fit(gg, rhs0, th, "B all 586, no PM2.5 control")
  out[[length(out)+1]] <- fit(gg[!(is.na(pm25) | pm25 == 0)], rhs0, th, "C 557 with PM2.5, no PM2.5 control")
  out[[length(out)+1]] <- fit(gg[!(is.na(pm25) | pm25 == 0)], paste(rhs0, "+ lpm"), th, "D 557 with PM2.5, lpm (= variant 2)")
  out[[length(out)+1]] <- fit(gg[pm25 != 0 | is.na(pm25)], paste(rhs0, "+ lpm + pm_miss"), th, "E 584: variant 3 without the 2 zero-PM2.5 districts")
}
print(rbindlist(out)[order(theta, spec)])
