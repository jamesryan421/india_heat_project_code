## Destination choice of urban work migrants: NSS 64 (2007-08) and PLFS 2020-21.
## Poisson on origin-state x destination-district counts with origin fixed effects (equivalent to a conditional logit).
suppressMessages({library(dplyr); library(readr); library(data.table); library(fixest); library(sf)})
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; OUT <- args[3]
options(width = 200)
lk <- read_csv(file.path(IN, "district_lookup.csv"), show_col_types = FALSE)
D  <- read_csv(file.path(IN, "destination_attributes.csv"), show_col_types = FALSE)
## district centroids from the population file polygons
pop <- fread(POPF, select = c("pc11_sd_id", "geometry", "pop_11_proj"))
g <- st_as_sfc(pop$geometry, crs = 4326); sf_use_s2(FALSE)
cen <- suppressWarnings(st_coordinates(st_point_on_surface(st_make_valid(g))))
cent <- data.frame(pc11_sd_id = pop$pc11_sd_id, lon = cen[, 1], lat = cen[, 2], pop11 = pop$pop_11_proj)
## origin-state centroids (population-weighted), in each survey's state codes
st_of <- lk %>% filter(!is.na(pc11_sd_id)) %>% distinct(state, pc11_sd_id) %>% mutate(state = as.integer(state))
state_cent <- function(scheme) {
  m <- st_of %>% mutate(code = if (scheme == "nss64") ifelse(state == 36, 28L, state) else state) %>% inner_join(cent, by = "pc11_sd_id")
  out <- m %>% group_by(code) %>% summarise(olon = weighted.mean(lon, pop11, na.rm = TRUE), olat = weighted.mean(lat, pop11, na.rm = TRUE), .groups = "drop")
  if (scheme == "plfs") out <- bind_rows(out, cent %>% filter(pc11_sd_id %in% c("01-003", "01-004")) %>% summarise(code = 37L, olon = mean(lon), olat = mean(lat)))
  out
}
hav <- function(lon1, lat1, lon2, lat2) { r <- pi/180; a <- sin((lat2-lat1)*r/2)^2 + cos(lat1*r)*cos(lat2*r)*sin((lon2-lon1)*r/2)^2; 2*6371*asin(sqrt(a)) }
## ---------------- migrant micro samples ----------------
nss <- fread(file.path(IN, "nss64_block6.csv"), colClasses = "character")
nss <- nss[B6_c7 == "1" & Sector == "2" & B6_c16 %in% sprintf("%02d", 1:6)]
nss[, `:=`(yrs = as.numeric(B6_c10), origin = as.integer(B6_c13), dstate = as.integer(state), nsscode = as.integer(state) * 100L + as.integer(District))]
nss <- merge(nss, lk %>% filter(!is.na(nsscode)) %>% distinct(nsscode, .keep_all = TRUE) %>% transmute(nsscode = as.integer(nsscode), dest = pc11_sd_id), by = "nsscode")
nss[, origin := ifelse(origin == 36L, 28L, origin)]
g1 <- function(s, a, w) trimws(substr(s, a, a + w - 1))
pl <- readLines(file.path(IN, "plfs2021_PERV1.txt"))
plfs <- data.table(sector = g1(pl, 12, 1), state = g1(pl, 13, 2), district = g1(pl, 15, 2), enum_diff = g1(pl, 313, 1),
                   loc = g1(pl, 316, 1), origin = g1(pl, 317, 2), reason = g1(pl, 319, 2))
rm(pl)
plfs <- plfs[sector == "2" & enum_diff == "1" & reason %in% c("01", "02") & loc %in% c("3", "4", "5", "6")]   # moved for work from another district
plfs[, `:=`(origin = as.integer(origin), dstate = as.integer(state), plfscode = as.integer(state) * 100L + as.integer(district))]
plfs <- merge(plfs, lk %>% filter(!is.na(plfscode)) %>% distinct(plfscode, .keep_all = TRUE) %>% transmute(plfscode = as.integer(plfscode), dest = pc11_sd_id), by = "plfscode")
cat(sprintf("NSS 64 urban work migrants mapped to districts: %d (moved within 5 years: %d)\n", nrow(nss), sum(nss$yrs <= 5, na.rm = TRUE)))
cat(sprintf("PLFS 2020-21 urban work migrants from another district, mapped: %d (from another state: %d)\n", nrow(plfs), sum(plfs$loc %in% c("5", "6"))))
## destination state code in each survey's scheme (for the same-state dummy)
dstate_nss <- unique(nss[, .(dest, dstate)])[, .SD[1], by = dest]; dstate_plfs <- unique(plfs[, .(dest, dstate)])[, .SD[1], by = dest]
dst_all <- lk %>% filter(!is.na(pc11_sd_id)) %>% distinct(pc11_sd_id, .keep_all = TRUE) %>% transmute(dest = pc11_sd_id, dstate_lk = as.integer(state))
## ---------------- grid and estimation ----------------
build <- function(mig, scheme, period) {
  cnt <- mig[, .N, by = .(origin, dest)]
  oc <- state_cent(scheme) %>% rename(origin = code)
  dd <- D %>% inner_join(cent, by = "pc11_sd_id") %>% rename(dest = pc11_sd_id) %>% left_join(dst_all, by = "dest") %>%
    mutate(dstate = if (scheme == "nss64") ifelse(dstate_lk == 36, 28L, dstate_lk) else dstate_lk)
  grid <- CJ(origin = unique(cnt$origin), dest = dd$dest)
  grid <- merge(grid, cnt, by = c("origin", "dest"), all.x = TRUE); grid[is.na(N), N := 0]
  grid <- merge(grid, as.data.table(dd), by = "dest"); grid <- merge(grid, as.data.table(oc), by = "origin")
  grid[, `:=`(dist = hav(olon, olat, lon, lat), same_state = as.integer(origin == dstate))]
  grid[, log_dist := log(pmax(dist, 25))]
  if (period == "early") grid[, `:=`(heat26 = c26_9908, heat32 = c32_9908, heat38 = c38_9908, lwage = wage_eus, lcons = cons_early, lrent = rent_hedonic)]
  else grid[, `:=`(heat26 = c26_1222, heat32 = c32_1222, heat38 = c38_1222, lwage = wage_plfs, lcons = cons_late, lrent = rent76_hed)]
  grid
}
wtp <- function(m, wvar) {   # value of 30 more days in each band, in log-wage units (share of income); positive = heat valued
  b <- coef(m); V <- vcov(m); h <- c("heat26", "heat32", "heat38")
  L <- rbind(c(1,0,0), c(1,1,0), c(1,1,1)); out <- NULL
  for (k in 1:3) {
    num <- sum(L[k, ] * b[h]); bw <- b[wvar]; est <- 30 * num / bw
    gvec <- setNames(rep(0, length(b)), names(b)); gvec[h] <- 30 * L[k, ] / bw; gvec[wvar] <- -30 * num / bw^2
    out <- rbind(out, c(est = est, se = sqrt(drop(t(gvec) %*% V %*% gvec))))
  }
  rownames(out) <- c("26-32", "32-38", ">38"); out
}
run <- function(grid, label, wvar = "lwage", controls = "", rent = FALSE, interstate = FALSE) {
  g <- copy(grid); if (interstate) g <- g[same_state == 0]
  rhs <- paste0(wvar, " + heat26 + heat32 + heat38 + log_dist", if (!interstate) " + same_state" else "", " + log_urban_pop", if (rent) " + lrent" else "", controls)
  vars <- all.vars(as.formula(paste("N ~", rhs))); g <- g[complete.cases(g[, ..vars])]
  m <- fepois(as.formula(paste("N ~", rhs, "| origin")), g, cluster = ~dest, notes = FALSE, warn = FALSE)
  w <- wtp(m, wvar); ct <- coeftable(m)
  cat(sprintf("\n--- %s | migrants %d, destinations %d, origins %d\n", label, sum(g$N), uniqueN(g$dest), uniqueN(g$origin)))
  for (v in intersect(c(wvar, "lrent", "heat26", "heat32", "heat38", "log_dist", "same_state", "log_urban_pop"), rownames(ct)))
    cat(sprintf("   %-14s %9.4f (%7.4f)\n", v, ct[v, 1], ct[v, 2]))
  cat(sprintf("   value of 30 more days, share of income: 26-32 %6.3f (%5.3f) | 32-38 %6.3f (%5.3f) | >38 %6.3f (%5.3f)\n", w[1,1], w[1,2], w[2,1], w[2,2], w[3,1], w[3,2]))
  data.frame(spec = label, n_mig = sum(g$N), n_dest = uniqueN(g$dest), b_wage = ct[wvar, 1], se_wage = ct[wvar, 2],
             v2632 = w[1,1], se2632 = w[1,2], v3238 = w[2,1], se3238 = w[2,2], v38 = w[3,1], se38 = w[3,2])
}
ctl <- " + coast_dist + slope + log(pm25) + manuf_share + ec_emp_pc"
gE <- build(nss[yrs <= 5], "nss64", "early"); gE10 <- build(nss[yrs <= 10], "nss64", "early"); gL <- build(plfs, "plfs", "late")
res <- rbind(
  run(gE, "NSS64 <=5y: wage"), run(gE, "NSS64 <=5y: wage + controls", controls = ctl),
  run(gE, "NSS64 <=5y: wage + rent + controls", controls = ctl, rent = TRUE),
  run(gE, "NSS64 <=5y: interstate, wage + controls", controls = ctl, interstate = TRUE),
  run(gE10, "NSS64 <=10y: wage + controls", controls = ctl),
  run(gE, "NSS64 <=5y: consumption + controls", wvar = "lcons", controls = ctl),
  run(gL, "PLFS21: wage"), run(gL, "PLFS21: wage + controls", controls = ctl),
  run(gL, "PLFS21: wage + rent + controls", controls = ctl, rent = TRUE),
  run(gL, "PLFS21: interstate, wage + controls", controls = ctl, interstate = TRUE),
  run(gL, "PLFS21: consumption + controls", wvar = "lcons", controls = ctl))
write_csv(res, file.path(OUT, "mig_choice_results.csv"))

## ---------------- round 2: one heat measure, destination-state FE, pooled surveys, net wage ----------------
run1 <- function(grid, label, wvar = "lwage", hvar = "heat32", controls = "", dstateFE = FALSE, interstate = FALSE, scale = 30) {
  g <- copy(grid); if (interstate) g <- g[same_state == 0]
  rhs <- paste0(wvar, " + ", hvar, " + log_dist", if (!interstate) " + same_state" else "", " + log_urban_pop", controls)
  vars <- c(all.vars(as.formula(paste("N ~", rhs))), "origin", "dstate"); g <- g[complete.cases(g[, ..vars])]
  fe <- if ("origin_s" %in% names(g)) "origin_s" else "origin"; if (dstateFE) fe <- paste(fe, "+ dstate_s")
  if (!"dstate_s" %in% names(g)) g[, dstate_s := dstate]
  m <- fepois(as.formula(paste("N ~", rhs, "|", fe)), g, cluster = ~dest, notes = FALSE, warn = FALSE)
  b <- coef(m); V <- vcov(m); est <- scale * b[hvar] / b[wvar]
  gr <- setNames(rep(0, length(b)), names(b)); gr[hvar] <- scale / b[wvar]; gr[wvar] <- -scale * b[hvar] / b[wvar]^2
  se <- sqrt(drop(t(gr) %*% V %*% gr))
  cat(sprintf("%-48s mig %5d dest %3d | wage %6.3f (%5.3f) | %s %8.5f (%7.5f) | value %6.3f (%5.3f)\n", label, sum(g$N), uniqueN(g$dest),
      b[wvar], se(m)[wvar], hvar, b[hvar], se(m)[hvar], est, se))
  data.frame(spec = label, heat = hvar, n_mig = sum(g$N), n_dest = uniqueN(g$dest), b_wage = b[wvar], se_wage = se(m)[wvar],
             b_heat = b[hvar], se_heat = se(m)[hvar], value = est, se_value = se)
}
for (gg in c("gE", "gE10", "gL")) { x <- get(gg); x[, `:=`(heat32_only = heat32, mean_utci = if (gg == "gL") mean_utci_1222 else mean_utci_9908)]; assign(gg, x) }
gE[, lnet := lwage - 0.2 * lrent]; gL[, lnet := lwage - 0.2 * lrent]
pool <- rbind(gE[, survey := "nss64"], gL[, survey := "plfs21"], fill = TRUE)
pool[, `:=`(origin_s = paste(survey, origin), dstate_s = paste(survey, dstate))]
cat("\nValue = share of income for 30 more days above 32 C (heat32) or for 1 C of mean daytime UTCI (mean_utci); negative = heat is a disamenity\n")
r2 <- rbind(
  run1(gE, "NSS64 <=5y: wage"),
  run1(gE, "NSS64 <=5y: wage + controls", controls = ctl),
  run1(gE, "NSS64 <=5y: wage + controls + dest-state FE", controls = ctl, dstateFE = TRUE),
  run1(gE, "NSS64 <=5y: net wage (rent) + controls", wvar = "lnet", controls = ctl),
  run1(gL, "PLFS21: wage"),
  run1(gL, "PLFS21: wage + controls", controls = ctl),
  run1(gL, "PLFS21: wage + controls + dest-state FE", controls = ctl, dstateFE = TRUE),
  run1(gL, "PLFS21: net wage (rent) + controls", wvar = "lnet", controls = ctl),
  run1(gL, "PLFS21: consumption + controls", wvar = "lcons", controls = ctl),
  run1(pool, "Pooled: wage", ),
  run1(pool, "Pooled: wage + controls", controls = ctl),
  run1(pool, "Pooled: wage + controls + dest-state FE", controls = ctl, dstateFE = TRUE),
  run1(pool, "Pooled: interstate, wage + controls", controls = ctl, interstate = TRUE),
  run1(pool, "Pooled: wage + controls, mean UTCI", hvar = "mean_utci", controls = ctl, scale = 1),
  run1(pool, "Pooled: wage + controls without PM2.5", controls = " + coast_dist + slope + manuf_share + ec_emp_pc"))
write_csv(r2, file.path(OUT, "mig_choice_results_round2.csv"))
