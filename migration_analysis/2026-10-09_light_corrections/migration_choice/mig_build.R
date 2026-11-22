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

ctl <- " + coast_dist + slope + log(pm25) + manuf_share + ec_emp_pc"
gE <- build(nss[yrs <= 5], "nss64", "early"); gL <- build(plfs, "plfs", "late")
