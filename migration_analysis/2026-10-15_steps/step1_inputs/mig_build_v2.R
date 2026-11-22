## Step 1 (15 Oct 2026): destination-choice grids for urban work migrants, rebuilt.
## Same model and data as migration_choice/mig_build.R (9 Oct), with switches for the sample fixes:
##   drop_within: NSS 64 keeps only migrants whose last residence was in another district (Block 6 col 11 = 3-6),
##                as the PLFS sample already does (loc 3-6);
##   men:         men aged 15-64 only, as in the theta sample (NSS 64 sex and age from Block 4, PLFS from cols 41-44);
##   weighted:    counts weighted by the survey multipliers (NSS 64 wgt_combined, PLFS cols 352-361), scaled to the sample size.
## destination_attributes_v2.csv (build_dest_v2.R) rebuilds PM2.5 and urban population on 2011 Census codes.
## The PM2.5 control gets a version that keeps every destination: zeros are set to the smallest positive value and
## missing values to the state mean of log PM2.5, with a missing indicator (lpm, pm_miss). The old log(pm25) dropped
## 57 destinations with no PM2.5 and, silently, 2 with zero PM2.5.
suppressMessages({library(dplyr); library(readr); library(data.table); library(fixest); library(sf)})
options(width = 200)
hav <- function(lon1, lat1, lon2, lat2) { r <- pi/180; a <- sin((lat2-lat1)*r/2)^2 + cos(lat1*r)*cos(lat2*r)*sin((lon2-lon1)*r/2)^2; 2*6371*asin(sqrt(a)) }
g1 <- function(s, a, w) trimws(substr(s, a, a + w - 1))
## secondary school or more. The two surveys code general education differently:
##   NSS 64 (Sch 10.2, Block 4 col 7): 01-05 no formal schooling, 06 below primary, 07 primary, 08 middle, 10 secondary,
##     11 higher secondary, 12 diploma, 13 graduate, 14 postgraduate;
##   PLFS 2020-21 (cols 46-47): 01-04 no formal schooling, 05 below primary, 06 primary, 07 middle, 08 secondary,
##     10 higher secondary, 11 diploma, 12 graduate, 13 postgraduate.
## Anything else (blank, invalid) is missing.
edu_sec <- function(code, survey) {
  hi <- if (survey == "nss64") c("10", "11", "12", "13", "14") else c("08", "10", "11", "12", "13")
  lo <- if (survey == "nss64") sprintf("%02d", 1:8) else sprintf("%02d", 1:7)
  fifelse(code %in% hi, 1L, fifelse(code %in% lo, 0L, NA_integer_))
}

prep_D <- function(path) {
  D <- as.data.table(read_csv(path, show_col_types = FALSE))
  D[, st2 := substr(pc11_sd_id, 1, 2)]
  D[, pm_miss := as.integer(is.na(pm25))]
  D[, lpm := log(pmax(pm25, min(pm25[pm25 > 0], na.rm = TRUE)))]
  D[, lpm := fifelse(is.na(lpm), mean(lpm, na.rm = TRUE), lpm), by = st2]
  D[is.na(lpm), lpm := mean(D$lpm, na.rm = TRUE)]
  D
}

load_inputs <- function(IN, POPF, B4F, ATTRF = file.path(IN, "destination_attributes.csv")) {
  lk <- read_csv(file.path(IN, "district_lookup.csv"), show_col_types = FALSE)
  D <- prep_D(ATTRF)
  pop <- fread(POPF, select = c("pc11_sd_id", "geometry", "pop_11_proj"))
  g <- st_as_sfc(pop$geometry, crs = 4326); sf_use_s2(FALSE)
  cen <- suppressWarnings(st_coordinates(st_point_on_surface(st_make_valid(g))))
  cent <- data.frame(pc11_sd_id = pop$pc11_sd_id, lon = cen[, 1], lat = cen[, 2], pop11 = pop$pop_11_proj)
  nss <- fread(file.path(IN, "nss64_block6.csv"), colClasses = "character")
  b4 <- fread(B4F, colClasses = "character", select = c("key_memb", "B4_c4", "B4_c5", "B4_c7"))
  stopifnot(!anyDuplicated(b4$key_memb))
  n0 <- nrow(nss); nss <- merge(nss, b4, by = "key_memb", all.x = TRUE); stopifnot(nrow(nss) == n0)
  pl <- readLines(file.path(IN, "plfs2021_PERV1.txt"))
  plfs <- data.table(sector = g1(pl, 12, 1), state = g1(pl, 13, 2), district = g1(pl, 15, 2), sex = g1(pl, 41, 1), gedu = g1(pl, 46, 2),
                     age = suppressWarnings(as.integer(g1(pl, 42, 3))), enum_diff = g1(pl, 313, 1),
                     loc = g1(pl, 316, 1), origin = g1(pl, 317, 2), reason = g1(pl, 319, 2),
                     wt = suppressWarnings(as.numeric(g1(pl, 352, 10))))
  rm(pl)
  list(lk = lk, D = D, cent = cent, nss = nss, plfs = plfs)
}

make_grids <- function(X, drop_within = TRUE, men = TRUE, verbose = TRUE, weighted = FALSE, by_edu = FALSE) {
  lk <- X$lk; D <- X$D; cent <- X$cent
  st_of <- lk %>% filter(!is.na(pc11_sd_id)) %>% distinct(state, pc11_sd_id) %>% mutate(state = as.integer(state))
  state_cent <- function(scheme) {
    m <- st_of %>% mutate(code = if (scheme == "nss64") ifelse(state == 36, 28L, state) else state) %>% inner_join(cent, by = "pc11_sd_id")
    out <- m %>% group_by(code) %>% summarise(olon = weighted.mean(lon, pop11, na.rm = TRUE), olat = weighted.mean(lat, pop11, na.rm = TRUE), .groups = "drop")
    if (scheme == "plfs") out <- bind_rows(out, cent %>% filter(pc11_sd_id %in% c("01-003", "01-004")) %>% summarise(code = 37L, olon = mean(lon), olat = mean(lat)))
    out
  }
  ## NSS 64: urban work migrants (reasons 01-06)
  nss <- X$nss[B6_c7 == "1" & Sector == "2" & B6_c16 %in% sprintf("%02d", 1:6)]
  cnt <- c(nss64_all = nss[as.numeric(B6_c10) <= 5, .N])
  if (drop_within) nss <- nss[B6_c11 %in% c("3", "4", "5", "6")]
  cnt <- c(cnt, nss64_other_district = nss[as.numeric(B6_c10) <= 5, .N])
  if (men) nss <- nss[B4_c4 == "1" & as.integer(B4_c5) >= 15 & as.integer(B4_c5) <= 64]
  cnt <- c(cnt, nss64_after_sex_age = nss[as.numeric(B6_c10) <= 5, .N])
  nss[, `:=`(yrs = as.numeric(B6_c10), origin = as.integer(B6_c13), dstate = as.integer(state), nsscode = as.integer(state) * 100L + as.integer(District),
             wt = as.numeric(wgt_combined), sec = edu_sec(B4_c7, "nss64"))]
  nss <- merge(nss, lk %>% filter(!is.na(nsscode)) %>% distinct(nsscode, .keep_all = TRUE) %>% transmute(nsscode = as.integer(nsscode), dest = pc11_sd_id), by = "nsscode")
  nss[, origin := ifelse(origin == 36L, 28L, origin)]
  ## PLFS 2020-21: urban work migrants (reasons 01-02) from another district
  plfs <- X$plfs[sector == "2" & enum_diff == "1" & reason %in% c("01", "02") & loc %in% c("3", "4", "5", "6")]
  cnt <- c(cnt, plfs_all = nrow(plfs))
  if (men) plfs <- plfs[sex == "1" & age >= 15 & age <= 64]
  cnt <- c(cnt, plfs_after_sex_age = nrow(plfs))
  plfs[, `:=`(origin = as.integer(origin), dstate = as.integer(state), plfscode = as.integer(state) * 100L + as.integer(district),
              sec = edu_sec(gedu, "plfs"))]
  plfs <- merge(plfs, lk %>% filter(!is.na(plfscode)) %>% distinct(plfscode, .keep_all = TRUE) %>% transmute(plfscode = as.integer(plfscode), dest = pc11_sd_id), by = "plfscode")
  dst_all <- lk %>% filter(!is.na(pc11_sd_id)) %>% distinct(pc11_sd_id, .keep_all = TRUE) %>% transmute(dest = pc11_sd_id, dstate_lk = as.integer(state))
  build <- function(mig, scheme, period) {
    mw <- mean(mig$wt, na.rm = TRUE)   # overall mean weight (not by cell), so weighted counts keep the sample's scale
    by_v <- if (by_edu) c("origin", "dest", "sec") else c("origin", "dest")
    if (by_edu) mig <- mig[!is.na(sec)]
    cn <- if (weighted) mig[, .(N = sum(wt) / mw), by = by_v] else mig[, .N, by = by_v]
    oc <- state_cent(scheme) %>% rename(origin = code)
    dd <- as.data.frame(D) %>% inner_join(cent, by = "pc11_sd_id") %>% rename(dest = pc11_sd_id) %>% left_join(dst_all, by = "dest") %>%
      mutate(dstate = if (scheme == "nss64") ifelse(dstate_lk == 36, 28L, dstate_lk) else dstate_lk)
    grid <- if (by_edu) CJ(origin = unique(cn$origin), dest = dd$dest, sec = 0:1) else CJ(origin = unique(cn$origin), dest = dd$dest)
    grid <- merge(grid, cn, by = by_v, all.x = TRUE); grid[is.na(N), N := 0]
    grid <- merge(grid, as.data.table(dd), by = "dest"); grid <- merge(grid, as.data.table(oc), by = "origin")
    grid[, `:=`(dist = hav(olon, olat, lon, lat), same_state = as.integer(origin == dstate))]
    grid[, log_dist := log(pmax(dist, 25))]
    if (period == "early") grid[, `:=`(heat26 = c26_9908, heat32 = c32_9908, heat38 = c38_9908, lwage = wage_eus, lcons = cons_early, lrent = rent_hedonic)]
    else grid[, `:=`(heat26 = c26_1222, heat32 = c32_1222, heat38 = c38_1222, lwage = wage_plfs, lcons = cons_late, lrent = rent76_hed)]
    grid
  }
  gE <- build(nss[yrs <= 5], "nss64", "early"); gL <- build(plfs, "plfs", "late")
  cnt <- c(cnt, nss64_in_grid = sum(gE$N), plfs_in_grid = sum(gL$N))
  if (verbose) { cat(sprintf("  drop_within=%s men=%s:\n", drop_within, men)); print(cnt) }
  list(gE = gE, gL = gL, counts = cnt)
}
