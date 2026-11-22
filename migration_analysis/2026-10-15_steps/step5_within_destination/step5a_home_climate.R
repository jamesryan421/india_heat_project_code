## Step 5a (15 Oct 2026): do migrants from hot states go relatively more to hot districts?
## Census 2011 D-3 counts of men who moved for work into the urban part of each district in the last 0-4 years, by state
## of last residence. Origin and destination fixed effects: the destination effects absorb wages, rents, jobs, networks
## at the destination and every other attraction of a place, so the only heat term left is the interaction of home
## climate and destination climate. Heat = days above 32 C (UTCI), 2001-2011, in units of 30 days.
## Controls for origin-destination ties that also follow climate: log distance, same state, shared state border,
## same main language of the two states.
## Usage: Rscript step5a_home_climate.R <counts> <attributes_v2> <POPF> <out dir>
suppressMessages({library(data.table); library(fixest); library(sf)})
a <- commandArgs(trailingOnly = TRUE); OUT <- a[4]
options(width = 220)
hav <- function(lon1, lat1, lon2, lat2) { r <- pi/180; x <- sin((lat2-lat1)*r/2)^2 + cos(lat1*r)*cos(lat2*r)*sin((lon2-lon1)*r/2)^2; 2*6371*asin(sqrt(x)) }
C <- fread(a[1], colClasses = c(dest = "character", o = "character"))
D <- fread(a[2])
pop <- fread(a[3], select = c("pc11_sd_id", "geometry", "pop_11_proj"))
pop <- pop[!grepl("-000$", pc11_sd_id)]
sf_use_s2(FALSE)
geom <- st_make_valid(st_as_sfc(pop$geometry, crs = 4326))
cen <- suppressWarnings(st_coordinates(st_point_on_surface(geom)))
cent <- data.table(dest = pop$pc11_sd_id, lon = cen[, 1], lat = cen[, 2], pop11 = pop$pop_11_proj)
oc <- cent[, .(olon = weighted.mean(lon, pop11, na.rm = TRUE), olat = weighted.mean(lat, pop11, na.rm = TRUE)), by = .(o = substr(dest, 1, 2))]
## state adjacency from district polygons (buffered slightly so that slivers do not break shared borders)
dg <- st_sf(st = substr(pop$pc11_sd_id, 1, 2), geometry = geom)
sp <- do.call(rbind, lapply(split(dg, dg$st), function(x) st_sf(st = x$st[1], geometry = st_union(suppressWarnings(st_buffer(x, 0.02))))))
adj <- st_intersects(sp, sp, sparse = FALSE); dimnames(adj) <- list(sp$st, sp$st)
cat("State pairs sharing a border:", (sum(adj) - nrow(adj)) / 2, "| check: Delhi borders", paste(sp$st[adj["07", ]], collapse = " "), "\n")
## main language of each state (Census 2011 largest mother tongue; Arunachal, Nagaland, Meghalaya coded by their own)
lang <- c("01" = "ks", "02" = "hi", "03" = "pa", "04" = "hi", "05" = "hi", "06" = "hi", "07" = "hi", "08" = "hi", "09" = "hi", "10" = "hi",
          "11" = "ne", "12" = "ar", "13" = "nl", "14" = "mni", "15" = "lus", "16" = "bn", "17" = "kha", "18" = "as", "19" = "bn", "20" = "hi",
          "21" = "or", "22" = "hi", "23" = "hi", "24" = "gu", "25" = "gu", "26" = "gu", "27" = "mr", "28" = "te", "29" = "kn", "30" = "kok",
          "31" = "ml", "32" = "ml", "33" = "ta", "34" = "ta", "35" = "bn")
## home climate: population-weighted mean of district heat in the origin state
hd <- merge(D[, .(dest = pc11_sd_id, heat_d = c32_0111 / 30)], cent[, .(dest, pop11)], by = "dest")
ho <- hd[, .(heat_o = weighted.mean(heat_d, pop11, na.rm = TRUE)), by = .(o = substr(dest, 1, 2))]
cn <- C[area == "Urban" & dur %in% c("0", "1-4"), .(N = sum(M_work)), by = .(o, dest)]
g <- merge(cn, D[, .(dest = pc11_sd_id, heat_d = c32_0111 / 30, log_urban_pop)], by = "dest")
g <- merge(g, cent[, .(dest, lon, lat)], by = "dest"); g <- merge(g, oc, by = "o"); g <- merge(g, ho, by = "o")
g[, `:=`(dst = substr(dest, 1, 2), log_dist = log(pmax(hav(olon, olat, lon, lat), 25)))]
g[, `:=`(same_state = as.integer(o == dst), border = as.integer(adj[cbind(o, dst)] & o != dst), same_lang = as.integer(lang[o] == lang[dst]))]
g[, `:=`(hh = heat_o * heat_d, cdist = abs(heat_o - heat_d))]
g <- g[is.finite(log_urban_pop) & !is.na(heat_d)]
cat(sprintf("Cells %d | destinations %d | origins %d | migrants %.2f million\n", nrow(g), uniqueN(g$dest), uniqueN(g$o), sum(g$N) / 1e6))
w <- g[same_state == 0, .(N = sum(N)), by = .(o, heat_o)]
qm <- w[, as.list(quantile(rep(heat_o, round(N / 100)), c(.25, .75)))]
cat(sprintf("Home heat of interstate migrants (30-day units): P25 %.2f, P75 %.2f (difference %.0f days): most come from UP and Bihar\n", qm[[1]], qm[[2]], 30 * (qm[[2]] - qm[[1]])))
q <- as.list(quantile(ho$heat_o, c(.1, .9)))   # spread across the 35 origin states, unweighted
cat(sprintf("Home heat across the 35 origin states: P10 %.2f, P90 %.2f (difference %.0f days); gap below uses P90 - P10\n", q[[1]], q[[2]], 30 * (q[[2]] - q[[1]])))
specs <- list(
  "1 product, distance + same state" = "hh + log_dist + same_state",
  "2 product, + border + language" = "hh + log_dist + same_state + border + same_lang",
  "3 product, + border + language, interstate only" = "hh + log_dist + border + same_lang",
  "4 climate distance |home - destination|, + border + language" = "cdist + log_dist + same_state + border + same_lang",
  "5 climate distance, + border + language, interstate only" = "cdist + log_dist + border + same_lang")
res <- list()
for (nm in names(specs)) {
  d <- if (grepl("interstate", nm)) g[same_state == 0] else g
  m <- fepois(as.formula(paste("N ~", specs[[nm]], "| o + dest")), d, cluster = ~ dest, notes = FALSE, warn = FALSE)
  v <- if (grepl("^[45]", nm)) "cdist" else "hh"
  s2 <- se(summary(m, cluster = ~ o + dest))[[v]]
  res[[nm]] <- data.table(spec = nm, term = v, coef = coef(m)[[v]], se_dest = se(m)[[v]], se_twoway = s2, n = nobs(m))
}
R <- rbindlist(res)
R[, gap := ifelse(term == "hh", coef * (q[[2]] - q[[1]]), NA_real_)]
for (eta in c(2, 4, 12.9)) R[, paste0("cost_gap_eta", eta) := round(-100 * gap / eta, 2)]
num <- c("coef", "se_dest", "se_twoway", "gap"); R[, (num) := lapply(.SD, function(z) round(z, 4)), .SDcols = num]
cat("\nHome climate x destination heat (per 30 days at destination per 30 days at home). gap = difference in the effect of\n",
    "30 more destination hot days between migrants from P90 and P10 home climates (log points of flows);\n",
    "cost_gap_etaX = how much less 30 hot days cost the P90-home migrants, in percent of income, if eta = X\n")
print(R)
fwrite(R, file.path(OUT, "step5a_home_climate.csv"))
