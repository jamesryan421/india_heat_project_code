## Census 2011 D-03: people in each destination district by state of last residence -> migration shares pi_od
## pi_od = (people whose last residence was state o, living in district d) / (all people whose last residence was state o)
## Natives of d's own state = district population - migrants from other states - migrants from abroad - unclassifiable.
suppressMessages({library(data.table); library(readxl); library(haven); library(sf)})
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; POPF <- args[2]; OUT <- args[3]
options(width = 200)
st_names <- c("Jammu & Kashmir","Himachal Pradesh","Punjab","Chandigarh","Uttarakhand","Haryana","NCT of Delhi","Rajasthan",
              "Uttar Pradesh","Bihar","Sikkim","Arunachal Pradesh","Nagaland","Manipur","Mizoram","Tripura","Meghalaya","Assam",
              "West Bengal","Jharkhand","Odisha","Chhattisgarh","Madhya Pradesh","Gujarat","Daman & Diu","Dadra & Nagar Haveli",
              "Maharashtra","Andhra Pradesh","Karnataka","Goa","Lakshadweep","Kerala","Tamil Nadu","Puducherry","Andaman & Nicobar Islands")
syn <- c("Jammu and Kashmir" = "Jammu & Kashmir", "Delhi" = "NCT of Delhi", "Orissa" = "Odisha", "Pondicherry" = "Puducherry",
         "Uttaranchal" = "Uttarakhand", "Andaman and Nicobar Islands" = "Andaman & Nicobar Islands", "Daman and Diu" = "Daman & Diu",
         "Dadra and Nagar Haveli" = "Dadra & Nagar Haveli", "A & N Islands" = "Andaman & Nicobar Islands")
num <- function(v) suppressWarnings(as.numeric(gsub("[^0-9.]", "", v)))
rd <- function(f) {
  x <- as.data.table(suppressMessages(read_excel(f, col_names = FALSE, col_types = "text")))
  setnames(x, paste0("v", seq_len(ncol(x))))
  x <- x[grepl("^D", v1) & !is.na(v3)]
  x[, .(st = v2, dist = v3, area = trimws(v5), dur = trimws(v6), lr = trimws(gsub("\\s+", " ", v7)), lra = trimws(v8),
        P = num(v9), M = num(v10), Fe = num(v11), Mw = num(v13), Pw = num(v12))]
}
files <- sort(list.files(file.path(IN, "d03"), pattern = "xlsx$", full.names = TRUE))
d03 <- rbindlist(lapply(files, rd))
cat("D-03 rows read:", nrow(d03), "| states:", uniqueN(d03$st), "| durations:", paste(unique(d03$dur), collapse = "; "), "\n")
w <- d03[dur == "All durations of residence" & lra == "Total" & dist != "000"]
w[lr %in% names(syn), lr := syn[lr]]
w[, d := paste0(st, "-", dist)]
w[, o := fcase(lr == "Total", "TOT", lr == "States in India beyond the state of enumeration", "BEYOND",
               lr == "Last residence outside India", "ABROAD", lr == "Unclassifiable", "UNCL",
               lr %in% st_names, sprintf("%02d", match(lr, st_names)), default = NA_character_)]
cat("Last-residence labels not mapped (expected: within-state and country rows):\n"); print(w[is.na(o), .N, by = lr])
cat("Areas:", paste(unique(w$area), collapse = "; "), "\n")
w[, area := fcase(area == "Total", "T", area == "Rural", "R", area == "Urban", "U")]
agg <- dcast(w[o %in% c("TOT", "BEYOND", "ABROAD", "UNCL")], d + area ~ o, value.var = c("M", "P"), fun.aggregate = sum)
inter <- w[grepl("^[0-9]{2}$", o) & o != substr(d, 1, 2), .(M = sum(M, na.rm = TRUE), P = sum(P, na.rm = TRUE), Mw = sum(Mw, na.rm = TRUE), Pw = sum(Pw, na.rm = TRUE)), by = .(d, area, o)]
chk <- merge(inter[, .(sumM = sum(M)), by = .(d, area)], agg[, .(d, area, M_BEYOND)], by = c("d", "area"))
cat(sprintf("Check: sum of state rows vs 'beyond the state' row, max abs gap = %.0f (district-areas %d)\n", max(abs(chk$sumM - chk$M_BEYOND)), nrow(chk)))
## population by district and area (Census 2011 PCA total; rural from the village directory; urban = total - rural)
pca <- as.data.table(read_dta(file.path(IN, "pc11_pca_clean_pc11dist.dta")))[, .(d = paste0(pc11_state_id, "-", pc11_district_id), T_m = pc11_pca_tot_m, T_p = pc11_pca_tot_p)]
vd  <- as.data.table(read_dta(file.path(IN, "pc11_vd_clean_pc11dist.dta")))[, .(d = paste0(pc11_state_id, "-", pc11_district_id), R_m = pc11_vd_t_m, R_p = pc11_vd_t_p)]
pop <- merge(pca, vd, by = "d", all.x = TRUE); pop[is.na(R_m), `:=`(R_m = 0, R_p = 0)]
pop[, `:=`(U_m = pmax(T_m - R_m, 0), U_p = pmax(T_p - R_p, 0))]
popl <- rbind(pop[, .(d, area = "T", pop_m = T_m, pop_p = T_p)], pop[, .(d, area = "R", pop_m = R_m, pop_p = R_p)], pop[, .(d, area = "U", pop_m = U_m, pop_p = U_p)])
cat(sprintf("Districts: D-03 %d, PCA %d, matched %d\n", uniqueN(agg$d), nrow(pca), length(intersect(agg$d, pca$d))))
nat <- merge(agg, popl, by = c("d", "area"))
nat[, `:=`(M = pop_m - M_BEYOND - M_ABROAD - M_UNCL, P = pop_p - P_BEYOND - P_ABROAD - P_UNCL, o = substr(d, 1, 2))]
cat(sprintf("Male migrants as share of male population (T): %.3f; interstate male migrants: %.1f million; negative native counts: %d\n",
            nat[area == "T", sum(M_TOT) / sum(pop_m)], nat[area == "T", sum(M_BEYOND)] / 1e6, nat[, sum(M < 0)]))
nat[M < 0, M := NA]; nat[P < 0, P := NA]
cells <- rbind(inter[, .(d, area, o, M, P, Mw, Pw)], nat[, .(d, area, o, M, P, Mw = NA_real_, Pw = NA_real_)])
No <- cells[area == "T", .(N_m = sum(M, na.rm = TRUE), N_p = sum(P, na.rm = TRUE)), by = o]
cells <- merge(cells, No, by = "o")
cells[, `:=`(pi_m = M / N_m, pi_p = P / N_p, pi_w = Mw / N_m)]
wide <- dcast(cells, o + d ~ area, value.var = c("M", "pi_m", "pi_p", "pi_w"))
cat("People by state of last residence (millions, males):\n"); print(No[order(o), .(o, N_m = round(N_m / 1e6, 2))], nrows = 40)
cat("Share of each state's men living in another state (from the shares):\n")
print(merge(wide[substr(d, 1, 2) != o, .(out = sum(pi_m_T, na.rm = TRUE)), by = o], No[, .(o)], by = "o")[order(-out)][, out := round(out, 4)], nrows = 40)
## ---------------- geography: district centroids, origin-state centroids, distance, contiguity ----------------
pg <- fread(POPF, select = c("pc11_sd_id", "geometry", "pop_11_proj"))
sf_use_s2(FALSE)
g <- st_make_valid(st_as_sfc(pg$geometry, crs = 4326))
cen <- suppressWarnings(st_coordinates(st_point_on_surface(g)))
cent <- data.table(d = pg$pc11_sd_id, lon = cen[, 1], lat = cen[, 2], pop11 = pg$pop_11_proj)
cent <- cent[!grepl("-000$", d)]
cent[, s := substr(d, 1, 2)]
scen <- cent[, .(olon = weighted.mean(lon, pop11, na.rm = TRUE), olat = weighted.mean(lat, pop11, na.rm = TRUE)), by = .(o = s)]
## state contiguity from touching district polygons (10 km tolerance for slivers)
keep <- !grepl("-000$", pg$pc11_sd_id)
gs <- st_buffer(st_simplify(g[keep], dTolerance = 0.005), 0.05)
nb <- st_intersects(gs, gs)
sid <- substr(pg$pc11_sd_id[keep], 1, 2)
adj <- unique(rbindlist(lapply(seq_along(nb), function(i) data.table(a = sid[i], b = sid[nb[[i]]])))[a != b])
cat("Contiguous state pairs:", nrow(adj) / 2, "\n")
hav <- function(lon1, lat1, lon2, lat2) { r <- pi / 180; a <- sin((lat2 - lat1) * r / 2)^2 + cos(lat1 * r) * cos(lat2 * r) * sin((lon2 - lon1) * r / 2)^2; 2 * 6371 * asin(sqrt(a)) }
wide <- merge(wide, scen, by = "o", all.x = TRUE)
wide <- merge(wide, cent[, .(d, lon, lat)], by = "d", all.x = TRUE)
wide[, `:=`(dist_km = hav(olon, olat, lon, lat), same_state = as.integer(o == substr(d, 1, 2)))]
wide[, contig := as.integer(paste(o, substr(d, 1, 2)) %in% paste(adj$a, adj$b))]
## neighbour-origin shares: share of men from states bordering o (other than d's state) living in d
nbr <- adj[, .(o = a, nb = b)]
nshare <- merge(nbr, cells[area == "T", .(nb = o, d, M, N_m)], by = "nb", allow.cartesian = TRUE)[nb != substr(d, 1, 2)]
nshare <- nshare[, .(pi_nbr = sum(M, na.rm = TRUE) / sum(N_m)), by = .(o, d)]
wide <- merge(wide, nshare, by = c("o", "d"), all.x = TRUE)
fwrite(wide, file.path(OUT, "pi_cells.csv"))
fwrite(No, file.path(OUT, "origin_totals.csv"))
cat(sprintf("Wrote %d origin-destination cells (%d with positive interstate male counts)\n", nrow(wide), wide[same_state == 0 & M_T > 0, .N]))
