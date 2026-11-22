## Step 1 (15 Oct 2026): destination attributes with two inputs rebuilt on 2011 Census district codes.
##  - log_urban_pop: Census 2011 PCA total minus village-directory rural population (as in theta_census.R). The old values
##    came from a PLFS-coded file and were missing for 27 districts, including all 10 Telangana districts (Hyderabad), which
##    therefore dropped out of every destination-choice run.
##  - pm25: WUSTL district means for 2010-2020 matched on the national 2011 district code (censuscode), with Telangana's
##    state code 36 mapped back to 28. The old values were name-matched and missing for 57 districts (all of Delhi).
## Usage: Rscript build_dest_v2.R <old destination_attributes.csv> <WUSTL xlsx> <PCA dta> <VD dta> <out csv>
suppressMessages({library(data.table); library(haven); library(readxl)})
a <- commandArgs(trailingOnly = TRUE)
D <- fread(a[1])
## PM2.5
w <- as.data.table(read_excel(a[2]))[!is.na(ST_CEN_CD) & !is.na(censuscode) & ST_CEN_CD != 99]
w[, st := fifelse(ST_CEN_CD == 36, 28L, as.integer(ST_CEN_CD))]
w[, pc11_sd_id := sprintf("%02d-%03d", st, as.integer(censuscode))]
w[, pm25_new := rowMeans(.SD, na.rm = TRUE), .SDcols = paste0("Y", 2010:2020)]
stopifnot(!anyDuplicated(w$pc11_sd_id))
## urban population
pca <- as.data.table(read_dta(a[3]))[, .(pc11_sd_id = paste0(pc11_state_id, "-", pc11_district_id), T_p = pc11_pca_tot_p)]
vd  <- as.data.table(read_dta(a[4]))[, .(pc11_sd_id = paste0(pc11_state_id, "-", pc11_district_id), R_p = pc11_vd_t_p)]
up <- merge(pca, vd, by = "pc11_sd_id", all.x = TRUE); up[is.na(R_p), R_p := 0]
up[, urban_pop_new := pmax(T_p - R_p, 0)]
D <- merge(D, w[, .(pc11_sd_id, pm25_new)], by = "pc11_sd_id", all.x = TRUE)
D <- merge(D, up[, .(pc11_sd_id, urban_pop_new)], by = "pc11_sd_id", all.x = TRUE)
## checks against the old values where both exist
b <- D[!is.na(pm25) & !is.na(pm25_new)]
cat(sprintf("PM2.5: old present %d, new present %d; where both exist corr %.4f, mean abs diff %.3f, max abs diff %.2f\n",
            D[!is.na(pm25), .N], D[!is.na(pm25_new), .N], cor(b$pm25, b$pm25_new), mean(abs(b$pm25 - b$pm25_new)), max(abs(b$pm25 - b$pm25_new))))
u <- D[!is.na(log_urban_pop) & urban_pop_new > 0]
cat(sprintf("Urban population: old present %d, new present (>0) %d; where both exist corr of logs %.4f, mean abs diff of logs %.3f\n",
            D[!is.na(log_urban_pop), .N], D[urban_pop_new > 0, .N], cor(u$log_urban_pop, log(u$urban_pop_new)), mean(abs(u$log_urban_pop - log(u$urban_pop_new)))))
cat("Largest log differences in urban population:\n"); print(u[order(-abs(log_urban_pop - log(urban_pop_new)))][1:8, .(pc11_sd_id, log_urban_pop, log_new = log(urban_pop_new))])
D[, `:=`(pm25_old = pm25, log_urban_pop_old = log_urban_pop)]
D[, `:=`(pm25 = pm25_new, log_urban_pop = fifelse(urban_pop_new > 0, log(urban_pop_new), NA_real_))]
D[, c("pm25_new", "urban_pop_new") := NULL]
cat(sprintf("After rebuild: missing pm25 %d, missing log_urban_pop %d (of %d)\n", D[is.na(pm25), .N], D[is.na(log_urban_pop), .N], nrow(D)))
cat("Telangana after rebuild:\n"); print(D[substr(pc11_sd_id, 1, 2) == "28" & as.integer(substr(pc11_sd_id, 4, 6)) %in% 532:541, .(pc11_sd_id, pm25, log_urban_pop, wage_eus, wage_plfs)])
fwrite(D, a[5])
