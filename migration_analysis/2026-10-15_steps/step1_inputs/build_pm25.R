## PM2.5 by 2011 district straight from the WUSTL district file (Census 2011 codes), mean of 2010-2020.
## Replaces the name-matched pm25_annual_district.csv, which lost 57 districts (including Delhi, Palghar, Howrah, Goa).
suppressMessages({library(data.table); library(readxl)})
B <- "/Users/tanmaydevi/Library/CloudStorage/Box-Box/Urban Economics India"
x <- as.data.table(read_excel(file.path(B, "Data/RawData/PM25/india_wustl_pm25_bydistrict.xlsx")))
x <- x[!is.na(ST_CEN_CD) & !is.na(censuscode)]
yrs <- paste0("Y", 2010:2020)
x[, pc11_sd_id := sprintf("%02d-%03d", as.integer(ST_CEN_CD), as.integer(censuscode))]   # censuscode = national 2011 district code (DT_CEN_CD is within-state)
x[, pm25_wustl := rowMeans(.SD, na.rm = TRUE), .SDcols = yrs]
cat("WUSTL rows with codes:", nrow(x), "| unique pc11 ids:", uniqueN(x$pc11_sd_id), "| missing PM2.5:", x[is.na(pm25_wustl), .N], "\n")
cat("Delhi rows:\n"); print(x[ST_CEN_CD == 7, .(pc11_sd_id, dist_name, pm25_wustl = round(pm25_wustl, 1))])
D <- fread(file.path(B, "docs/audits/2026-10-09_light_corrections/migration_choice/destination_attributes.csv"))
m <- merge(D[, .(pc11_sd_id, pm25)], x[, .(pc11_sd_id, dist_name, pm25_wustl)], by = "pc11_sd_id", all.x = TRUE)
cat("\nDestinations:", nrow(m), "| old pm25 present:", m[!is.na(pm25), .N], "| new present:", m[!is.na(pm25_wustl), .N], "\n")
cat("Where both exist: correlation", round(m[!is.na(pm25) & !is.na(pm25_wustl), cor(pm25, pm25_wustl)], 4),
    "| mean abs difference", round(m[!is.na(pm25) & !is.na(pm25_wustl), mean(abs(pm25 - pm25_wustl))], 3), "\n")
cat("Largest differences where both exist:\n"); print(m[!is.na(pm25) & !is.na(pm25_wustl)][order(-abs(pm25 - pm25_wustl))][1:8])
cat("Formerly missing, now filled (first 15):\n"); print(m[is.na(pm25) & !is.na(pm25_wustl)][order(-pm25_wustl)][1:15])
cat("Still missing:", m[is.na(pm25_wustl), .N], "\n"); print(m[is.na(pm25_wustl)])
fwrite(x[, .(pc11_sd_id, pm25_wustl)], "pm25_wustl_2010_2020_pc11.csv")
