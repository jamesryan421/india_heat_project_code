## Step 3 (15 Oct 2026): Census 2011 D-3 counts of men who moved for work, by state of last residence and destination
## district, by time since moving.
## D-3 = "Migrants by place of last residence, duration of residence and reason for migration". Columns 9-11 total
## migrants (persons, males, females); 12-14 work/employment; 15-17 business; 18-20 education; 21-23 marriage;
## 24-26 moved after birth; 27-29 moved with household; 30-32 others. (theta_census.R calls columns 12 and 13 Pw and Mw:
## they are people who moved for work, not workers.)
## Origins: each other state (interstate), and "in other districts of the state of enumeration" (same state, other
## district), which matches the surveys' "another district". Moves within the district are left out.
## Usage: Rscript census_counts.R <dir with D03_*.xlsx> <out dir>
suppressMessages({library(data.table); library(readxl)})
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; OUT <- args[2]
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
  stopifnot(ncol(x) >= 32)
  hdr <- tolower(paste(unlist(x[1:4]), collapse = " "))
  stopifnot(grepl("reason for migration", hdr), grepl("work/employment", hdr), grepl("business", hdr))
  x <- x[grepl("^D", v1) & !is.na(v3)]
  x[, .(st = v2, dist = v3, area = trimws(v5), dur = trimws(v6), lr = trimws(gsub("\\s+", " ", v7)), lra = trimws(v8),
        M = num(v10), M_work = num(v13), M_bus = num(v16), M_edu = num(v19), M_mar = num(v22), M_hh = num(v28), F_mar = num(v23))]
}
files <- sort(list.files(IN, pattern = "^D03_.*xlsx$", full.names = TRUE))
d <- rbindlist(lapply(files, rd))
cat("D-3 rows:", nrow(d), "| files:", length(files), "| durations:", paste(unique(d$dur), collapse = "; "), "\n")
d <- d[dist != "000" & lra == "Total"]
d[lr %in% names(syn), lr := syn[lr]]
d[, `:=`(dest = paste0(st, "-", dist),
         o = fcase(lr %in% st_names, sprintf("%02d", match(lr, st_names)),
                   lr == "In other districts of the state of enumeration", st,
                   lr == "States in India beyond the state of enumeration", "BEYOND",
                   lr == "Elsewhere in the district of enumeration", "SAMEDIST",
                   default = NA_character_))]
d[, dur := fcase(dur == "All durations of residence", "all", dur == "Duration of residence less than 1 year", "0",
                 dur == "Duration of residence 1-4 years", "1-4", dur == "Duration of residence 5-9 years", "5-9",
                 dur == "Duration of residence 10 years and above", "10+", default = dur)]
## check 1: state rows add up to the "beyond the state" row
inter <- d[grepl("^[0-9]{2}$", o) & o != st]
chk <- merge(inter[, .(s = sum(M_work, na.rm = TRUE)), by = .(dest, area, dur)], d[o == "BEYOND", .(dest, area, dur, b = M_work)], by = c("dest", "area", "dur"))
cat(sprintf("Check: interstate state rows vs 'beyond the state' row (men moved for work): max abs gap %.0f over %d district-area-durations\n",
            max(abs(chk$s - chk$b)), nrow(chk)))
## check 2: each district also lists its own state by name among the states; those rows should be empty
own <- d[lr %in% st_names & o == st]
cat(sprintf("Own-state rows listed by name: %d, men moved for work in them: %.0f (should be 0); they are dropped\n", nrow(own), sum(own$M_work, na.rm = TRUE)))
## keep interstate rows and same-state-other-district rows
k <- d[(lr %in% st_names & o != st) | lr == "In other districts of the state of enumeration"]
k[, inter := o != st]
long <- k[, .(M = sum(M, na.rm = TRUE), M_work = sum(M_work, na.rm = TRUE), M_bus = sum(M_bus, na.rm = TRUE), M_hh = sum(M_hh, na.rm = TRUE),
              F_mar = sum(F_mar, na.rm = TRUE)), by = .(dest, area, dur, o, inter)]   # M_hh, F_mar: placebo groups
cat(sprintf("Origin-destination-area-duration cells: %d | destinations %d | origins %d\n", nrow(long), uniqueN(long$dest), uniqueN(long$o)))
s <- long[area == "Urban", .(M_work = sum(M_work), M_bus = sum(M_bus), M = sum(M)), by = .(dur, inter)][order(inter, dur)]
s[, `:=`(M_work = round(M_work / 1e6, 2), M_bus = round(M_bus / 1e6, 2), M = round(M / 1e6, 2))]
cat("Men in urban areas, millions, by time since moving (inter = from another state; otherwise another district of the same state):\n"); print(s)
fwrite(long, file.path(OUT, "census_d3_counts.csv"))
