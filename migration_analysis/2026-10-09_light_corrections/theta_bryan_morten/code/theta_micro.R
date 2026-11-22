## Wage earners with origin state (state of last usual residence) and destination district: NSS 64 (2007-08) and PLFS 2020-21.
## Daily wage = wage and salary earnings / days in wage work (NSS 64 current week; PLFS regular monthly / 30, casual daily wages).
suppressMessages({library(data.table)})
args <- commandArgs(trailingOnly = TRUE); IN <- args[1]; MIG <- args[2]; OUT <- args[3]
options(width = 200)
lk <- fread(file.path(MIG, "district_lookup.csv"), colClasses = "character")
## ---------------- NSS 64 ----------------
b4 <- fread(file.path(IN, "Block-4-demographic-usual-activity-members-records.csv"), colClasses = "character",
            select = c("key_memb", "Sector", "state", "District", "B4_c3", "B4_c4", "B4_c5", "B4_c7", "wgt_combined"))
b5 <- fread(file.path(IN, "Block-5-members-time-disposition-records.csv"), colClasses = "character",
            select = c("key_memb_acty", "B5_c4", "B5_c14", "B5_c17"))
b6 <- fread(file.path(IN, "Block-6-members-migration-records.csv"), colClasses = "character",
            select = c("key_memb", "B6_c7", "B6_c8", "B6_c10", "B6_c11", "B6_c13", "B6_c16"))
b5[, `:=`(key_memb = substr(key_memb_acty, 1, 11), st = as.integer(B5_c4), days = as.numeric(B5_c14), earn = as.numeric(B5_c17))]
w5 <- b5[st %in% c(31L, 41L, 42L, 51L, 71L, 72L),
         .(earn = sum(earn, na.rm = TRUE), days = sum(days, na.rm = TRUE),
           reg = sum(days[st %in% c(31L, 71L, 72L)], na.rm = TRUE) >= sum(days[st %in% c(41L, 42L, 51L)], na.rm = TRUE)), by = key_memb]
nss <- merge(b4, w5[earn > 0 & days > 0], by = "key_memb")
nss <- merge(nss, b6, by = "key_memb", all.x = TRUE)
nss[, nsscode := as.character(as.integer(state) * 100L + as.integer(District))]
nss <- merge(nss, lk[nsscode != "" & !is.na(nsscode), .(nsscode, d = pc11_sd_id)][, .SD[1], by = nsscode], by = "nsscode")
nss[, loc := suppressWarnings(as.integer(B6_c11))]
nss[, miss6 := is.na(B6_c7) | !(B6_c7 %in% c("1", "2")) | (B6_c7 == "1" & is.na(loc))]
nss[, mover := !miss6 & B6_c7 == "1" & loc %in% 5:6]
nss[, oc := suppressWarnings(as.integer(B6_c13))]
nss[, o := fifelse(mover, sprintf("%02d", oc), substr(d, 1, 2))]
nss[, abroad := !miss6 & B6_c7 == "1" & loc == 7]
nss[, bad := mover & (is.na(oc) | !(oc %in% 1:35) | o == substr(d, 1, 2))]
cat(sprintf("NSS 64 wage earners mapped: %d; missing migration record %d; interstate movers %d; from abroad %d; inconsistent origin %d\n",
            nrow(nss), nss[, sum(miss6)], nss[, sum(mover)], nss[, sum(abroad)], nss[, sum(bad)]))
nss <- nss[!miss6 & !abroad & !bad]
nss[, edu := fcase(B4_c7 %in% sprintf("%02d", 1:5), 1L, B4_c7 %in% c("06", "07"), 2L, B4_c7 == "08", 3L, B4_c7 %in% c("10", "11"), 4L, B4_c7 %in% c("12", "13", "14"), 5L, default = NA_integer_)]
nss[, work := mover & B6_c16 %in% sprintf("%02d", 1:6)]
nss[, transfer := mover & B6_c16 == "05"]
N64 <- nss[, .(survey = "nss64", d, sector = Sector, o, mover, male = B4_c4 == "1", age = as.integer(B4_c5), edu, head = B4_c3 == "1",
               lw = log(earn / days), reg, work, transfer, ret = mover & B6_c8 == "1", yrs = suppressWarnings(as.numeric(B6_c10)), wt = as.numeric(wgt_combined))]
## ---------------- PLFS 2020-21 (first visit) ----------------
pl <- readLines(file.path(MIG, "plfs2021_PERV1.txt"))
g1 <- function(a, w) trimws(substr(pl, a, a + w - 1))
P <- data.table(sector = g1(12, 1), state = g1(13, 2), district = g1(15, 2), rel = g1(40, 1), sex = g1(41, 1), age = suppressWarnings(as.integer(g1(42, 3))),
                gedu = g1(46, 2), cws = g1(290, 2), earn_reg = suppressWarnings(as.numeric(g1(297, 8))),
                enum_diff = g1(313, 1), was_upr = g1(315, 1), loc = g1(316, 1), ostate = g1(317, 2), reason = g1(319, 2), mult = suppressWarnings(as.numeric(g1(352, 10))))
cw <- numeric(length(pl)); cd <- numeric(length(pl))
for (s in c(108, 134, 160, 186, 212, 238, 264)) {
  s1 <- g1(s, 2); w1 <- suppressWarnings(as.numeric(g1(s + 6, 5))); s2 <- g1(s + 11, 2); w2 <- suppressWarnings(as.numeric(g1(s + 17, 5)))
  wd <- fifelse(s1 %in% c("41", "42", "51") & !is.na(w1), w1, 0) + fifelse(s2 %in% c("41", "42", "51") & !is.na(w2), w2, 0)
  cw <- cw + wd; cd <- cd + as.numeric(wd > 0)
}
rm(pl)
P[, `:=`(cas_w = cw, cas_d = cd)]
P[, dw := fcase(cws %in% c("31", "71", "72") & earn_reg > 0, earn_reg / 30, cws %in% c("41", "42", "51") & cas_d > 0, cas_w / cas_d, default = NA_real_)]
P[, reg := cws %in% c("31", "71", "72")]
cat(sprintf("PLFS: regular workers with earnings %d; casual workers with daily wages %d\n", P[reg == TRUE & !is.na(dw), .N], P[reg == FALSE & !is.na(dw), .N]))
P <- P[!is.na(dw) & dw > 0]
P[, plfscode := as.character(as.integer(state) * 100L + as.integer(district))]
P <- merge(P, lk[plfscode != "" & !is.na(plfscode), .(plfscode, d = pc11_sd_id)][, .SD[1], by = plfscode], by = "plfscode")
P[, mover := enum_diff == "1" & loc %in% c("5", "6")]
P[, oc := suppressWarnings(as.integer(ostate))]
P[oc == 36L, oc := 28L]; P[oc == 37L, oc := 1L]
P[, o := fifelse(mover, sprintf("%02d", oc), substr(d, 1, 2))]
P[, abroad := enum_diff == "1" & loc == "7"]
P[, bad := mover & (is.na(oc) | !(oc %in% 1:35) | o == substr(d, 1, 2))]
cat(sprintf("PLFS wage earners mapped: %d; interstate movers %d (of which same 2011 state after recoding Telangana/Ladakh: %d); from abroad %d\n",
            nrow(P), P[, sum(mover)], P[, sum(mover & o == substr(d, 1, 2), na.rm = TRUE)], P[, sum(abroad)]))
P[mover == TRUE & o == substr(d, 1, 2), mover := FALSE]   # e.g. Telangana <-> Andhra Pradesh: one state in 2011
P[, bad := mover & (is.na(oc) | !(oc %in% 1:35))]
P <- P[!abroad & !bad]
P[, edu := fcase(gedu %in% sprintf("%02d", 1:4), 1L, gedu %in% c("05", "06"), 2L, gedu == "07", 3L, gedu %in% c("08", "10"), 4L, gedu %in% c("11", "12", "13"), 5L, default = NA_integer_)]
P[, work := mover & reason %in% c("01", "02")]
P21 <- P[, .(survey = "plfs21", d, sector, o, mover, male = sex == "1", age, edu, head = rel == "1", lw = log(dw), reg, work, transfer = NA, ret = mover & was_upr == "1",
             yrs = NA_real_, wt = mult)]
micro <- rbind(N64, P21)
micro <- micro[male == TRUE & age >= 15 & age <= 64 & is.finite(lw)]
micro[, male := NULL]
cat("Male wage earners aged 15-64 by survey and sector, with interstate movers:\n")
print(micro[, .(n = .N, movers = sum(mover), return_movers = sum(ret, na.rm = TRUE), work_movers = sum(work, na.rm = TRUE), mean_lw = round(mean(lw), 3)), by = .(survey, sector)][order(survey, sector)])
fwrite(micro, file.path(OUT, "micro.csv"))
