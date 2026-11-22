## Dense theta grid for Figure 5 (15 Oct 2026): census_choice.R, main specification only, theta from 0.5 to 32.4.
## Step 3 (15 Oct 2026): destination choice with Census 2011 full counts.
## N_od = men who moved for work (D-3 reason "work/employment") from state o into the urban part of district d in the
## last 0-4 years (durations "<1" and "1-4"). Same Poisson model as the survey grids: origin fixed effects (or origin x
## destination-state), wage offset theta * log wage, heat = days above 32 C (UTCI) over 2001-2011, destination-clustered
## standard errors. Value of 30 more hot days = 30 * heat coefficient / theta (negative = cost).
## Usage: Rscript census_choice.R <census_d3_counts.csv> <destination_attributes_v2.csv> <POPF> <step1 grids rds> <out dir>
suppressMessages({library(data.table); library(fixest); library(sf)})
a <- commandArgs(trailingOnly = TRUE); OUT <- a[5]
options(width = 220)
hav <- function(lon1, lat1, lon2, lat2) { r <- pi/180; x <- sin((lat2-lat1)*r/2)^2 + cos(lat1*r)*cos(lat2*r)*sin((lon2-lon1)*r/2)^2; 2*6371*asin(sqrt(x)) }
C <- fread(a[1], colClasses = c(dest = "character", o = "character"))
D <- fread(a[2])
pop <- fread(a[3], select = c("pc11_sd_id", "geometry", "pop_11_proj"))
sf_use_s2(FALSE)
cen <- suppressWarnings(st_coordinates(st_point_on_surface(st_make_valid(st_as_sfc(pop$geometry, crs = 4326)))))
cent <- data.table(dest = pop$pc11_sd_id, lon = cen[, 1], lat = cen[, 2], pop11 = pop$pop_11_proj)
cent <- cent[!grepl("-000$", dest)]
oc <- cent[, .(olon = weighted.mean(lon, pop11, na.rm = TRUE), olat = weighted.mean(lat, pop11, na.rm = TRUE)), by = .(o = substr(dest, 1, 2))]
## counts by origin x urban destination for a set of durations and a count variable
counts <- function(durs, vars = "M_work") C[area == "Urban" & dur %in% durs, .(N = sum(rowSums(.SD))), by = .(o, dest), .SDcols = vars]
grid_for <- function(cn) {
  g <- merge(cn, D, by.x = "dest", by.y = "pc11_sd_id")
  g <- merge(g, cent[, .(dest, lon, lat)], by = "dest"); g <- merge(g, oc, by = "o")
  g[, `:=`(dist = hav(olon, olat, lon, lat), same_state = as.integer(o == substr(dest, 1, 2)), dst = substr(dest, 1, 2))]
  g[, `:=`(log_dist = log(pmax(dist, 25)), heat = c32_0111, lwage = wage_eus, lpm = log(pm25))]
  g
}
v_need <- c("N", "w", "heat", "log_dist", "same_state", "log_urban_pop", "coast_dist", "slope", "lpm", "manuf_share", "ec_emp_pc")
ctl <- " + coast_dist + slope + lpm + manuf_share + ec_emp_pc"
thetas <- c(0, 0.5, 0.65, 0.8, 1, 1.25, 1.5, 2, 2.5, 3, 4, 5, 6.5, 8, 10, 12.9, 15, 17.2, 20, 25, 27.6, 32.4)
run <- function(g, label, wvar = "lwage", heatvar = "c32_0111", fe = "o", interstate = FALSE) {
  g <- copy(g); g[, `:=`(w = get(wvar), heat = get(heatvar))]
  g <- g[complete.cases(g[, ..v_need]) & is.finite(lpm) & is.finite(log_urban_pop)]
  if (interstate) g <- g[same_state == 0]
  ss <- if (!interstate && fe == "o") " + same_state" else ""
  rhs <- paste0("heat + log_dist", ss, " + log_urban_pop", ctl, " | ", fe)
  out <- list()
  r0 <- fepois(as.formula(paste("N ~ w +", rhs)), g, cluster = ~ dest, notes = FALSE, warn = FALSE)
  b <- coef(r0); V <- vcov(r0)[c("heat", "w"), c("heat", "w")]
  v30 <- 30 * b[["heat"]] / b[["w"]]; gr <- c(30 / b[["w"]], -30 * b[["heat"]] / b[["w"]]^2); s30 <- sqrt(drop(t(gr) %*% V %*% gr))
  out[[1]] <- data.table(spec = label, theta = "free", theta_hat = b[["w"]], theta_se = se(r0)[["w"]], heat_coef = b[["heat"]], heat_se = se(r0)[["heat"]],
                         cost30 = -v30, lo = -v30 - 1.96 * s30, hi = -v30 + 1.96 * s30, b_size = b[["log_urban_pop"]], se_size = se(r0)[["log_urban_pop"]])
  for (th in thetas) {
    g[, off := th * w]
    r <- fepois(as.formula(paste("N ~", rhs)), g, offset = ~ off, cluster = ~ dest, notes = FALSE, warn = FALSE)
    bh <- coef(r)[["heat"]]; sh <- se(r)[["heat"]]
    out[[length(out) + 1]] <- data.table(spec = label, theta = as.character(th), theta_hat = th, theta_se = NA_real_, heat_coef = bh, heat_se = sh,
                                         cost30 = if (th > 0) -30 * bh / th else NA_real_, lo = if (th > 0) -30 * (bh + 1.96 * sh) / th else NA_real_,
                                         hi = if (th > 0) -30 * (bh - 1.96 * sh) / th else NA_real_, b_size = coef(r)[["log_urban_pop"]], se_size = se(r)[["log_urban_pop"]])
  }
  o <- rbindlist(out); o[, `:=`(migrants = sum(g$N), dest = uniqueN(g$dest), cells = nrow(g))]; o
}
g04 <- grid_for(counts(c("0", "1-4")))
cat(sprintf("Main grid: %d cells, %d destinations, %d origins, %.2f million men moved for work in the last 0-4 years\n",
            nrow(g04), uniqueN(g04$dest), uniqueN(g04$o), sum(g04$N) / 1e6))
## validation: Census 0-4 year counts by destination against the survey samples (step 1 grids, NSS 64 and PLFS)
G <- readRDS(a[4])
sv <- rbind(G$gE[, .(svN = sum(N)), by = dest][, survey := "NSS 64"], G$gL[, .(svN = sum(N)), by = dest][, survey := "PLFS 2020-21"])
cd <- g04[, .(cN = sum(N)), by = dest]
vv <- merge(sv, cd, by = "dest")
cat("Correlation of log counts by destination, Census 0-4 years vs survey samples (destinations with both > 0):\n")
print(vv[svN > 0 & cN > 0, .(destinations = .N, cor_log = round(cor(log(svN), log(cN)), 3)), by = survey])
R <- run(g04, "A main: work, 0-4 years, origin FE")
nc <- c("theta_hat", "theta_se", "heat_coef", "heat_se", "cost30", "lo", "hi", "b_size", "se_size")
R[, (nc) := lapply(.SD, function(v) round(v, 4)), .SDcols = nc]
fwrite(R, file.path(OUT, "census_grid_dense.csv"))
cat("\nCost of 30 more days above 32 C (share of income; positive = cost), Census counts:\n")
print(R[, .(spec, theta, theta_hat, theta_se, heat_coef, heat_se, cost30, lo, hi, b_size, migrants, dest)], nrows = 200)
## wage gradient across destinations with the same controls (for the break-even theta = beta_N / beta_w)
dd <- unique(g04[, .(dest, wage_eus, c32_0111, log_urban_pop, coast_dist, slope, lpm, manuf_share, ec_emp_pc, dst)])
rw <- feols(wage_eus ~ c32_0111 + log_urban_pop + coast_dist + slope + lpm + manuf_share + ec_emp_pc, dd[is.finite(lpm)], vcov = "hetero")
bN <- R[spec %like% "^A" & theta == "0", heat_coef]; bw <- coef(rw)[["c32_0111"]]
cat(sprintf("\nReduced forms per 30 more hot days: migrant inflow %.1f%% (s.e. %.1f), district wage %.1f%% (s.e. %.1f); break-even theta = %.1f\n",
            100 * 30 * bN, 100 * 30 * R[spec %like% "^A" & theta == "0", heat_se], 100 * 30 * bw, 100 * 30 * se(rw)[["c32_0111"]], bN / bw))
