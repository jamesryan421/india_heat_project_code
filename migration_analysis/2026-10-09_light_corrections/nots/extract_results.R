## Summarise a finished run: per-band MWTP from the bootstrap draws (cumulative over the three heat variables)
args <- commandArgs(trailingOnly = TRUE); run <- args[1]; out <- args[2]
suppressMessages(library(targets)); options(width = 200)
st <- file.path(run, "_targets")
M <- tar_read_raw("master_boot_results_obs", store = st)          # (R_1*R_2) x 47
d <- tar_read_raw("bootstrap_data_obs", store = st)
cat("draws:", nrow(M), " columns:", ncol(M), " second-stage districts:", nrow(d), "\n")
band <- function(cols, lab) {                                       # cols: per-variable values (above 26, 32, 38)
  X <- M[, cols, drop = FALSE]; C <- t(apply(X, 1, cumsum))          # 26-32, 32-38, >38
  data.frame(quantity = lab, band = c("26-32", "32-38", ">38"),
             mean = colMeans(C, na.rm = TRUE), lo = apply(C, 2, quantile, .025, na.rm = TRUE), hi = apply(C, 2, quantile, .975, na.rm = TRUE))
}
res <- rbind(band(21:23, "MWTP early"), band(24:26, "MWTP late"), band(27:29, "MWTP late, mobility-adjusted"),
             band(30:32, "Wage gradient early"), band(33:35, "Rent gradient early"), band(36:38, "Wage gradient late"), band(39:41, "Rent gradient late"))
eps <- M[, 45]
res <- rbind(res, data.frame(quantity = "epsilon (migration)", band = "", mean = mean(eps, na.rm = TRUE), lo = quantile(eps, .025, na.rm = TRUE), hi = quantile(eps, .975, na.rm = TRUE)))
res[, 3:5] <- round(res[, 3:5], 5); rownames(res) <- NULL
print(res)
write.csv(res, out, row.names = FALSE)
