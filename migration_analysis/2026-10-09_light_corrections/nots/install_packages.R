## Installs the pipeline's R packages into a user library on NOTS, then applies the stargazer patch
lib <- Sys.getenv("R_LIBS_USER"); dir.create(lib, recursive = TRUE, showWarnings = FALSE); .libPaths(c(lib, .libPaths()))
pk <- c("targets","tarchetypes","crew","broom","plm","stargazer","haven","lubridate","here","ggExtra","AER","boot","survey","svrep",
        "dplyr","purrr","readr","tidyr","stringr","tibble","ggplot2","fixest","data.table")
miss <- pk[!pk %in% rownames(installed.packages())]
cat("missing:", miss, "\n")
if (length(miss)) install.packages(miss, lib = lib, repos = "https://cloud.r-project.org", Ncpus = 4)
## stargazer 5.2.3: move the length(s) > 1 check ahead of is.na(s) (R >= 4.2 errors on vectors in if())
v <- as.character(packageVersion("stargazer")); td <- tempfile(); dir.create(td); old <- setwd(td)
download.file(paste0("https://cran.r-project.org/src/contrib/stargazer_", v, ".tar.gz"), "sg.tar.gz", quiet = TRUE); untar("sg.tar.gz")
src <- readLines("stargazer/R/stargazer-internal.R"); i <- grep("if (length(s) > 1) { return(\"\") }", src, fixed = TRUE)
j <- grep("if (!is.character(s)) { return(\"\") }", src, fixed = TRUE)
if (length(i) == 1 && length(j) == 1 && i > j) { chk <- src[i]; src <- src[-i]; src <- append(src, chk, after = j - 1); writeLines(src, "stargazer/R/stargazer-internal.R")
  install.packages("stargazer", repos = NULL, type = "source", lib = lib); cat("stargazer patched\n") } else cat("stargazer patch not needed or pattern not found\n")
setwd(old); for (p in pk) cat(p, requireNamespace(p, quietly = TRUE), "\n")
