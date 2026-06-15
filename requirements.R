# requirements.R

# 1. Dynamically build a standard, writable personal library path in your home directory
#r_version <- paste0(R.version$major, ".", substr(R.version$minor, 1, 1)) # Extract version (e.g., "4.3")
#user_lib  <- file.path(Sys.getenv("HOME"), "R", "x86_64-pc-linux-gnu-library", r_version)

user_lib <- Sys.getenv("R_LIBS")

# 2. Create the folder if it doesn't exist yet
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}

# 3. Prepend this personal directory to R's active search paths
.libPaths(c(user_lib, .libPaths()))

# 1. FIX: Point R to Posit's pre-compiled Linux binary repository
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/manylinux_2_28/latest"))

# 2. FIX: Configure the User Agent string so the server delivers binaries instead of source code
options(HTTPUserAgent = sprintf(
  "R/%s R (%s)", 
  getRversion(), 
  paste(getRversion(), R.version["platform"], R.version["arch"], R.version["os"])
))

# 3. Define core packages
base_packages <- c(
  "targets", "tarchetypes", "crew", "summarytools", 
  "broom", "plm", "stargazer", "haven", "here", "ggExtra", 
  "AER", "survey", "svrep", "lubridate"
)

# 4. FIX: Swap full 'tidyverse' for just the core data-wrangling engines
core_tidyverse <- c("dplyr", "ggplot2", "purrr", "readr", "tidyr", "stringr", "tibble")
required_packages <- c(base_packages, core_tidyverse)

# 5. Run the installation
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
message("Missing packages: ", toString(missing_packages))


# 6. Install missing packages directly into your personal folder
if(length(missing_packages) > 0) {
  message("Installing missing packages into user space: ", user_lib)
  install.packages(missing_packages, lib = user_lib, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed!")
}