# requirements.R

# 1. Dynamically build a standard, writable personal library path in your home directory
r_version <- paste0(R.version$major, ".", substr(R.version$minor, 1, 1)) # Extract version (e.g., "4.3")
user_lib  <- file.path(Sys.getenv("HOME"), "R", "x86_64-pc-linux-gnu-library", r_version)

# 2. Create the folder if it doesn't exist yet
if (!dir.exists(user_lib)) {
  dir.create(user_lib, recursive = TRUE, showWarnings = FALSE)
}

# 3. Prepend this personal directory to R's active search paths
.libPaths(c(user_lib, .libPaths()))

# 4. Your complete package manifest
required_packages <- c(
  "targets", "tarchetypes", "crew", "tidyverse", "summarytools", 
  "broom", "plm", "stargazer", "haven", "here", "ggExtra", 
  "AER", "survey", "svrep"
)

# 5. Check what's missing across all accessible libraries
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

# 6. Install missing packages directly into your personal folder
if(length(missing_packages) > 0) {
  message("Installing missing packages into user space: ", user_lib)
  install.packages(missing_packages, lib = user_lib, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed!")
}