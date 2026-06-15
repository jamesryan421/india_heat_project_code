# requirements.R
required_packages <- c("targets","tarchetypes","crew","tidyverse","summarytools","broom","plm","stargazer","haven",
                       "here","ggExtra","AER","boot","survey","svrep")

missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]

if(length(missing_packages) > 0) {
  message("Installing missing packages: ", paste(missing_packages, collapse = ", "))
  install.packages(missing_packages, repos = "https://cloud.r-project.org")
} else {
  message("All required packages are already installed!")
}