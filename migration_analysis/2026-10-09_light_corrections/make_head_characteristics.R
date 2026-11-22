suppressMessages({library(haven); library(dplyr); library(readr)})
args <- commandArgs(trailingOnly=TRUE)
r <- read_dta(args[1]) %>% filter(rel_head == 1) %>%
  transmute(hhid = as.character(hhid), head_age = age, head_male = if_else(sex == 0, 1, 0), head_edu = edu)
stopifnot(!any(duplicated(r$hhid)))
write_csv(r, args[2])
cat("wrote", nrow(r), "heads; male share", round(mean(r$head_male),3), "; mean edu", round(mean(r$head_edu, na.rm=TRUE),2), "; NA edu", sum(is.na(r$head_edu)), "\n")
