# Pipeline config parameters

## Path to data - ADJUST THIS FOR PORTABILITY
main_data_path <- file.path("C","Users","jdr42","Documents","Projects","india_heat_project")

## Sample size grid search
filter_sizes=seq(0,200,10)
opt_threshold=30
pval_thresh=0.05

## Differentials plot options
recent_data=F
highlight_signif=T
add_marginal=F

## UTCI variable columns
col_prefixes=c("ex_min","vs_min","s_min","m_min","or_min",
               "ex_max","vs_max","s_max","m_max","or_max")
col_prefixes_redux = c("ex_max","vs_max","s_max","m_max","or_max")
max_temp_hr_cols=c("ex_max","vs_max","s_max","m_max","or_max")

## Select years to use for climate variable averaging
year_suffixes_early = c("07","08","09","10","11")
year_suffixes_late = c("18","19","20","21","22")

## Bootstrapping options
seed=8008315
R_1 <- 100
R_2 <- 50
housing_exp_share=0.15
alpha=0.05

## What to do with lonely survey strata - DON'T TOUCH THIS
options(survey.lonely.psu = "adjust")

# Select temperature file here
#utci_daily_max_file <- "district_daily_max_utci_full.csv.gz"
#utci_daytime_mean_file <- "district_daytime_mean_utci_full.csv.gz"
utci_hottest_mean_file <- "district_daytime_hottest_mean_utci_full.csv.gz"

selected_utci_file <- utci_hottest_mean_file