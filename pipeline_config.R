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
#col_prefixes_redux = c("ex_max","vs_max","s_max","m_max","or_max")
# col_prefixes_redux = c("ex_max", "vsh_max", "vsmh_max", "vsml_max", "vsl_max",
#                        "sh_max", "sm_max", "sl_max",
#                        "mh_max", "mm_max", "ml_max",
#                        "or_max")
col_prefixes_redux <- c("days_above_26", "days_above_32", "days_above_38")
#max_temp_hr_cols=c("ex_max","vs_max","s_max","m_max","or_max")
max_temp_hr_cols = c("ex_max", "vsh_max", "vsmh_max", "vsml_max", "vsl_max",
                     "sh_max", "sm_max", "sl_max",
                     "mh_max", "mm_max", "ml_max",
                     "or_max")

## Select years to use for climate variable averaging
#year_suffixes_early = c("07","08","09","10","11")
year_suffixes_early = sapply(seq(1, 11), function(x){sprintf("%02d",x)})
#year_suffixes_late = c("18","19","20","21","22")
year_suffixes_late = sapply(seq(12, 22), function(x){sprintf("%02d",x)})

## Bootstrapping options
seed=8008315
R_1 <- 100
R_2 <- 25
#R_1 <- 10
#R_2 <- 5
# Housing share: HCES 2022-23 urban tenants spend 0.20 of total expenditure on rent (item 520 / totexp, weighted)
housing_exp_share=0.20
alpha=0.05

## Sample definitions (light corrections, Oct 9)
# Worker sample: "all" urban households, or "regular_wage" households (household type 2).
# Household type is the household's main income source, not job formality.
worker_sample <- "all"
# Minimum number of tenant households per district for a rent fixed effect
rent_min_n <- 10
# Late-period population: "pop_23_proj" (Census-based, same basis as pop_11_proj) or "pop_23_wp" (WorldPop)
late_pop_col <- "pop_23_proj"

## What to do with lonely survey strata - DON'T TOUCH THIS
options(survey.lonely.psu = "adjust")

# Select temperature file here
#utci_daily_max_file <- "district_daily_max_utci_full.csv.gz"
#utci_daytime_mean_file <- "district_daytime_mean_utci_full.csv.gz"
utci_hottest_mean_file <- "district_daytime_hottest_mean_utci_full.csv.gz"

selected_utci_file <- utci_hottest_mean_file