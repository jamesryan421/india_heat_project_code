# Pipeline part 3: Plots and logs

get_plot_log_pipeline <- function(){
  pipeline_estimation <- list(
    # Get plots
    tar_target(boot_means_ci_bounds_proj, 
               get_boot_means_ci_bounds(master_boot_results_proj[,25:ncol(master_boot_results_proj)])),
    tar_target(boot_means_ci_bounds_obs,
               get_boot_means_ci_bounds(master_boot_results_obs[,25:ncol(master_boot_results_obs)])),
    tar_target(boot_estimates_proj, boot_means_ci_bounds_proj[["means"]]),
    tar_target(boot_estimates_obs, boot_means_ci_bounds_obs[["means"]]),
    tar_target(ci_list_proj, boot_means_ci_bounds_proj[c("ci_lower","ci_upper")]),
    tar_target(ci_list_obs, boot_means_ci_bounds_obs[c("ci_lower","ci_upper")]),
    tar_target(plot_data_proj, get_plot_data_boot(boot_estimates_proj, ci_list_proj)),
    tar_target(plot_data_obs, get_plot_data_boot(boot_estimates_obs, ci_list_obs)),
    tar_target(combined_plot_data_list, get_combined_plot_data(plot_data_proj, plot_data_obs)),
    tar_target(combined_plot_data_early, combined_plot_data_list[[1]]),
    tar_target(combined_plot_data_late, combined_plot_data_list[[2]]),
    tar_target(combined_plot_data_late_mig, combined_plot_data_list[[3]]),
    tar_target(boot_plot_obs, get_bootstrap_plot(plot_data_obs, vis_output_path, temp_data="Observed"), format="file"),
    tar_target(boot_plot_proj, get_bootstrap_plot(plot_data_proj, vis_output_path, temp_data="Corrected"), format="file"),
    tar_target(obs_corr_plot_early, get_obs_corrected_plot(combined_plot_data_early, vis_output_path, parameter="Early")),
    tar_target(obs_corr_plot_late, get_obs_corrected_plot(combined_plot_data_late, vis_output_path, parameter="Late")),
    tar_target(obs_corr_plot_late_mig, get_obs_corrected_plot(combined_plot_data_late_mig, vis_output_path, parameter="Late Mig Costs")),
    # Get summary stat tables
    # Get regression output tables
    tar_target(wage_early_stargazer,
               get_stargazer_bootstrap(
                 wage_early_coefs_list,
                 wage_early_base_models_list,
                 type="text",
                 covariate.labels=c("Intercept", "Age", "Age Sq.", "Male",
                                    "No Formal Schooling", "High School", "Post-Secondary",
                                    "Hindu", "Scheduled Caste/Tribe"),
                 single.row=T,
                 add.lines=list(c("District FE", "Yes", "Yes")),
                 dep.var.labels=c("Expenditure", "Expenditure per Capita"),
                 intercept.top=T,
                 intercept.bottom=F),
               error = "null"
    ),
    tar_target(wage_late_stargazer,
               get_stargazer_bootstrap(
                 wage_late_coefs_list,
                 wage_late_base_models_list,
                 type="text",
                 covariate.labels=c("Intercept", "Age", "Age Sq.", "Male",
                                    "Education", "Hindu", "Scheduled Caste/Tribe"),
                 single.row=T,
                 add.lines=list(c("District FE", "Yes", "Yes")),
                 dep.var.labels=c("Expenditure", "Expenditure per Capita"),
                 intercept.top=T,
                 intercept.bottom=F),
               error = "null"
    ),
    tar_target(rent_early_stargazer,
               get_stargazer_bootstrap(
                 rent_early_coefs_list,
                 rent_early_base_models_list,
                 type="text",
                 covariate.labels=c("Intercept", "Pucca Walls", "Pucca Floor", "Pucca Roof",
                                    "Piped Water", "Own Latrine", "Electricity"),
                 single.row=T,
                 add.lines=list(c("District FE", "Yes", "Yes")),
                 dep.var.labels=c("Rent", "Rent per Capita"),
                 intercept.top=T,
                 intercept.bottom=F),
               error = "null"
    ),
    tar_target(rent_late_stargazer,
               get_stargazer_bootstrap(
                 rent_late_coefs_list,
                 rent_late_base_models_list,
                 type="text",
                 covariate.labels=c("Intercept", "Pucca Walls", "Pucca Floor", "Pucca Roof",
                                    "Gas/Electric Cooking Fuel", "Electric Lighting", "Piped Water", "Own Latrine"),
                 single.row=T,
                 add.lines=list(c("District FE", "Yes", "Yes")),
                 dep.var.labels=c("Rent", "Rent per Capita"),
                 intercept.top=T,
                 intercept.bottom=F),
               error = "null"
    ),
    tar_target(stargazer_tables_list,
               list(
                 wage_early = wage_early_stargazer,
                 wage_late = wage_late_stargazer,
                 rent_early = rent_early_stargazer,
                 rent_late = rent_late_stargazer
               )),
    tar_target(logs_path, file.path(main_data_path, "logs")),
    tar_target(text_stargazer_outfile,
               write_text_stargazer_logs(stargazer_tables_list, file.path(logs_path, "first_stage_text_regression_summaries.txt")), format="file"),
    # Second-stage models
    tar_target(early_proj_stargazer,
               get_stargazer_bootstrap(
                 second_stage_early_proj_coefs_list,
                 second_stage_early_proj_base_models_list,
                 type="text",
                 covariate.labels=c("Intercept", "Hindu", "Scheduled Caste/Tribe", "Age", "High School", "Post-Secondary"),
                 single_row=T,
                 add_lines=list(c("Temperature Correction", "No", "No")),
                 dep.var.labels=c("Wage per capita", "Rent per capita"),
                 intercept.top=T,
                 intercept.bottom=F
               ),
               error="null"),
    tar_target(late_proj_stargazer,
               get_stargazer_bootstrap(
                 second_stage_late_proj_coefs_list,
                 second_stage_late_proj_base_models_list,
                 type="text",
                 covariate.labels=c("Intercept", "Hindu", "Scheduled Caste/Tribe", "Age", "High School", "Post-Secondary"),
                 single_row=T,
                 add_lines=list(c("Temperature Correction", "No", "No")),
                 dep.var.labels=c("Wage per capita", "Rent per capita"),
                 intercept.top=T,
                 intercept.bottom=F
               ),
               error="null"),
    tar_target(early_obs_stargazer,
               get_stargazer_bootstrap(
                 second_stage_early_obs_coefs_list,
                 second_stage_early_obs_base_models_list,
                 type="text",
                 covariate.labels=c("Intercept", "Hindu", "Scheduled Caste/Tribe", "Age", "High School", "Post-Secondary"),
                 single_row=T,
                 add_lines=list(c("Temperature Correction", "No", "No")),
                 dep.var.labels=c("Wage per capita", "Rent per capita"),
                 intercept.top=T,
                 intercept.bottom=F
               ),
               error="null"),
    tar_target(late_obs_stargazer,
               get_stargazer_bootstrap(
                 second_stage_late_obs_coefs_list,
                 second_stage_late_obs_base_models_list,
                 type="text",
                 covariate.labels=c("Intercept", "Hindu", "Scheduled Caste/Tribe", "Age", "High School", "Post-Secondary"),
                 single_row=T,
                 add_lines=list(c("Temperature Correction", "No", "No")),
                 dep.var.labels=c("Wage per capita", "Rent per capita"),
                 intercept.top=T,
                 intercept.bottom=F
               ),
               error="null"),
    tar_target(stargazer_second_stage_tables_list,
               list(
                 second_stage_early_proj = early_proj_stargazer,
                 second_stage_late_proj = late_proj_stargazer,
                 second_stage_early_obs = early_obs_stargazer,
                 second_stage_late_obs = late_obs_stargazer
               )),
    tar_target(text_stargazer_outfile_second_stage,
               write_text_stargazer_logs(stargazer_second_stage_tables_list, file.path(logs_path, "second_stage_text_regression_summaries.txt")), format="file"),
    # Summary statistics
    # Summary statistics for early period wages
    tar_target(nss_ind_reg_ss,nss_ind_reg[,c("logepc","Age","male",
                                             "educ_ill","educ_nfs","educ_hs","educ_ps",
                                             "hindu","scst")]),
    # Summary statistics for early period rents
    tar_target(dfhm_ss, df_housing_merged[,c("loghc","loghcpc","pucca_walls","pucca_floor","pucca_roof",
                                             "piped_water","own_latrine","elec")]),
    # summary statistics for late period wages
    tar_target(hcesme_w_ss, hces_merged_emp[,c("logepc","age","male","edu","hindu","scstbc")]),
    # Summary statistics for late period rents
    tar_target(hcesme_r_ss, hces_merged_emp_housing[,c("loghc","loghcpc","pucca_walls","pucca_floor","pucca_roof",
                                                       "cooking_fuel_gas_electric",
                                                       "lighting_electric",
                                                       "piped_water","own_latrine")]),
    # summary statistics for migration costs
    #tar_target(d_ss, joined_diffs[,c("delta_logpop","delta_wage","delta_rent","delta_netwage")]),
    # Summary tables options
    # Early period wages
    tar_target(st_epw_opt,list(
      title = "Wage Regression Summary Statistics, Early Period",
      covariate.labels = c("Log Expenditure Per Capita","Age","Male",
                           "Illiterate","No Formal Schooling","High School or Below","Post-Secondary",
                           "Hindu","Scheduled Caste/Tribe")
    )),
    # Early period rents
    tar_target(st_epr_opt,list(
      title = "Rent Regression Summary Statistics, Early Period",
      covariate.labels = c("Log Housing Costs","Log Housing Costs Per Capita",
                           "Pucca Walls","Pucca Floor","Pucca Roof","Piped Water Access",
                           "Exclusive Latrine","Electricity")
    )),
    # Late period wages
    tar_target(st_lpw_opt,list(
      title = "Wage Regression Summary Statistics, Late Period",
      covariate.labels = c("Log Monthly Expenditure Per Capita",
                           "Age","Male","Years of Schooling",
                           "Hindu","Scheduled Caste/Tribe")
    )),
    # Late period rents
    tar_target(st_lpr_opt,list(
      title = "Rent Regression Summary Statistics, Late Period",
      covariate.labels = c("Log Housing Costs","Log Housing Costs Per Capita",
                           "Pucca Walls","Pucca Flooring","Pucca Roofing",
                           "Gas/Electric Cooking Fuel",
                           "Electric Lighting",
                           "Piped Water","Exclusive Latrine Access")
    )),
    # summary statistics for second-stage models
    tar_target(early_period_wages_stargazer,
               get_stargazer_summary_table(
                 nss_ind_reg_ss, st_epw_opt
               )),
    tar_target(early_period_rents_stargazer,
               get_stargazer_summary_table(
                 dfhm_ss, st_epr_opt
               )),
    tar_target(late_period_wages_stargazer,
               get_stargazer_summary_table(
                 hcesme_w_ss, st_lpw_opt
               )),
    tar_target(late_period_rents_stargazer,
               get_stargazer_summary_table(
                 hcesme_r_ss, st_lpr_opt
               )),
    tar_target(first_stage_summary_tables_list,
               list(
                 early_period_wages_summary = early_period_wages_stargazer,
                 early_period_rents_summary = early_period_rents_stargazer,
                 late_period_wages_summary = late_period_wages_stargazer,
                 late_period_rents_summary = late_period_rents_stargazer
               )),
    tar_target(text_stargazer_first_stage_summary,
               write_text_stargazer_logs(first_stage_summary_tables_list, file.path(logs_path, "first_stage_summary_stats.txt")), format="file"),
    # Second stage temperature stats
    tar_target(utci_ewp_ss, utci_ewp[,2:ncol(utci_ewp)]),
    tar_target(utci_ewo_ss, utci_ewo[,2:ncol(utci_ewo)]),
    tar_target(utci_lwp_ss, utci_lwp[,2:ncol(utci_lwp)]),
    tar_target(utci_lwo_ss, utci_lwo[,2:ncol(utci_lwo)]),
    tar_target(temp_covariate_labels, 
               c("Extreme","Very Strong, High","Very Strong, Mid-High", "Very Strong, Mid-Low", "Very Strong, Low",
                 "Strong, High", "Strong, Mid", "Strong, Low",
                 "Moderate, High", "Moderate, Mid", "Moderate, Low",
                 "Optimal Range",
                 "Unsuitable Slope Share", "Internal Water Share", "Coast Distance", "Bartik Shock")),
    tar_target(utci_ewp_ss_opt,
               list(
                 title="Early Period UTCI Summary Stats, Projected",
                 covariate.labels = temp_covariate_labels
               )),
    tar_target(utci_ewo_ss_opt,
               list(
                 title="Early Period UTCI Summary Stats, Observed",
                 covariate.labels = temp_covariate_labels
               )),
    tar_target(utci_lwp_ss_opt,
               list(
                 title="Late Period UTCI Summary Stats, Projected",
                 covariate.labels = temp_covariate_labels
               )),
    tar_target(utci_lwo_ss_opt,
               list(
                 title="Late Period UTCI Summary Stats, Observed",
                 covariate.labels = temp_covariate_labels
               )),
    tar_target(utci_ewp_stargazer, get_stargazer_summary_table(utci_ewp_ss, utci_ewp_ss_opt)),
    tar_target(utci_ewo_stargazer, get_stargazer_summary_table(utci_ewo_ss, utci_ewo_ss_opt)),
    tar_target(utci_lwp_stargazer, get_stargazer_summary_table(utci_lwp_ss, utci_lwp_ss_opt)),
    tar_target(utci_lwo_stargazer, get_stargazer_summary_table(utci_lwo_ss, utci_lwo_ss_opt)),
    tar_target(second_stage_summary_tables_list,
               list(
                 utci_early_proj_summary = utci_ewp_stargazer,
                 utci_early_obs_summary = utci_ewo_stargazer,
                 utci_late_proj_summary = utci_lwp_stargazer,
                 utci_late_obs_summary = utci_lwo_stargazer
               )),
    tar_target(text_stargazer_second_stage_summary,
               write_text_stargazer_logs(second_stage_summary_tables_list, file.path(logs_path, "second_stage_summary_stats.txt")), format="file")
  )
  return(pipeline_estimation)
}