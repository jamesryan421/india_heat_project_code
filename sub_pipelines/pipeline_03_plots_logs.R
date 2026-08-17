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
                 dep.var.labels=c("Expenditure", "Expenditure per Capita")),
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
                 dep.var.labels=c("Expenditure", "Expenditure per Capita")),
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
                 dep.var.labels=c("Rent", "Rent per Capita")),
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
                 dep.var.labels=c("Rent", "Rent per Capita")),
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
               write_text_stargazer_logs(stargazer_tables_list, file.path(logs_path, "first_stage_text_regression_summaries.txt")))
  )
  return(pipeline_estimation)
}