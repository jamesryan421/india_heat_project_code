# Split pipeline core

get_split_pipeline <- function(){
  pipeline_estimation <- list(
    ##
    # Establish master survey designs and draw weights
    ##
    ### Early period
    tar_target(hcs_survey_design,
               svydesign(
                 ids = ~FSU,
                 strata = ~Stratum,
                 weights = ~Wgt_combined,
                 data = df_housing_merged,
                 nest = T
               )),
    tar_target(nss_survey_design,
               svydesign(
                 ids = ~FSU_Serial_no,
                 strata = ~Stratum,
                 weights = ~Combined_multiplier,
                 data = filter_nss(nss_ind_reg,opt_threshold),
                 nest = T
               )),
    tar_target(nss_ind_reg_boot_wts,
               get_first_stage_boot_wts(
                 nss_survey_design, R_1
               )),
    tar_target(hcs_boot_wts,
               get_first_stage_boot_wts(
                 hcs_survey_design, R_1
               )),
    ### Late period
    tar_target(hces_survey_design,
               svydesign(
                 ids = ~fsu,
                 strata = ~interaction(stratum, sub_stratum),
                 weights = ~mult,
                 data = hces_merged_emp_housing,
                 nest = T
               )),
    tar_target(hces_boot_wts,
               get_first_stage_boot_wts(
                 hces_survey_design, R_1
               )),
    ##
    # Run parallelized first-stage estimation
    ##
    ### Set replications
    tar_target(rep_ids,1:R_1),
    ### Early period
    tar_target(wage_boot_early_outputs,
               get_boot_wage_model_early(
                 rep_ids,
                 nss_ind_reg_boot_wts
               ),
               pattern = map(rep_ids),
               iteration = "list"),
    tar_target(wage_boot_design_early,
               wage_boot_early_outputs$wage_design_early,
               pattern = map(wage_boot_early_outputs),
               iteration = "list"),
    # tar_target(wage_boot_early_coef,
    #            wage_boot_early_outputs$wage_early,
    #            pattern = map(wage_boot_early_outputs)),
    # tar_target(wage_boot_pc_early_coef,
    #            wage_boot_early_outputs$wage_pc_early,
    #            pattern = map(wage_boot_early_outputs)),
    tar_target(wage_early_boot_coef,
               get_boot_reg_outputs_vec(
                 wage_boot_early_outputs$wage_early,alpha
               ),
               pattern = map(wage_boot_early_outputs),
               iteration = "list"),
    tar_target(wage_pc_early_boot_coef,
               get_boot_reg_outputs_vec(
                 wage_boot_early_outputs$wage_pc_early,alpha
               ),
               pattern = map(wage_boot_early_outputs),
               iteration = "list"),
    tar_target(rent_boot_early_outputs,
               get_boot_rent_model_early(
                 rep_ids,
                 hcs_boot_wts
               ),
               pattern = map(rep_ids),
               iteration = "list"),
    tar_target(rent_boot_design_early,
               rent_boot_early_outputs$rent_design_early,
               pattern = map(rent_boot_early_outputs),
               iteration = "list"),
    tar_target(rent_early_boot_coef,
               get_boot_reg_outputs_vec(
                 rent_boot_early_outputs$rent_early,alpha
               ),
               pattern = map(rent_boot_early_outputs),
               iteration = "list"),
    tar_target(rent_pc_early_boot_coef,
               get_boot_reg_outputs_vec(
                 rent_boot_early_outputs$rent_pc_early,alpha
               ),
               pattern = map(rent_boot_early_outputs),
               iteration = "list"),
    ### Late period
    tar_target(wage_boot_late_outputs,
               get_boot_wage_model_late(
                 rep_ids,
                 hces_boot_wts
               ),
               pattern = map(rep_ids),
               iteration = "list"),
    tar_target(wage_boot_design_late,
               wage_boot_late_outputs$wage_design_late,
               pattern = map(wage_boot_late_outputs),
               iteration = "list"),
    tar_target(wage_late_boot_coef,
               get_boot_reg_outputs_vec(
                 wage_boot_late_outputs$wage_late,alpha
               ),
               pattern = map(wage_boot_late_outputs),
               iteration = "list"),
    tar_target(wage_pc_late_boot_coef,
               get_boot_reg_outputs_vec(
                 wage_boot_late_outputs$wage_pc_late,alpha
               ),
               pattern = map(wage_boot_late_outputs),
               iteration = "list"),
    # tar_target(wage_boot_late_coef,
    #            wage_boot_late_outputs$wage_late,
    #            pattern = map(wage_boot_late_outputs)),
    # tar_target(wage_boot_pc_late_coef,
    #            wage_boot_late_outputs$wage_pc_late,
    #            pattern = map(wage_boot_late_outputs)),
    tar_target(rent_boot_late_outputs,
               get_boot_rent_model_late(
                 rep_ids,
                 hces_boot_wts
               ),
               pattern = map(rep_ids),
               iteration = "list"),
    tar_target(rent_boot_design_late,
               rent_boot_late_outputs$rent_design_late,
               pattern = map(rent_boot_late_outputs),
               iteration = "list"),
    # tar_target(rent_boot_late_coef,
    #            rent_boot_late_outputs$rent_late,
    #            pattern = map(rent_boot_late_outputs)),
    # tar_target(rent_boot_pc_late_coef,
    #            rent_boot_late_outputs$rent_pc_late,
    #            pattern = map(rent_boot_late_outputs))
    tar_target(rent_late_boot_coef,
               get_boot_reg_outputs_vec(
                 rent_boot_late_outputs$rent_late,alpha
               ),
               pattern = map(rent_boot_late_outputs),
               iteration = "list"),
    tar_target(rent_pc_late_boot_coef,
               get_boot_reg_outputs_vec(
                 rent_boot_late_outputs$rent_pc_late,alpha
               ),
               pattern = map(rent_boot_late_outputs),
               iteration = "list"),
    ### Get combined coefficients for one iteration for each period,
    ### then merge on population data
    ### For now, only do this for the per capita models
    tar_target(combined_iter_coef_early,
               get_combined_iter_coef(
                 wage_pc_early_boot_coef$coef,
                 rent_pc_early_boot_coef$coef,
                 seq(1,9),
                 seq(1,7)
               ),
               pattern = map(wage_pc_early_boot_coef,rent_pc_early_boot_coef),
               iteration = "list"),
    tar_target(combined_iter_coef_late,
               get_combined_iter_coef(
                 wage_pc_late_boot_coef$coef,
                 rent_pc_late_boot_coef$coef,
                 seq(1,7),
                 seq(1,8)
               ),
               pattern = map(wage_pc_late_boot_coef, rent_pc_late_boot_coef),
               iteration = "list"),
    tar_target(combined_iter_coef_early_pop,
               merge_pop_plot_data(
                 combined_iter_coef_early,
                 df_pop, recent_data =F
               ),
               pattern = map(combined_iter_coef_early),
               iteration = "list"),
    tar_target(combined_iter_coef_late_pop,
               merge_pop_plot_data(
                 combined_iter_coef_late,
                 df_pop, recent_data = T
               ),
               pattern = map(combined_iter_coef_late),
               iteration = "list"),
    ### Get joined differentials
    tar_target(joined_diffs,
               get_joined_differentials(
                 combined_iter_coef_early_pop,
                 combined_iter_coef_late_pop
               ),
               pattern = map(combined_iter_coef_early_pop, combined_iter_coef_late_pop),
               iteration = "list"),
    ### Get joined district-level controls
    tar_target(district_controls,
               get_district_controls(
                 wage_boot_design_early,
                 wage_boot_design_late
               ),
               pattern = map(wage_boot_design_early, wage_boot_design_late),
               iteration = "list"),
    ### Get bootstrap data for projected and observed temperatures
    tar_target(bootstrap_data_proj,
               get_bootstrap_data_redux(
                 joined_diffs, utci_ewp, utci_lwp, district_controls
               ),
               pattern = map(joined_diffs, district_controls),
               iteration = "list"),
    tar_target(bootstrap_data_obs,
               get_bootstrap_data_redux(
                 joined_diffs, utci_ewp, utci_lwp, district_controls
               ),
               pattern = map(joined_diffs, district_controls),
               iteration = "list"),
    ### Run bootstrap estimation
    tar_target(boot_results_proj,
               run_bootstrap_estimation(
                 bootstrap_data_proj, seed, R_2, max_temp_hr_cols, housing_exp_share
               ),
               pattern = map(bootstrap_data_proj),
               iteration = "list"),
    tar_target(boot_results_obs,
               run_bootstrap_estimation(
                 bootstrap_data_obs, seed, R_2, max_temp_hr_cols, housing_exp_share
               ),
               pattern = map(bootstrap_data_obs),
               iteration = "list"),
    ### Extract parameter estimates
    # tar_target(boot_results_proj_t0,
    #            boot_results_proj$t0,
    #            pattern = map(boot_results_proj),
    #            iteration = "list"),
    # Dimension on each branch: R_2 * 45
    # Fill parameter matrix (mean + CI bounds) with nrow=9, ncol=5
    tar_target(boot_results_proj_t,
               boot_results_proj$t,
               pattern = map(boot_results_proj),
               iteration = "list"),
    # tar_target(boot_results_obs_t0,
    #            boot_results_obs$t0,
    #            pattern = map(boot_results_obs),
    #            iteration = "list"),
    # Dimension on each branch: R_2 * 45
    tar_target(boot_results_obs_t,
               boot_results_obs$t,
               pattern = map(boot_results_obs),
               iteration = "list"),
    ### Combine into one matrix of dimension ((R_1*R_2) * 45)
    # Use this matrix to get means and CIs
    tar_target(boot_results_full_proj,
               do.call(rbind, boot_results_proj_t)),
    tar_target(boot_results_full_obs,
               do.call(rbind, boot_results_proj_t)), 
    # TODO: Split this into a new pipeline and add summary tables
    tar_target(boot_means_ci_bounds_proj, 
               get_boot_means_ci_bounds(boot_results_full_proj)),
    tar_target(boot_means_ci_bounds_obs,
               get_boot_means_ci_bounds(boot_results_full_obs)),
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
    tar_target(boot_plot_obs, get_bootstrap_plot(plot_data_obs, vis_output_path, temp_data="Observed")),
    tar_target(boot_plot_proj, get_bootstrap_plot(plot_data_proj, vis_output_path, temp_data="Corrected")),
    tar_target(obs_corr_plot_early, get_obs_corrected_plot(combined_plot_data_early, vis_output_path, parameter="Early")),
    tar_target(obs_corr_plot_late, get_obs_corrected_plot(combined_plot_data_late, vis_output_path, parameter="Late")),
    tar_target(obs_corr_plot_late_mig, get_obs_corrected_plot(combined_plot_data_late_mig, vis_output_path, parameter="Late Mig Costs"))
  )
  return(pipeline_estimation)
}