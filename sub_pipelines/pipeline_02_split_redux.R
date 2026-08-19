# Split pipeline, now with lower memory usage

get_split_pipeline <- function(){
  pipeline_estimation <- list(
    ##
    # Get weights from master survey designs
    ##
    ### Early period wages
    tar_target(
      name = hcs_boot_wts,
      command = {
        hcs_survey_design <- svydesign(
          ids = ~FSU,
          strata = ~Stratum,
          weights = ~Wgt_combined,
          data = df_housing_merged,
          nest = T
        )
        
        get_first_stage_boot_wts(hcs_survey_design, R_1)
      }
    ),
    ### Early period rents
    tar_target(
      name = nss_ind_reg_boot_wts,
      command = {
        nss_survey_design <- svydesign(
          ids = ~FSU_Serial_no,
          strata = ~Stratum,
          weights = ~Combined_multiplier,
          data = filter_nss(nss_ind_reg,opt_threshold),
          nest = T
        )
        
        get_first_stage_boot_wts(nss_survey_design, R_1)
      }
    ),
    ### Late period (wages and rents)
    tar_target(
      name = hces_boot_wts,
      command = {
        hces_survey_design <- svydesign(
          ids = ~fsu,
          strata = ~interaction(stratum, sub_stratum),
          weights = ~mult,
          data = hces_merged_emp_housing,
          nest = T
        )
        
        get_first_stage_boot_wts(hces_survey_design, R_1)
      }
    ),
    ##
    # Run parallelized first-stage estimation
    ##
    tar_target(rep_ids, 1:R_1),
    ### Early period wages
    tar_target(
      name = combined_stages,
      command = {
        # Early wages
        wage_boot_early_outputs <- get_boot_wage_model_early(rep_ids, nss_ind_reg_boot_wts)
        wage_boot_design_early <- wage_boot_early_outputs$wage_design_early
        wage_early_coefs <- get_boot_reg_outputs_vec(wage_boot_early_outputs$wage_early, alpha)$coef
        wage_pc_early_coefs <- get_boot_reg_outputs_vec(wage_boot_early_outputs$wage_pc_early, alpha)$coef
        # Early rents
        rent_boot_early_outputs <- get_boot_rent_model_early(rep_ids, hcs_boot_wts)
        rent_early_coefs <- get_boot_reg_outputs_vec(rent_boot_early_outputs$rent_early, alpha)$coef
        rent_pc_early_coefs <- get_boot_reg_outputs_vec(rent_boot_early_outputs$rent_pc_early, alpha)$coef
        # Late wages
        wage_boot_late_outputs <- get_boot_wage_model_late(rep_ids, hces_boot_wts)
        wage_boot_design_late <- wage_boot_late_outputs$wage_design_late
        wage_late_coefs <- get_boot_reg_outputs_vec(wage_boot_late_outputs$wage_late, alpha)$coef
        wage_pc_late_coefs <- get_boot_reg_outputs_vec(wage_boot_late_outputs$wage_pc_late, alpha)$coef
        # Late rents
        rent_boot_late_outputs <- get_boot_rent_model_late(rep_ids, hces_boot_wts)
        rent_late_coefs <- get_boot_reg_outputs_vec(rent_boot_late_outputs$rent_late, alpha)$coef
        rent_pc_late_coefs <- get_boot_reg_outputs_vec(rent_boot_late_outputs$rent_pc_late, alpha)$coef
        # Joined district-level controls
        district_controls <- get_district_controls(wage_boot_design_early, wage_boot_design_late)
        # Combined early coefficients
        combined_iter_coef_early <- get_combined_iter_coef(
          wage_pc_early_coefs,
          rent_pc_early_coefs,
          seq(1,9), seq(1,7)
        )
        combined_iter_coef_early_pop <- merge_pop_plot_data(combined_iter_coef_early, df_pop, recent_data=F)
        # Combined late coefficients
        combined_iter_coef_late <- get_combined_iter_coef(
          wage_pc_late_coefs,
          rent_pc_late_coefs,
          seq(1,7), seq(1,8)
        )
        combined_iter_coef_late_pop <- merge_pop_plot_data(combined_iter_coef_late, df_pop, recent_data=T)
        # Joined differentials
        joined_diffs <- get_joined_differentials(combined_iter_coef_early_pop, combined_iter_coef_late_pop)
        # Projected bootstrap data
        bootstrap_data_proj <- get_bootstrap_data_redux(joined_diffs, utci_ewp, utci_lwp, district_controls)
        # Observed bootstrap data
        bootstrap_data_obs <- get_bootstrap_data_redux(joined_diffs, utci_ewo, utci_lwo, district_controls)
        # Run estimation
        boot_results_proj_unified <- run_bootstrap_estimation_unified(bootstrap_data_proj, seed, R_2, housing_exp_share)
        boot_results_obs_unified <- run_bootstrap_estimation_unified(bootstrap_data_obs, seed, R_2, housing_exp_share)
        ## After Estimation
        # Extract first-stage parameter estimates
        # Order: wage early, rent early, wage late, rent late
        first_stage_coefs_list <- list(
          wage_early = wage_early_coefs,
          wage_pc_early = wage_pc_early_coefs,
          rent_early = rent_early_coefs,
          rent_pc_early = rent_pc_early_coefs,
          wage_late = wage_late_coefs,
          wage_pc_late = wage_pc_late_coefs,
          rent_late = rent_late_coefs,
          rent_pc_late = rent_pc_late_coefs
        )
        # Extract second-stage parameter estimates
        boot_results_t <- list(
          boot_results_proj_t = boot_results_proj_unified$t,
          boot_results_obs_t = boot_results_obs_unified$t
        )
        # Get one sample of bootstrap data
        bootstrap_data_samples <- list(
          boot_data_proj = bootstrap_data_proj,
          boot_data_obs = bootstrap_data_obs
        )
        # Return final values
        list(
          first_stage = first_stage_coefs_list,
          second_stage = boot_results_t,
          data = bootstrap_data_samples
        )
      },
      pattern = map(rep_ids),
      memory = "transient"
    ),
    # Extract first stage elements
    tar_target(first_stage_elements,
               combined_stages$first_stage,
               pattern = map(combined_stages),
               iteration = "list"),
    tar_target(second_stage_elements,
               combined_stages$second_stage,
               pattern = map(combined_stages),
               iteration = "list"),
    tar_target(data_elements,
               combined_stages$data,
               pattern = map(combined_stages),
               iteration = "list",
               memory = "transient"),
    ### Combine master results
    #### First stage
    tar_target(master_wage_early_coefs,
               get_merged_estimates(lapply(first_stage_elements, '[[', "wage_early"))),
    tar_target(master_wage_pc_early_coefs,
               get_merged_estimates(lapply(first_stage_elements, '[[', "wage_pc_early"))),
    tar_target(master_rent_early_coefs,
               get_merged_estimates(lapply(first_stage_elements, '[[', "rent_early"))),
    tar_target(master_rent_pc_early_coefs,
               get_merged_estimates(lapply(first_stage_elements, '[[', "rent_pc_early"))),
    tar_target(master_wage_late_coefs,
               get_merged_estimates(lapply(first_stage_elements, '[[', "wage_late"))),
    tar_target(master_wage_pc_late_coefs,
               get_merged_estimates(lapply(first_stage_elements, '[[', "wage_pc_late"))),
    tar_target(master_rent_late_coefs,
               get_merged_estimates(lapply(first_stage_elements, '[[', "rent_late"))),
    tar_target(master_rent_pc_late_coefs,
               get_merged_estimates(lapply(first_stage_elements, '[[', "rent_pc_late"))),
    #### Second stage
    tar_target(master_boot_results_proj,
               do.call(rbind, lapply(second_stage_elements, '[[', "boot_results_proj_t"))),
    tar_target(master_boot_results_obs,
               do.call(rbind, lapply(second_stage_elements, '[[', "boot_results_obs_t"))),
    #### Data
    tar_target(bootstrap_data_proj,
               data_elements[[1]]$boot_data_proj),
    tar_target(bootstrap_data_obs,
               data_elements[[1]]$boot_data_obs),
    ### Get stargazer outputs
    #### First stage
    tar_target(wage_early_coefs_list,
               list(master_wage_early_coefs[1:9,], master_wage_pc_early_coefs[1:9,])),
    tar_target(wage_early_base_models_list,
               list(
                 lm(log(totexp) ~ poly(Age,2)+male+educ+hindu+scst, data=nss_ind_reg),
                 lm(logepc ~ poly(Age,2)+male+educ+hindu+scst, data=nss_ind_reg)
               )),
    tar_target(rent_early_coefs_list,
               list(master_rent_early_coefs[1:7,], master_rent_pc_early_coefs[1:7,])),
    tar_target(rent_early_base_models_list,
               list(
                 lm(loghc ~ pucca_walls+pucca_floor+pucca_roof+piped_water+own_latrine+elec, data=df_housing_merged),
                 lm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+piped_water+own_latrine+elec, data=df_housing_merged)
               )),
    tar_target(wage_late_coefs_list,
               list(master_wage_late_coefs[1:7,], master_wage_pc_late_coefs[1:7,])),
    tar_target(wage_late_base_models_list,
               list(
                 lm(log(totexp) ~ poly(age,2)+male+edu+hindu+scstbc, data=hces_merged_emp_housing),
                 lm(logepc ~ poly(age,2)+male+edu+hindu+scstbc, data=hces_merged_emp_housing)
               )),
    tar_target(rent_late_coefs_list,
               list(master_rent_late_coefs[1:8,], master_rent_pc_early_coefs[1:8,])),
    tar_target(rent_late_base_models_list,
               list(
                 lm(loghc ~ pucca_walls+pucca_floor+pucca_roof+cooking_fuel+lighting_source+piped_water+own_latrine, data=hces_merged_emp_housing),
                 lm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+cooking_fuel+lighting_source+piped_water+own_latrine, data=hces_merged_emp_housing)
               )),
    tar_target(second_stage_early_proj_coefs_list,
               list(t(master_boot_results_proj[,1:6]), t(master_boot_results_proj[,7:12]))),
    tar_target(second_stage_early_proj_base_models_list,
               list(
                 lm(estimate_wage_early ~ hindu_early + scst_early + Age_early + educ_hs_early + educ_ps_early, data=bootstrap_data_proj),
                 lm(estimate_rent_early ~ hindu_early + scst_early + Age_early + educ_hs_early + educ_ps_early, data=bootstrap_data_proj)
               )),
    tar_target(second_stage_late_proj_coefs_list,
               list(t(master_boot_results_proj[,13:18]), t(master_boot_results_proj[,19:24]))),
    tar_target(second_stage_late_proj_base_models_list,
               list(
                 lm(estimate_wage_late ~ hindu_late + scst_late + Age_late + educ_hs_late + educ_ps_late, data=bootstrap_data_proj),
                 lm(estimate_rent_late ~ hindu_late + scst_late + Age_late + educ_hs_late + educ_ps_late, data=bootstrap_data_proj)
               )),
    
    tar_target(second_stage_early_obs_coefs_list,
               list(t(master_boot_results_obs[,1:6]), t(master_boot_results_obs[,7:12]))),
    tar_target(second_stage_early_obs_base_models_list,
               list(
                 lm(estimate_wage_early ~ hindu_early + scst_early + Age_early + educ_hs_early + educ_ps_early, data=bootstrap_data_obs),
                 lm(estimate_rent_early ~ hindu_early + scst_early + Age_early + educ_hs_early + educ_ps_early, data=bootstrap_data_obs)
               )),
    tar_target(second_stage_late_obs_coefs_list,
               list(t(master_boot_results_obs[,13:18]), t(master_boot_results_obs[,19:24]))),
    tar_target(second_stage_late_obs_base_models_list,
               list(
                 lm(estimate_wage_late ~ hindu_late + scst_late + Age_late + educ_hs_late + educ_ps_late, data=bootstrap_data_obs),
                 lm(estimate_rent_late ~ hindu_late + scst_late + Age_late + educ_hs_late + educ_ps_late, data=bootstrap_data_obs)
               ))
  )
  return(pipeline_estimation)
}