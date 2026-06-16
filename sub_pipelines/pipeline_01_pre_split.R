# Pipeline for pre-split

get_pre_split_pipeline <- function(){
  pipeline_data_prep <- list(
    ##
    # Establish filenames
    ##
    ### Directories
    # TODO: Perhaps establish a "master" directory for portability?
    tar_target(nss_clean_path, file.path(main_data_path,"data","nss","CleanData")),
    tar_target(nss_raw_path, file.path(main_data_path,"data","nss","RawData")),
    tar_target(hces_2022_clean_path, file.path(main_data_path,"data","hces_2022","clean_data")),
    tar_target(hces_2022_raw_path, file.path(main_data_path,"data","hces_2022","raw_data")),
    tar_target(pop_projections_path, file.path(main_data_path,"data","population_projections")),
    tar_target(temperature_output_path, file.path(main_data_path,"temperature_data","output")),
    tar_target(geog_path, file.path(main_data_path,"geodata")),
    tar_target(bartik_path, file.path(main_data_path,"data","bartik")),
    tar_target(vis_output_path, file.path(main_data_path, "vis")),
    ### Early period
    tar_target(nss_ind_file,
               get_filename_str(here(nss_clean_path,"IND_2011_v2.dta")),
               format="file"),
    tar_target(hces_distcodes_file,
               get_filename_str(here(hces_2022_clean_path,"HCES2022_distcodes.dta")),
               format="file"),
    tar_target(df_hcs3_file,
               get_filename_str(here(nss_raw_path,"hcs_block_3.dta")),
               format="file"),
    tar_target(df_hcs4_file,
               get_filename_str(here(nss_raw_path,"hcs_block_4.dta")),
               format="file"),
    tar_target(df_hcs6_file,
               get_filename_str(here(nss_raw_path,"hcs_block_6.dta")),
               format="file"),
    ### Late period
    tar_target(hces_assets_file,
               get_filename_str(here(hces_2022_clean_path,"hh_assets.dta")),
               format="file"),
    tar_target(hces_consumption_file,
               get_filename_str(here(hces_2022_clean_path,"hh_consumption.dta")),
               format="file"),
    tar_target(hces_ident_file,
               get_filename_str(here(hces_2022_clean_path,"hh_ident.dta")),
               format="file"),
    tar_target(hces_roster_file,
               get_filename_str(here(hces_2022_clean_path,"hh_roster.dta")),
               format="file"),
    tar_target(hces_types_file,
               get_filename_str(here(hces_2022_clean_path,"hh_types.dta")),
               format="file"),
    tar_target(level3_raw_file,
               get_filename_str(here(hces_2022_raw_path,"LEVEL - 03.dta")),
               format="file"),
    tar_target(level9_raw_file,
               get_filename_str(here(hces_2022_raw_path,"LEVEL - 09 (Section 9 & 10 & 11).dta")),
               format="file"),
    ### Both periods
    tar_target(df_pop_file,
               get_filename_str(here(pop_projections_path,"india_pop_worldpop.csv")),
               format="file"),
    ### Second stage
    # Select UTCI variable in config file
    tar_target(utci_daily_filename,
               get_filename_str(here(temperature_output_path,selected_utci_file)),
               format="file"),
    tar_target(geog_file,
               get_filename_str(here(geog_path,"district_slope_water_instruments.csv")),
               format="file"),
    tar_target(bartik_file,
               get_filename_str(here(bartik_path,"bartik_instruments_ec05_ec13.csv")),
               format="file"),
    ##
    # Read in files
    ##
    ### Early period
    tar_target(nss_ind,
               read_dta_file(nss_ind_file)),
    tar_target(hces_distcodes,
               read_dta_file(hces_distcodes_file)),
    tar_target(df_hcs3,
               read_dta_file(df_hcs3_file)),
    tar_target(df_hcs4,
               read_dta_file(df_hcs4_file)),
    tar_target(df_hcs6,
               read_dta_file(df_hcs6_file)),
    ### Late period
    tar_target(hces_assets,
               read_dta_file(hces_assets_file)),
    tar_target(hces_consumption,
               read_dta_file(hces_consumption_file)),
    tar_target(hces_ident,
               read_dta_file(hces_ident_file)),
    tar_target(hces_roster,
               read_dta_file(hces_roster_file)),
    tar_target(hces_types,
               read_dta_file(hces_types_file)),
    tar_target(level3_raw,
               read_dta_file(level3_raw_file)),
    tar_target(level9_raw,
               read_dta_file(level9_raw_file)),
    ### Both periods
    tar_target(df_pop,
               read_input_csv(df_pop_file)),
    ### Second stage
    tar_target(utci_daily_input,
               read_input_csv(utci_daily_filename)),
    tar_target(geog,
               read_input_csv(geog_file)),
    tar_target(bartik,
               read_input_csv(bartik_file)),
    
    ##
    # Assemble data inputs
    ##
    ### Early period - filtered
    tar_target(nss_ind_reg,
               filter_nss(get_nss_data(nss_ind, hces_distcodes),opt_threshold)),
    tar_target(df_housing_merged,
               filter_nss(get_housing_data_early(df_hcs3, df_hcs4, df_hcs6, hces_distcodes))),
    ### Late period
    tar_target(hces_merged_emp,
               get_hces_merged_emp(
                 hces_assets, hces_distcodes, hces_consumption, hces_ident, hces_roster,
                 hces_types, level3_raw, level9_raw
               )),
    tar_target(hces_merged_emp_housing,
               get_housing_data_late(hces_merged_emp)),
    
    ### Second stage
    #### Instruments
    tar_target(instr_merged,
               get_instruments(geog,bartik)),
    tar_target(instr_merged_new,
               parse_sd_id_instr(instr_merged)),
    #### Temperature data
    tar_target(utci_daily,
               fix_utci_celsius(utci_daily_input)),
    tar_target(utci_new,
               get_adjusted_utci(utci_daily)),
    tar_target(utci_pivot_tables,
               get_utci_proj_obs(utci_new)),
    tar_target(early_window_utci_proj,
               get_window_utci_redux(
                 utci_pivot_tables[["utci_pivot_proj"]], col_prefixes_redux, year_suffixes_early
               )
    ),
    tar_target(early_window_utci_obs,
               get_window_utci_redux(
                 utci_pivot_tables[["utci_pivot_obs"]], col_prefixes_redux, year_suffixes_early
               )
    ),
    tar_target(late_window_utci_proj,
               get_window_utci_redux(
                 utci_pivot_tables[["utci_pivot_proj"]], col_prefixes_redux, year_suffixes_late
               )
    ),
    tar_target(late_window_utci_obs,
               get_window_utci_redux(
                 utci_pivot_tables[["utci_pivot_obs"]], col_prefixes_redux, year_suffixes_late
               )
    ),
    tar_target(utci_ewp,
               merge_utci_instr_redux(early_window_utci_proj, instr_merged_new)),
    tar_target(utci_ewo,
               merge_utci_instr_redux(early_window_utci_obs, instr_merged_new)),
    tar_target(utci_lwp,
               merge_utci_instr_redux(late_window_utci_proj, instr_merged_new)),
    tar_target(utci_lwo,
               merge_utci_instr_redux(late_window_utci_obs, instr_merged_new))
    
  )
  return(pipeline_data_prep)
}
