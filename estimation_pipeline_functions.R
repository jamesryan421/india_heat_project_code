# R Pipeline Functions

## :::::::::::
## General - Read in Data from Filename
## :::::::::::
read_dta_file=function(filename){
  return(read_dta(filename))
}

read_input_csv=function(filename){read_csv(filename)}

get_filename_str=function(filename_here){return(as.character(filename_here))}

## :::::::::::
## Early Period
## :::::::::::

### Wage Regressions
get_nss_data=function(nss_ind,hces_distcodes){
  # Start with reading clean data in NSS files for 2011
  
  #nss_cloth=read_dta(here("../data/nss/CleanData","hh_cloth.dta")) #Clothing consumption
  #nss_consumption=read_dta(here("../data/nss/CleanData","hh_consumption.dta")) #Total household consumption
  #nss_ident=read_dta(here("../data/nss/CleanData","hh_ident.dta"))
  #nss_roster=read_dta(here("../data/nss/CleanData","hh_roster.dta")) #Years of education, household head, religion, marital status
  #nss_types=read_dta(here("../data/nss/CleanData","hh_types.dta"))
  #nss_ind=read_dta(here("../data/nss/CleanData","IND_2011_v2.dta")) #Skip V1
  
  # Since nss_id looks to be merged, take the columns from that one for now
  # Note that districts in this data correspond to the 2001 delineations, not the 2011 districts
  # Also, it looks like there's only one observation per household; presume these are
  # heads of household who are employed, but check with Tanmay on this
  demographic_cols=c(
    "num_hhmem","Sex","Age","Marital_status","Education",
    "hindu","scst","Relation"
  )
  exp_cols=c(
    "totexp","expdur","totexp_excldur","tot"
  )
  id_cols=c(
    "hhid","year","Sector","State_code","state_id","District_code","District",
    "FSU_Serial_no","Stratum","Combined_multiplier"
  )
  
  nss_ind_cols=c(
    id_cols,exp_cols,demographic_cols
  )
  
  nss_ind_reg<-nss_ind[,nss_ind_cols]
  
  # Fixing columns
  nss_ind_reg <- nss_ind_reg %>% mutate(
    educ=case_when(
      Education=="01" ~ "Illiterate",
      Education %in% c("02","03","04") ~ "No Formal School",
      Education %in% c("05","06","07","08","10") ~ "HS or Less",
      Education %in% c("11","12","13") ~ "Post-Secondary",
      Education %in% c("",NA) ~ NA
    ),
    educ=factor(educ,levels=c("Illiterate","No Formal School","HS or Less","Post-Secondary"),ordered=T),
    # Get dummies for summary stats
    educ_ill=if_else(educ=="Illiterate",1,0),
    educ_nfs=if_else(educ=="No Formal School",1,0),
    educ_hs=if_else(educ=="HS or Less",1,0),
    educ_ps=if_else(educ=="Post-Secondary",1,0),
    logepc=log(totexp/num_hhmem),
    male=if_else(Sex=="1",1,0) #Think this is right?
  )
  # Try removing the filter on heads of household
  # ) %>% filter(
  #   Relation==1 #Only heads of household
  # )
  
  # Merging on state and district names
  # Assume (VERIFY LATER) that early-panel data is at the 2011 district level
  # Because we need consistent fixed effects across the panel, the goal here is to
  # merge on the district names which we used in the late panel
  # All of the districts in the housing data appear in the wage data for the early panel,
  # but not vice-versa
  # From the superset of wage data, not all of the districts in the late panel appear in the early data,
  # and vice versa
  # Still, we should get enough of a sample to proceed
  # TODO: Get more specific sample attrition metrics
  
  # Load in district names from HCES late sample data
  #hces_distcodes=read_dta(here("../data/hces_2022/clean_data","HCES2022_distcodes.dta"))
  #hces_distcodes_to_merge=hces_distcodes[,c("state","district","state_name","district_name")]
  #names(hces_distcodes_to_merge)=c("State_code","District","state_name","district_name")
  # Join distcodes using nsscode
  hces_distcodes <- hces_distcodes %>%
    mutate(
      #nsscode <- replace_na(nsscode,0),
      nsscode = as.character(sprintf("%04d", nsscode))
    )
  
  nss_ind_reg<-inner_join(
    nss_ind_reg,
    hces_distcodes,
    by=c("District_code" = "nsscode")
  )
  return(nss_ind_reg)
}

filter_nss<-function(nss_ind_reg,ss_filter=0){
  # Filter to only keep districts with more than "ssfilter" observations
  district_freq=nss_ind_reg %>% count(district_name)
  selected_dists=(district_freq %>% filter(n>=ss_filter))$district_name
  
  return(nss_ind_reg %>% filter(district_name %in% selected_dists))
}

get_wage_regs_early=function(nss_ind_reg,survey_design,summary_format="latex"){
  # Wage regressions
  # Summary stats
  wage_reg_vars=c("logepc","Age","male",
                  "educ_ill","educ_nfs","educ_hs","educ_ps",
                  "hindu","scst")
  stargazer(as.data.frame(nss_ind_reg[,wage_reg_vars]),type=summary_format,summary=T,
            covariate.labels=c("Log Monthly Expenditure Per Capita",
                               "Age","Male",
                               "Illiterate","Literate, No Formal School","Literate, HS or Less","Literate, Post-Secondary",
                               "Hindu","Scheduled Caste/Tribe"))
  # # No fixed effects
  # wage_model1=lm(log(totexp) ~ poly(Age,2)+male+educ+hindu+scst,
  #                data=nss_ind_reg)
  # # With fixed effects
  # wage_model2=lm(log(totexp) ~ poly(Age,2)+male+educ+hindu+scst+factor(district_name),
  #                data=nss_ind_reg)
  # Models with survey design
  wage_model1 <- svyglm(log(totexp) ~ poly(Age,2)+male+educ+hindu+scst,
                        design = survey_design)
  wage_model2 <- svyglm(log(totexp) ~ poly(Age,2)+male+educ+hindu+scst+factor(district_name),
                        design = survey_design)
  
  stargazer(wage_model1,wage_model2,
            type=summary_format,
            keep=c("Intercept","Age","I(Age^2)","male","educ.L","educ.Q","educ.C","hindu","scst"),
            add.lines=list(c("District FE","No","Yes"))
  )
  # Models with per-capita expenditure
  # No fixed effects
  # wage_model1_pc=lm(logepc ~ poly(Age,2)+male+educ+hindu+scst,
  #                   data=nss_ind_reg)
  # # With fixed effects
  # wage_model2_pc=lm(logepc ~ poly(Age,2)+male+educ+hindu+scst+factor(district_name),
  #                   data=nss_ind_reg)
  # Models with survey design
  wage_model1_pc <- svyglm(logepc ~ poly(Age,2)+male+educ+hindu+scst,
                           design = survey_design)
  wage_model2_pc <- svyglm(logepc ~ poly(Age,2)+male+educ+hindu+scst+factor(district_name),
                           design = survey_design)
  stargazer(wage_model1_pc,wage_model2_pc,
            type="latex",
            keep=c("Intercept","Age","I(Age^2)","male","educ.L","educ.Q","educ.C","hindu","scst"),
            add.lines=list(c("District FE","No","Yes"))
  )
  # Update: return summary stats for regression
  list_to_return=list(wage_model1,wage_model2,wage_model1_pc,wage_model2_pc,nss_ind_reg[,wage_reg_vars])
  return(list_to_return)
}

get_sample_size_diagnostics_wage_early=function(nss_ind_reg,filter_sizes){
  # Diagnostic exercise: trade-off between significant FEs and sample size
  filtered_models=lapply(filter_sizes,function(x){
    get_wage_regs(filter_nss(nss_ind_reg,x),summary_format="text")
  })
  sample_size_signif_wage=sapply(filtered_models,function(x){
    get_prop_signif_fes(x[[2]])
  })
  sample_size_signif_wage_pc=sapply(filtered_models,function(x){
    get_prop_signif_fes(x[[4]])
  })
  list_to_return=list(
    "results_wage"=cbind(filter_sizes,t(sample_size_signif_wage)),
    "results_wage_pc"=cbind(filter_sizes,t(sample_size_signif_wage_pc))
  )
  return(list_to_return)
}

### Housing Regressions
get_housing_data_early=function(df_hcs3,df_hcs4,df_hcs6,hces_distcodes){
  #df_hcs1_2=read_dta(here("../data/nss/RawData","hcs_block_1_2.dta")) #Identification
  #df_hcs3=read_dta(here("../data/nss/RawData","hcs_block_3.dta")) #Household characteristics
  #df_hcs4=read_dta(here("../data/nss/RawData","hcs_block_4.dta")) #Latrine type and electricity
  #df_hcs5=read_dta(here("../data/nss/RawData","hcs_block_5.dta")) #Environment records
  #df_hcs6=read_dta(here("../data/nss/RawData","hcs_block_6.dta")) # Dwelling particulars, including building materials, rent, rooms, and area
  
  # These household IDs don't match with the household IDs we use for the wage regressions
  # That's not a big problem, just something to keep in mind
  housing_merge_cols=c(
    "Key_hhold","State","Region","District","FSU","Stratum","Wgt_combined"
  )
  
  # Characteristics in df_hcs3
  # B3_q3: Total household size
  hcs3_cols=c("B3_q3")
  hcs3_names=c("hhsize")
  
  # Characteristics in df_hcs4
  # B4_q1_1: Major source of drinking water
  # B4_q4: Facility of drinking water
  # B4_q5: Distance to drinking water source
  # B4_q6: Facility of bathroom
  # B4_q8: Use of latrine
  # B4_q9: Type of latrine
  # B4_q10: Electricity for domestic use
  
  hcs4_cols=c("B4_q1_1","B4_q4","B4_q5","B4_q6","B4_q8","B4_q9","B4_q10")
  hcs4_names=c("water_source","water_facility","water_source_dist",
               "bathroom","latrine_use","latrine_type","elec")
  
  # Characteristics in df_hcs6
  # B6_q8: Total floor area
  # B6_q13: Kitchen type
  # B6_q14: Floor type
  # B6_q15: Wall type
  # B6_q16: Roof type
  # B6_q17: Monthly rent in Rs
  
  df_hcs6 <- df_hcs6 %>% rename(Wgt_combined = Wgt_Combined)
  hcs6_cols=c("B6_q8","B6_q13","B6_q14","B6_q15","B6_q16","B6_q17")
  hcs6_names=c("total_area","kitchen_type","floor_type","wall_type","roof_type","rent")
  
  df_housing_merged=inner_join(
    df_hcs3[,c(housing_merge_cols,hcs3_cols)],
    inner_join(
      df_hcs4[,c(housing_merge_cols,hcs4_cols)],
      df_hcs6[,c(housing_merge_cols,hcs6_cols)],
      by=housing_merge_cols
    ),
    by=housing_merge_cols
  )
  
  # Rename cols
  names(df_housing_merged)=c(housing_merge_cols,hcs3_names,hcs4_names,hcs6_names)
  
  # Recode columns
  df_housing_merged<-df_housing_merged %>% 
    mutate(
      rent=as.numeric(rent)
    ) %>%
    filter(rent>0) %>% 
    mutate(
      # hcs4_cols
      piped_water=if_else(water_source %in% c("02","03","04","05"),1,0), #Double check consistency later
      exclusive_water=if_else(water_facility=="1",1,0),
      water_in_dwelling=if_else(water_source_dist %in% c("1","2"),1,0),
      bathroom_type=case_when(
        bathroom=="1" ~ "b_attached",
        bathroom=="2" ~ "b_detached",
        bathroom=="3" ~ "no_b"
      ),
      bathroom_type=factor(bathroom_type,levels=c("b_attached","b_detached","no_b"),ordered=F),
      own_latrine=if_else(latrine_use=="1",1,0),
      flush_latrine=if_else(latrine_type=="3",1,0),
      elec=if_else(elec=="1",1,0),
      # hcs6 cols
      kitchen_type=case_when(
        kitchen_type=="1" ~ "k_water",
        kitchen_type=="2" ~ "k_no_water",
        kitchen_type=="3" ~ "no_k"
      ),
      kitchen_type=factor(kitchen_type,levels=c("k_water","k_no_water","no_k"),ordered=F),
      pucca_floor=if_else(floor_type %in% c("3","4","5","6","9"),1,0),
      pucca_walls=if_else(wall_type %in% c("5","6","7","8","9"),1,0),
      pucca_roof=if_else(roof_type %in% c("5","6","7","8","9"),1,0),
      loghc=log(rent),
      loghcpc=log(rent/hhsize)
    )
  
  # Merge on districts
  # Load in district names from HCES late sample data
  #hces_distcodes=read_dta(here("../data/hces_2022/clean_data","HCES2022_distcodes.dta"))
  hces_distcodes_to_merge=hces_distcodes[,c("state","district","state_name","district_name")]
  names(hces_distcodes_to_merge)=c("State","District","state_name","district_name")
  
  df_housing_merged<-inner_join(
    df_housing_merged,
    hces_distcodes_to_merge,
    by=c("State","District")
  )
  return(df_housing_merged)
}

get_rent_regs_early=function(df_housing_merged,survey_design,summary_format="latex"){
  # Summary stats
  rent_reg_vars=c("loghc","loghcpc","pucca_walls","pucca_floor","pucca_roof",
                  "piped_water","own_latrine","elec")
  stargazer(as.data.frame(df_housing_merged[,rent_reg_vars]),type=summary_format,summary=T,
            covariate.labels=c("Log Rent","Log Rent per Capita","Pucca Walls","Pucca Floor","Pucca Roof",
                               "Piped Drinking Water","Exclusive Latrine","Electricity"))
  
  # Initial regressions
  # No fixed effects
  # rent_model1=lm(loghc ~ pucca_walls+pucca_floor+pucca_roof+
  #                  piped_water+own_latrine+elec,
  #                data=df_housing_merged)
  # # Fixed effects
  # rent_model2=lm(loghc ~ pucca_walls+pucca_floor+pucca_roof+
  #                  piped_water+own_latrine+elec+factor(district_name),
  #                data=df_housing_merged)
  rent_model1 <- svyglm(loghc ~ pucca_walls+pucca_floor+pucca_roof+piped_water+own_latrine+elec,
                        design = survey_design)
  rent_model2 <- svyglm(loghc ~ pucca_walls+pucca_floor+pucca_roof+piped_water+own_latrine+elec+factor(district_name),
                        design = survey_design)
  stargazer(rent_model1,rent_model2,
            type=summary_format,
            keep=c("Intercept","pucca_walls","pucca_floor","pucca_roof",
                   "piped_water","own_latrine","elec"),
            add.lines=list(c("District FE","No","Yes")))
  # # Rent per capita regressions
  # rent_model1_pc=lm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+
  #                     piped_water+own_latrine+elec,
  #                   data=df_housing_merged)
  # # Fixed effects
  # rent_model2_pc=lm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+
  #                     piped_water+own_latrine+elec+factor(district_name),
  #                   data=df_housing_merged)
  rent_model1_pc <- svyglm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+piped_water+own_latrine+elec,
                           design = survey_design)
  rent_model2_pc <- svyglm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+piped_water+own_latrine+elec+factor(district_name),
                           design = survey_design)
  stargazer(rent_model1_pc,rent_model2_pc,
            type=summary_format,
            keep=c("Intercept","pucca_walls","pucca_floor","pucca_roof",
                   "piped_water","own_latrine","elec"),
            add.lines=list(c("District FE","No","Yes")))
  list_to_return=list(rent_model1,rent_model2,rent_model1_pc,rent_model2_pc)
  return(list_to_return)
}

get_prop_signif_fes=function(model){
  # Get number of FEs and proportion which are statistically significant
  return(
    tidy(model) %>%
      filter(str_starts(term,"factor")) %>%
      summarize(prop_signif_05=mean(p.value<0.05,na.rm=T),
                prop_signif_10=mean(p.value<0.1,na.rm=T),
                n=sum(p.value>0,na.rm=T)) %>% unlist()
  )
}

get_sample_size_diagnostics_rent_early=function(df_housing_merged,filter_sizes){
  # Diagnostic exercise: trade-off between significant FEs and sample size
  filtered_models=lapply(filter_sizes,function(x){
    get_rent_regs(filter_nss(df_housing_merged,x),summary_format="text")
  })
  sample_size_signif_rent=sapply(filtered_models,function(x){
    get_prop_signif_fes(x[[2]])
  })
  sample_size_signif_rent_pc=sapply(filtered_models,function(x){
    get_prop_signif_fes(x[[4]])
  })
  list_to_return=list(
    "results_rent"=cbind(filter_sizes,t(sample_size_signif_rent)),
    "results_rent_pc"=cbind(filter_sizes,t(sample_size_signif_rent_pc))
  )
  return(list_to_return)
}

### Both Models
get_n_signif_both_models_early=function(nss_ind_reg,rent_survey_design,df_housing_merged,wage_survey_design,opt_threshold,pval_thresh){
  # Get the number of fixed effects which are statistically significant in each model, and in both models
  # Also get the wage and rent regressions for a given threshold
  opt_wage_regs=get_wage_regs_early(
    filter_nss(nss_ind_reg,opt_threshold),rent_survey_design,summary_format="text"
  )
  opt_rent_regs=get_rent_regs_early(
    filter_nss(df_housing_merged,opt_threshold),wage_survey_design,summary_format="text"
  )
  
  wage_model_diffs=tidy(opt_wage_regs[[4]]) %>%
    filter(str_starts(term,"factor")) %>%
    mutate(signif=if_else(p.value<pval_thresh,"Significant","Not Significant"),
           district_name=str_replace_all(term,"factor\\(district_name\\)","")) %>%
    select(district_name,p.value,signif) %>%
    rename(wage_pval = p.value)
  
  rent_model_diffs=tidy(opt_rent_regs[[4]]) %>%
    filter(str_starts(term,"factor")) %>%
    mutate(signif=if_else(p.value<pval_thresh,"Significant","Not Significant"),
           district_name=str_replace_all(term,"factor\\(district_name\\)","")) %>%
    select(district_name,p.value,signif) %>%
    rename(rent_pval = p.value)
  
  n_wage_signif=sum(wage_model_diffs$wage_pval<pval_thresh)
  n_rent_signif=sum(rent_model_diffs$rent_pval<pval_thresh)
  # How many differentials are significant at some threshold for both models?
  model_diffs_merged=inner_join(
    wage_model_diffs,
    rent_model_diffs,
    by="district_name"
  )
  n_both_signif=model_diffs_merged %>%
    mutate(
      both_signif = ((wage_pval<pval_thresh) & (rent_pval<pval_thresh))
    ) %>%
    summarize(
      both_signif=sum(both_signif)
    ) %>% pull()
  n_signif=c("Wage Signif."=n_wage_signif,
             "Rent signif."=n_rent_signif,
             "Both Signif."=n_both_signif)
  return(list(opt_wage_regs,opt_rent_regs,n_signif))
}

extract_opt_wage_model=function(opt_models){
  # Helper function: get per-capita wage model
  return(opt_models[[1]][[4]])
}

extract_opt_rent_model=function(opt_models){
  return(opt_models[[2]][[4]])
}

extract_n_signif=function(opt_models){
  return(opt_models[[3]])
}

### Plots
get_plot_data=function(wage_model,rent_model,pval_thresh=0.05){
  # Extract coefficients from both models; should run with per capita models
  df_wage_diffs <- tidy(wage_model) %>% 
    filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)")))) %>%
    select(district, estimate_wage = estimate, wage_pval=p.value)
  
  df_rent_diffs <- tidy(rent_model) %>% 
    filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)")))) %>%
    select(district, estimate_rent = estimate, rent_pval=p.value)
  
  # Join them together
  plot_data <- inner_join(df_wage_diffs, df_rent_diffs, by = "district")
  # Get a column for whether both p-values are below the threshold
  plot_data <- plot_data %>%
    mutate(
      both_signif=((wage_pval<pval_thresh) & (rent_pval<pval_thresh))
    )
  return(plot_data)
}

merge_pop_plot_data=function(plot_data,df_pop,recent_data=F){
  # Merge in population data using latest projections
  #df_pop<-read_csv(here("../data/population_projections","india_population_projections.csv"))
  
  if (recent_data) {
    # Use 2022 projected population since this matches when the data was collected
    plot_data <- inner_join(
      plot_data,
      #df_pop[,c("Dist_Name","pop_22")] %>% rename(pop = pop_22),
      # Update: use worldpopdata
      df_pop[,c("Dist_name","pop_23_wp")] %>% rename(pop = pop_23_wp),
      by=c("district" = "Dist_name")
    ) %>%
      mutate(logpop=log(pop))
  } else {
    # If this option is turned off, use data from 2011, which was the last census
    # Use 2022 projected population since this matches when the data was collected
    plot_data <- inner_join(
      plot_data,
      #df_pop[,c("Dist_Name","pop_11")] %>% rename(pop = pop_11),
      df_pop[,c("Dist_name","pop_11_proj")] %>% rename(pop = pop_11_proj),
      by=c("district" = "Dist_name")
    ) %>%
      mutate(logpop=log(pop))
  }
  
  # Get a categorical variable for district population tier
  plot_data <- plot_data %>% mutate(
    popcat=case_when(
      pop<1500000 ~ "<1.5m",
      ((pop>=1500000) & (pop<2500000)) ~ "1.5-2.5m",
      ((pop>=2500000) & (pop<3500000)) ~ "2.5-3.5m",
      ((pop>=3500000)) ~ ">3.5m"
    ),
    popcat=factor(popcat,levels=c("<1.5m","1.5-2.5m","2.5-3.5m",">3.5m"),ordered=T)
  )
  return(plot_data)
}

get_wage_rent_diffs_plot=function(plot_data,period="early",highlight_signif=F,add_marginal=F){
  # Get a scatter plot of wage and rent differentials
  wage_price_diffs=ggplot(plot_data,aes(x=estimate_wage,y=estimate_rent))+
    geom_abline(slope=1,intercept=0,linetype="dashed",color="gray50")+ #45-degree line
    geom_vline(xintercept=0,linetype="dashed",color="gray70")+
    geom_hline(yintercept=0,linetype="dashed",color="gray70")
  if (highlight_signif){
    wage_price_diffs <- wage_price_diffs +
      geom_point(aes(shape=popcat,color=popcat,fill=popcat,alpha=both_signif)) +
      scale_alpha_manual(name="Both Signif.",values=c("FALSE"=0.3,"TRUE"=1.0)) +
      scale_shape_manual(name="District Pop.",values=c("<1.5m"=3,"1.5-2.5m"=23,"2.5-3.5m"=22,">3.5m"=21))+
      scale_color_manual(name="District Pop.",values=c("<1.5m"="firebrick","1.5-2.5m"="olivedrab","2.5-3.5m"="cadetblue",">3.5m"="darkblue"))+
      scale_fill_manual(name="District Pop.",values=c("<1.5m"="firebrick","1.5-2.5m"="olivedrab","2.5-3.5m"="cadetblue",">3.5m"="darkblue"))
  } else{
    wage_price_diffs <- wage_price_diffs +
      geom_point(aes(shape=popcat,color=popcat,fill=popcat),alpha=0.7) +
      scale_shape_manual(name="District Pop.",values=c("<1.5m"=3,"1.5-2.5m"=23,"2.5-3.5m"=22,">3.5m"=21))+
      scale_color_manual(name="District Pop.",values=c("<1.5m"="firebrick","1.5-2.5m"="olivedrab","2.5-3.5m"="cadetblue",">3.5m"="darkblue"))+
      scale_fill_manual(name="District Pop.",values=c("<1.5m"="firebrick","1.5-2.5m"="olivedrab","2.5-3.5m"="cadetblue",">3.5m"="darkblue"))
  }
  wage_price_diffs <- wage_price_diffs +
    #geom_text(aes(label=district),vjust=-1,size=3,check_overlap=T)+
    #scale_size_manual(values=c(1,2,3,4))+
    xlim(1.25*min(plot_data$estimate_wage),abs(1.25*max(plot_data$estimate_wage)))+
    ylim(1.25*min(plot_data$estimate_rent),abs(1.25*max(plot_data$estimate_rent)))+
    labs(
      title="Log Wage vs. Price Differentials",
      x="Log Wage Differential",
      y="Log Rent Differential",
      caption="Source: 2022-23 HCES, 2022 Population (Proj.)"
    )
  if (add_marginal) {
    # Add marginal densities
    wage_price_diffs_final=ggMarginal(wage_price_diffs,
                                      type="density",
                                      fill="lightblue",alpha=0.5)
  } else {
    wage_price_diffs_final=wage_price_diffs
  }
  
  # Write out
  ggsave(here("../vis",paste0(period,"_panel_wage_price_diffs.png")),
         wage_price_diffs_final,width=8,height=6,units="in")
  print(wage_price_diffs_final)
  return(wage_price_diffs_final)
}

get_wage_rent_cor=function(plot_data,output_format="text"){
  cor_table=plot_data %>% group_by(both_signif) %>%
    summarize(
      "Wage-Rent Correlation"=cor(estimate_wage,estimate_rent),
      "Wage-Pop Correlation"=cor(estimate_wage,logpop),
      "Rent-Pop Correlation"=cor(estimate_rent,logpop)
    )
  stargazer(as.data.frame(cor_table),summary=F,type=output_format)
  # Check relationship between differentials and pop
  wage_diff_popcat_reg=lm(estimate_wage ~ logpop,data=plot_data)
  rent_diff_popcat_reg=lm(estimate_rent ~ logpop,data=plot_data)
  
  stargazer(wage_diff_popcat_reg,
            rent_diff_popcat_reg,
            type=output_format,
            covariate.labels=c("Population","Intercept"))
  list_to_return=list(cor_table,wage_diff_popcat_reg,rent_diff_popcat_reg)
  return(list_to_return)
}

extract_cor_table=function(x){return(x[[1]])}
extract_wage_diff_popcat_reg=function(x){return(x[[2]])}
extract_rent_diff_popcat_reg=function(x){return(x[[3]])}

write_diffs=function(plot_data,period="early"){
  # Write differentials to disc
  write_csv(plot_data,here("../data/differentials",paste0(period,"_period_differentials.csv")))
}

## :::::::::::
## Late Period
## :::::::::::

### Wage Regressions
get_hces_merged_emp <- function(hces_assets,hces_distcodes,hces_consumption,
                                hces_ident,hces_roster,hces_types,
                                level3_raw,level9_raw){
  # First load in HCES data
  #setwd(file.path(getwd(),"hces_2022\\clean_data"))
  
  ###
  # DESCRIPTION OF DIFFERENT CLEANED FILEs
  # HCES2022_distcodes.dta - District codes and names
  # hh_assets
  # hh_consumption - Data on consumption at the household level
  # hh_ident - Geographic identification for the household level
  # hh_roster - Breakdown of household composition with individual characteristics
  # hh_types - Breakdown of household characteristics aggregated to hhid level
  ###
  
  # hces_assets=read_dta(here("../data/hces_2022/clean_data","hh_assets.dta"))
  # hces_distcodes=read_dta(here("../data/hces_2022/clean_data","HCES2022_distcodes.dta"))
  # hces_consumption=read_dta(here("../data/hces_2022/clean_data","hh_consumption.dta"))
  # hces_ident=read_dta(here("../data/hces_2022/clean_data","hh_ident.dta"))
  # hces_roster=read_dta(here("../data/hces_2022/clean_data","hh_roster.dta"))
  # hces_types=read_dta(here("../data/hces_2022/clean_data","hh_types.dta"))
  
  #names(hces_assets) #Has hhid and district
  #names(hces_distcodes)# Has district
  #names(hces_consumption)
  #names(hces_ident)
  #names(hces_roster)
  #names(hces_types)
  
  hces_assets_cols=c("hhid","sector","state","nss_region","district","stratum","sub_stratum","panel","sub_sample",
                     "fod_subregion","questionaire_no","fsu","mult")
  #hces_assets[,hces_assets_cols]
  #hces_distcodes
  
  # Get hhid merged with district
  assets_merge_cols=c("hhid","state","district")
  distcodes_merge_cols=c("state","district","state_name","district_name")
  hhid_district_merged<-left_join(hces_assets[,hces_assets_cols],
                                  hces_distcodes[,distcodes_merge_cols],
                                  by=c("state","district"))
  # Merge consumption with states and districts
  hces_consumption_district_merged<-inner_join(hces_consumption,
                                               hhid_district_merged,
                                               by="hhid")
  
  #head(hces_consumption_district_merged)
  
  # Merge assets
  # All of the "b4pt343..." variables are about how many of a given asset a household has purchased
  # Skip these for now, but come back to them later
  
  # Merge roster data on 
  #names(hces_roster)
  #hces_roster %>% freq(edu)
  
  # First, filter down the roster data to heads of households
  hces_roster_hh<-hces_roster  %>% filter(rel_head==1)
  
  # Raw data files
  # Level 3: Data on economic activity
  #setwd(file.path(getwd(),"..\\raw_data"))
  
  ## Key columns for raw data
  raw_key_cols=c("fsu","sub_sample","b1q1pt7","b1q1pt10","b1q1pt11","b1q1pt12")
  assets_hhid=hces_assets[,c(raw_key_cols,"hhid")]
  
  # level3_raw<-read_dta(here("../data/hces_2022/raw_data","LEVEL - 03.dta"))
  # level9_raw<-read_dta(here("../data/hces_2022/raw_data","LEVEL - 09 (Section 9 & 10 & 11).dta"))
  # 
  # Merge on household IDs
  level9_hhids=inner_join(level9_raw,
                          assets_hhid,
                          by=raw_key_cols)
  
  level3_hhids=inner_join(level3_raw,
                          assets_hhid,
                          by=raw_key_cols)
  
  #head(level3_raw)
  ## Level3 Variables
  # b4q4pt1: Whether any household member engaged in economic activity in the past yera
  # b4q4pt3: NCO-2015 code (industry classification)
  # pt6: Broad activities from which income was derived (self-employment, regular wage, casual labor)
  # pt7,8,9: Whether earnings in self-employment, regular wage, or casual labor (respectively) were in agriculture
  # pt10: Household type?
  # pt11: Religion of head of household
  # pt12: Social group of head of household
  # pt13: Land ownership identifier
  # pt14: Type of land owned
  # pt15: Total area of land owned, in acres
  # pt16: Does household have dwelling unit
  # pt17: Type of dwelling unit
  # pt18: Material used for dwelling walls
  # pt19: Material used for roof
  # pt20: Material used for floor
  # pt21: Cooking energy source
  # pt22: Lighting energy source
  # pt23: Drinking water source
  # pt25: Type of access to latrine
  # pt26: Type of latrine the household has access to
  
  # Get dwelling characteristics and employment status
  dwelling_chars_emp=level3_hhids %>%
    select(c(hhid,b4q4pt1,b4q4pt3,b4q4pt6,
             b4q4pt13,b4q4pt15,b4q4pt16,b4q4pt17,b4q4pt18,b4q4pt19,b4q4pt20,b4q4pt21,
             b4q4pt22,b4q4pt23,b4q4pt25,b4q4pt26))
  names(dwelling_chars_emp)=c("hhid","empstat","nco2015","emptype",
                              "landown","landacsq","hasdwelling",
                              "dwellingtype","walls","roof","floor",
                              "cooking","lighting","water",
                              "latrineacc","latrinetype")
  #dwelling_chars_emp
  
  ## Level 9 Variables of Interest
  # Get monthly rents
  monthly_rents=level9_hhids[,c("hhid","b9pt1q2","b9pt1q3")] %>%
    filter(b9pt1q2=="400") %>%
    select(c("hhid","b9pt1q3"))
  names(monthly_rents)=c("hhid","mrent")
  # Get monthly water
  monthly_water=level9_hhids[,c("hhid","b9pt1q2","b9pt1q3")] %>%
    filter(b9pt1q2=="404") %>%
    select(c("hhid","b9pt1q3"))
  names(monthly_water)=c("hhid","mwater")
  
  # Get one merged DataFrame with all the variables we need for the regression
  hhid_district_merged<-left_join(hces_assets[,hces_assets_cols],
                                  hces_distcodes[,distcodes_merge_cols],
                                  by=c("state","district"))
  # Merge consumption with states and districts
  hces_merged_full=inner_join(hces_consumption,
                              hhid_district_merged,
                              by="hhid") %>%
    inner_join(.,hces_roster_hh,by="hhid") %>%
    inner_join(.,dwelling_chars_emp,by="hhid") %>%
    inner_join(.,monthly_rents,by="hhid") %>%
    inner_join(.,monthly_water,by="hhid")
  
  # Subset to employed individuals
  hces_merged_emp<-hces_merged_full %>%
    filter(empstat=="1")
  
  #names(hces_merged_emp)
  
  # Try restricting this to districts where we actually have NCEI data
  # This might fix some of the population problems, since the NCEI data only focuses on urban districts
  # It's also consistent with our climate data
  #setwd(file.path(getwd(),"..\\clean_data"))
  #hces_ncei <- read_dta(file.path("hces_2022\\clean_data","HCES2022_NCEI.dta"))
  
  # Get districts in NCEI data
  #hces_ncei %>% summarize(n_districts=n_distinct(district_name))
  
  #hces_merged_emp <- hces_merged_emp %>% filter(district_name %in% hces_ncei$district_name)
  
  # Get male flag and per capita household expenditure variable
  hces_merged_emp<-hces_merged_emp %>%
    mutate(male=if_else(sex==0,1,0),
           logtexp=log(totexp),
           logepc=log(totexp/hhsize))
  return(hces_merged_emp)
}

filter_hces_merged<-function(hces_merged_emp,ss_filter=0){
  # Filter to only keep districts with more than "ssfilter" observations
  district_freq=hces_merged_emp %>% count(district_name)
  selected_dists=(district_freq %>% filter(n>=ss_filter))$district_name
  
  return(hces_merged_emp %>% filter(district_name %in% selected_dists))
}

get_wage_regs_late<-function(hces_merged_emp,survey_design,summary_format="latex"){
  # summary stats of regressors (also including log expenditure per capita)
  wage_reg_vars=c("logepc","age","male","edu","hindu","scstbc")
  stargazer(as.data.frame(hces_merged_emp[,wage_reg_vars]),type=summary_format,summary=T,
            covariate.labels=c("Log Monthly Expenditure Per Capita",
                               "Age","Male","Years of Schooling",
                               "Hindu","Scheduled Caste/Tribe"))
  
  ## Regressions with log totexp - note orthgonal polynomial terms
  # First model: no fixed effects
  # wage_model1=lm(log(totexp) ~ poly(age,2) + male + edu + hindu + scstbc,
  #                data=hces_merged_emp)
  wage_model1 <- svyglm(log(totexp) ~ poly(age,2)+male+edu+hindu+scstbc,
         design = survey_design)
  # Second model: including fixed effects
  # wage_model2=lm(log(totexp) ~ poly(age,2) + male + edu + hindu + scstbc + factor(district_name),
  #                data=hces_merged_emp)
  wage_model2 <- svyglm(log(totexp) ~ poly(age,2)+male+edu+hindu+scstbc+factor(district_name),
                        design = survey_design)
  # Get outputs
  stargazer(wage_model1,wage_model2,
            type=summary_format,
            keep=c("Intercept","age","I(age^2)","male","hs","univ","hindu","scstbc"),
            omit="factor(district_name)davanagere",
            dep.var.labels=c("Log Total Exp.","Log Total Exp."),
            add.lines=list(c("District FE","No","Yes")))
  ## Regressions with log expenditure per capita
  # First model: no fixed effects
  # wage_model1_pc=lm(logepc ~ poly(age,2)+male+edu+hindu+scstbc,
  #                   data=hces_merged_emp)
  wage_model1_pc <- svyglm(logepc ~ poly(age,2)+male+edu+hindu+scstbc,
                           design = survey_design)
  # Second model: including fixed effects
  # wage_model2_pc=lm(logepc ~ poly(age,2)+male+edu+hindu+scstbc+factor(district_name),
  #                   data=hces_merged_emp)
  wage_model2_pc <- svyglm(logepc ~ poly(age,2)+male+edu+hindu+scstbc+factor(district_name),
                           design = survey_design)
  
  stargazer(wage_model1_pc,wage_model2_pc,
            type=summary_format,
            keep=c("Intercept","age","I(age^2)","male","edu","hindu","scstbc"),
            omit="factor(district_name)davanagere",
            add.lines=list(c("District FE","No","Yes")),
            dep.var.labels=c("Log Monthly Exp. PC","Log Monthly Exp. PC"),
            covariate.labels=c("Age","Age Sq.","Male","Years of Schooling","Hindu","Scheduled Caste/Tribe"),
            single.row=T)
  model_list=list(wage_model1,wage_model2,wage_model1_pc,wage_model2_pc)
  names(model_list)=c("Tot. Exp, No FE","Tot. Exp, FE","EPC, No FE","EPC, FE")
  return(model_list)
}

get_prop_signif_fes=function(model){
  # Get number of FEs and proportion which are statistically significant
  return(
    tidy(model) %>%
      filter(str_starts(term,"factor")) %>%
      summarize(prop_signif_05=mean(p.value<0.05,na.rm=T),
                prop_signif_10=mean(p.value<0.1,na.rm=T),
                n=sum(p.value>0,na.rm=T)) %>% unlist()
  )
}

get_sample_size_diagnostics_wage_late=function(hces_merged_emp,filter_sizes){
  # Diagnostic exercise: trade-off between significant FEs and sample size
  filtered_models=lapply(filter_sizes,function(x){
    get_wage_regs(filter_hces_merged(hces_merged_emp,x),summary_format="text")
  })
  sample_size_signif_wage=sapply(filtered_models,function(x){
    get_prop_signif_fes(x[[2]])
  })
  sample_size_signif_wage_pc=sapply(filtered_models,function(x){
    get_prop_signif_fes(x[[4]])
  })
  list_to_return=list(
    "results_wage"=cbind(filter_sizes,t(sample_size_signif_wage)),
    "results_wage_pc"=cbind(filter_sizes,t(sample_size_signif_wage_pc))
  )
  return(list_to_return)
}

### Housing Regressions
get_housing_data_late<-function(hces_merged_emp){
  # Get monthly housing expenditure as sum of rent and water
  # Change hasdwelling, dwellingtype, walls, roof, floor, cooking, lighting, water, latrineacc,latrinetype to numeric
  cols_to_convert=c("hasdwelling","dwellingtype","walls","roof","floor","cooking","lighting",
                    "water","latrineacc","latrinetype")
  hces_merged_emp<-hces_merged_emp %>%
    mutate(across(all_of(cols_to_convert),as.numeric))
  # Subset to households which have dwelling (hasdwelling=="1")
  # Not immediately clear that mrent changes based on ownership or rent? Just leave it be for now
  # (i.e., don't subset to hired dwellings at all)
  # Create a variable for whether walls are made from kutcha (temporary materials) or pucca (permanent materials)
  # Same for roof and floor
  kutcha_cats=seq(1,4)
  pucca_cats=seq(5,9)
  # Create a variable for whether the house uses natural gas/kerosene or electricity for cooking vs. biofuels vs. no cooking at all
  biofuel_cats=c(1,4,6,7,8,9,10)
  non_biofuel_cats=setdiff(seq(1,11),biofuel_cats)
  # Create a variable for whether the house has electric lighting vs. gas lighting vs. no light
  gas_light_cats=c(seq(2,5),6)
  # Create an indicator for whether the house has piped water onto the property
  piped_water_cats=c(2,3,4)
  # Get indicator for whether the house has exclusive use of a latrine
  # For now, make no distinction between the kind of latrine in use
  hces_merged_emp<-hces_merged_emp %>%
    mutate(pucca_walls=if_else(walls %in% pucca_cats,1,0),
           pucca_roof=if_else(roof %in% pucca_cats,1,0),
           pucca_floor=if_else(floor %in% pucca_cats,1,0),
           cooking_fuel=case_when(
             cooking %in% biofuel_cats ~ "Biofuel",
             cooking %in% non_biofuel_cats ~ "Gas/Electric",
             cooking==12 ~ "No Cooking Source"
           ),
           cooking_fuel=factor(x=cooking_fuel,levels=c("No Cooking Source","Biofuel","Gas/Electric"),ordered=F),
           # Dummy vars for summary stats
           cooking_fuel_biofuel=(cooking_fuel=="Biofuel"),
           cooking_fuel_gas_electric=(cooking_fuel=="Gas/Electric"),
           lighting_source=case_when(
             lighting %in% gas_light_cats ~ "Non-Electric",
             lighting==1 ~ "Electric",
             lighting==6 ~ "No Lighting"
           ),
           lighting_source=factor(
             x=lighting_source,levels=c("No Lighting","Non-Electric","Electric"),ordered=F
           ),
           # Dummy vars for summary stats
           lighting_electric=(lighting_source=="Electric"),
           lighting_non_electric=(lighting_source=="Non-Electric"),
           no_lighting=(lighting_source=="No Lighting"),
           piped_water=if_else(water %in% piped_water_cats,1,0),
           own_latrine=if_else(latrineacc==1,1,0),
           housing_cost=mrent+mwater,
           loghc=log(housing_cost),
           loghcpc=log(housing_cost/hhsize) #Per capita housing expenditure
    ) %>%
    filter(cooking_fuel!="No Cooking Source")
  return(hces_merged_emp)
}

get_rent_regs_late <- function(hces_merged_emp,survey_design,summary_format="latex"){
  # Summary stats
  housing_reg_vars=c("loghc","loghcpc","pucca_walls","pucca_floor","pucca_roof",
                     "cooking_fuel_gas_electric",
                     "lighting_electric",
                     "piped_water","own_latrine")
  stargazer(as.data.frame(hces_merged_emp[,housing_reg_vars]),type=summary_format,summary=T,
            covariate.labels=c("Log Housing Costs","Log Housing Costs Per Capita",
                               "Pucca Walls","Pucca Flooring","Pucca Roofing",
                               "Gas/Electric Cooking Fuel",
                               "Electric Lighting",
                               "Piped Water","Exclusive Latrine Access"))
  #stargazer(ftable(hces_merged_emp$cooking_fuel),type="text")
  
  #@ Models without per-capita figures
  # Model 1: No fixed effects
  # rent_model1=lm(loghc ~ pucca_walls+pucca_floor+pucca_roof+
  #                  cooking_fuel+lighting_source+piped_water+own_latrine,
  #                data=hces_merged_emp)
  rent_model1 <- svyglm(loghc ~ pucca_walls+pucca_floor+pucca_roof+cooking_fuel+lighting_source+piped_water+own_latrine,
                        design = survey_design)
  
  # Model 2: With fixed effects
  # rent_model2=lm(loghc ~ pucca_walls+pucca_floor+pucca_roof+
  #                  cooking_fuel+lighting_source+piped_water+own_latrine+factor(district_name),
  #                data=hces_merged_emp)
  rent_model2 <- svyglm(loghc ~ pucca_walls+pucca_floor+pucca_roof+cooking_fuel+lighting_source+piped_water+own_latrine+factor(district_name),
                        design = survey_design)
  
  stargazer(rent_model1,rent_model2,
            keep=c("pucca_walls","pucca_floor","pucca_roof",
                   "cooking_fuelGas/Electric","lighting_sourceElectric",
                   "piped_water","own_latrine"),
            type=summary_format,
            add.lines=list(c("District FE","No","Yes")))
  # Kind of odd that we're not getting many statistically significant attributes
  # Also pucca walls reduces the housing cost? Maybe look into how this cost was calculated
  
  # Models with per-capita figures
  # Model 1: No fixed effects
  # rent_model1_pc=lm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+
  #                     cooking_fuel+piped_water+own_latrine,
  #                   data=hces_merged_emp)
  rent_model1_pc <- svyglm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+cooking_fuel+lighting_source+piped_water+own_latrine,
                           design = survey_design)
  
  
  # Model 2: With fixed effects
  # rent_model2_pc=lm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+
  #                     cooking_fuel+piped_water+own_latrine+factor(district_name),
  #                   data=hces_merged_emp)
  rent_model2_pc <- svyglm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+cooking_fuel+lighting_source+piped_water+own_latrine+factor(district_name),
                           design = survey_design)
  
  stargazer(rent_model1_pc,rent_model2_pc,
            keep=c("pucca_walls","pucca_floor","pucca_roof",
                   "cooking_fuelGas/Electric",
                   "piped_water","own_latrine"),
            type=summary_format,
            add.lines=list(c("District FE","No","Yes")),
            dep.var.labels=c("Log Monthly Housing Exp. PC"),
            covariate.labels=c("Pucca Walls","Pucca Flooring","Pucca Roof",
                               "Gas/Electric Cooking Fuel",
                               "Piped Water","Exclusive Latrine"),
            single.row=T)
  model_list=list(rent_model1,rent_model2,rent_model1_pc,rent_model2_pc)
  names(model_list)=c("Housing Exp, No FE","Housing Exp, FE","Housing PC, No Fe","Housing PC, FE")
  return(model_list)
}

get_sample_size_diagnostics_rent_late=function(hces_merged_emp_housing,filter_sizes){
  # Diagnostic exercise: trade-off between significant FEs and sample size
  filtered_models=lapply(filter_sizes,function(x){
    get_rent_regs(filter_hces_merged(hces_merged_emp_housing,x),summary_format="text")
  })
  sample_size_signif_rent=sapply(filtered_models,function(x){
    get_prop_signif_fes(x[[2]])
  })
  sample_size_signif_rent_pc=sapply(filtered_models,function(x){
    get_prop_signif_fes(x[[4]])
  })
  list_to_return=list(
    "results_rent"=cbind(filter_sizes,t(sample_size_signif_rent)),
    "results_rent_pc"=cbind(filter_sizes,t(sample_size_signif_rent_pc))
  )
  return(list_to_return)
}

### Both Models
get_n_signif_both_models_late=function(hces_merged_emp,hces_survey_design,opt_threshold,pval_thresh){
  # Get the number of fixed effects which are statistically significant in each model, and in both models
  # Also get the wage and rent regressions for a given threshold
  filtered_hces=filter_hces_merged(hces_merged_emp,opt_threshold)
  opt_wage_regs=get_wage_regs_late(filtered_hces,hces_survey_design,summary_format="text")
  opt_rent_regs=get_rent_regs_late(
    get_housing_data_late(
      filtered_hces
    ),hces_survey_design,summary_format="text"
  )
  
  wage_model_diffs=tidy(opt_wage_regs[[4]]) %>%
    filter(str_starts(term,"factor")) %>%
    mutate(signif=if_else(p.value<pval_thresh,"Significant","Not Significant"),
           district_name=str_replace_all(term,"factor\\(district_name\\)","")) %>%
    select(district_name,p.value,signif) %>%
    rename(wage_pval = p.value)
  
  rent_model_diffs=tidy(opt_rent_regs[[4]]) %>%
    filter(str_starts(term,"factor")) %>%
    mutate(signif=if_else(p.value<pval_thresh,"Significant","Not Significant"),
           district_name=str_replace_all(term,"factor\\(district_name\\)","")) %>%
    select(district_name,p.value,signif) %>%
    rename(rent_pval = p.value)
  
  n_wage_signif=sum(wage_model_diffs$wage_pval<pval_thresh)
  n_rent_signif=sum(rent_model_diffs$rent_pval<pval_thresh)
  # How many differentials are significant at some threshold for both models?
  model_diffs_merged=inner_join(
    wage_model_diffs,
    rent_model_diffs,
    by="district_name"
  )
  n_both_signif=model_diffs_merged %>%
    mutate(
      both_signif = ((wage_pval<pval_thresh) & (rent_pval<pval_thresh))
    ) %>%
    summarize(
      both_signif=sum(both_signif)
    ) %>% pull()
  n_signif=c("Wage Signif."=n_wage_signif,
             "Rent signif."=n_rent_signif,
             "Both Signif."=n_both_signif)
  return(list(opt_wage_regs,opt_rent_regs,n_signif))
}

## :::::::::::
## Get De-Meaned Temperature Data
## :::::::::::

fix_utci_celsius=function(utci_daily){
  # Convert UTCI observations to Celsius from Kelvin
  return(utci_daily %>%
           mutate(utci = utci - 273.15))
}

get_adjusted_utci=function(utci_daily){
  # Implement procedure from Jones et al. (2026)
  # Step 1: Define x_{cdmt} as observed UTCI for district c, day d, month m, year t
  # This is already done from the input data
  
  # Step 2: Construct mean observed temperature of each district-month-year
  
  utci_monthly_avgs <- utci_daily %>%
    mutate(
      date = as.Date(date),
      year = year(date),
      month = month(date)
    ) %>%
    group_by(pc11_sd_id, year, month) %>%
    summarize(
      mean_utci = mean(utci, na.rm = TRUE),
      .groups = "drop"
    )
  # Step 3: Estimate linear time trend in mean temperature for each district-month
  district_trends <- utci_monthly_avgs %>%
    group_by(pc11_sd_id, month) %>%
    nest() %>%  # Crushes the data for each district into its own mini-table
    mutate(
      # Run the linear model on each nested data frame
      # 'factor(month)' acts as a dummy control for each month's baseline climate
      model = map(data, ~ lm(mean_utci ~ year, data = .x)),
      
      # Use broom::tidy to convert the model summaries into clean tables
      results = map(model, tidy)
    ) %>%
    # Unwrap the regression results back into a normal data frame
    unnest(results)
  
  # Extract annual trends
  gamma_trends <- district_trends %>%
    filter(term == "year") %>%
    select(pc11_sd_id, month, estimate, std.error, p.value) %>%
    rename(gamma_dm = estimate, p_val = p.value) # Sort by fastest warming districts
  
  # Step 4: Demean actual daily temperature realizations
  utci_daily_prepped <- utci_daily %>%
    mutate(
      date = as.Date(date),
      year = year(date),
      month = month(date)
    )
  
  utci_demeaned <- utci_daily_prepped %>%
    # Match by district and month so each day gets its specific mu and gamma
    left_join(gamma_trends, by = c("pc11_sd_id", "month")) %>%
    mutate(
      # Option 2: Pure Detrending (Removes the climate trend, keeps seasonal baseline)
      utci_detrended = utci - (gamma_dm * (year-1970))
    ) %>%
    # Drop the intermediate regression parameter columns to keep it clean
    select(date, year, month, pc11_sd_id, utci, utci_detrended)
  
  # Step 5: Combine de-trended temperature realizations in each district-month into one distribution
  # Should already have this from the previous step
  
  # Step 6: Project district-month distributions forward using gammas
  
  # Step 7: Aggregate across months within each district-year to get counterfactual distribution,
  # then get the number of days which fall into each bin
  utci_projections <- utci_demeaned %>% 
    left_join(gamma_trends %>% select(pc11_sd_id,month,gamma_dm),
              by=c("pc11_sd_id","month")) %>%
    mutate(
      projected_utci = utci_detrended + (gamma_dm * (year-2007))
    )
  
  utci_projections <- utci_projections %>%
    mutate(
      heat_risk_cat = cut(
        projected_utci,
        breaks = c(-Inf,9,26,32,38,46,Inf),
        labels = c("Cold","Optimal Range","Moderate","Strong","Very Strong","Extreme"),
        right=T
      )
    )
  
  dy_heat_risk_projections=utci_projections %>%
    group_by(pc11_sd_id,year,heat_risk_cat) %>%
    summarize(
      days_count = n(),
      .groups = "drop_last"
    ) %>%
    mutate(
      share = days_count / sum(days_count)
    ) %>%
    ungroup() %>%
    rename(
      proj_days_count = days_count,
      proj_share = share
    )
  
  # Compare to actual observed days in each heat risk category *without* accounting for trends
  
  dy_heat_risk_observed <- utci_daily_prepped %>% 
    mutate(
      heat_risk_cat = cut(
        utci,
        breaks = c(-Inf,9,26,32,38,46,Inf),
        labels = c("Cold","Optimal Range","Moderate","Strong","Very Strong","Extreme"),
        right=T
      )
    ) %>%
    group_by(pc11_sd_id,year,heat_risk_cat) %>%
    summarize(
      days_count = n(),
      .groups = "drop_last"
    ) %>%
    mutate(
      share = days_count / sum(days_count)
    ) %>%
    ungroup() %>%
    rename(
      obs_days_count = days_count,
      obs_share = share
    )
  
  # Merge together
  dy_heat_risk_joined <- inner_join(
    dy_heat_risk_projections,
    dy_heat_risk_observed,
    by=c("pc11_sd_id","year","heat_risk_cat")
  )
  
  # Write out to disc
  # write_csv(dy_heat_risk_joined,
  #           here("../temperature_data/output","district_year_max_utci_risk_cats.csv"))
  
  return(dy_heat_risk_joined)
}

## :::::::::::
## Bootstrap Estimation
## :::::::::::



# Construct a mean of days in each heat risk category
# Over a period of five years for the early and late periods
# For the early period, this is 2007-2011
# For the late period, this is 2018-2022

# DEPRECATED
get_window_average <- function(df,prefix,col_indices){
  foo=df %>% select(starts_with(prefix)) %>%
    select(col_indices)
  return(rowMeans(foo,na.rm=T))
}

# DEPRECATED
get_window_utci=function(utci,col_prefixes,col_indices){
  window_average_cats <- sapply(col_prefixes,get_window_average,df=utci,col_indices=col_indices)
  
  window_utci=data.frame(cbind(window_average_cats)) %>%
    mutate(d_name = utci$d_name)
  return(window_utci)
}

get_utci_proj_obs = function(utci_new){
  # Split UTCI data into pivoted tables for projected (corrected) and observed (uncorrected) data
  # Recode heat_risk_cat and get year suffix
  # Drop days which are cold
  utci_new <- utci_new %>%
    mutate(
      heat_risk_cat = case_when(
        heat_risk_cat == "Optimal Range" ~ "or_max",
        heat_risk_cat == "Moderate" ~ "m_max",
        heat_risk_cat == "Strong" ~ "s_max",
        heat_risk_cat == "Very Strong" ~ "vs_max",
        heat_risk_cat == "Extreme" ~ "ex_max",
        heat_risk_cat == "Cold" ~ NA
      ),
      year_suffix=sprintf("%02d",year %% 100)
    ) %>%
    filter(
      !is.na(heat_risk_cat)
    )
  
  # Pivot wide for both the observed and the corrected data
  utci_pivot_proj <- utci_new %>%
    pivot_wider(
      id_cols = pc11_sd_id,
      names_from = c(heat_risk_cat,year_suffix),
      names_sep = "_",
      values_from = proj_days_count,
      names_expand = T,
      values_fill = 0
    )
  
  utci_pivot_obs <- utci_new %>%
    pivot_wider(
      id_cols = pc11_sd_id,
      names_from = c(heat_risk_cat,year_suffix),
      names_sep = "_",
      values_from = obs_days_count,
      names_expand = T,
      values_fill = 0
    )
  list_to_return=list("utci_pivot_proj" = utci_pivot_proj,
                      "utci_pivot_obs" = utci_pivot_obs)
  return(list_to_return)
}

# Get window UTCI for both projected and observed
get_window_average_redux <- function(df,cols){
  return(rowMeans(df %>% select(all_of(cols)),na.rm=T))
}

get_window_utci_redux <- function(utci,col_prefixes,year_suffixes){
  window_average_cats <- sapply(col_prefixes,function(col){
    get_window_average_redux(utci,
                             sapply(year_suffixes,function(suf){
                               paste0(col,"_",suf)
                             }))
  })
  window_utci <- data.frame(cbind(window_average_cats)) %>%
    mutate(pc11_sd_id = utci$pc11_sd_id)
  return(window_utci)
}


parse_sd_id_instr=function(instr_merged_new){
  instr_merged_new <- instr_merged_new %>%
    mutate(
      pc11_sd_id = paste0(pc11_s_id,"-",pc11_d_id)
    )
}

### Get data for bootstrap
# DEPRECATED
get_bootstrap_data=function(early_window_utci,early_diffs,
                            late_window_utci,late_diffs,
                            utci_merged){
  early_data=inner_join(
    early_window_utci,early_diffs,by=c("d_name" = "district")
  )
  late_data=inner_join(
    late_window_utci,late_diffs,by=c("d_name" = "district")
  )
  joined_diffs=get_joined_differentials()
  #joined_diffs_instr=get_instruments(joined_diffs)
  #migration_costs=get_migration_costs(joined_diffs_instr)
  instr_cols=c("Unsuitable_Slope_Share","Internal_Water_Share","bartik_shock")
  joined_diffs_instr=inner_join(
    joined_diffs,
    utci_merged[,c("d_name",instr_cols)],
    by=c("district_early"="d_name")
  )
  
  joined_data_full=inner_join(joined_diffs_instr,
                              inner_join(
                                early_data[,c("d_name",col_prefixes)] %>%
                                  rename_with(~ paste0(.x,"_early")),
                                late_data[,c("d_name",col_prefixes)] %>%
                                  rename_with(~ paste0(.x,"_late")),
                                by=c("d_name_early"="d_name_late")
                              ),
                              by=c("district_early"="d_name_early"))
  return(joined_data_full)
}

# Get bootstrap data
get_bootstrap_data_redux=function(joined_diffs,utci_early,utci_late, district_controls){
  col_prefixes_redux = c("ex_max","vs_max","s_max","m_max","or_max")
  instr_cols=c("Unsuitable_Slope_Share","Internal_Water_Share","bartik_shock","coast_dist")
  bootstrap_data <- inner_join(joined_diffs,
                               inner_join(
                                 utci_early[,c("d_name",col_prefixes_redux,instr_cols)] %>%
                                   rename_with(~ paste0(.x,"_early")),
                                 utci_late[,c("d_name",col_prefixes_redux,instr_cols)] %>%
                                   rename_with(~ paste0(.x,"_late")),
                                 by=c("d_name_early" = "d_name_late",
                                      "Unsuitable_Slope_Share_early" = "Unsuitable_Slope_Share_late",
                                      "Internal_Water_Share_early" = "Internal_Water_Share_late",
                                      "bartik_shock_early" = "bartik_shock_late",
                                      "coast_dist_early" = "coast_dist_late")
                               ),
                               by=c("district_early" = "d_name_early")) %>%
    rename(
      "Unsuitable_Slope_Share" = "Unsuitable_Slope_Share_early",
      "Internal_Water_Share" = "Internal_Water_Share_early",
      "bartik_shock" = "bartik_shock_early",
      "coast_dist" = "coast_dist_early"
    )
  bootstrap_data <- inner_join(
    bootstrap_data,
    district_controls,
    by=c("district_early" = "district_name")
  )
  return(bootstrap_data)
}

get_district_controls <- function(nss_survey_design, hces_survey_design){
  dist_controls_early <- svyby(
    formula = ~educ_hs + educ_ps + hindu + scst + Age,
    by = ~district_name,
    design = nss_survey_design,
    FUN = svymean,
    na.rm = T
  )
  dist_controls_early_fixed <- dist_controls_early %>% select(
    district_name, hindu, scst, Age, educ_hs, educ_ps
  ) %>% rename(
    educ_hs_early = educ_hs,
    educ_ps_early = educ_ps,
    hindu_early = hindu,
    scst_early = scst,
    Age_early = Age
  ) %>% mutate(
    district_name = str_to_sentence(district_name)
  )
  
  dist_controls_late <- svyby(
    # edu=12 for high school, edu=15 for post-secondary
    formula = ~hindu + scstbc + age + (edu==12) + (edu==15),
    by = ~district_name,
    design = hces_survey_design,
    FUN = svymean,
    na.rm=T
  )
  dist_controls_late_fixed <- dist_controls_late %>% select(
    district_name,hindu, scstbc, age, `edu == 12TRUE`, `edu == 15TRUE`
  ) %>% rename(
    educ_hs_late = `edu == 12TRUE`,
    educ_ps_late = `edu == 15TRUE`,
    Age_late = age,
    hindu_late = hindu,
    scst_late = scstbc
  ) %>% mutate(
    district_name = str_to_sentence(district_name)
  )
  
  dist_controls_full <- inner_join(
    dist_controls_early_fixed,
    dist_controls_late_fixed,
    by="district_name"
  )
  return(dist_controls_full)
}

## Estimating Migration costs
## Migrated (and modified) from migration_cost_reduced_form

get_joined_differentials=function(early_diffs,late_diffs){
  #early_diffs=read_csv(here("../data/differentials","early_period_differentials.csv"))
  #late_diffs=read_csv(here("../data/differentials","late_period_differentials.csv"))
  cols_to_keep=c("district","estimate_wage","estimate_rent","pop","logpop")
  
  early_diffs[,cols_to_keep] %>%
    rename_with(~ paste0(.,"_early"))
  
  joined_diffs=inner_join(
    early_diffs[,cols_to_keep] %>%
      rename_with(~ paste0(.,"_early")),
    late_diffs[,cols_to_keep] %>%
      rename_with(~ paste0(.,"_late")),
    by=c("district_early" = "district_late")
  )
  # Get deltas
  joined_diffs <- joined_diffs %>%
    mutate(
      delta_pop = pop_late-pop_early,
      delta_logpop=logpop_late - logpop_early,
      delta_wage = estimate_wage_late - estimate_wage_early,
      delta_rent = estimate_rent_late - estimate_rent_early,
      delta_netwage = (delta_wage - 0.15*delta_rent) #Assumes 15% of expenditure on housing
    )
  return(joined_diffs)
}

get_joined_diffs_sample_sizes=function(early_diffs,late_diffs){
  # Get sample sizes for early and late diffs, as well as joined diffs
  early_signif <- early_diffs %>% rename(
    both_signif_early = both_signif
  ) %>% select(
    district, both_signif_early
  )
  
  late_signif <- late_diffs %>% rename(
    both_signif_late = both_signif
  ) %>% select(
    district, both_signif_late
  )
  # Join together
  joined_signif <- inner_join(
    early_signif,
    late_signif,
    by = "district"
  ) %>% mutate(
    both_signif_both_periods = both_signif_early & both_signif_late
  )
  
  # Get total sample size and number which are distinct
  early_stats <- data.frame(early_signif %>% summarize(
    n= n_distinct(district),
    n_both_signif = sum(both_signif_early)
  ))
  
  late_stats <- data.frame(late_signif %>% summarize(
    n = n_distinct(district),
    n_both_signif = sum(both_signif_late)
  ))
  
  joined_stats <- data.frame(joined_signif %>% summarize(
    n = n_distinct(district),
    n_both_signif = sum(both_signif_both_periods)
  ))
  
  full_stats <- rbind(early_stats,late_stats,joined_stats)
  rownames(full_stats) <- c("Early","Late","Joined")
  
  return(full_stats)
}

get_joined_diffs_summary_stats=function(joined_diffs,output_format="text"){
  # Summary stats
  migration_cost_cols=c("delta_logpop","delta_wage","delta_rent","delta_netwage")
  stargazer(as.data.frame(joined_diffs[,migration_cost_cols]),type=output_format,summary=T)
}

get_instruments=function(geog, bartik){
  #geog <- read_csv(here("../geodata","district_slope_water_instruments.csv"))
  #bartik <- read_csv(here("../data/bartik","bartik_instruments_ec05_ec13.csv"))
  names(bartik)=c("pc11_state_id","pc11_district_id","bartik_shock")
  geog <- geog %>% rename(coast_dist = NEAR_DIST)
  instr_merged <- inner_join(
    geog[,c("pc11_s_id","pc11_d_id","d_name","Unsuitable_Slope_Share","Internal_Water_Share","coast_dist")],
    bartik[,c("pc11_state_id","pc11_district_id","bartik_shock")],
    by=c("pc11_s_id"="pc11_state_id","pc11_d_id"="pc11_district_id")
  )
  return(instr_merged)
}

merge_utci_instr=function(utci,instr_merged){
  # Join UTCI and instruments and remove duplicated districts
  # For now, just take the maximum across all numeric columns
  inner_join(utci,
             instr_merged,
             by=c("pc11_s_id","pc11_d_id","d_name")) %>%
    group_by(d_name) %>%
    summarize(across(where(is.numeric),\(x) mean(x,na.rm=T)),.groups="drop")
}

merge_utci_instr_redux=function(utci,instr_merged){
  # Join UTCI and instruments and remove duplicated districts
  # For now, just take the maximum across all numeric columns
  inner_join(utci,
             instr_merged,
             by=c("pc11_sd_id")) %>%
    group_by(d_name) %>%
    summarize(across(where(is.numeric),\(x) mean(x,na.rm=T)),.groups="drop")
}

# get_instruments=function(joined_diffs){
#   geog <- read_csv(here("../geodata","district_slope_water_instruments.csv"))
#   bartik <- read_csv(here("../data/bartik","bartik_instruments_ec05_ec13.csv"))
#   names(bartik)=c("pc11_state_id","pc11_district_id","bartik_shock")
#   instr_merged <- inner_join(
#     geog[,c("pc11_s_id","pc11_d_id","d_name","Unsuitable_Slope_Share","Internal_Water_Share")],
#     bartik[,c("pc11_state_id","pc11_district_id","bartik_shock")],
#     by=c("pc11_s_id"="pc11_state_id","pc11_d_id"="pc11_district_id")
#   )
#   
#   joined_diffs<-inner_join(
#     joined_diffs,
#     instr_merged[,c("d_name","Unsuitable_Slope_Share","Internal_Water_Share","bartik_shock")],
#     by=c("district_early" = "d_name")
#   )
#   return(joined_diffs)
# }

get_instr_summary_stats=function(joined_diffs,output_format="text"){
  summary_cols=c("delta_logpop","delta_netwage","Unsuitable_Slope_Share","Internal_Water_Share","bartik_shock")
  stargazer(as.data.frame(joined_diffs[,summary_cols]),summary=T,type=output_format)
}

get_migration_costs=function(joined_diffs,output_format="text",suppress_output=T){
  migration_model1=lm(delta_logpop ~ delta_netwage,
                      data=joined_diffs)
  migration_model2=ivreg(delta_logpop ~ delta_netwage | Unsuitable_Slope_Share + Internal_Water_Share + bartik_shock,
                         data=joined_diffs)
  # if (!suppress_output){
  #   stargazer(migration_model1,migration_model2,type=output_format,
  #             covariate.labels=c("Migration Term","Intercept"),
  #             add.lines=list(c("Instruments","No","Yes")))
  # }
  list_to_return=list("model_no_instr"=migration_model1,
                      "model_instr"=migration_model2)
  return(list_to_return)
}

get_manual_tsls=function(joined_diffs,output_format="text"){
  # Try doing the 2SLS manually and see if that changes anything
  first_stage=lm(delta_netwage ~ Unsuitable_Slope_Share + Internal_Water_Share + bartik_shock,
                 data=joined_diffs)
  stargazer(first_stage,type=output_format,
            covariate.labels=c("Unsuitable Slope","Internal Water","Bartik Shock","Intercept"))
  xhat=predict(first_stage,data=joined_diffs)
  second_stage=lm(joined_diffs$delta_logpop ~ xhat)
  list_to_return=list("first_stage"=first_stage,
                      "second_stage"=second_stage)
  return(list_to_return)
}

## First-stage Bootstrap Estimation
get_first_stage_boot_wts <- function(survey_design, R_1){
  boot_design <- as_bootstrap_design(
    survey_design,
    replicates = R_1,
    type = "Rao-Wu-Yue-Beaumont"
  )
  data_with_boot_weights <- as_data_frame_with_weights(boot_design)
  return(data_with_boot_weights)
}

get_boot_wage_model_early <- function(i,nss_ind_reg_boot_weights){
  options(survey.lonely.psu = "adjust")
  # Run one bootstrap iteration of wage models for early period
  wgt_colname <- as.formula(paste0("~REP_WGT_",i))
  test_svydesign <- svydesign(
    ids = ~FSU_Serial_no,
    strata = ~Stratum,
    weights = wgt_colname,
    data = nss_ind_reg_boot_weights,
    nest = T
  )
  # Run weighted regression
  wage_model_rep <- svyglm(log(totexp) ~ poly(Age,2)+male+educ+hindu+scst+factor(district_name),
                           design = test_svydesign)
  wage_model_rep_pc <- svyglm(logepc ~ poly(Age,2)+male+educ+hindu+scst+factor(district_name),
                              design = test_svydesign)
  
  # Extract coefficients with district names
  wage_model_rep_fes <- tidy(wage_model_rep) %>%
    #filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)"))))
  wage_model_rep_coefs_vec <- wage_model_rep_fes %>% pull(estimate)
  names(wage_model_rep_coefs_vec) <- wage_model_rep_fes %>% pull(district)
  
  wage_model_rep_pc_fes <- tidy(wage_model_rep) %>%
    #filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)"))))
  wage_model_rep_pc_coefs_vec <- wage_model_rep_pc_fes %>% pull(estimate)
  names(wage_model_rep_pc_coefs_vec) <- wage_model_rep_pc_fes %>% pull(district)
  
  list_to_return <- list(
    "wage_design_early" = test_svydesign,
    "wage_early" = wage_model_rep_coefs_vec,
    "wage_pc_early" = wage_model_rep_pc_coefs_vec
  )
  print(paste("Finished with boot iteration",i))
  return(list_to_return)
}

get_boot_rent_model_early <- function(i,hcs_reg_boot_weights){
  options(survey.lonely.psu = "adjust")
  # Run one bootstrap iteration of wage models for early period
  wgt_colname <- as.formula(paste0("~REP_WGT_",i))
  test_svydesign <- svydesign(
    ids = ~FSU,
    strata = ~Stratum,
    weights = wgt_colname,
    data = hcs_reg_boot_weights,
    nest = T
  )
  # Run weighted regression
  rent_model_rep <- svyglm(loghc ~ pucca_walls+pucca_floor+pucca_roof+piped_water+own_latrine+elec+factor(district_name),
                           design = test_svydesign)
  rent_model_rep_pc <- svyglm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+piped_water+own_latrine+elec+factor(district_name),
                              design = test_svydesign)
  
  # Extract coefficients with district names
  rent_model_rep_fes <- tidy(rent_model_rep) %>%
    #filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)"))))
  rent_model_rep_coefs_vec <- rent_model_rep_fes %>% pull(estimate)
  names(rent_model_rep_coefs_vec) <- rent_model_rep_fes %>% pull(district)
  
  rent_model_rep_pc_fes <- tidy(rent_model_rep) %>%
    #filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)"))))
  rent_model_rep_pc_coefs_vec <- rent_model_rep_pc_fes %>% pull(estimate)
  names(rent_model_rep_pc_coefs_vec) <- rent_model_rep_pc_fes %>% pull(district)
  
  list_to_return <- list(
    "rent_design_early" = test_svydesign,
    "rent_early" = rent_model_rep_coefs_vec,
    "rent_pc_early" = rent_model_rep_pc_coefs_vec
  )
  print(paste("Finished with boot iteration",i))
  return(list_to_return)
}

get_boot_wage_model_late <- function(i,hces_merged_emp_housing_boot_weights){
  options(survey.lonely.psu = "adjust")
  wgt_colname <- as.formula(paste0("~REP_WGT_",i))
  test_svydesign <- svydesign(
    ids = ~fsu,
    strata = ~interaction(stratum, sub_stratum),
    weights = wgt_colname,
    data = hces_merged_emp_housing_boot_weights,
    nest = T
  )
  
  # Run weighted regression
  wage_model_rep <-svyglm(log(totexp) ~ poly(age,2)+male+edu+hindu+scstbc+factor(district_name),
                          design = test_svydesign)
  
  wage_model_pc_rep <- svyglm(logepc ~ poly(age,2)+male+edu+hindu+scstbc+factor(district_name),
                              design = test_svydesign)
  
  # Extract coefficients with district names
  wage_model_rep_fes <- tidy(wage_model_rep) %>%
    #filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)"))))
  wage_model_rep_coefs_vec <- wage_model_rep_fes %>% pull(estimate)
  names(wage_model_rep_coefs_vec) <- wage_model_rep_fes %>% pull(district)
  
  wage_model_rep_pc_fes <- tidy(wage_model_rep) %>%
    #filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)"))))
  wage_model_rep_pc_coefs_vec <- wage_model_rep_pc_fes %>% pull(estimate)
  names(wage_model_rep_pc_coefs_vec) <- wage_model_rep_pc_fes %>% pull(district)
  
  list_to_return <- list(
    "wage_design_late" = test_svydesign,
    "wage_late" = wage_model_rep_coefs_vec,
    "wage_pc_late" = wage_model_rep_pc_coefs_vec
  )
  print(paste("Finished with boot iteration",i))
  return(list_to_return)
}

get_boot_rent_model_late <- function(i,hces_merged_emp_housing_boot_weights){
  options(survey.lonely.psu = "adjust")
  wgt_colname <- as.formula(paste0("~REP_WGT_",i))
  test_svydesign <- svydesign(
    ids = ~fsu,
    strata = ~interaction(stratum, sub_stratum),
    weights = wgt_colname,
    data = hces_merged_emp_housing_boot_weights,
    nest = T
  )
  
  # Run weighted regression
  rent_model_rep <-svyglm(loghc ~ pucca_walls+pucca_floor+pucca_roof+cooking_fuel+lighting_source+piped_water+own_latrine+factor(district_name),
                          design = test_svydesign)
  
  rent_model_pc_rep <- svyglm(loghcpc ~ pucca_walls+pucca_floor+pucca_roof+cooking_fuel+lighting_source+piped_water+own_latrine+factor(district_name),
                              design = test_svydesign)
  
  # Extract coefficients with district names
  rent_model_rep_fes <- tidy(rent_model_rep) %>%
    #filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)"))))
  rent_model_rep_coefs_vec <- rent_model_rep_fes %>% pull(estimate)
  names(rent_model_rep_coefs_vec) <- rent_model_rep_fes %>% pull(district)
  
  rent_model_rep_pc_fes <- tidy(rent_model_rep) %>%
    #filter(str_detect(term, "district_name")) %>%
    mutate(district = str_to_title(str_remove(term, fixed("factor(district_name)"))))
  rent_model_rep_pc_coefs_vec <- rent_model_rep_pc_fes %>% pull(estimate)
  names(rent_model_rep_pc_coefs_vec) <- rent_model_rep_pc_fes %>% pull(district)
  
  list_to_return <- list(
    "rent_design_late" = test_svydesign,
    "rent_late" = rent_model_rep_coefs_vec,
    "rent_pc_late" = rent_model_rep_pc_coefs_vec
  )
  print(paste("Finished with boot iteration",i))
  return(list_to_return)
}

get_boot_reg_outputs <- function(wage_reg_boot_outputs, model_name, alpha){
  # Helper function to get a clean data frame with bootstrapped wage differentials
  # Concatenate
  # wage_early_fes <- wage_reg_boot_outputs[model_name,] %>%
  #   map(~ as.data.frame(as.list(.))) %>%
  #   bind_rows() %>%
  #   t()
  wage_early_fes <- wage_reg_boot_outputs %>% 
    purrr::map(~ .x[[model_name]]) %>%
    do.call(rbind, .) %>%
    t()
  # Get bootstrap means and CIs
  wage_early_boot_means <- apply(wage_early_fes,1,mean, na.rm=T)
  # Get (1-alpha) symmetric confidence interval
  wage_early_boot_cis <- apply(wage_early_fes, 1, quantile, probs=c((alpha/2), (1-(alpha/2))), na.rm=T)
  # Check statistical significance for p=0.05
  wage_early_boot_signif <- apply(wage_early_boot_cis,2,function(x){
    return(x[1]*x[2]>0)
  })
  # Combine outputs
  wage_early_boot_diffs_outputs <- cbind(wage_early_boot_means,t(wage_early_boot_cis), wage_early_boot_signif)
  colnames(wage_early_boot_diffs_outputs) <- c("diff_mean","diff_ci_lower","diff_ci_upper","signif")
  # Return raw coefficients and means with CIs
  return(list(
    "coef" = wage_early_fes,
    "mean_ci_signif" = wage_early_boot_diffs_outputs
  )
  )
}

get_boot_reg_outputs_vec <- function(wage_reg_boot_outputs_vec, alpha){
  # Redesigned getter function to work with vectors from split model
  wage_early_fes <- (as.matrix(wage_reg_boot_outputs_vec))
  # Get bootstrap means and CIs
  wage_early_boot_means <- apply(wage_early_fes,1,mean, na.rm=T)
  # Get (1-alpha) symmetric confidence interval
  wage_early_boot_cis <- apply(wage_early_fes, 1, quantile, probs=c((alpha/2), (1-(alpha/2))), na.rm=T)
  # Check statistical significance for p=0.05
  wage_early_boot_signif <- apply(wage_early_boot_cis,2,function(x){
    return(x[1]*x[2]>0)
  })
  # Combine outputs
  wage_early_boot_diffs_outputs <- cbind(wage_early_boot_means,t(wage_early_boot_cis), wage_early_boot_signif)
  colnames(wage_early_boot_diffs_outputs) <- c("diff_mean","diff_ci_lower","diff_ci_upper","signif")
  # Return raw coefficients and means with CIs
  return(list(
    "coef" = wage_early_fes,
    "mean_ci_signif" = wage_early_boot_diffs_outputs
  )
  )
}

get_combined_iter_coef <- function(wage_coefs, rent_coefs, wage_controls_to_drop, rent_controls_to_drop){
  # Get combined wage and rent differentials for one period, for one iteration
  wage_diffs <- wage_coefs[-wage_controls_to_drop,]
  rent_diffs <- rent_coefs[-rent_controls_to_drop,]
  # Convert to data frames and merge
  wage_diffs_to_merge <- data.frame(
    district = names(wage_diffs),
    estimate_wage = as.matrix(wage_diffs),
    row.names = NULL
  )
  rent_diffs_to_merge <- data.frame(
    district = names(rent_diffs),
    estimate_rent = as.matrix(rent_diffs),
    row.names = NULL
  )
  
  combined_late_diffs <- inner_join(
    wage_diffs_to_merge,
    rent_diffs_to_merge,
    by="district"
  )
  return(combined_late_diffs)
}

## Run Second-stage Bootstrap Estimation

get_mwtp_single_temp_var=function(data,indices,temp_var,housing_exp_share){
  d <- data[indices,]
  wage_formula_early=as.formula(paste("estimate_wage_early ~ hindu_early + scst_early + Age_early + educ_hs_early + educ_ps_early + ",paste0(temp_var,"_early")))
  rent_formula_early=as.formula(paste("estimate_rent_early ~ hindu_early + scst_early + Age_early + educ_hs_early + educ_ps_early + ",paste0(temp_var,"_early")))
  wage_formula_late=as.formula(paste("estimate_wage_late ~ hindu_late + scst_late + Age_late + educ_hs_late + educ_ps_late + ",paste0(temp_var,"_late")))
  rent_formula_late=as.formula(paste("estimate_rent_late ~ hindu_late + scst_late + Age_late + educ_hs_late + educ_ps_late + ",paste0(temp_var,"_late")))
  pop_formula=as.formula(paste("logpop_late ~ ",paste0(temp_var,"_late")))
  # Models for early period
  model_wage_early=lm(wage_formula_early,data=d)
  model_rent_early=lm(rent_formula_early,data=d)
  # Models for late period
  model_wage_late=lm(wage_formula_late,data=d)
  model_rent_late=lm(rent_formula_late,data=d)
  # Models for late population
  model_pop_late=lm(pop_formula,data=d)
  # Model for migration costs
  migration_costs_models=get_migration_costs(d,suppress_output=F)
  migration_cost_term=coef(migration_costs_models[[2]])["delta_netwage"]
  # Calculate MWTP
  # Index of temperature var is 7
  mwtp_early_unadj=(housing_exp_share*exp(coef(model_rent_early)[7]))-exp(coef(model_wage_early))[7]
  mwtp_late_unadj=(housing_exp_share*exp(coef(model_rent_late)[7]))-exp(coef(model_wage_late))[7]
  correction_term=(1/migration_cost_term)*coef(model_pop_late)[2]
  mwtp_late_adj=mwtp_late_unadj+correction_term
  # Edit: also include explicit parameters beyond just MWTP
  vec_to_return=c(mwtp_early_unadj,mwtp_late_unadj,mwtp_late_adj,
                  coef(model_wage_early)[7], coef(model_wage_late)[7],
                  coef(model_rent_early)[7], coef(model_rent_late)[7],
                  coef(model_pop_late)[7], migration_cost_term)
  names(vec_to_return)=c("mwtp_early_unadj","mwtp_late_unadj","mwtp_late_adj",
                         "wage_elasticity_early","wage_elasticity_late",
                         "rent_elasticity_early","rent_elasticity_late",
                         "pop_elasticity_late","migration_cost_param")
  return(vec_to_return)
}

get_mwtp_all_temp_vars=function(data,indices,temp_vars,housing_exp_share){
  return(
    sapply(temp_vars,function(x){
      get_mwtp_single_temp_var(data,indices,x,housing_exp_share)
    })
  )
}

## Perform bootstrap estimation
run_bootstrap_estimation=function(joined_data_full,seed,R,max_temp_hr_cols,housing_exp_share){
  set.seed(seed)
  boot_results <- boot(
    data=joined_data_full,
    statistic=get_mwtp_all_temp_vars,
    R=R,
    temp_var=max_temp_hr_cols,
    housing_exp_share=housing_exp_share
  )
  return(boot_results)
}


## Parse bootstrap outputs

# DEPRECATED
parse_bootstrap_outputs=function(boot_results){
  boot_estimates=boot_results$t0
  rownames(boot_estimates)<- c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs",
                               "Early Wage Elasticity", "Late Wage Elasticity",
                               "Early Rent Elasticity", "Late Rent Elasticity",
                               "Late Pop Elasticity", "Migration Cost Parameter")
  colnames(boot_estimates)<- c("Extreme","Very Strong","Strong","Moderate","Optimal Range")
  return(boot_estimates)
}

# DEPRECATED
get_boot_ci_bounds=function(boot_results){
  # ci_lower <- matrix(NA,nrow=n_row,ncol=n_col)
  # ci_upper <- matrix(NA,nrow=n_row,ncol=n_col)
  # 
  # index_counter <- 1
  # for (i in 1:n_row){
  #   for (j in 1:n_col){
  #     ci_out <- boot.ci(boot_results,type="perc",index=index_counter)
  #     
  #     ci_lower[i,j]<- ci_out$percent[4]
  #     ci_upper[i,j]<- ci_out$percent[5]
  #     index_counter <- index_counter + 1
  #   }
  # }
  ci_lower <- matrix(apply(boot_results$t, 2, quantile, probs=0.025, na.rm=T), nrow=9, ncol=5)
  ci_upper <- matrix(apply(boot_results$t, 2, quantile, probs=0.975, na.rm=T), nrow=9, ncol=5)
  rownames(ci_lower)<- c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs",
                         "Early Wage Elasticity", "Late Wage Elasticity",
                         "Early Rent Elasticity", "Late Rent Elasticity",
                         "Late Pop Elasticity", "Migration Cost Parameter")
  rownames(ci_upper)<- c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs",
                         "Early Wage Elasticity", "Late Wage Elasticity",
                         "Early Rent Elasticity", "Late Rent Elasticity",
                         "Late Pop Elasticity", "Migration Cost Parameter")
  colnames(ci_lower)<- c("Extreme","Very Strong","Strong","Moderate","Optimal Range")
  colnames(ci_upper)<- c("Extreme","Very Strong","Strong","Moderate","Optimal Range")
  list_to_return=list(
    "ci_lower"=ci_lower,
    "ci_upper"=ci_upper
  )
  return(list_to_return)
}

get_boot_means_ci_bounds=function(boot_results_full){
  # ci_lower <- matrix(NA,nrow=n_row,ncol=n_col)
  # ci_upper <- matrix(NA,nrow=n_row,ncol=n_col)
  # 
  # index_counter <- 1
  # for (i in 1:n_row){
  #   for (j in 1:n_col){
  #     ci_out <- boot.ci(boot_results,type="perc",index=index_counter)
  #     
  #     ci_lower[i,j]<- ci_out$percent[4]
  #     ci_upper[i,j]<- ci_out$percent[5]
  #     index_counter <- index_counter + 1
  #   }
  # }
  means <- matrix(apply(boot_results_full, 2, mean, na.rm = T), nrow = 9, ncol = 5)
  ci_lower <- matrix(apply(boot_results_full, 2, quantile, probs=0.025, na.rm=T), nrow=9, ncol=5)
  ci_upper <- matrix(apply(boot_results_full, 2, quantile, probs=0.975, na.rm=T), nrow=9, ncol=5)
  rownames(means) <- c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs",
                       "Early Wage Elasticity", "Late Wage Elasticity",
                       "Early Rent Elasticity", "Late Rent Elasticity",
                       "Late Pop Elasticity", "Migration Cost Parameter")
  rownames(ci_lower)<- c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs",
                         "Early Wage Elasticity", "Late Wage Elasticity",
                         "Early Rent Elasticity", "Late Rent Elasticity",
                         "Late Pop Elasticity", "Migration Cost Parameter")
  rownames(ci_upper)<- c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs",
                         "Early Wage Elasticity", "Late Wage Elasticity",
                         "Early Rent Elasticity", "Late Rent Elasticity",
                         "Late Pop Elasticity", "Migration Cost Parameter")
  colnames(means) <- c("Extreme","Very Strong","Strong","Moderate","Optimal Range")
  colnames(ci_lower)<- c("Extreme","Very Strong","Strong","Moderate","Optimal Range")
  colnames(ci_upper)<- c("Extreme","Very Strong","Strong","Moderate","Optimal Range")
  list_to_return=list(
    "means" = means,
    "ci_lower"=ci_lower,
    "ci_upper"=ci_upper
  )
  return(list_to_return)
}

## Plot bootstrapped estimators
# Plot results
# 1. Convert matrices to dataframes and add a Row identifier
prep_matrix <- function(mat, value_name) {
  as.data.frame(mat) %>%
    mutate(Row = rownames(mat)) %>%
    pivot_longer(cols = -Row, names_to = "Column", values_to = value_name)
}

# 2. Merge them all together into one clean plotting dataset
get_plot_data_boot=function(boot_estimates,ci_list){
  plot_data <- prep_matrix(boot_estimates, "Estimate") %>%
    left_join(prep_matrix(ci_list$ci_lower, "Lower"), by = c("Row", "Column")) %>%
    left_join(prep_matrix(ci_list$ci_upper, "Upper"), by = c("Row", "Column")) %>%
    mutate(
      Column=factor(Column,levels=c("Optimal Range","Moderate","Strong","Very Strong","Extreme"),ordered=T),
      Row=factor(Row,levels=c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs"),ordered=T)
    )
  return(plot_data)
}

# Get combined plot data which compares observed and projected temperatures side-by-side
# Three tables: one for early MWTP, another for late MWTP, and a third for late MWTP w/ Mig. Costs
get_combined_plot_data <- function(plot_data_proj,plot_data_obs){
  est_params=c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs")
  list_to_return=lapply(est_params,function(p){
    rbind(plot_data_proj %>% filter(Row==p) %>%
            mutate(
              Row = "W/ Temp. Corr."
            ),
          plot_data_obs %>% filter(Row==p) %>%
            mutate(Row = "W/O Temp. Corr.")
    )})
  return(list_to_return)
}

# Define a dodging width so the bars sit side-by-side instead of overlapping
get_bootstrap_plot=function(plot_data,temp_data="Unadjusted"){
  dodge_width <- 0.6
  plot_data_filtered <- plot_data %>% filter(Row %in% c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs"))
  # Set color palette
  custom_color_palette <- c(
    "Early MWTP, Unadj." = "#bf4d40",
    "Late MWTP, Unadj." = "#40bf4d",
    "Late MWTP w/ Mig. Costs" = "#4d40bf"
  )
  bootstrap_ci_plot=ggplot(plot_data_filtered, aes(x = Column, y = Estimate, color = Row, group = Row)) +
    # Draw the 95% CI error bars
    geom_errorbar(
      aes(ymin = Lower, ymax = Upper), 
      width = 0.2, 
      position = position_dodge(width = dodge_width),
      linewidth = 0.8
    ) +
    # Draw the point estimates on top of the bars
    geom_point(
      position = position_dodge(width = dodge_width), 
      size = 2.0
    ) +
    geom_hline(aes(yintercept=0),linetype="dashed",color="gray")+
    labs(
      title = paste("Bootstrap Estimates and 95% Confidence Intervals,",temp_data, "Temps"),
      x = "Model Columns",
      y = "Estimated Parameter Value",
      color = "Parameter"
    ) +
    theme(
      legend.position = "right",
      panel.grid.major.x = element_blank() # Removes vertical grid lines to separate columns cleanly
    ) +
    scale_color_manual(values = custom_color_palette)
  ggsave(here("../vis/",paste0("mwtp_bootstrap_estimates_",temp_data,".png")),
         bootstrap_ci_plot,
         width=8,height=6,unit="in")
  return(bootstrap_ci_plot)
}

# Get plots of the same parameter, looking at observed and corrected side-by-side
get_obs_corrected_plot=function(plot_data,parameter="Early"){
  dodge_width <- 0.6
  #plot_data_filtered <- plot_data %>% filter(Row %in% c("Early MWTP, Unadj.","Late MWTP, Unadj.","Late MWTP w/ Mig. Costs"))
  custom_color_palette <- c(
    "W/ Temp. Corr." = "#d42b8e",
    "W/O Temp. Corr." = "#2bd471"
  )
  bootstrap_ci_plot=ggplot(plot_data, aes(x = Column, y = Estimate, color = Row, group = Row)) +
    # Draw the 95% CI error bars
    geom_errorbar(
      aes(ymin = Lower, ymax = Upper), 
      width = 0.2, 
      position = position_dodge(width = dodge_width),
      linewidth = 0.8
    ) +
    # Draw the point estimates on top of the bars
    geom_point(
      position = position_dodge(width = dodge_width), 
      size = 2.0
    ) +
    geom_hline(aes(yintercept=0),linetype="dashed",color="gray")+
    labs(
      title = paste("Observed vs. Corrected Data Estimates and CIs,", parameter, "MWTP"),
      x = "Model Columns",
      y = "Estimated Parameter Value",
      color = "Parameter"
    ) +
    theme(
      legend.position = "right",
      panel.grid.major.x = element_blank() # Removes vertical grid lines to separate columns cleanly
    )+
    scale_color_manual(values = custom_color_palette)
  ggsave(here("../vis/",paste0("uncorrected_corrected_estimates_",parameter,".png")),
         bootstrap_ci_plot,
         width=8,height=6,unit="in")
  return(bootstrap_ci_plot)
}

## :::::::::::
## Writing Log Files
## :::::::::::

get_stargazer_summary_table = function(summary_table,summary_table_options,output_type="text"){
  # Helper function to pre-allocate stargazer output and save it as a distinct target
  base_args <- list(as.data.frame(summary_table),type=output_type,summary=T)
  custom_args <- summary_table_options
  final_args <- c(base_args, custom_args)
  table_output <- do.call(stargazer,final_args)
  return(table_output)
}

get_stargazer_regression_table = function(regression_models,regression_output_options,output_type="text"){
  # Helper function to pre-allocate stargazer outputs and save as distinct targets
  base_args <- list(regression_models, type = output_type)
  custom_args <- regression_output_options
  final_args <- c(base_args, custom_args)
  table_output <- do.call(stargazer, final_args)
  return(table_output)
}

get_stargazer_other_table = function(other_outputs,other_output_options,output_type="text"){
  base_args <- list(other_outputs, type = output_type)
  custom_args <- other_output_options
  final_args <- c(base_args, custom_args)
  table_output <- do.call(stargazer, final_args)
  return(table_output)
}

write_log_files_redux = function(summary_tables_text, summary_tables_latex,
                                 regression_outputs_text, regression_outputs_latex,
                                 other_tables_text, other_tables_latex,
                                 text_path, latex_path){
  text_path <- as.character(text_path)
  latex_path <- as.character(latex_path)
  
  # Text log header
  write("==================================================\n", file = text_path, append = FALSE)
  write(paste("PIPELINE LOG GENERATED ON:", Sys.time(), "\n"), file = text_path, append = TRUE)
  write("==================================================\n\n", file = text_path, append = TRUE)
  
  # LaTeX log header
  write(paste("% LaTeX Log Generated on:", Sys.time(), "\n\n"), file = latex_path, append = FALSE)
  
  # Write out logs
  for(summary_table_name in names(summary_tables_text)){
    # Write out text
    write(paste("\n---", summary_table_name, "Summary Statistics", "---\n"), file = text_path, append = TRUE)
    write(summary_tables_text[[summary_table_name]], file = text_path, sep = "\n", append = T)
    # Write out latex
    write(paste("\n% ---", summary_table_name, "Summary Statistics", "---\n"), file = latex_path, append = TRUE)
    write(summary_tables_latex[[summary_table_name]], file = latex_path, sep = "\n", append = T)
  }
  for(regression_output_name in names(regression_outputs_text)){
    # Write out text
    write(paste("\n---", regression_output_name, "Regression Outputs", "---\n"), file = text_path, append = TRUE)
    write(regression_outputs_text[[regression_output_name]], file = text_path, sep = "\n", append = T)
    # Write out latex
    write(paste("\n% ---", regression_output_name, "Regression Outputs", "---\n"), file = latex_path, append = TRUE)
    write(regression_outputs_latex[[regression_output_name]], file = latex_path, sep = "\n", append = T)
  }
  for(other_table_name in names(other_tables_text)){
    # Write out text
    write(paste("\n---", other_table_name, "Other Table", "---\n"), file = text_path, append = TRUE)
    write(other_tables_text[[other_table_name]], file = text_path, sep = "\n", append = T)
    # Write out latex
    write(paste("\n% ---", other_table_name, "Other Table", "---\n"), file = latex_path, append = TRUE)
    write(other_tables_latex[[other_table_name]], file = latex_path, sep = "\n", append = T)
  }
  return(c(text_path, latex_path))
  
}

# DEPRECATED
write_log_files=function(sumtl, sumto, regml, regmo, othtl, othto,
                         text_path, latex_path){
  
  text_path <- as.character(text_path)
  latex_path <- as.character(latex_path)
  
  write("==================================================\n", file = text_path, append = FALSE)
  write(paste("PIPELINE LOG GENERATED ON:", Sys.time(), "\n"), file = text_path, append = TRUE)
  write("==================================================\n\n", file = text_path, append = TRUE)
  
  # Summary Tables
  for (sumtn in names(sumtl)){
    write(paste("\n---", sumtn, "Summary Statistics", "---\n"), file = text_path, append = TRUE)
    base_args <- list(as.data.frame(sumtl[[sumtn]]), type = "text", summary = T)
    custom_args <- sumto[[sumtn]]
    final_args <- c(base_args, custom_args)
    # table_output <- stargazer(as.data.frame(summary_tables_list[[summary_table_name]]), type = "text",
    #           summary = T)
    table_output <- do.call(stargazer,final_args)
    write(table_output, file = text_path, sep = "\n", append = T)
  }
  
  # Regression Tables
  for (regmn in names(regml)){
    write(paste("\n---", regmn, "Regression Outputs", "---\n"), file = text_path, append = TRUE)
    base_args <- list(regml[[regmn]], type = "text")
    custom_args <- regmo[[regmn]]
    final_args <- c(base_args, custom_args)
    # table_output <- stargazer(as.data.frame(summary_tables_list[[summary_table_name]]), type = "text",
    #           summary = T)
    table_output <- do.call(stargazer,final_args)
    write(table_output, file = text_path, sep = "\n", append = T)
  }
  
  # Other tables
  for(othtn in names(othtl)){
    write(paste("\n---", othtn, "Other Tables", "---\n"), file = text_path, append = TRUE)
    base_args <- list(othtl[[othtn]], type = "text")
    custom_args <- othto[[othtn]]
    final_args <- c(base_args, custom_args)
    # table_output <- stargazer(as.data.frame(summary_tables_list[[summary_table_name]]), type = "text",
    #           summary = T)
    table_output <- do.call(stargazer,final_args)
    write(table_output, file = text_path, sep = "\n", append = T)
  }
  
  write(paste("% LaTeX Log Generated on:", Sys.time(), "\n\n"), file = latex_path, append = FALSE)
  
  for (sumtn in names(sumtl)) {
    write(paste("\n% ---", sumtn, "Summary Statistics", "---\n"), file = latex_path, append = TRUE)
    base_args <- list(as.data.frame(sumtl[[sumtn]]), type = "latex", summary = T)
    custom_args <- sumto[[sumtn]]
    final_args <- c(base_args, custom_args)
    
    # table_output <- stargazer(as.data.frame(summary_tables_list[[summary_table_name]]), type = "text",
    #           summary = T)
    table_output <- do.call(stargazer,final_args)
    write(table_output, file = latex_path, sep = "\n", append = T)
  }
  
  # Regression Tables
  for (regmn in names(regml)){
    write(paste("\n% ---", regmn, "Regression Outputs", "---\n"), file = latex_path, append = TRUE)
    base_args <- list(regml[[regmn]], type = "latex")
    custom_args <- regmo[[regmn]]
    final_args <- c(base_args, custom_args)
    # table_output <- stargazer(as.data.frame(summary_tables_list[[summary_table_name]]), type = "text",
    #           summary = T)
    table_output <- do.call(stargazer,final_args)
    write(table_output, file = latex_path, sep = "\n", append = T)
  }
  
  # Other tables
  for (othtn in names(othtl)){
    write(paste("\n% ---", othtn, "Other Tables", "---\n"), file = latex_path, append = TRUE)
    base_args <- list(othtl[[othtn]], type = "latex")
    custom_args <- othto[[othtn]]
    final_args <- c(base_args, custom_args)
    # table_output <- stargazer(as.data.frame(summary_tables_list[[summary_table_name]]), type = "text",
    #           summary = T)
    table_output <- do.call(stargazer,final_args)
    write(table_output, file = latex_path, sep = "\n", append = T)
  }
  
  return(c(text_path, latex_path))
}