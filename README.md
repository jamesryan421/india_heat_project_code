# Overview
This repository contains data cleaning and estimation code for the paper "Heat as a Disamenity in Urban India: A Rosen-Roback Spatial Equilibrium Approach" by Tanmay Devi and James Ryan. This is very much still a work in progress, and primarily contains code just for estimation at this time; we will expand this  out later as needed.

# Estimation Code Setup

To run the estimation code on your machine, do the following:

1. Set up directories for `data/`, `geodata/`, `temperature_data/`, and `vis/` acording to the schema outlined below. Ensure that these are all in the same main directory.
2. In the file `pipeline_config.R`, set the variable for `main_data_path` from the previous step. The pipeline should execute relative to this directory.


```text
--- File Schema for Code ---
code/
├─ _targets.R
├─ pipeline_config.R
├─ sub_pipelines/
|  ├─ pipeline_01_pre_split.R

--- File Schema for Data ---
data/
├─ nss/
│  ├─ RawData/
│  │  ├─ hcs_block_3.dta
│  │  ├─ hcs_block_4.dta
│  │  ├─ hcs_block_6.dta
│  ├─ CleanData/
│  │  ├─ IND_2011_v2.dta
├─ hces_2022/
│  ├─ clean_data/
│  │  ├─ hh_types.dta
│  │  ├─ hh_roster.dta
│  │  ├─ hh_ident.dta
│  │  ├─ hh_consumption.dta
│  │  ├─ hh_assets.dta
│  │  ├─ HCES2022_distcodes.dta
│  ├─ raw_data/
│  │  ├─ LEVEL - 03.dta
│  │  ├─ LEVEL - 09 (Section 9 & 10 & 11).dta
├─ population_projections/
│  ├─ india_pop_worldpop.csv
├─ bartik/
│  ├─ bartik_instruments_ec05_ec13.csv
geodata/
├─ district_slope_water_instruments.csv
temperature_data/
├─ output /
|  ├─ district_daily_max_utci_full.csv.gz
|  ├─ district_daytime_mean_utci_full.csv.gz
|  ├─ district_daytime_hottest_mean_utci_full.csv.gz
├─ vis /

```
