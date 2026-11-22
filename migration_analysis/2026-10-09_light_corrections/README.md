# Light corrections to the estimation pipeline (9 October 2026)

Corrections to github.com/jamesryan421/india_heat_project_code at commit 322b110 ("Subsetting to formal wages and housing"). Nothing was pushed to GitHub.

## Files

- `light_corrections_2026-10-09.patch`: all code changes. From the repo root at 322b110, run `git apply light_corrections_2026-10-09.patch`.
- `record_of_changes.pdf`: every change, with old lines in black and new lines in red, and the before and after estimates.
- `nss68_head_characteristics.csv`: new input. Put it in `data/nss/CleanData/`. It holds the household head's age, sex and years of schooling from the NSS 68 Type 1 roster (`Data/RawData/NSS/NSSO_68_T1_2011-12/CleanData/hh_roster.dta`, `rel_head == 1`).
- `make_head_characteristics.R`: the script that builds that file.
- `results/`: output of the corrected run on NOTS (R_1 = 100, R_2 = 25). It holds the MWTP plots, regression logs and the per-band MWTP table, for the main specification and the regular-wage-household variant.

## Settings added to pipeline_config.R

- `housing_exp_share = 0.20`: HCES 2022-23 urban tenants spend 0.20 of total expenditure on rent.
- `worker_sample`: `"all"` (default) or `"regular_wage"`.
- `rent_min_n = 10`: minimum number of tenants per district for a rent fixed effect.
- `late_pop_col`: `"pop_23_proj"` (default) or `"pop_23_wp"`.

## Running

The run is unchanged: `tar_make()`. stargazer 5.2.3 still needs the length-check patch in `stargazer_fix.R` under R 4.2 or later.

## Other folders

- `nots/`: the environment, the job scripts and the extraction script used on NOTS. The run directory there is `/scratch/td52/heat_light_2026-10-09`.
- `results/robustness_point_estimates.txt`: point estimates with robust standard errors for the main specification and five variants (regular-wage households, no tenant minimum, WorldPop population, housing share 0.15, early rent without size controls). They were produced by `results/robustness_harness.R`, which calls the corrected pipeline functions with full-sample weights.
- `root_cause/`: why the corrected hedonic estimates have the wrong sign. Scripts and output behind Table 1 of the note: how hot districts differ, controls added step by step, per-person consumption against actual wages (PLFS, NSS 68 Schedule 10), and tenant hedonic rents (NSS 65, NSS 76).
- `migration_choice/`: destination-choice models for urban work migrants (NSS 64 Block 6 and PLFS 2020-21), Poisson on origin-state by destination-district counts with origin fixed effects. `mig_choice.R` gives the uninstrumented estimates (`mig_choice_results*.csv`, `mig_choice_output.txt`). `mig_choice_iv.R` instruments the destination wage with the Bartik shock and `mig_choice_iv_altwage.R` with the other survey's wage for the same district, and `mig_choice_iv_asi.R` with the ASI 2009-10 average wage per factory worker (`asi2010_district_wages.csv`, from the Harari 2020 replication files in `AER_Urban Geometry in India/`); both use a control function with 300 bootstrap draws over destinations (`mig_choice_iv_*`). `mig_build.R` builds the samples, and `export_mig_inputs.R` writes `destination_attributes.csv` and `district_lookup.csv`. The jobs ran on NOTS (`job*.slurm`).
- `theta_bryan_morten/`: India's theta (how strongly migrants respond to pay) from the Bryan-Morten (2019) earnings equation, with Census 2011 D-03 migration shares and NSS 64 / PLFS 2020-21 movers' wages, and the value of heat in the choice model with theta fixed. See its own `README.txt`.
- `heat_project_summary_2026-10-11.pdf` (and `.tex`): the current summary note, covering everything above in order and the overall view of the paper, with five figures and one table.
- `heat_project_summary_figures/`: the note's figures. Figure 1 is the pipeline's own MWTP plot before (from the draft's Figures folder) and after the corrections, cropped. Figures 2 to 5 come from `make_figs.py` (wage steps, heat map, Bryan-Morten relationship and theta across samples, cost of heat across theta). `figdata.R` (run on NOTS with `job_fig.slurm`) makes `data/`: the heat value with theta fixed on a grid, the binned wage-share relationship and simplified district and state boundaries.
- `note_for_james_2026-10-09.pdf` (and `.tex`): earlier note of 10 October, replaced by `heat_project_summary_2026-10-11.pdf`.
