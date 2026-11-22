# Step 1: fix the inputs of the destination-choice model (15 October 2026)

Runs on NOTS in `/scratch/td52/heat_steps_2026-10-15/step1` (inputs from `/scratch/td52/heat_light_2026-10-09`).

## Files

- `mig_build_v2.R`: the 9 October build (`migration_choice/mig_build.R`) with switches for the sample fixes, a PM2.5 control that keeps every destination, and a path for the destination attributes.
- `step1_grid.R`: theta grid for variants 0 to 7 on the old attributes. Variant 0 reproduces the 11 October grid exactly (all 69 fixed-theta rows and the three free-theta rows).
- `build_pm25.R`, `build_dest_v2.R`: rebuild PM2.5 and urban population on 2011 Census district codes; writes `destination_attributes_v2.csv` (on NOTS).
- `step1b_grid.R`: theta grid on the corrected attributes (variants 2 and 8 to 14), plus heat gradients of district wages and rents.
- Results: `results/step1_grid.csv`, `results/step1b_grid.csv`, `results/step1b_heat_gradients.csv`, logs.

## What was wrong in the inputs

1. NSS 64 included migrants who moved within their own district (1,749 of 6,805 recent urban work migrants); PLFS excluded them. Now both require another district.
2. No sex or age filter, while theta comes from men aged 15 to 64. Now men 15 to 64 (NSS 64: 4,430 of 4,983; PLFS: 6,555 of 8,035).
3. `log(pm25)` dropped 57 destinations with missing PM2.5 and, silently, 2 with zero PM2.5. The PM2.5 values had been matched by district name and missed all of Delhi, Palghar, Howrah, Goa and others. Rebuilt from the WUSTL file on national 2011 codes: 640 of 641 districts, identical where both exist (correlation 0.9998; Bilaspur, Chhattisgarh, had taken another Bilaspur's value).
4. `log_urban_pop` was missing for 27 districts, including all 10 Telangana districts (Hyderabad), because its source is keyed on PLFS codes (Telangana = 36). Since it enters every specification, Hyderabad was in none of the earlier runs. It also gave each of Delhi's 9 districts the urban population of all of Delhi (log 16.61 instead of 13.3 to 14.8). Rebuilt from Census 2011 PCA minus village-directory population (as in `theta_census.R`): 637 districts, correlation of logs 0.98 with the old values elsewhere.
5. PLFS 2020-21 has no time-since-moving field (only "moved after March 2020"), so its migrants are of all durations while NSS 64 uses moves in the last five years. Cannot be fixed within PLFS; the Census counts (step 3) can be limited to 0-4 years.

## What was not changed, and why

- District wages. Split-half reliability of the district wage effects is 0.85 for the PLFS 2017-20 wage (median 131 workers per district) and 0.78 for the NSS 68 wage (median 28), and 0.82 and 0.72 conditional on the model's controls. Sampling noise therefore attenuates the wage coefficient by only 20 to 30 percent; it cannot explain an OLS theta of 0.65 against 4 or more. Pooling more PLFS rounds would add little. (The person-level file `plfs_wage_individual_pooled.dta`, which adds 2022-23, gives district effects with SD 1.07 against 0.22 in the 2017-20 pool, so it has a units or coding problem; the pipeline does not use it.)
- Heat measure. Everything in formula (4) now uses days above 32 C (UTCI). The heat gradients of wages and rents are recomputed with that measure (`step1b_heat_gradients.csv`). Days above 38 C give a value near zero with very wide intervals.

## Results (pooled NSS 64 + PLFS, cost of 30 more days above 32 C, percent of income; positive = cost)

| | theta = 1 | 2 | 4 | 8 | 12.9 | 17.2 | migrants | destinations |
|---|---|---|---|---|---|---|---|---|
| 0 Old grid (11 Oct) | 4.0 | 1.4 | 0.0 (-1.6 to 1.6) | -0.9 | -1.4 (-2.8 to 0.1) | -1.5 | 8,924 | 557 |
| 2 Old attributes, sample fixes | 6.2 | 2.6 | 0.8 | -0.3 | -0.9 | -1.2 | 6,211 | 555 |
| 8 New attributes, old sample | 7.8 | 3.7 | 1.6 | 0.5 | 0.0 | -0.1 | 10,260 | 594 |
| 9 New attributes, sample fixes (baseline) | 10.2 | 4.9 | 2.2 (0.3 to 4.1) | 0.8 | 0.1 (-1.3 to 1.6) | -0.1 | 7,276 | 594 |
| 12 Baseline on rent sample, pay net of rent | 13.2 | 6.6 | 3.2 (1.0 to 5.5) | 1.6 | 1.1 | 1.0 | 5,843 | 289 |
| 14 Baseline, interstate movers only | 13.4 | 6.7 | 3.2 (0.8 to 5.7) | 1.5 | 0.7 | 0.4 | 3,414 | 594 |

Leaving out Delhi, Telangana or the big metros moves the theta = 4 cost between 1.6 and 3.2 percent; NSS 64 alone gives 2.1 and PLFS alone 0.8 (both with wide intervals).

The cost now crosses zero at theta of about 15, not 4. At theta = 4 it is about the US figure (Albouy et al. 2016: 0.07 to 0.08 percent of income per day, 2.1 to 2.4 percent for 30 days).

## A model check that weighs against high theta

The coefficient on log urban population should be at most 1 if a district is a bundle of locations (step 2). With theta estimated it is 0.88 (s.e. 0.08). Fixing theta at 12.9 pushes it to 1.66 (0.14) and at 17.2 to 2.10 (0.17): the wage offset then explains too much and size compensates. Suggestive, not decisive, since city size can also be an attraction in itself.

## Heat gradients across urban destinations (per 30 more days above 32 C, same controls)

District wage: -1.7 percent (s.e. 0.7) early, -0.1 (0.9) late; within state -0.5 (1.0) and -1.4 (1.2). Rents: -1.9 (1.4) early and -0.8 (1.9) late; within state +2.3 (2.3) and +1.1 (2.5).

## Step 1c: survey weights (added after step 3)

6. The survey grids counted sampled migrants without weights. NSS 64 and PLFS sample states at different rates, so unweighted counts misstate where migrants go. `mig_build_v2.R` now has `weighted = TRUE` (NSS 64 `wgt_combined`, PLFS multiplier in cols 352-361, each scaled to its sample size). With weights, the destination-level correlation of log counts with the Census 0-4 year counts rises from 0.59 to 0.76 (NSS 64) and from 0.65 to 0.70 (PLFS). (`step1c_weights.R`, `results/step1c_weights.csv`.)

Baseline (variant 9) with weights, pooled: wage coefficient 1.80 (s.e. 0.43); 30 more hot days lower the migrant inflow by 7.1 percent (95 percent interval 0.1 to 14.1); cost of 30 hot days 6.3 percent at theta = 1, 2.8 (0.05 to 5.5) at 2, 1.0 (-0.6 to 2.6) at 4, 0.2 at 8 and -0.2 (-1.4 to 1.0) at 12.9. NSS 64 alone: -0.1 at theta = 4. PLFS alone: flat and imprecise.

The weighted version supersedes the unweighted variant 9 above: with weights the surveys agree with the Census counts (step 3), and the cost crosses zero at theta of about 4 to 9, not 15.
