# Step 3: the heat effect on migrant flows from the Census full counts (15 October 2026)

Runs on NOTS in `/scratch/td52/heat_steps_2026-10-15/step3`.

## Data

`census_counts.R` reads all 35 state files of Census 2011 table D-3, "Migrants by place of last residence, duration of residence and reason for migration". The columns that `theta_census.R` calls `Pw` and `Mw` are people who moved **for work or employment**, not workers. So D-3 gives the count of men who moved for work, by state of last residence, destination district (urban part), and time since moving. That is the survey definition at full-count scale.

- Origins: each other state, plus "other districts of the same state" (the surveys' "another district"). Moves within the district are left out.
- Checks: the state rows add up exactly to the "beyond the state" row in all 9,600 district-area-duration cells; the rows that list a district's own state by name are all zero and are dropped.
- Size: men who moved for work into urban areas in the last 0-4 years number 2.6 million from other states and 2.2 million from other districts of the same state (4.6 million in the estimation sample, 579 destinations), against 7,276 in the survey samples.
- Agreement with the surveys: correlation of log counts by destination 0.59 (NSS 64) and 0.65 (PLFS) with unweighted survey counts, 0.76 and 0.70 with weighted counts (step 1c).

## Model

`census_choice.R`: the survey model applied to the Census counts. Poisson, origin fixed effects (or origin x destination-state), offset theta x log wage (NSS 68, 2011-12), heat = days above 32 C over 2001-2011, controls as before, destination-clustered standard errors.

## Results

Columns: 30 x the heat effect on the migrant inflow (percent, theta = 0); the freely estimated wage coefficient; the coefficient on log urban population at that estimate; cost of 30 more hot days in percent of income at theta = 1, 2, 4 and 12.9 (positive = cost).

| Specification | Heat effect on inflow | Wage coef. | Size coef. | theta = 1 | theta = 2 | theta = 4 | theta = 12.9 |
|---|---|---|---|---|---|---|---|
| A Work, 0-4 years, origin FE | -8.4 (4.8) | 1.81 (0.25) | 1.23 | 6.5 (-2.8 to 15.9) | 2.3 (-2.5 to 7.1) | 0.2 (-2.6 to 3.0) | -0.7 (-2.4 to 1.0) |
| B Origin x destination-state FE | +1.5 (4.8) | 1.29 (0.20) | 1.27 | -2.3 (-11.9 to 7.3) | -1.7 (-6.7 to 3.4) | -1.5 (-4.4 to 1.4) | -2.0 (-3.7 to -0.2) |
| C Interstate only | -12.9 (6.0) | 1.80 (0.32) | 1.40 | 10.8 (-0.1 to 21.6) | 4.3 (-1.0 to 9.6) | 1.2 (-1.7 to 4.2) | 0.0 (-1.9 to 1.9) |
| D Work + business | -8.4 (4.8) | 1.78 (0.25) | 1.22 | 6.6 | 2.4 | 0.2 | -0.7 |
| E Work, 5-9 years | -9.9 (3.9) | 1.86 (0.24) | 1.31 | 7.9 (0.2 to 15.6) | 2.8 | 0.3 | -0.8 |
| F Work, all durations | -11.1 (3.0) | 1.74 (0.21) | 1.26 | 9.0 (4.0 to 13.9) | 3.4 (0.8 to 5.9) | 0.5 (-1.3 to 2.3) | -0.8 (-2.2 to 0.7) |
| G Wage = PLFS 2017-20 (379 dest.) | -1.8 (5.1) | 1.38 (0.44) | 1.20 | 2.3 | 1.4 | 0.9 | -0.1 |
| H Heat over 1999-2008 | -8.7 (4.8) | 1.80 (0.25) | 1.23 | 6.8 | 2.4 | 0.2 | -0.8 |

District wage gradient with the same controls: -1.9 percent per 30 hot days (s.e. 0.7). The cost crosses zero at theta of about 4.4 (= 8.4 / 1.9).

## What this says

1. Heat lowers flows across urban India: 30 more hot days mean 8 percent fewer recent work migrants (13 percent among interstate migrants, 11 percent with all durations, where it is most precise). But within a destination state the effect is zero (specification B): hotter districts do not get fewer migrants than cooler districts of the same state. The cross-state pattern could be heat, or anything else that differs between hot and cool states.
2. The Census counts do not narrow the intervals, as feared: what is left is variation between destinations, which more migrants do not reduce. The intervals are wider than the weighted survey ones (at theta = 4: -2.6 to 3.0 against -0.6 to 2.6), because the Poisson weights destinations by their counts and a few large destinations dominate.
3. The wage coefficient is 1.8 (s.e. 0.25), more precise than in the surveys. Corrected for the 20 to 30 percent attenuation from sampling noise in the wage, it is about 2.3 to 2.6. At theta = 2 the cost of 30 hot days is 2.3 percent (similar to the US figure, 2.1 to 2.4) but the interval includes zero.
4. The size coefficient is above 1 at every theta here (1.23 at the estimated wage coefficient), so the bound from step 2 cannot discriminate between values of theta in the Census data. I no longer use it as evidence against high theta.
