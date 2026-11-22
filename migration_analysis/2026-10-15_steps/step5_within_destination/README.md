# Step 5: comparisons among migrants choosing the same destinations (15 October 2026)

Runs on NOTS in `/scratch/td52/heat_steps_2026-10-15/step5`. Destination fixed effects absorb wages, rents, jobs and every other attraction of a place, so only differences between groups of migrants in how they respond to heat are identified.

## 5a Home climate (`step5a_home_climate.R`, Census 2011 counts, men who moved for work in the last 0-4 years)

Origin and destination fixed effects; controls for log distance, same state, shared state border (62 pairs, from district polygons) and same main language of the two states. Heat in units of 30 days above 32 C. Home heat across the 35 origin states runs from 1.5 (P10) to 8.2 (P90), a gap of 202 days; among interstate migrants the P25-P75 gap is only 5 days, because most come from UP and Bihar.

| Specification | Coefficient | s.e. (destination) | s.e. (origin and destination) |
|---|---|---|---|
| Home heat x destination heat, distance + same state | 0.023 | 0.009 | 0.012 |
| + border + language | 0.023 | 0.009 | 0.012 |
| + border + language, interstate only | 0.009 | 0.010 | 0.011 |
| Climate distance, + border + language | -0.091 | 0.030 | 0.041 |
| Climate distance, interstate only | -0.042 | 0.037 | 0.041 |

With all moves, migrants from P90-heat states are 15 percent less deterred by 30 extra destination hot days than migrants from P10 states (about 4 percent of income at eta = 4). But the interstate-only estimates are a third to a half as large and not significant, so the pattern rests on moves within states, where home climate and destination climate are close for reasons (home bias) the model cannot separate from climate preference. No firm evidence of adaptation to home climate.

## 5b Education (`step5b_education.R`, survey migrants, weighted)

Men 15-64 who moved for work from another district, counts weighted by survey multipliers, split by secondary school or more (NSS 64: codes 10-14; PLFS: 08-13; the two surveys code education differently, checked against the NSS 64 schedule and the PLFS code frequencies). Secondary or more: 54 percent (NSS 64), 58 percent (PLFS). Destination x survey and origin x education x survey fixed effects; distance and same-state effects differ by education.

| Sample | Heat x secondary | s.e. |
|---|---|---|
| Pooled | -0.011 | 0.057 |
| NSS 64 | 0.007 | 0.068 |
| PLFS 2020-21 | -0.024 | 0.051 |

No sign that the better educated avoid heat more. The interval (about +-11 percent of flows per 30 hot days) rules out only large differences: at eta = 3, an extra cost for the better educated above about 4 percent of income per 30 hot days.

## Bug caught on the way

The first run coded NSS 64 education with PLFS codes (code 08 is middle school in NSS 64 but secondary in PLFS, and NSS 64 has a code 14), which counted 828 men with middle schooling as secondary and dropped 329 postgraduates. Fixed in `mig_build_v2.R` (`edu_sec(code, survey)`); the results above are from the corrected run.
