# Step 6: cohort design for the flow elasticity eta (15 October 2026)

The design proposed in step 4 compares migrants who arrived in 2001-06 with those who arrived in 2006-11, with origin x destination fixed effects. It needs district wages for about 2004-05 (NSS 61, Schedule 10). Box has NSS 61, 62 and 66 only as consumption schedules (in `Individual Poverty Over Time/Data/IND/NSS/`), and no NSS 61 employment file, so that version needs a MoSPI download. This step runs the same design on the two most recent arrival cohorts, for which we have wages.

- `nss64_wage_halves.R`: NSS 64 (2007-08) urban district wage effects built like the pipeline's NSS 68 ones (status 31/41/51 in the current week, weekly wage earnings, ages 15-65, Mincer with age, age^2, male and two schooling dummies; NSS 64's education codes mapped to the NSS 68 definitions). 34,856 urban wage earners, 564 districts with 10 or more. Across 548 districts, the NSS 64 and NSS 68 effects correlate 0.53 (0.65 corrected for noise); the 2007-08 to 2011-12 change has SD 0.28 and reliability of about 0.62 (independent halves correlate 0.45).
- `cohort_design.R`: Census 2011 D-3 counts of men who moved for work, arrived in the last year (about 2010-11; 1.03 million) and 1-4 years earlier (about 2006-10; 3.52 million), with pay at arrival from NSS 68 and NSS 64. Poisson with origin x destination and origin x cohort fixed effects, so climate, distance, networks and every fixed attraction drop out. Wage instrumented by the other random half of each district's earners (control function), or by the Bartik shock for the recent cohort. Bootstrap over destinations (B = 200). Placebos: men who moved with their household, women who moved for marriage.

## Results

| Outcome | Instrument | First-stage F | eta | s.e. | 95% interval |
|---|---|---|---|---|---|
| Men, moved for work | none | | 0.24 | 0.11 | -0.02 to 0.44 |
| | split half | 111 | 0.26 | 0.23 | -0.26 to 0.62 |
| | split half, reversed | 118 | 0.49 | 0.23 | 0.06 to 0.91 |
| | Bartik | 3.8 | weak | | |
| Men, moved for work, interstate | none | | 0.27 | 0.14 | -0.03 to 0.55 |
| Placebo: men who moved with household | none / split | | 0.08 / 0.16 | 0.05 / 0.12 | |
| Placebo: women who moved for marriage | none / split | | -0.03 / -0.04 | 0.04 / 0.07 | |

## What this says

Within an origin-destination pair, flows of work migrants respond to changes in destination pay over four years, and the placebo groups do not, so the design is picking up something real. But the response is small: about 0.25 to 0.5, against 1.8 to 3.7 across destinations (step 4) and 13 to 17 from movers' earnings (Bryan-Morten). The three designs measure different things. Changes over four years are partly transitory and partly caused by the inflows themselves (inflows push wages down, which biases this estimate toward zero), while migrants choose places on expected long-run pay. So this is a short-run response and not the eta that formula (4) needs; plugged into (4) it would put the cost of 30 hot days above 15 percent of income. The NSS 61 version (2001-06 against 2006-11 arrivals, with 2004-05 and 2009-10 wages) would face the same issues.
