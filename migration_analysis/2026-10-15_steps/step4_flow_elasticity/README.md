# Step 4: the flow elasticity eta (15 October 2026)

Runs on NOTS in `/scratch/td52/heat_steps_2026-10-15/step4`.

- `wage_halves.R`: NSS 68 district wage effects from two random halves of each district's urban wage earners (same Mincer as the pipeline). The full-sample effects reproduce `wage_eus` exactly (correlation 1.0000, slope 1.000); the two halves correlate at 0.61. Output `nss68_wage_halves_pc11.csv`.
- `census_iv.R`: Census 2011 counts of men who moved for work into urban districts in the last 0-4 years (step 3), Poisson with the destination wage instrumented by a control function (first stage at the destination level with heat and the destination controls), bootstrap over destinations (B = 200). Results in `results/census_iv.csv`.

## Results (origin fixed effects)

| Instrument | Destinations | First-stage F | eta | 95% bootstrap interval | Cost of 30 hot days at that eta |
|---|---|---|---|---|---|
| None | 579 | | 1.81 | 1.25 to 2.28 | 2.7 (-1.9 to 11.0) |
| Split half (half A instrumented by half B) | 579 | 187 | 2.86 | 1.91 to 3.77 | 1.4 (-2.0 to 6.8) |
| Split half, reversed | 579 | 176 | 2.43 | 1.09 to 3.37 | 1.3 (-2.3 to 9.2) |
| PLFS 2017-20 wage | 364 | 18 | 3.69 | 0.56 to 6.82 | 0.3 (-3.3 to 6.8) |
| ASI 2009-10 factory wage | 457 | 10 | 13.2 | 7.2 to 42.5 | -1.0 (-2.4 to 0.7) |
| Bartik (EC 2005-13) | 578 | 0.003 | no first stage | | |

With origin x destination-state fixed effects: no instrument 1.29, split half 2.29 and 1.97 (F 93 and 107), PLFS 3.4 (F 4, weak), ASI 9.3 (F 5, weak), Bartik F 0.8.

## What this says

1. Removing sampling noise raises eta from 1.8 to 2.4-2.9, as the reliability calculation in step 1 predicted (attenuation of about 0.7). The persistent part of district pay (other-survey instrument) gives 3.7, imprecisely. ASI is weak and very imprecise; the Bartik shock does not predict 2011-12 wages at all.
2. The credible range is eta of about 2.5 to 4, far below the Bryan-Morten 12.9 to 17.2. By step 2 this gap means tastes for places vary a lot across people, not only skills.
3. In that range 30 more hot days cost about 0.2 to 2 percent of income on the Census counts, with intervals of roughly -2 to +7 percent: they include both zero and the US figure (2.1 to 2.4).
4. The remaining bias in the split-half estimate runs downward if better-paid districts have worse unobserved amenities (the Roback logic). The other-survey estimate can be biased upward if persistent pay goes with persistent attractions such as jobs. Neither instrument is free of this; a design that holds amenities fixed (changes over time within destination, needing NSS 61 wages for 2004-05) is the next step.
