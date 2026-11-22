# Step 5c: age split and reweighting (22 November 2026)

James asked (22 Nov) whether the destination-choice estimates, which describe men who moved for work, can say something about the broader urban population in the Rosen-Roback sample.

`step5c_age.R` uses the same model, sample and data as step 5b (`../2026-10-15_steps/step5_within_destination/step5b_education.R`), with the education split replaced by an age split. It ran locally in 21 seconds with the unchanged builder `../2026-10-15_steps/step1_inputs/mig_build_v2.R`, which has to sit next to the script. It first reruns step 5b and reproduces it exactly (pooled -0.0109, s.e. 0.0567; 5,775 and 4,577 migrants; 638 destinations).

| Split, NSS 64 and PLFS 2020-21 pooled | Heat x group | s.e. | Extra cost of 30 hot days for the group, eta = 2.9 |
|---|---|---|---|
| Secondary school or more (step 5b) | -0.011 | 0.057 | +0.4 |
| Age 30 or more | +0.027 | 0.033 | -0.9 |
| Age 35 or more | +0.018 | 0.031 | -0.6 |

Older migrants avoid heat no more than younger ones (if anything slightly less, not significant). The two surveys point in opposite directions: NSS 64 +0.082 (s.e. 0.054), PLFS -0.032 (0.058).

Reweighting to urban men aged 15-64 in PLFS 2020-21 (first visit, weighted): the migrants and the target look alike (aged 30 or more: 63 against 61 percent; 35 or more: 47 against 50; secondary school or more: 56 against 60), so the cost of 30 hot days changes by 0.02 points of income or less, with intervals within plus or minus 0.16. Even for a population made up entirely of men aged 30 or more, it would move by -0.3 points (-1.2 to +0.5) at eta = 2.9.

Assumptions: eta is the same in both groups, and within a group movers and non-movers have the same taste for heat. The Census counts have no age or education breakdown, so this uses the survey migrants only. Nothing here speaks to women or to men who never move.

Files: `step5c_age.R` (usage line at the top), `step5c.log`, `step5c_age_splits.csv` (all splits, pooled and by survey), `step5c_reweighting.csv` (shares and adjustments).
