# Step 7: joint standard errors for formula (4) (15 October 2026)

`joint_se.R`: one bootstrap over destinations (B = 500) that re-estimates every piece of MWTP = s_y beta_p - beta_w + beta_N / eta on the same draw, on the Census sample of step 3 (579 destinations, 4.6 million men who moved for work in the last 0-4 years; rents from NSS 65 for 258 destinations). eta is the split-half instrumented estimate of step 4. Results in `results/joint_se.csv`; all 500 draws in `results/joint_se_draws.csv`.

| Piece (per 30 more hot days) | Estimate | s.e. | 95% interval |
|---|---|---|---|
| beta_N, heat effect on migrant inflow (percent) | -8.3 | 5.3 | -19.5 to 0.3 |
| eta, flow elasticity | 2.86 | 0.48 | 1.96 to 3.84 |
| beta_w, district wage (percent) | -1.9 | 0.7 | -3.4 to -0.6 |
| beta_p, district tenant rent (percent) | -1.6 | 1.5 | -4.5 to 1.4 |
| Cost of 30 hot days, one model: -(gamma/eta + s_y beta_p) | 1.7 | 2.3 | -1.4 to 7.2 |
| Cost of 30 hot days, formula (4) term by term | 1.3 | 2.3 | -2.1 to 6.3 |
| Cost with eta fixed at 2.5 | 1.8 | 2.5 | -1.4 to 7.7 |
| Cost with eta fixed at 4 | 0.5 | 1.9 | -1.9 to 5.4 |

Costs in percent of income, positive = cost. The pieces are nearly uncorrelated across draws except beta_N and gamma (0.81), which are the same heat effect with and without the wage held fixed. 81 percent of draws give a positive cost and 48 percent a cost above the US figure (Albouy et al. 2016, about 2.1 percent for 30 days).

The point estimate is a little below the US figure; the interval runs from a small negative value to about three times the US figure. This interval takes eta from the split-half instrument as given by the model; it does not include the uncertainty about which eta is the right one (0.25 to 0.5 from changes within origin-destination pairs, step 6; 3.7 from the other-survey instrument, step 4; 13 to 17 from movers' earnings).
