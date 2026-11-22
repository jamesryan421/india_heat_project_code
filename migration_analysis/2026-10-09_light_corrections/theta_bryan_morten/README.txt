Bryan-Morten (2019) estimate of theta for India, Oct 10 2026 (run on NOTS: /scratch/td52/heat_light_2026-10-09/theta).

Selection equation: log daily wage = destination(district x sector x survey) effect + origin-state x survey effect
                    - (1/theta) log pi_od + age, age^2.
pi_od = share of men whose last residence was state o who live in district d (Census 2011 D-03, all 35 states, all durations;
natives = PCA male population - interstate - foreign - unclassifiable). Wages: male wage earners 15-64, NSS 64 (2007-08,
current-week earnings / days) and PLFS 2020-21 first visit (regular monthly / 30; casual daily wages).
Instrument for log pi_od: log distance from the origin-state population centroid (+ state contiguity). Two-way clustered SEs
(origin state, destination district). theta interval = inverted 95% interval for the slope.

Preferred (interstate movers, return migrants excluded, both surveys): OLS 12.9 [10.1, 17.7]; IV 17.2 [11.7, 32.4], F = 417.
NSS 64 alone: OLS 10.3, IV 14.0 [9.3, 27.8]. Bryan-Morten: Indonesia 12.5, US 27.6.

theta_implied.R fixes the wage coefficient of the destination-choice model (migration_choice/, urban work migrants) at theta
and reports the value of 30 extra days above 32 C: pooled +1.4% of income [-0.1, 2.8] at theta 12.9, +1.5% [0.2, 2.8] at 17.2
(positive = no heat penalty). Rows with the Bryan-Morten destination wage at theta 32.4 did not converge cleanly; ignore them.

code/     theta_census.R (D-03 -> pi_cells.csv), theta_micro.R (survey samples), theta_est.R (theta), theta_implied.R (heat value)
results/  theta_results.csv, theta_implied_heat_value.csv, theta_implied_wage_gradients.csv, dest_fe_*.csv (destination
          effects), logs 1-3. Census D-03 XLSX downloads are on NOTS in theta/in/d03 (public, censusindia.gov.in NADA catalog).
