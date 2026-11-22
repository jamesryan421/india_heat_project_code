# Step 2: the model behind Section 7 (15 October 2026)

- `model_note.tex` / `.pdf`: three-page note for James. Bryan-Morten's model with taste differences added (Cardell split of the Gumbel draw into skill and taste), rents, and a district as a bundle of locations. Derives the gravity equation, formula (4) with the flow elasticity eta, and the movers' earnings equation.
- `mc_theta_wedge.py` / `mc_theta_wedge_output.txt`: Monte Carlo check. With skill only, the earnings regression recovers eta exactly. With taste variance half of skill variance, eta = 3 gives an earnings theta of 8.7 and eta = 6 gives 16.7; the linear approximation eta(1 + taste/skill variance) understates the gap, because selection into unpopular places runs mostly through taste.

Main points:
1. Formula (4) needs eta, the elasticity of flows to pay. The Bryan-Morten theta is an upper bound on eta, not an estimate of it. Our 12.9 to 17.2 is consistent with an eta of 3 to 6 or lower.
2. Data check: the binned earnings-share relation behind Figure 4(a) is flatter at low shares (theta 12.8) than at high shares (8.9), as taste differences predict, but the difference is not significant.
3. Log urban population should have a coefficient of at most 1 if a district is a bundle of locations. In the survey data this holds up to theta of about 5 and fails above; in the Census data (step 3) it fails at every theta, so the check is not informative about theta.
4. The estimates describe men who moved to a city for work. Their willingness to pay equals the urban population's only if tastes for heat do not vary with what makes people migrate; the education split (step 5) lets us reweight on observables.
