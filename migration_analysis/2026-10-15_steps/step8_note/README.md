# Step 8: updated summary note and Figure 5 (15 October 2026)

- `heat_project_summary_2026-10-15.tex` / `.pdf`: replaces `../../2026-10-09_light_corrections/heat_project_summary_2026-10-11.pdf` (left unchanged as the version James received). Sections 1 to 3 (the hedonic pipeline) are the 11 October text with one sentence updated on the patch; the introduction and Sections 4 to 9 are rewritten with the results of steps 1 to 7. Figures 1 to 4 are taken from the 11 October folder (relative path in `\graphicspath`); Figure 5 is new.
- `make_fig5.py` -> `figs/fig5_eta_cost.pdf`: the 11 October Figure 5 design with new data: Census counts with eta fixed on a grid (`results/census_grid_dense.csv`, from `census_grid_dense.R`, the step 3 main specification with theta from 0.5 to 32.4), the estimated eta, and the instrumented estimates from step 4. The x axis is now eta, the flow elasticity; the shaded band is the earnings-based theta, labelled as an upper bound on eta.
- Every number in the note was checked against the result files of steps 1 to 7.

Section 7 of the note states a view on the direction of the paper (the valuation paper is not worth much more time in its current form; the gap between earnings-based and flow-based elasticities is the result most likely to interest a wider audience). Check it before sending.
