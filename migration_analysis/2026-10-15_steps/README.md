# Steps 1-5 after James's questions (15 October 2026)

Each folder has its own README with code, results and the checks run. Computation on NOTS in `/scratch/td52/heat_steps_2026-10-15`.

| Step | Folder | What it found |
|---|---|---|
| 1 Inputs | `step1_inputs/` | Six input problems, all fixed: within-district movers in NSS 64; no sex or age filter; PM2.5 and urban population missing for Delhi, Hyderabad and the rest of Telangana (and Delhi's urban population counted nine times over), which kept these destinations out of every earlier run; unweighted survey counts; PLFS migrants of all durations. Sampling noise in the district wage is modest (reliability 0.72 to 0.85). |
| 2 Model | `step2_model/` | Three-page note for James. The Bryan-Morten earnings theta equals the flow elasticity eta only if people differ in skill but not in taste; otherwise it overstates eta, by a factor of three or more with moderate taste differences (Monte Carlo). |
| 3 Census counts | `step3_census/` | D-3 counts men who moved for work (not workers). 30 more hot days mean 8 percent fewer recent work migrants (s.e. 5), 13 percent among interstate migrants, and no effect within a destination state. District wages are 1.9 percent lower. The cost crosses zero at eta of about 4.4. The full counts do not narrow the intervals. |
| 4 Flow elasticity | `step4_flow_elasticity/` | eta = 1.8 with district wages, 2.4-2.9 with a split-sample instrument (F about 180), 3.7 (s.e. 1.7) with the other survey's wage; ASI is weak and Bartik has no first stage. So eta is about 2.5 to 4, far below 12.9-17.2. |
| 5 Within destination | `step5_within_destination/` | Home climate matters only through moves within states (interstate estimates small and insignificant); no education gradient (interval rules out only large ones). |
| 6 Cohort design | `step6_cohorts/` | Arrival cohorts 2006-10 and 2010-11 within origin-destination pairs, with NSS 64 and NSS 68 wages (NSS 61 employment data are not on Box): eta 0.25 to 0.5, placebo groups do not respond. A short-run response, not the eta formula (4) needs. |
| 7 Joint standard errors | `step7_joint_se/` | All four terms of (4) bootstrapped together (eta from the split-half instrument): cost of 30 hot days 1.7 percent of income (-1.4 to 7.2); 81 percent of draws positive, 48 percent above the US figure. |
| 8 Updated note | `step8_note/` | `heat_project_summary_2026-10-15.pdf` with the new Figure 5; replaces the 11 October note. |
| Patch | `hedonic_patch/` | The 9 October hedonic patch as one commit on James's main, tested with `git am` on a fresh clone; read-only access, so not pushed. |

## Bottom line

With eta between 2.5 and 4, 30 more hot days cost about 0.2 to 2 percent of income; with all terms of (4) bootstrapped together, 1.7 percent (-1.4 to 7.2). The intervals include both zero and the US figure (2.1 to 2.4 percent), and the three designs for eta (0.25-0.5 from changes, 1.8-3.7 across destinations, 13-17 from earnings) disagree by more than an order of magnitude. The data cannot say whether heat's amenity cost in India is zero or US-sized. The claim in the 11 October summary, that the cost is about zero for any theta above 4 and at most a tenth of the US figure, relied on the Bryan-Morten theta and on inputs that left out Delhi and Hyderabad; it does not hold up.

## Still open

- The 2001-06 against 2006-11 cohort comparison with wages needs the NSS 61 employment schedule (MoSPI download); step 6 suggests it would measure the same short-run response.
- The patch needs James to apply it (and to copy `nss68_head_characteristics.csv` into `data/nss/CleanData/`).
