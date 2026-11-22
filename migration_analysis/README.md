# Migration and destination-choice analysis (October and November 2026)

This folder holds the code behind the 11 and 15 October summary notes (`heat_project_summary_2026-10-11.pdf` and `heat_project_summary_2026-10-15.pdf`) and the 22 November age split. The hedonic corrections are the first commit on this branch, in the pipeline files at the repository root.

## Layout

- `2026-10-09_light_corrections/` covers 9 to 11 October:
  - `migration_choice/`: Poisson destination-choice models for NSS 64 and PLFS 2020-21 urban work migrants, uninstrumented and with the Bartik, other-survey and ASI instruments.
  - `theta_bryan_morten/`: theta from the Bryan-Morten earnings equation, with its own `README.txt`.
  - `root_cause/`: why the corrected hedonic estimates have the wrong sign.
  - `results/`: regression logs from the corrected pipeline run, `robustness_harness.R` and its point estimates.
  - `nots/`: the environment and job scripts for that run on NOTS.
  - `heat_project_summary_figures/`: figure scripts for the 11 October note.
- `2026-10-15_steps/` holds steps 1 to 8, which answer James's 14 October questions. Each step folder has a README on what the step does, what it found and the checks run, and `2026-10-15_steps/README.md` sums them up in one table.
- `2026-11-22_age_split/` (step 5c) asks whether older migrants avoid heat more than younger ones and reweights the survey migrants to urban men aged 15 to 64. It reruns step 5b first as a check and needs `2026-10-15_steps/step1_inputs/mig_build_v2.R` next to the script.

## Files kept on Box

The repository's `.gitignore` excludes `*.csv`, `*.log`, `*.rds` and `data/`, so inputs, result tables and run logs stay on Box under `Urban Economics India/docs/audits/`, in the same folder structure as here. Three of them matter for new runs:

- `2026-10-09_light_corrections/nss68_head_characteristics.csv` is an input to the patched pipeline and goes in `data/nss/CleanData/`. `make_head_characteristics.R` builds it.
- `2026-10-15_steps/step1_inputs/results/destination_attributes_v2.csv` holds the corrected district attributes, with PM2.5 and urban population on Census 2011 codes and Delhi and Telangana included. Use it in place of `migration_choice/destination_attributes.csv`, which leaves out Delhi and Hyderabad.
- The two summary notes and the model note (`2026-10-15_steps/step2_model/model_note.pdf`).

## Running

The scripts keep the paths they ran with: the Box folder on Tanmay's laptop (`B <- ".../Box-Box/Urban Economics India"`) and two run directories on NOTS, `/scratch/td52/heat_light_2026-10-09` and `/scratch/td52/heat_steps_2026-10-15`. Change these to your own paths before running. The `.slurm` files show the arguments each script takes. The NOTS directories may have been purged since October; the first lines of each script name its inputs, and `step1_inputs/` has the scripts that rebuild the migrant samples and district attributes (`mig_build_v2.R`, `build_dest_v2.R`, `build_pm25.R`).
