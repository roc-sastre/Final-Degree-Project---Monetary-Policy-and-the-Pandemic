# Monetary Policy and the Pandemic  
### SVAR Analysis of Monetary Policy in the COVID-19 Era

This repository contains the code and materials for my final dissertation for the Double Degree in Economics and Statistics.

The project studies the effectiveness of monetary policy before, during, and after COVID-19 in the euro area and the United States using Structural Vector Autoregression (SVAR) models with sign restrictions, grounded in a New Keynesian framework.

📄 Full dissertation: [FP-ECOEST_Sastre_2025.pdf](paper/FP-ECOEST_Sastre_2025.pdf)

## Research Goals

- Evaluate how monetary policy transmission changed across periods:
  - Pre-COVID
  - COVID crisis
  - Post-COVID
- Compare responses between the euro area and the United States
- Identify and interpret structural monetary policy shocks
- Measure effects on:
  - Overnight interest rates
  - Inflation
  - Output gap

## Methodology

- Reduced-form Bayesian VAR estimation
- Independent normal–Wishart priors
- Structural identification via sign restrictions
- Impulse Response Functions (IRFs)
- Historical Decompositions (HDs)
- Time-series preprocessing and stationarity analysis

## Repository Structure

- data/        datasets (most of them obtained through some of the scripts)
- src/         scripts and models
- results/     outputs and figures
- paper/       final dissertation

## Requirements
- R 4.5.2 and the packages loaded by the scripts in `src/`
- MATLAB R2025a with the ECB BEAR toolbox for model estimation
- Microsoft Excel-compatible `.xlsx` support
- A personal FRED API key supplied through the `FRED_API_KEY` environment variable (never commit it)

## Running the code
Run the R scripts from the repository root in numerical order. The first script saves `data/1.RData`; the second creates the BEAR input workbooks in `data/`. Run `src/Modelitzacio.m` in MATLAB with the ECB BEAR toolbox installed. Place the six resulting `results_*.mat` files in `data/model_outputs/` before running `src/3. Xocs, IRFs i HDs.R`. The presentation graphics script (`src/5. Gràfics PPT.R`) uses the `results_list` object created by script 3, so run it in the same R session. Figure files are written to `results/figures/`. These are the original research scripts with portable paths; the repository does not include BEAR, proprietary inputs or generated MATLAB result files, and the full pipeline has not been independently rerun from a clean environment.

## Data
Some datasets are not public due to licensing restrictions.

## License
MIT License
