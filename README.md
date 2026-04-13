# MultiDimMortRecon

# Bayesian Multi-Dimensional Mortality Reconstruction

This repository contains code and materials for the paper:

> **"Bayesian multi-dimensional mortality reconstruction"**

**R version:** 4.4.0 (2024-04-24)

---

## Authors

**Andrea Tamburini**
International Institute for Applied Systems Analysis, Wittgenstein Centre for Demography and Global Human Capital (IIASA, OeAW, University of Vienna), Laxenburg, Austria.
📧 [tamburini@iiasa.ac.at](mailto:tamburini@iiasa.ac.at)

**Arkadiusz Wiśniowski**
Social Statistics Department, The University of Manchester, Manchester, United Kingdom.
📧 [a.wisniowski@manchester.ac.uk](mailto:a.wisniowski@manchester.ac.uk)

**Dilek Yildiz**
International Institute for Applied Systems Analysis, Wittgenstein Centre for Demography and Global Human Capital (IIASA, OeAW, University of Vienna), Laxenburg, Austria.
📧 [yildiz@iiasa.ac.at](mailto:yildiz@iiasa.ac.at)

---

## 📄 Abstract

**Background**  
Even though mortality differentials by socio-economic status and educational attainment level have been widely examined, the research is often limited to developed countries and recent years. This is primarily due to the absence of consistent and good-quality data. Systematic studies with a broad geographical and temporal spectrum that engage with the link between educational attainment and mortality are still lacking.

**Objective**  
To develop a statistical model that uses multiple patchy data sources to reconstruct mortality rates by age, sex, and the level of education.

**Methods**  
The proposed approach is a hierarchical Bayesian model that combines the strengths of multiple sources in order to estimate mortality rates by time periods, age groups, sex, and educational attainment.

**Results**  
We apply the model in a case study that includes 13 countries across South-East Europe, Western Asia, and North Africa, and calculate education-specific mortality rates for females in five-year age groups starting at age 15 for the 1980–2015 period. We then validate our estimates via posterior predictive checks.

**Contribution**  
There are two contributions of this work:  
1. We propose a novel and flexible probabilistic method to inform the research on the relationship between education and adult mortality.  
2. We provide age-, sex-, and education-specific mortality estimates for 13 countries.  

Our study addresses the lack of education-specific mortality differentials by providing a flexible method for their estimation.

---

The workflow combines **log-linear modelling**, **data preparation for JAGS**, and **principal component analysis (PCSs)** to reconstrsuct age–education–sex–country specific mortality profiles.

---

## Repository Structure

### Main Scripts

- **`paper_model_running.R`**  
  The main script that runs the Bayesian JAGS model.  
  **Workflow inside the script:**
  1. Load required libraries  
  2. Load and prepare input data (`d` dataset constructed in `d_profile_specific_construction.R`)  
  3. Build arrays for JAGS (education-specific deaths, populations, log-mortality rates)  
  4. Load and combine variance inputs (from DHS and log-linear models)  
  5. Prepare log-mx inputs  
  6. Define Principal Components (PCSs) for mortality curves  
  7. Set priors for PC distributions  
  8. Collect all inputs into a JAGS data list  
  9. Define parameters to monitor  
  10. Run the Bayesian model (`R2jags`)  
  11. Check convergence (Rhat, effective sample size)  
  12. Extract results  
  13. Generate visualizations (country-specific plots, caterpillar plots)

- **`d_profile_specific_construction.R`**  
  Prepares the input dataset `d` used by the main model.  
  **Steps inside the script:**
  1. Compare DHS and log-linear estimates at age 15–19  
  2. Reconstruct country- and period-specific mortality lines using DHS/UN/WiC data  
  3. Define a period table for harmonization (1975–2015)  
  4. Merge WPP interpolated life tables with education-specific splits  
  5. Correct starting values (population-weighted)  
  6. Reconstruct sex–country–education specific mortality trajectories  
  7. Build base dataset structure across age/sex/education/year  
  8. Construct full dataset `d` with deaths, populations, log-mx, and consistency adjustments

- **`PCS.R`**  
  Defines **Principal Components (PCSs)** used to model mortality patterns.  
  **Workflow inside the script:**
  - Load and clean UN WPP life tables (male & female)  
  - Interpolate mortality rates (`mx`) across years and ages  
  - Integrate WCDE (mean years of schooling) and UNESCO (primary education duration) data  
  - Define country groups by subregions  
  - Split countries into **lower** vs **mid/high education groups**  
  - Construct age–education mortality matrices  
  - Apply SVD to log-transformed matrices to obtain education-specific PCSs (first 3 components)

- **`log_linear_model_select_and_run.R`**  
  Performs systematic log-linear modeling to estimate variances used in the Bayesian model.  
  **Steps inside the script:**
  1. Load and prepare mortality data (age 15–19, education, region clusters)  
  2. Systematically generate model specifications with additive terms and interactions  
  3. Perform non-Bayesian model selection via BIC (Generalized Linear Models with Poisson family)  
  4. Select best-fitting specification  
  5. Refit selected model in Bayesian framework (`rstanarm::stan_glm`)  
  6. Perform convergence diagnostics (Rhat, effective sample size)

---

## Data

The scripts assume the presence of several datasets in `./data/`:

- **`d_DHS_1519_Recoded_loglin_country_time_specific.csv`** – input dataset combining DHS and log-linear outputs  
- **`log_lin_results.csv`** – results of log-linear modeling  
- **`DHS_sigma_values_estimates_withNAs.csv`** – DHS-based sigma estimates  
- **`pcs_lower_cluster1_Female.csv`, `pcs_midHigh_cluster1_Female.csv`** – precomputed PCS  
- **`estimated_normals_2edu_groups.csv`** – priors for PC distributions  
- **`df_WPP_interpolated.csv`** – interpolated WPP life tables  
- **`durantion_primary.csv`** – UNESCO dataset on primary school duration  
- **`WiC.data.interpolated.csv`** – interpolated WiC dataset  
- **`res_first_clustering.csv`** – clustering results for countries

(Additional input data are loaded directly from WPP and WCDE via provided scripts.)

---

## Dependencies

The following R packages are required:

- Data manipulation: `tidyverse`, `reshape2`, `zoo`  
- Reading/writing: `readxl`, `openxlsx`  
- Bayesian modeling: `rjags`, `R2jags`, `rstanarm`  
- Plotting/labels: `latex2exp`  
- Education data: `wcde`  

Install with:

```r
install.packages(c("tidyverse", "reshape2", "zoo", "readxl", "openxlsx", "latex2exp"))
# JAGS interface
install.packages(c("R2jags", "rstanarm"))
# WCDE (may need devtools::install_github)

