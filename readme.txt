=====================================
 Workflow for Bayesian Mortality Model
=====================================

Step 1: Log-linear model selection (used in input construction)
---------------------------------------------------------------
File: log_linear_model_select_and_run.R
- Fits log-linear models for age 15–19
- Selects best specification with BIC
- Runs Bayesian version (rstanarm) to estimate variances
- Outputs variance estimates used in dataset construction

(Note: This step has already been run to produce input files. 
You can re-run if you want to reproduce the variance estimation.)

Step 2: Construct input dataset
-------------------------------
File: d_profile_specific_construction.R
- Prepares education-specific mortality dataset (d)
- Combines DHS splits + log-linear results + WPP interpolated + WiC data
- Produces main input file for the Bayesian model

Step 3: Define Principal Components
-----------------------------------
File: PCS.R
- Computes mortality curve PCSs (via SVD)
- Produces pcs_lower_cluster1_Female.csv and pcs_midHigh_cluster1_Female.csv

Step 4: Run main Bayesian JAGS model
------------------------------------
File: paper_model_running.R
- Loads dataset d (from Step 2)
- Loads PCSs (from Step 3)
- Loads variance estimates (from Step 1, already precomputed)
- Builds JAGS arrays and priors
- Runs hierarchical model (R2jags)
- Performs convergence checks & extracts results
- Produces country plots and summary visualizations

=====================================
 Run order (if rebuilding everything):
 1. log_linear_model_select_and_run.R
 2. d_profile_specific_construction.R
 3. PCS.R
 4. paper_model_running.R

 Run order (if using repo as provided):
 1. paper_model_running.R
=====================================
