# MultiDimMortRecon

This repository contains code and materials for the paper:

**"Reconstructing Education-Specific Mortality Rates Using a Hierarchical Bayesian Model"**

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

## 📂 Repository structure

- `R/` – R functions used in the analysis  
- `scripts/` – analysis and model-fitting scripts  
- `data/` – input data (note: raw data not included due to size/licensing restrictions)  
- `docs/` – manuscript, supplementary material, and documentation  
- `results/` – model outputs, processed estimates, and validation results  

---

## ⚙️ Requirements

- R (≥ 4.2)  
- [JAGS](https://mcmc-jags.sourceforge.io/) (for Bayesian inference)  
- R packages:  
  - `R2jags`  
  - `tidyverse`  
  - `data.table`  
  - `posterior`  
  - `ggplot2`  
  - `bayesplot`  
  - `cmdstanr` (optional, for alternative inference)  

You can install the R dependencies with:
```r
install.packages(c("tidyverse", "rjags", "R2jags", "latex2exp","reshape2"))


