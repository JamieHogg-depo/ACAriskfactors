# Mapping the prevalence of cancer risk factors at the small area level in Australia

This repository contains the code and modelled results described in the manuscript "Mapping the prevalence of cancer risk factors at the small area level in Australia" by James Hogg, Jessica Cameron, Susanna Cramb, Peter Baade and Kerrie Mengersen.

The modelled results are available in long-format upon request. This dataset contains the following columns.

<!--from the authors. in the dataset `ModelledEstimates.csv`.--->

- `SA2_2016`: The 2016 9-digit SA2 codes
- `risk_factor`: Risk factor names

Plus the following 11 columns for each of the absolute (proportions - `mu_*`) and relative (odds ratios - `or_*`)

- `*_mean`: Posterior mean
- `*_median`: Posterior median
- `*_sd`: Posterior standard deviation
- `*_lower`: Lower limit of 95% highest posterior density interval (HPDI)
- `*_upper`: Upper limit of 95% HPDI
- `*_hpd38`: Interval size for 38% HPDI (Bayesian analogue of `*_sd`)
- `*_CV`: Coefficient of variation (%) using `*_sd`
- `*_CV_b`: Coefficient of variation (%) using `*_hpd38`
- `*_cisize`: `*_upper` - `*_lower` 
- `*_EP`: Exceedance probability
- `*_DPP`: Difference in posterior probability
- `*_DPPsig`: Binary vector indicating significant DPPs (cutoff > 0.6)
