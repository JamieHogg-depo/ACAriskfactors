##
# Create data for ViseR
##

# Packages
library(tidyverse)
library(readr)
library(readxl)
rm(list = ls())

## Functions ## ----------------------------------------------------------------

source('src/functions_ALL.R')
source('src/moreFuns.R')

# Load global data
global_obj <- readRDS("data/DataLabExport/global_obj.rds")

# Load raw estimates
raw_est <- pbapply::pblapply(list.files("data/DataLabExport", 
                                        pattern = "raw_est_*", full.names = T), readRDS)
names(raw_est) <- str_remove( 
  str_remove(
    list.files("data/DataLabExport", pattern = "raw_est_*"), "raw_est_"), ".rds")

## START FOR LOOP #### ---------------------------------------------------------

out <- list()

for(k in 1:8){
  
  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  # load data
  modelled_est <- readRDS(file = paste0("data/DataLabExport/modelled_est_", rf, ".rds"))
  
  # get mu quantiles
  mu_quants <- bind_rows(lapply(asplit(modelled_est$mu, 2), 
                                quantile, p = c(0.1,0.2,0.5,0.8,0.9), na.rm = T)) %>% 
    setNames(paste0("mu_", c("p10", "p20", "p50", "p80", "p90")))
  
  # get OR quantiles
  or_quants <- bind_rows(lapply(asplit(modelled_est$or, 2), 
                                quantile, p = c(0.1,0.2,0.5,0.8,0.9), na.rm = T)) %>% 
    setNames(paste0("or_", c("p10", "p20", "p50", "p80", "p90")))
  
  # get logOR quantiles
  logor_quants <- bind_rows(lapply(asplit(log(modelled_est$or), 2), 
                                quantile, p = c(0.1,0.2,0.5,0.8,0.9), na.rm = T)) %>% 
    setNames(paste0("or_", c("logp10", "logp20", "logp50", "logp80", "logp90")))
  
  # V-plot - exceedance probability
  v <- bind_cols(getDPP(modelled_est$or, null_value = 1)) %>% 
    dplyr::select(EP) %>% 
    setNames("v")
  
  # add all colums
  out[[k]] <- cbind(dplyr::select(global_obj$area_concor, SA2), or_quants, logor_quants, v, mu_quants) %>% 
    mutate(level = k)
  
  # cleanup
  rm(modelled_est, mu_quants, or_quants, logor_quants, v)
  
## FINISH FOR LOOP #### --------------------------------------------------------
  
}

data <- bind_rows(out) %>% 
  mutate(indicator = "riskfactor",
         sex = "Persons", 
         years = "2016-2017",
         baseline = "national_average") %>% 
  relocate(indicator, level, sex, years, baseline, SA2) %>% 
  write.csv(., "data/riskfactor_estimates_ViseR.csv")

## END SCRIPT #### -------------------------------------------------------------