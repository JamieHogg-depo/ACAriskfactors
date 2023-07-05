##
# Create data for ViseR
##

# Packages
library(tidyverse)
library(readr)
library(readxl)
library(sf)
library(openxlsx)
rm(list = ls())

## Functions ## ----------------------------------------------------------------

source('src/wrangle/functions_ALL.R')
source('src/wrangle/moreFuns.R')

# Load global data
global_obj <- readRDS("data/DataLabExport/global_obj.rds")

# Load raw estimates
raw_est <- pbapply::pblapply(list.files("data/DataLabExport", 
                                        pattern = "raw_est_*", full.names = T), readRDS)
names(raw_est) <- str_remove( 
  str_remove(
    list.files("data/DataLabExport", pattern = "raw_est_*"), "raw_est_"), ".rds")

# load map
map_sa2_full <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
  filter(!str_detect(SA2_NAME, "Island")) %>%
  filter(STATE_NAME != "Other Territories") %>% 
  dplyr::select(SA2_MAIN16, SA2_NAME) %>% 
  st_drop_geometry() %>% 
  rename(SA2 = SA2_MAIN16,
         SA2_name = SA2_NAME) %>% 
  mutate(SA2 = as.numeric(SA2))

# Load all modelled estimates
summsa2all <- readRDS("data/summary_files/summsa2all.rds")

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
    dplyr::select(EP, DPP) %>% 
    mutate(bivariate_cat = case_when(
      EP >= 0.8 ~ "High",
      EP <= 0.2 ~ "Low",
      EP > 0.2 & EP < 0.8 ~ "Unclear"
    )) %>%
    dplyr::select(bivariate_cat, DPP) %>% 
    setNames(c("bivariate_cat", "v")) 
    
  
  # add all colums
  out[[k]] <- cbind(dplyr::select(global_obj$area_concor, SA2), or_quants, logor_quants, v, mu_quants) %>% 
    mutate(riskfactorgrp = rf) %>% 
    left_join(., map_sa2_full, by = "SA2") %>% 
    rename(SA2_code = SA2)
  
  # cleanup
  rm(modelled_est, mu_quants, or_quants, logor_quants, v)
  
## FINISH FOR LOOP #### --------------------------------------------------------
  
}

bind_rows(out) %>% 
  mutate(indicator = "riskfactor",
         level = "",
         sex = "Persons", 
         years = "2017-2018",
         baseline = "national_average") %>% 
  relocate(indicator, level, sex, riskfactorgrp, years, baseline, SA2_code, SA2_name) %>% 
  write.csv(., "data/riskfactor_estimates_ViseR.csv")

## Atlas_estimates_95CIs_riskfactors ## ----------------------------------------

# split into 8 datasets
ll <- split(summsa2all, summsa2all$model)

# wrangle to correct format
foo <- function(x){
  x %>% 
    left_join(., map_sa2_full, by = "SA2") %>% 
    dplyr::select(SA2, SA2_name, or_median, or_lower, or_upper, or_DPP) %>% 
    mutate(SA2 = as.character(SA2)) %>% 
    make_numeric_decimal(2) %>% 
    mutate(col1 = paste0(or_median, " [", or_lower, ", ", or_upper, "]")) %>% 
    dplyr::select(SA2, SA2_name, col1, or_DPP) %>% 
    set_names(c("SA2 code", "SA2 name", "OR [95% CI]", "Probability differs from Aust average"))
}

# apply formatting
ll2 <- lapply(ll, foo)
names(ll2) <- c("Leisure physical activity",
                "All physical activity",
                "Alcohol",
                "Diet",
                "Obesity",
                "Overweight",
                "Current smoking",
                "Risky waist circumference")

# Write data to excel in different sheets
wb <- createWorkbook("data/Atlas_estimates_95CIs_riskfactors.xlsx")

for(i in 1:8){
  addWorksheet(wb, names(ll2)[i])
  writeData(wb, i, ll2[[i]])
}

saveWorkbook(wb, "data/Atlas_estimates_95CIs_riskfactors.xlsx", overwrite = T)

## END SCRIPT #### -------------------------------------------------------------