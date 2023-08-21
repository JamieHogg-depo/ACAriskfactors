#==========================================================================
# Format Risk factor data for Atlas database
#
# Step 1:
#        Set the directories
#        Load functions and data
#        Specify how the data should be truncated (both in magnitude and in precision)
#        Load concordance etc
#
# Step 2:
#        Set up the meta data
#        Set up the data for SA2s included in the modelling
#        Set up the data for SA2s excluded from the modelling
#
# Step 3:
#        Loop over eight risk factors for ORs, prevalence and counts
#        Compute 2.5, 10, 20, 50, 80, 90, and 97.5 percentiles, and PP value
#        Compute log base 2 percentiles
#        Obtain values for density ("wave plot")
#
# Step 4:
#        Combine estimates with empty rows for areas excluded from modelling
#        Combine estimates with meta data
#        Combine all estimates and save as csv
#
# Notes:
#        We may need to alter the format to accommodate the downloadable data
#        
# Authors: Jamie Hogg
# Created: 04/08/23 (Based on "Compute Estimates and Wave Plot Combined" script)
# Updated: 14/08/23
#          
#==========================================================================

# Packages
library(tidyverse)
library(readr)
library(readxl)
library(sf)
library(openxlsx)
rm(list = ls())

# function to convert 9DIG to 5DIG
s9_to_s5 <- function(x){paste0(str_sub(x, 1, 1), str_sub(x, start = -4))}

## Functions ## ----------------------------------------------------------------

#source('src/wrangle/functions_ALL.R')
#source('src/wrangle/moreFuns.R')

# Load SA2 concordance file
SA2_concordance <- read_csv("data/ViseR_Input_Data/Shapefile concordance.csv")

# Load adult pop
SA2_ERP <- read_csv("data/DataLabExport/SA2_ERP.csv", col_types = cols(X1 = col_skip())) %>% 
  dplyr::select(SA2, N_persons_adults)

# Load global data
global_obj <- readRDS("data/DataLabExport/global_obj.rds")
global_obj$census <- left_join(global_obj$census, SA2_ERP)

# Fix jervis bay SA2 and get 5dig sa2s
area_concor <- global_obj$area_concor %>% 
  mutate(SA2 = ifelse(SA2 == 114011271, 901031003, SA2),
         SA2_5d = as.numeric(s9_to_s5(SA2))) %>% 
  full_join(.,SA2_concordance, by = c("SA2_5d" = "SA2_Code_Short")) %>% 
  filter(!SA2_NAME16 %in% c("Norfolk Island", "Lord Howe Island", "Christmas Island", "Cocos (Keeling) Islands"))

# Load raw estimates
raw_est <- pbapply::pblapply(list.files("data/DataLabExport", 
                                        pattern = "raw_est_*", full.names = T), readRDS)
names(raw_est) <- str_remove( 
  str_remove(
    list.files("data/DataLabExport", pattern = "raw_est_*"), "raw_est_"), ".rds")

# # load map
# map <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>% 
#   mutate(SA2_5d = s9_to_s5(SA2_MAIN16))

# SA2s not modelled
na.sa2 <- as.numeric(area_concor$SA2_5d[is.na(area_concor$ps_area)])
na.sa2name <- area_concor$SA2_NAME16[is.na(area_concor$ps_area)]

# Load all modelled estimates
summsa2all <- readRDS("data/summary_files/summsa2all.rds")

## SETUP ## --------------------------------------------------------------------

# We need to truncate the data differently for 
# relative estimates compared with modeled numbers
trunc <- 
  data.frame(
    measure = c("Relative", "Absolute"),
    lower = c(1/4, 0),
    upper = c(4, 100000),
    dp = c(2,2), # Number of decimal places for the estimates
    log_dp = c(4,2)) # Number of decimal places for the log estimates

# # get map
# sa2_name <- st_drop_geometry(map) %>% 
#   mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
#   dplyr::select(SA2, SA2_NAME)
# 
# # concordance file
# concordance <- global_obj$area_concor %>% 
#   dplyr::select(ps_area, SA2) %>% 
#   left_join(.,sa2_name)

# template for final output
final.format = data.frame(SA2_code = area_concor$SA2_5d[!is.na(area_concor$ps_area)],
                          SA2_name = area_concor$SA2_NAME16[!is.na(area_concor$ps_area)],
                          p025 = NA, # These are for downloadable data
                          p10 = NA,
                          p20 = NA,
                          p50 = NA,
                          p80 = NA,
                          p90 = NA,
                          p975 = NA, # For downloadable data
                          logp10 = NA,
                          logp20 = NA,
                          logp50 = NA,
                          logp80 = NA,
                          logp90 = NA,
                          xValues = NA,
                          yValues = NA,
                          prob = NA, # For downloadable data
                          v = NA,
                          bivariate_cat = NA
)

# Construct a similar data set for the areas excluded from modelling
na.areas = final.format[1:length(na.sa2),]
na.areas$SA2_code = na.sa2
na.areas$SA2_name = na.sa2name

# Load wave plot function
source("src/ACA_Database/getWavePlotVars2.R")

# Construct a data set with all the meta data for the different measures
# You may construct this using "expand.grid" and then
# filter out impossible combinations (eg female prostate cancer) and
# merge in the codes to match the strings
grid <- expand.grid(sub_indicator_code = paste0("00", c(0,1,2,3,4,5,6,7)),
                    measure_level_code = c("01","04","07"))
measure_level <- data.frame(measure_level_string = c("Relative ratios", "Modelled number of people", "Proportions"),
                            measure_level_code = c("01", "04", "07"),
                            measure_string = c("Relative", "Absolute", "Absolute"),
                            measure_code = as.character(c(1,2,2)))
sub_indicator <- data.frame(sub_indicator_string = c("Current smoker", "Risky alcohol consumption",
                                                     "Inadequate diet", "Overweight or obese",
                                                     "Obese", "Risky waist circumference",
                                                     "Inadequate physical activity (leisure)",
                                                     "Inadequate physical acitivty (all)"),
                            jamie_ind = c("smoking", "alcohol", "diet",
                                           "overweight", "obesity", "waist_circum",
                                           "activityleis", "activityleiswkpl"),
                            sub_indicator_code = paste0("00", c(0,1,2,3,4,5,6,7)))

# Combine all data
Ref <- grid %>% 
  left_join(.,measure_level) %>% 
  left_join(sub_indicator) %>% 
  mutate(model_string = "Spatial",
         model_code = "3",
         indicator_string = "Risk factors",
         indicator_code = "03",
         sub_sub_indicator_string = as.character(NA),
         sub_sub_indicator_code = as.character(NA),
         sex_string = "Persons",
         sex_code = "3",
         yeargrp = "2017-2018") %>% 
  relocate(model_string, model_code, measure_string,
           measure_code, measure_level_string, measure_level_code,
           indicator_string, indicator_code, sub_indicator_string,
           sub_indicator_code, sub_sub_indicator_string,
           sub_sub_indicator_code, sex_string,
           sex_code, yeargrp)

## Relative ratios #### --------------------------------------------------------

rr_estimates <- list()

for(k in 1:8){
  
  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  # Ref subset
  Ref_sub <- Ref %>% 
    filter(jamie_ind == rf,
           measure_level_code == "01")
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1_full.rds"))
  draws <- modelled_est$draws$rr # relative ratios NOT odds ratios
  temp.quant <- final.format
  
  # Fix the number of decimal places
  dp = trunc$dp[trunc$measure == "Relative"]
  log_dp = trunc$log_dp[trunc$measure == "Relative"]
  truncation = as.numeric(trunc[trunc$measure == "Relative", c("lower", "upper")])
  
  # obtain quantiles
  temp.quant$p025 = round(apply(draws, 2, quantile, 0.025), dp)
  temp.quant$p10 = round(apply(draws, 2, quantile, 0.1), dp)
  temp.quant$p20 = round(apply(draws, 2, quantile, 0.2), dp)
  temp.quant$p50 = round(apply(draws, 2, quantile, 0.5), dp)
  temp.quant$p80 = round(apply(draws, 2, quantile, 0.8), dp)
  temp.quant$p90 = round(apply(draws, 2, quantile, 0.9), dp)
  temp.quant$p975 = round(apply(draws, 2, quantile, 0.975), dp)
  
  # Obtain logged quantiles rounded to log_dp digits
  temp.quant$logp10 = round(log2(temp.quant$p10), log_dp)
  temp.quant$logp20 = round(log2(temp.quant$p20), log_dp)
  temp.quant$logp50 = round(log2(temp.quant$p50), log_dp)
  temp.quant$logp80 = round(log2(temp.quant$p80), log_dp)
  temp.quant$logp90 = round(log2(temp.quant$p90), log_dp)
  
  # obtain posterior probability and bivariate category
  temp.quant$prob = apply(draws, 2, function(x) sum(x > 1)) / nrow(draws)
  temp.quant$v = abs(2* temp.quant$prob - 1)
  temp.quant$bivariate_cat = 1 + as.numeric(temp.quant$prob >= 0.2) + as.numeric(temp.quant$prob > 0.8)
  
  # Truncate quantiles and logged quantiles so all values are between 1/4 and 4 (for atlas only, not DLD)
  for (col in paste0("p", c(10, 20, 50, 80, 90))) {
    temp.quant[which(temp.quant[,col] < truncation[1], arr.ind = TRUE), col] <- truncation[1]
    temp.quant[which(temp.quant[,col] > truncation[2], arr.ind = TRUE), col] <- truncation[2]
    
    col2 = paste0("log", col)
    temp.quant[which(temp.quant[,col2] < log2(truncation[1]), arr.ind = TRUE), col2] <- log2(truncation[1])
    temp.quant[which(temp.quant[,col2] > log2(truncation[2]), arr.ind = TRUE), col2] <- log2(truncation[2])
  }
  
  # Get points for re-constructing density (wave) plot of log estimates
  temp.wave <-
    do.call("rbind",
            lapply(1:ncol(draws),
                   function(x, draws) {
                     getWavePlotVars(draws[,x],
                                     truncation = truncation)
                   },
                   draws = draws)
    )
  
  temp.quant$xValues = temp.wave$xValues
  temp.quant$yValues = temp.wave$yValues
  
  remove(draws)
  gc()
  
  # Add in rows for areas excluded from the modelling
  temp.quant <- 
    rbind(temp.quant, 
          na.areas) %>%
    arrange(SA2_code)
  
  # Produce replicates of the meta data, one for each SA2
  temp.ref <-
    do.call("rbind",
            lapply(1:nrow(temp.quant),
                   function(x)
                     Ref_sub
            ))
  
  # Store estimates in a list of data frames
  rr_estimates[[k]] <- 
    cbind(
      temp.ref,
      temp.quant
    ) %>% 
    dplyr::select(-jamie_ind)
  
}

rr_estimates_all <- do.call("rbind", rr_estimates)

## FINISH RR ## ----------------------------------------------------------------

## Proportions #### ------------------------------------------------------------

prop_estimates <- list()

for(k in 1:8){
  
  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  # Ref subset
  Ref_sub <- Ref %>% 
    filter(jamie_ind == rf,
           measure_level_code == "07")
  
  # load data
  modelled_est <- readRDS(file = paste0("data/DataLabExport/modelled_est_", rf, ".rds"))
  draws <- modelled_est$mu # prevalence
  temp.quant <- final.format
  
  # Fix the number of decimal places
  dp = trunc$dp[trunc$measure == "Absolute"]
  log_dp = trunc$log_dp[trunc$measure == "Absolute"]
  truncation = as.numeric(trunc[trunc$measure == "Absolute", c("lower", "upper")])
  
  # obtain quantiles
  temp.quant$p025 = round(apply(draws, 2, quantile, 0.025), dp)
  temp.quant$p10 = round(apply(draws, 2, quantile, 0.1), dp)
  temp.quant$p20 = round(apply(draws, 2, quantile, 0.2), dp)
  temp.quant$p50 = round(apply(draws, 2, quantile, 0.5), dp)
  temp.quant$p80 = round(apply(draws, 2, quantile, 0.8), dp)
  temp.quant$p90 = round(apply(draws, 2, quantile, 0.9), dp)
  temp.quant$p975 = round(apply(draws, 2, quantile, 0.975), dp)
  
  # Obtain logged quantiles rounded to log_dp digits
  temp.quant$logp10 = round(log2(temp.quant$p10), log_dp)
  temp.quant$logp20 = round(log2(temp.quant$p20), log_dp)
  temp.quant$logp50 = round(log2(temp.quant$p50), log_dp)
  temp.quant$logp80 = round(log2(temp.quant$p80), log_dp)
  temp.quant$logp90 = round(log2(temp.quant$p90), log_dp)
  
  # Truncate quantiles and logged quantiles so all values are between 1/4 and 4 (for atlas only, not DLD)
  for (col in paste0("p", c(10, 20, 50, 80, 90))) {
    temp.quant[which(temp.quant[,col] < truncation[1], arr.ind = TRUE), col] <- truncation[1]
    temp.quant[which(temp.quant[,col] > truncation[2], arr.ind = TRUE), col] <- truncation[2]
    
    col2 = paste0("log", col)
    temp.quant[which(temp.quant[,col2] < log2(truncation[1]), arr.ind = TRUE), col2] <- log2(truncation[1])
    temp.quant[which(temp.quant[,col2] > log2(truncation[2]), arr.ind = TRUE), col2] <- log2(truncation[2])
  }
  
  # Get points for re-constructing density (wave) plot of log estimates
  temp.wave <-
    do.call("rbind",
            lapply(1:ncol(draws),
                   function(x, draws) {
                     getWavePlotVars(draws[,x],
                                     truncation = truncation)
                   },
                   draws = draws)
    )
  
  temp.quant$xValues = temp.wave$xValues
  temp.quant$yValues = temp.wave$yValues
  
  remove(draws)
  gc()
  
  # Add in rows for areas excluded from the modelling
  temp.quant <- 
    rbind(temp.quant, 
          na.areas) %>%
    arrange(SA2_code)
  
  # Produce replicates of the meta data, one for each SA2
  temp.ref <-
    do.call("rbind",
            lapply(1:nrow(temp.quant),
                   function(x)
                     Ref_sub
            ))
  
  # Store estimates in a list of data frames
  prop_estimates[[k]] <- 
    cbind(
      temp.ref,
      temp.quant
    ) %>% 
    dplyr::select(-jamie_ind)
  
}

prop_estimates_all <- do.call("rbind", prop_estimates)

## FINISH prop ## --------------------------------------------------------------

## Counts #### -----------------------------------------------------------------

count_estimates <- list()
nat_count <- list()

for(k in 1:8){
  
  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  # Ref subset
  Ref_sub <- Ref %>% 
    filter(jamie_ind == rf,
           measure_level_code == "04")
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1_full.rds"))
  draws <- modelled_est$draws$count # count
  temp.quant <- final.format
  
  # Get posterior median national count
  nat_count[[k]] <- median(apply(draws, 1, sum))
  
  # Fix the number of decimal places
  dp = trunc$dp[trunc$measure == "Absolute"]
  log_dp = trunc$log_dp[trunc$measure == "Absolute"]
  truncation = as.numeric(trunc[trunc$measure == "Absolute", c("lower", "upper")])
  
  # obtain quantiles
  temp.quant$p025 = round(apply(draws, 2, quantile, 0.025), dp)
  temp.quant$p10 = round(apply(draws, 2, quantile, 0.1), dp)
  temp.quant$p20 = round(apply(draws, 2, quantile, 0.2), dp)
  temp.quant$p50 = round(apply(draws, 2, quantile, 0.5), dp)
  temp.quant$p80 = round(apply(draws, 2, quantile, 0.8), dp)
  temp.quant$p90 = round(apply(draws, 2, quantile, 0.9), dp)
  temp.quant$p975 = round(apply(draws, 2, quantile, 0.975), dp)
  
  # Obtain logged quantiles rounded to log_dp digits
  temp.quant$logp10 = round(log2(temp.quant$p10), log_dp)
  temp.quant$logp20 = round(log2(temp.quant$p20), log_dp)
  temp.quant$logp50 = round(log2(temp.quant$p50), log_dp)
  temp.quant$logp80 = round(log2(temp.quant$p80), log_dp)
  temp.quant$logp90 = round(log2(temp.quant$p90), log_dp)
  
  # Truncate quantiles and logged quantiles so all values are between 1/4 and 4 (for atlas only, not DLD)
  for (col in paste0("p", c(10, 20, 50, 80, 90))) {
    temp.quant[which(temp.quant[,col] < truncation[1], arr.ind = TRUE), col] <- truncation[1]
    temp.quant[which(temp.quant[,col] > truncation[2], arr.ind = TRUE), col] <- truncation[2]
    
    col2 = paste0("log", col)
    temp.quant[which(temp.quant[,col2] < log2(truncation[1]), arr.ind = TRUE), col2] <- log2(truncation[1])
    temp.quant[which(temp.quant[,col2] > log2(truncation[2]), arr.ind = TRUE), col2] <- log2(truncation[2])
  }
  
  # Get points for re-constructing density (wave) plot of log estimates
  temp.wave <-
    do.call("rbind",
            lapply(1:ncol(draws),
                   function(x, draws) {
                     getWavePlotVars(draws[,x],
                                     truncation = truncation)
                   },
                   draws = draws)
    )
  
  temp.quant$xValues = temp.wave$xValues
  temp.quant$yValues = temp.wave$yValues
  
  remove(draws)
  gc()
  
  # Add in rows for areas excluded from the modelling
  temp.quant <- 
    rbind(temp.quant, 
          na.areas) %>%
    arrange(SA2_code)
  
  # Produce replicates of the meta data, one for each SA2
  temp.ref <-
    do.call("rbind",
            lapply(1:nrow(temp.quant),
                   function(x)
                     Ref_sub
            ))
  
  # Store estimates in a list of data frames
  count_estimates[[k]] <- 
    cbind(
      temp.ref,
      temp.quant
    ) %>% 
    dplyr::select(-jamie_ind)
  
}

count_estimates_all <- do.call("rbind", count_estimates)
names(nat_count) <- names(raw_est)

## FINISH count ## -------------------------------------------------------------

# --------------------------------
# Final format and export
# --------------------------------

estimates = do.call("rbind", list(rr_estimates_all, prop_estimates_all, count_estimates_all))

# Remove row headings
row.names(estimates) <- NULL

# Export
write.csv(estimates, file = "src/ACA_Database/RiskFactor estimates for ViseR.csv", row.names = FALSE)

## National estimates ## -------------------------------------------------------

sub_indicator <- data.frame(sub_indicator_string = c("Current smoker", "Risky alcohol consumption",
                                                     "Inadequate diet", "Overweight or obese",
                                                     "Obese", "Risky waist circumference",
                                                     "Inadequate physical activity (leisure)",
                                                     "Inadequate physical acitivty (all)"),
                            jamie_ind = c("smoking", "alcohol", "diet",
                                          "overweight", "obesity", "waist_circum",
                                          "activityleis", "activityleiswkpl"),
                            sub_indicator_code = paste0("00", c(0,1,2,3,4,5,6,7)),
                            newCasesPerYear = rep(0.0, 8),
                            #summedCounts = rep(0.0, 8),
                            ratePer100k = rep(0.0, 8))

# Loop over raw estimates 
for(i in 1:nrow(sub_indicator)){
  if(sub_indicator$jamie_ind[i] == "waist_circum"){
    tot_pop <- sum(global_obj$census$N_persons_adults)
    #sub_indicator$summedCounts[i] = nat_count[[which(names(nat_count)==sub_indicator$jamie_ind[i])]]
    sub_indicator$newCasesPerYear[i] <- raw_est[[sub_indicator$jamie_ind[i]]]$national[1]*tot_pop
    sub_indicator$ratePer100k[i] <- raw_est[[sub_indicator$jamie_ind[i]]]$national[1] * 100000
  }else{
    tot_pop <- sum(global_obj$census$N_persons)
    #sub_indicator$summedCounts[i] = nat_count[[which(names(nat_count)==sub_indicator$jamie_ind[i])]]
    sub_indicator$newCasesPerYear[i] <- raw_est[[sub_indicator$jamie_ind[i]]]$national[1]*tot_pop
    sub_indicator$ratePer100k[i] <- raw_est[[sub_indicator$jamie_ind[i]]]$national[1] * 100000
  }
}

# Add other columns and arrange
Ref_national <- sub_indicator %>% 
  dplyr::select(-jamie_ind) %>% 
  mutate(model_string = "Spatial",
         model_code = "3",
         indicator_string = "Risk factors",
         indicator_code = "03",
         measure_level_string = "Modelled number of people",
         measure_level_code = "04",
         measure_code = "2",
         measure_string = "Absolute",
         sub_sub_indicator_string = as.character(NA),
         sub_sub_indicator_code = as.character(NA),
         sex_string = "Persons",
         sex_code = "3",
         yeargrp = "2017-2018",
         p50 = NA,
         logp50	= NA,
         meet = NA) %>% 
  relocate(model_string, model_code, 
           measure_string, measure_code, 
           measure_level_string, measure_level_code,
           indicator_string, indicator_code, sub_indicator_string,
           sub_indicator_code, sub_sub_indicator_string,
           sub_sub_indicator_code, sex_string,
           sex_code, yeargrp)

# Remove row headings
row.names(Ref_national) <- NULL

# Export
write.csv(Ref_national, file = "src/ACA_Database/RiskFactor Aus for ViseR.csv", row.names = FALSE)

## SCRIPT END ## ---------------------------------------------------------------