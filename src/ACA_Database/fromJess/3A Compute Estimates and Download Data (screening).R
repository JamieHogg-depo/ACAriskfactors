#==========================================================================
# Format data for Atlas database
#
# Step 1:
#        Set the directories
#        Specify how the data should be truncated (both in magnitude and in precision)
#        Should the modelled numbers be per study period or divide by number of years to obtain annual counts
#        Load concordance etc
#
# Step 2:
#        Set up the meta data
#        Set up the data for SA2s included in the modelling
#        Set up the data for SA2s excluded from the modelling
#
# Step 3:
#        Loop over different combinations of measures
#        Compute 2.5, 10, 20, 50, 80, 90, and 97.5 percentiles, and PP value for each Cancer-sex-SA2
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
# Authors: Jess Cameron, Jamie Hogg, Earl Duncan
# Created: 04/08/23 (Based on "Compute Estimates and Wave Plot Combined" script)
# Updated: 
#          
#==========================================================================

# Load packages
library(dplyr)
# library(openxlsx) # Currently need to save as xlsx to retain leading "0"s in codes


# Thanks to Jamie for packaging up the code for obtaining the wave plot data
source("G:/Screening/BreastScreen/Programs/2016 ASGS/getWavePlotVars2.R")

# Set filepaths
fp.root <- "G:/Screening/BreastScreen/"
fp.wd <- paste0(fp.root, "/Results/2016 ASGS/Export Data")
if (!dir.exists(fp.wd)) dir.create(fp.wd)
setwd(fp.wd)

# Edit this function so that you can load the MCMC
fp.data <- function(x) paste0(fp.root, "/Results/2016 ASGS/MCMC Output/MCMC BreastScreen_BYM_201920_", x, ".Rdata")
# Only required if calculating estimates for non-participants
fp.pop <- "G:/Screening/BreastScreen/Data/2-year/Eligible_population_BreastScreen_1920.Rdata"

# Length of study period
# This is to obtain annual counts if multiple years were used in the model
number.of.years = 1 # Currently thinking is to provide counts by study period


# We need to truncate the data differently for 
# relative estimates compared with modeled numbers
trunc <- 
  data.frame(
    measure = c("Relative", "Absolute"),
    lower = c(1/4, 0),
    upper = c(4, 100000),
    dp = c(2,2), # Number of decimal places for the estimates
    log_dp = c(4,2)) # Number of decimal places for the log estimates



# The concordance file will make sure we have
# a  row for all SA2s - even SA2s excluded from the modelling
# Viser need data for all SA2s EXCEPT remote islands

# Read in concordance data for BreastScreen data - use the concordance used for your modelling
load(paste0(fp.root, "Geography/Adj_matrix_concordance_BreastScreen.Rdata"))

# Remove remote islands
concordance <- subset(concordance, 
                      !grepl("Lord Howe|Norfolk Is|Cocos|Christmas Is", SA2_Name))
stopifnot(2288 == nrow(concordance))

#################################################################################
# 
# sTEP 2
# Construct data frames with all the combinations of data sets to be included
# in the final csv
# 
#################################################################################

# For one type of screening, we don't need to loop,
# but I've retained the code below because it may be useful in the future
# Note that the data set to be sent to Viser will need a 0 in front of the site10group codes

# # Define cancer and sex codes
# Ref.cancer <- data.frame(
#     code = c(11, 12, 14, 18, 20, 23, 27, 29, 33, 35, 36, 37, 39, 42, 43, 45, 48,
#              53, 54, 60, 64, 69, 71),
#     value = c("Oesophageal", "Stomach", "Colorectal", "Liver", "Pancreatic",
#               "Lung", "Melanoma", "Mesothelioma", "Breast", "Cervical", "Uterine", 
#               "Ovarian", "Prostate", "Kidney", "Bladder", "Brain", "Thyroid",
#               "Non-Hodgkin lymphoma", "Leukaemia", "Myeloma", "All Invasive", 
#               "Myeloproliferative neoplasms", "Head and neck")
# )
# 
# Ref.sex <- data.frame(
#     code = c(1, 2, 3),
#     value = c("Males", "Females", "Persons")
# )
# 
# row.names(Ref.cancer) <- NULL

# Construct a data set with all the meta data for the different measures
# You may construct this using "expand.grid" and then
# filter out impossible combinations (eg female prostate cancer) and
# merge in the codes to match the strings
Ref <- data.frame(model_string = "Spatial",
                  model_code = 3,
                  measure_string = c("Relative", "Absolute", "Absolute"),
                  measure_code = c(1,2,2),
                  measure_level_string = c("Relative ratios", "Modelled number of participants", "Modelled number of non-participants"),
                  measure_level_code = c("01", "04", "06"),
                  indicator_string = "Screening",
                  indicator_code = "04",
                  sub_indicator_string = "Breast cancer",
                  sub_indicator_code = "033",
                  sub_sub_indicator_string = NA,
                  sub_sub_indicator_code = NA,
                  sex_string = "Females",
                  sex_code = 2,
                  yeargrp = "2019-2020")

# Construct a data set for the estimates
# IMPORTANT:
# ensure SA2_code and SA2_name are in the same order as your data / adjacency matrix
final.format = data.frame(SA2_code = concordance$SA2_Code_Short[!is.na(concordance$ID_2190)],
                          SA2_name = concordance$SA2_Name[!is.na(concordance$ID_2190)],
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
na.areas = final.format[1:sum(is.na(concordance$ID_2190)), ]
na.areas$SA2_code = concordance$SA2_Code_Short[is.na(concordance$ID_2190)]
na.areas$SA2_name = concordance$SA2_Name[is.na(concordance$ID_2190)]

# Set up an object to take all the data sets
# With each data set corresponding to a row of Ref
estimates = vector("list", nrow(Ref))


#==========================================================================
# LOOP OVER DIFFERENT MEASURES
#==========================================================================


for (idx in 1:nrow(Ref)) {
  
  # sex.code = Ref$sex_code[idx]
  
  cat("Row ", idx, "\n", "\n")
  
  # Initialise the data set
  temp.quant <- final.format
  
  # The name of the measure
  iter = gsub("non-", "", Ref$measure_level_string[idx])
  
  # Fix the number of decimal places
  dp = trunc$dp[trunc$measure == Ref$measure_string[idx]]
  log_dp = trunc$log_dp[trunc$measure == Ref$measure_string[idx]]
  
  # Define how the data are truncated based on the type of data
  truncation = 
    as.numeric(
      trunc[grep(Ref$measure_string[idx], trunc$measure), 
            c("lower", "upper")])
  
  # If the MCMC data don't exist, skip this combination
  if (!file.exists(fp.data(iter))) {
    message("File not found. Skipping this combination.")
    
    # Non-participants must be calculated after calculating participants
  } else if (!grepl("number of non-participants", Ref$measure_level_string[idx])) {
    
    # Load MCMC data
    # This object is a matrix of area-level MCMC estimates
    draws <- get(load(fp.data(iter)))
    rm(list = ls(pattern = "SPR|count.data"))
    
    # To edit the code for loading a CARBayes object:
    # model <- get(load(fp.data(iter)))
    # draws <- model$samples$fitted
    # or 
    # draws <- exp(model$samples$phi + as.vector(model$samples$beta))
    # remove(model)
    # gc()
    
    # If the data are modelled counts, obtain counts for the specified period
    if (grepl(iter, "Modelled number")) draw = draws / number.of.years
    
    # Obtain quantiles
    temp.quant$p025 = apply(draws, 2, quantile, 0.025)
    temp.quant$p10 = apply(draws, 2, quantile, 0.1)
    temp.quant$p20 = apply(draws, 2, quantile, 0.2)
    temp.quant$p50 = apply(draws, 2, quantile, 0.5)
    temp.quant$p80 = apply(draws, 2, quantile, 0.8)
    temp.quant$p90 = apply(draws, 2, quantile, 0.9)
    temp.quant$p975 = apply(draws, 2, quantile, 0.975)
    
    # Obtain logged quantiles rounded to log_dp digits
    temp.quant$logp10 = round(log2(temp.quant$p10), log_dp)
    temp.quant$logp20 = round(log2(temp.quant$p20), log_dp)
    temp.quant$logp50 = round(log2(temp.quant$p50), log_dp)
    temp.quant$logp80 = round(log2(temp.quant$p80), log_dp)
    temp.quant$logp90 = round(log2(temp.quant$p90), log_dp)
    
    # Obtain posterior probabilities and bivariate category
    
    if (grepl("Modelled number", iter)) {
      temp.quant$prob = apply(draws, 2, function(x) sum(x > 1)) / nrow(draws)
      temp.quant$v = abs(2* temp.quant$prob - 1)
      temp.quant$bivariate_cat = 1 + as.numeric(temp.quant$prob >= 0.2) + as.numeric(temp.quant$prob > 0.8)
    }
    
    # Round estimates
    temp.quant$p025 = round(temp.quant$p025, dp)
    temp.quant$p10 = round(temp.quant$p10, dp)
    temp.quant$p20 = round(temp.quant$p20, dp)
    temp.quant$p50 = round(temp.quant$p50, dp)
    temp.quant$p80 = round(temp.quant$p80, dp)
    temp.quant$p90 = round(temp.quant$p90, dp)
    temp.quant$p975 = round(temp.quant$p975, dp)
    
    
    # Truncate quantiles and logged quantiles so all values are between 1/4 and 4 (for atlas only, not DLD)
    for (col in paste0("p", c(10, 20, 50, 80, 90))) {
      temp.quant[which(temp.quant[,col] < truncation[1], arr.ind = TRUE), col] <- truncation[1]
      temp.quant[which(temp.quant[,col] > truncation[2], arr.ind = TRUE), col] <- truncation[2]
      
      col2 = paste0("log", col)
      temp.quant[which(temp.quant[,col2] < log2(truncation[1]), arr.ind = TRUE), col2] <- log2(truncation[1])
      temp.quant[which(temp.quant[,col2] > log2(truncation[2]), arr.ind = TRUE), col2] <- log2(truncation[2])
    }
    
  } else {
    if (is.null(estimates[[grep("Modelled number of participants", Ref$measure_level_string)]])) {
      message("Please run 'Modelled number of non-participants' after 'Modelled number of non-participants'. Skipping this combination.")
    } else {
      load(fp.pop)
      
      # Obtain data for number of participants
      temp.part <- subset(estimates[[grep("Modelled number of participants", Ref$measure_level_string)]],
                          SA2_code %in% concordance$SA2_Code_Short[!is.na(concordance$ID_2190)])
      
      # Join participant data with eligible population
      temp.part <- full_join(temp.part, eligible.pop, by = c("SA2_code" = "sa2"))
      remove(eligible.pop)
      
      # Obtain estimates for non-participants
      for (cols in grep("^p[[:digit:]]+$", colnames(temp.quant), value=T)) {
        col2 = gsub("p", "", cols)
        if (!(col2 %in% c("025", "975"))) col2 = paste0(col2, 0)
        col2 = 100 - as.numeric(col2)/10
        if (grepl("\\.", col2)) col2 = 10 * col2
        if (col2 == "25") col2 = paste0(0, col2)
        if (any(temp.part$pop - temp.part[,paste0("p", col2)] < 0)) message("Check estimates of non-participants. Some numbers were sub-zero")
        temp.quant[,cols] = max(round(temp.part$pop - temp.part[,paste0("p", col2)], dp), 0)
      }
      
      # Obtain log2 estimates for non-participants
      for (cols in grep("^logp[[:digit:]]+$", colnames(temp.quant), value=T)) {
        col2 = gsub("logp", "", cols)
        if (!(col2 %in% c("025", "975"))) col2 = paste0(col2, 0)
        col2 = 100 - as.numeric(col2)/10
        if (grepl("\\.", col2)) col2 = 10 * col2
        if (col2 == "25") col2 = paste0(0, col2)
        temp.quant[,cols] = round(log2(max(temp.part$pop - temp.part[,paste0("p", col2)], 0.001)), log_dp)
      }
      
      # Load MCMC
      draws <- get(load(fp.data(iter)))
      rm(list = ls(pattern = "SPR|count.data"))
    }
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
                     Ref[idx,]
            ))
  
  # Store estimates in a list of data frames
  estimates[[idx]] <- 
    cbind(
      temp.ref,
      temp.quant
    )
  
}


# --------------------------------
# Final format and export
# --------------------------------

estimates = do.call("rbind", estimates)

# Remove row headings
row.names(estimates) <- NULL

# Export
write.csv(estimates, file = "BreastScreen estimates for ViseR.csv", row.names = FALSE)

# EOF

