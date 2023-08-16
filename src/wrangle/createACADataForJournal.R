##
# Create data for Journal page
##

source('src/ms.R')

# Packages
library(readr)
library(readxl)
library(openxlsx)

# create excel
summsa2all %>% 
  # fix jervis bay
  mutate(SA2 = ifelse(SA2 == 114011271, 901031003, SA2)) %>% 
  dplyr::select(SA2, model, contains(c("mu_", "or_"))) %>% 
  dplyr::select(-contains("logor_")) %>% 
  rename(risk_factor = model,
         SA2_2016 = SA2) %>% 
  mutate(risk_factor = case_when(
    risk_factor == "activityleis" ~ "Inadequate physical activity (leisure)",
    risk_factor == "activityleiswkpl" ~ "Inadequate physical activity (all)",
    risk_factor == "alcohol" ~ "Risky alcohol consumption",
    risk_factor == "diet" ~ "Inadequate diet",
    risk_factor == "obesity" ~ "Obese",
    risk_factor == "overweight" ~ "Overweight/obese",
    risk_factor == "smoking" ~ "Current smoking",
    risk_factor == "waist_circum" ~ "Risky waist circumference"
  )) %>% 
  write.csv(., "ModelledEstimates.csv")

## END SCRIPT #### -------------------------------------------------------------