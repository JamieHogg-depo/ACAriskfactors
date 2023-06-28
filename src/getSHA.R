##
# Get SHA estimates by PHA
##

# functions
foo <- function(x){as.numeric(ifelse(x == "..", "", x))/100}

## PHD IDs ## ------------------------------------------------------------------

ids <- read_excel(path = "data/phidu_data_phn_pha_parts_aust.xlsx",
                         sheet = "Estimates_risk_factors_adults",
                         range = "A5:A1225") %>% 
  setNames(c("pha"))

# which rows to keep
id_keep <- which(!str_starts(ids$pha, "PHN"))

## Risk factors ## -------------------------------------------------------------

# overweight
overweight <- read_excel(path = "data/phidu_data_phn_pha_parts_aust.xlsx",
                         sheet = "Estimates_risk_factors_adults",
                         range = "BX5:BZ1225") %>% 
  setNames(paste0("shaout_overweight_", c("estimate", "lower", "upper"))) %>% 
  mutate_all(foo) %>% 
  slice(id_keep)

# obese
obese <- read_excel(path = "data/phidu_data_phn_pha_parts_aust.xlsx",
                         sheet = "Estimates_risk_factors_adults",
                         range = "CG5:CI1225") %>% 
  setNames(paste0("shaout_obese_", c("estimate", "lower", "upper"))) %>% 
  mutate_all(foo) %>% 
  slice(id_keep)

# smoking
smoking <- read_excel(path = "data/phidu_data_phn_pha_parts_aust.xlsx",
                         sheet = "Estimates_risk_factors_adults",
                         range = "DH5:DJ1225") %>% 
  setNames(paste0("shaout_smoking_", c("estimate", "lower", "upper"))) %>% 
  mutate_all(foo) %>% 
  slice(id_keep)

# alcohol
alcohol <- read_excel(path = "data/phidu_data_phn_pha_parts_aust.xlsx",
                         sheet = "Estimates_risk_factors_adults",
                         range = "EI5:EK1225") %>% 
  setNames(paste0("shaout_alcohol_", c("estimate", "lower", "upper"))) %>% 
  mutate_all(foo) %>% 
  slice(id_keep)

# fruit
fruit <- read_excel(path = "data/phidu_data_phn_pha_parts_aust.xlsx",
                         sheet = "Estimates_risk_factors_adults",
                         range = "ER5:ET1225") %>% 
  setNames(paste0("shaout_fruit_", c("estimate", "lower", "upper"))) %>% 
  mutate_all(foo) %>% 
  slice(id_keep)

# exercise
exercise <- read_excel(path = "data/phidu_data_phn_pha_parts_aust.xlsx",
                         sheet = "Estimates_risk_factors_adults",
                         range = "FA5:FC1225") %>% 
  setNames(paste0("shaout_exercise_", c("estimate", "lower", "upper"))) %>% 
  mutate_all(foo) %>% 
  slice(id_keep)

## Join datasets ## ------------------------------------------------------------

SHA_pha <- list(ids[id_keep,], exercise, fruit, alcohol, smoking, overweight, obese) %>% 
  reduce(bind_cols)

# cleanup
rm(ids, exercise, fruit, alcohol, smoking, overweight, obese, foo, id_keep)
