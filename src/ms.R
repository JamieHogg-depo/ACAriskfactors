## -----------------------------------------------------------------------------
## Setup ## --------------------------------------------------------------------
## -----------------------------------------------------------------------------

# Packages
library(tidyverse)
#library(scales)
library(sf)
library(MASS)
library(patchwork)
library(readr)
library(readxl)
library(grid)
library(gridExtra)
library(Matrix)
library(spdep)
rm(list = ls())

base_folder <- "C:/r_proj/ACAriskfactors/out"

## Functions ## ----------------------------------------------------------------

source('src/wrangle/functions_ALL.R')
source('src/wrangle/moreFuns.R')

## Load adult population ## ----------------------------------------------------

#source("src/wrangle/getAdultPopulation.R")
SA2_ERP <- read_csv("data/DataLabExport/SA2_ERP.csv", col_types = cols(X1 = col_skip())) %>% 
  dplyr::select(SA2, N_persons_adults)

## Load Data ## ----------------------------------------------------------------

# Load global data
global_obj <- readRDS("data/DataLabExport/global_obj.rds")
global_obj$census <- left_join(global_obj$census, SA2_ERP)
listw <- spdep::mat2listw(global_obj$W)

# Load raw estimates
raw_est <- pbapply::pblapply(list.files("data/DataLabExport", 
                             pattern = "raw_est_*", full.names = T), readRDS)
names(raw_est) <- str_remove( 
  str_remove(
    list.files("data/DataLabExport", pattern = "raw_est_*"), "raw_est_"), ".rds")

# Load all modelled estimates
summsa2all <- readRDS("data/summary_files/summsa2all.rds")
summphaall <- readRDS("data/summary_files/summphaall.rds")

# Model building
model_building <- lapply(list.files("data/DataLabExport", 
                             pattern = "model_building_*", full.names = T), readRDS)
names(model_building) <- str_remove( 
  str_remove(
    list.files("data/DataLabExport", pattern = "model_building_*"), "model_building_"), ".rds")

# Load map
map_sa2_full <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>%
  filter(!str_detect(SA2_NAME, "Island")) %>%
  filter(STATE_NAME != "Other Territories")

# keep non-estimated geometries
map_sa2 <- map_sa2_full %>%
  left_join(.,global_obj$area_concor, by = "SA2")

# Australia outline
aus_border <- suppressMessages(map_sa2 %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_as_sf() %>%
  st_transform(4326))

# State outline
state_border <- suppressMessages(map_sa2 %>% 
  mutate(state = str_sub(SA2, 1, 1)) %>% 
  group_by(state, STATE_NAME) %>% 
  summarise(geometry = st_union(geometry), .groups = "drop") %>% 
  filter(!st_is_empty(.)) %>% 
  #mutate(st_init = c("NSW", "VIC", "QLD", "SA", "WA", NA, "NT", NA)) %>% 
  st_as_sf() %>%
  st_transform(4326))

## Other code ## --------------------------------------------------------------

# City Insets 
lims <- data.frame(
  xmin = c(152.6, 150.35, 144.5, 115.45, 138.1, 146.8, 148.6, 130.3),
  xmax = c(153.6, 151.35, 145.5, 116.45, 139.1, 147.8, 149.6, 131.3),
  ymin = -c(28, 34.4, 38.4, 32.5, 35.4, 43.4, 35.8, 13),
  ymax = -c(27, 33.4, 37.4, 31.5, 34.4, 42.4, 34.8, 12),
  city = c("Brisbane", "Sydney", "Melbourne", "Perth", "Adelaide", "Hobart", "Canberra", "Darwin"),
  position = c("r", "r", "b", "l", "b", "b", "r", "l"),
  inset_labs = c("B - Brisbane (Qld)", "S - Sydney (NSW)",
                 "M - Melbourne (Vic)", "P - Perth (WA)",
                 "A - Adelaide (SA)", "Ho - Hobart (Tas)",
                 "C - Canberra (ACT)", "D - Darwin (NT)"),
  jitter = c(1,1,1,1,1,1,2,1)
) %>% 
  mutate(initials = c("B", "S", "M", "P", "A", "Ho", "C", "D"))

# quantiles for IRSD
irsd_5c <- mutate(global_obj$census, 
       irsd_5c = case_when(
         ABS_irsd_decile_nation_complete %in% c("1", "2") ~ "1 - most\ndisadvantaged",
         ABS_irsd_decile_nation_complete %in% c("3", "4") ~ "2",
         ABS_irsd_decile_nation_complete %in% c("5", "6") ~ "3",
         ABS_irsd_decile_nation_complete %in% c("7", "8") ~ "4",
         ABS_irsd_decile_nation_complete %in% c("9", "10") ~ "5 - least\ndisadvantaged"
       )) %>% 
  dplyr::select(ps_area, irsd_5c)

# Full names
lookup <- data.frame(rf = names(raw_est),
                     rf_full = c("Leisure physical activity",
                                 "All physical activity",
                                 "Alcohol",
                                 "Diet",
                                 "Obesity",
                                 "Overweight",
                                 "Current smoking",
                                 "Risky waist circumference"))

# Full names2
lookup <- data.frame(rf = names(raw_est),
                     sha = c("exercise", "exercise", "alcohol", 
                             "fruit", "obese", "overweight",
                             "smoking", "overweight"),
                     rf_full = c("Inadequate physical activity (leisure)",
                                 "Inadequate physical activity (all)",
                                 "Risky alcohol consumption",
                                 "Inadequate diet",
                                 "Obese",
                                 "Overweight/obese",
                                 "Current smoking",
                                 "Risky waist circumference"))

## Load PHA SHA data ## --------------------------------------------------------

source("src/wrangle/getSHA.R")

## Load census data ## ---------------------------------------------------------

source("src/wrangle/loadCensusData.R")

## To cut ## -------------------------------------------------------------------

s9_to_s5 <- function(x){paste0(str_sub(x, 1, 1), str_sub(x, start = -4))}
flt <- filter(summsa2all, rr_CV_b > 50 | N_persons < 100)
SA2_to_suppress <- split(flt$SA2, flt$model)
SA2_to_suppress <- lapply(SA2_to_suppress, s9_to_s5)
#saveRDS(SA2_to_suppress, "data/ViseR_Input_Data/SA2_to_suppress.rds")

## END SCRIPT ## --------------------------------------------------------------
