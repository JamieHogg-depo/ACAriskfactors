## -----------------------------------------------------------------------------
## Setup ## --------------------------------------------------------------------
## -----------------------------------------------------------------------------

# Packages
library(tidyverse)
library(scales)
library(sf)
library(MASS)
library(patchwork)
library(readr)
library(readxl)
library(gridExtra)
rm(list = ls())

base_folder <- "C:/r_proj/ACAriskfactors/out"
export <- FALSE

## Functions ## ----------------------------------------------------------------

source('src/functions_ALL.R')

jsave <- function(filename, base_folder, 
                  plot = last_plot(), 
                  square = T, square_size = 5000, 
                  ratio = c(6,9)){
  if(square){
    ggsave(filename = filename,
           plot = plot,
           path = base_folder,
           dpi = 1000,
           width = square_size,
           height = square_size,
           scale = 1,
           units = "px")
  }else{
    total = square_size^2
    a <- sqrt((total*ratio[1])/ratio[2])
    b <- (ratio[2]*a)/ratio[1]
    ggsave(filename = filename,
           plot = plot, 
           path = base_folder,
           dpi = 1000,
           width = round(b),
           height = round(a),
           scale = 1,
           units = "px")
  }
}


make_numeric_decimal <- function(.data, digits = 2){
  df <- .data
  cols_to_format <- unlist(lapply(df, is.numeric))
  df[,cols_to_format] <- bind_cols(lapply(df[,cols_to_format], sprintf, fmt = paste0('%#.', digits, 'f')))
  return(df)
}

addBoxLabel <- function(i, color = "white", size = 0.5){
  if(lims$position[i] == "r"){
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", y = mean(c(lims$ymin[i], lims$ymax[i])), 
               x = lims$xmax[i] + 1, label = lims$initials[i],
               size = 3) 
    )
  } else if(lims$position[i] == "b"){
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", x = mean(c(lims$xmin[i], lims$xmax[i])), 
               y = lims$ymin[i] - 1, label = lims$initials[i],
               size = 3) 
    )
  } else if(lims$position[i] == "l"){
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", y = mean(c(lims$ymin[i], lims$ymax[i])), 
               x = lims$xmin[i] - 1, label = lims$initials[i],
               size = 3) 
    )
  }else{
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", x = mean(c(lims$xmin[i], lims$xmax[i])), 
               y = lims$ymax[i] + 1, label = lims$initials[i],
               size = 3) 
    )
  }
}

## Load Data ## ----------------------------------------------------------------

# state names
state_name_concor <- data.frame(ps_state = 1:8,
                                state_name_short = c("NSW", "VIC", "QLD",
                                                     "SA", "WA", "TAS",
                                                     "NT", "ACT"),
                                state_name = c("New South Wales",
                                               "Victoria",
                                               "Queensland",
                                               "South Australia",
                                               "Western Australia",
                                               "Tasmania",
                                               "Northern Territory",
                                               "Australian Capital Territory"))

# Load global data
global_obj <- readRDS("data/2021-033_o_0002a_cleared/global_obj.rds")

# Load raw estimates
raw_est <- lapply(list.files("data/2021-033_o_0002a_cleared", 
                             pattern = "raw_est_*", full.names = T), readRDS)
names(raw_est) <- str_remove( 
  str_remove(
    list.files("data/2021-033_o_0002a_cleared", pattern = "raw_est_*"), "raw_est_"), ".rds")

# Model building
model_building <- lapply(list.files("data/2021-033_o_0002a_cleared", 
                             pattern = "model_building_*", full.names = T), readRDS)

# Load map
map_sa2_full <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%  
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>% 
  filter(!str_detect(SA2_NAME, "Island")) %>% 
  filter(STATE_NAME != "Other Territories")
map_sa2 <- map_sa2_full %>%  
  right_join(.,global_obj$area_concor, by = "SA2")

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
                 "A - Adelaide (SA)", "H - Hobart (Tas)",
                 "C - Canberra (ACT)", "D - Darwin (NT)")
) %>% 
  mutate(initials = str_sub(city, 1, 1))

## END SCRIPT ## --------------------------------------------------------------
