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
rm(list = ls())

base_folder <- "C:/r_proj/ACAriskfactors/out"
export <- TRUE

## Functions ## ----------------------------------------------------------------

source('C:/r_proj/ACAriskfactors/src/functions_ALL.R')

jsave <- function(filename, square = T, square_size = 5000, ratio = c(6,9)){
  if(square){
    ggsave(filename = filename,
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
           path = base_folder,
           dpi = 1000,
           width = round(b),
           height = round(a),
           scale = 1,
           units = "px")
  }
}

make_numeric_decimal <- function(.data){
  df <- .data
  cols_to_format <- unlist(lapply(df, is.numeric))
  df[,cols_to_format] <- bind_cols(lapply(df[,cols_to_format], sprintf, fmt = '%#.2f'))
  return(df)
}

## Load Data ## ----------------------------------------------------------------

# Load modelled results
b_est <- readRDS("C:/r_proj/ACAriskfactors/data/sf_list_bench.rds")
nb_est <- readRDS("C:/r_proj/ACAriskfactors/data/sf_list_nobench.rds")

# Load map
map_sa2 <- st_read("C:/r_proj/ACAriskfactors/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%  
  filter(STATE_CODE %in% c("8", "1", "3", "2")) %>% 
  mutate(SA2 = as.numeric(SA2_MAIN16)) %>% 
  left_join(.,b_est$area_concor, by = "SA2")
  
# get overlap
state_overlay <- map_sa2 %>% 
  mutate(state = str_sub(SA2, 1, 1)) %>% 
  group_by(state) %>% 
  summarise(geometry = st_union(geometry)) %>% 
  st_as_sf() %>%
  st_transform(4326)

# Load remoteness
ra1 <- read_csv("C:/r_proj/ACAriskfactors/data/aust_ASGS2016_sa2_detls.csv") %>% dplyr::select(-1)
ra2 <- read_csv("C:/r_proj/ACAriskfactors/data/Australia_SA22016_geogdet.csv") %>% dplyr::select(-1)

ra <- ra2 %>% 
  mutate(ra_sa2 = factor(ra2$ra_cat, levels = c(0,1,2,3,4), labels = unique(ra1$ra_name)[c(2,1,3,5,4)])) %>% 
  rename(SA2 = sa2maincode) %>% 
  dplyr::select(SA2, ra_sa2) %>% 
  mutate(ra_sa2 = str_replace(ra_sa2, " Australia", ""),
         ra_sa2 = str_replace(ra_sa2, " of", "")) %>% 
  mutate(ra_sa2_3c = as.factor(case_when(
    ra_sa2 %in% c("Outer Regional", "Remote", "Very Remote") ~ "Outer regional to very remote",
    ra_sa2 == "Major Cities" ~ "Major Cities",
    ra_sa2 == "Inner Regional" ~ "Inner Regional" 
  ))) %>% 
  mutate(ra_sa2_3c = fct_relevel(ra_sa2_3c, "Major Cities"))
rm(ra1, ra2)

# Load IRSD values
irsd <- read_excel("C:/r_proj/ACAriskfactors/data/irsd.xlsx") %>% 
  mutate(ABS_irsd_decile_nation = as.factor(ABS_irsd_decile_nation))

# Create aux
aux2 <- inner_join(ra, irsd, by = "SA2") %>% 
  left_join(.,b_est$area_concor, by = "SA2")

# create shapefile
b_est$summ_mu %>% 
  filter(model == "TSLN") %>% 
  left_join(.,map_sa2, by = "ps_area") %>% 
  left_join(.,inner_join(ra, irsd, by = "SA2"), by = "SA2") %>% 
  st_as_sf() %>%
  st_transform(4326) %>% 
  st_write(., "C:/r_proj/ACAriskfactors/data/summ_mu_shapefile/summ_mu_shapefile.shp")

## Other code ## --------------------------------------------------------------

# Set HT and HT_mu
HT_mu <- 0.147
HT_odds <- 0.147/(1-0.147)
getHT <- function(x){(x/(1-x))/HT_odds}

# Colors 
hue_pal()(5) # gets the first few default colors from ggplot
jcol <- data.frame(model = c("TSLN", "ELN", "LOG", "HT"),
                   color = c("#C77CFF", "#7CAE00", "#00BFC4", "#ae3200"))

# Create subset data
prev_median_wide <- dplyr::select(b_est$summ_mu, median, model, ps_area) %>% 
  pivot_wider(names_from = model, values_from = median)
prev_ci_wide <- dplyr::select(b_est$summ_mu, cisize, model, ps_area) %>% 
  pivot_wider(names_from = model, values_from = cisize)

# Add elements to sample_agg
sample_agg <- b_est$sa2_direct %>% 
  mutate(cisize = 2*1.96*HT_SE,
         HT_lower = HT - 1.96 * HT_SE,
         HT_upper = HT + 1.96 * HT_SE,
         OR = getHT(HT),
         OR_lower = getHT(HT_lower),
         OR_upper = getHT(HT_upper))
sample_agg <- sample_agg %>% 
  bind_rows(data.frame(ps_area = c(1:1630)[!1:1630 %in% sample_agg$ps_area]))

# Get missing geometries (areas we didn't estimate for)
temp <- b_est$summ_mu %>%
  right_join(.,map_sa2, by = "ps_area") %>% 
  st_as_sf() %>% 
  filter(is.na(model)) %>% 
  dplyr::select(SA2_MAIN16, geometry)
mis_geos <- data.frame(SA2_MAIN16 = rep(temp$SA2_MAIN16, 4),
                       geometry = rep(temp$geometry, 4),
                       model = rep(c("TSLN", "LOG", "ELN", "Direct"), 67))
rm(temp)

# City Insets 
lims <- data.frame(
  xmin = c(152.6, 150.35, 144.5, 115.45, 138.1, 146.8, 148.6, 130.3),
  xmax = c(153.6, 151.35, 145.5, 116.45, 139.1, 147.8, 149.6, 131.3),
  ymin = -c(28, 34.4, 38.4, 32.5, 35.4, 43.4, 35.8, 13),
  ymax = -c(27, 33.4, 37.4, 31.5, 34.4, 42.4, 34.8, 12),
  city = c("Brisbane", "Sydney", "Melbourne", "Perth", "Adelaide", "Hobart", "Canberra", "Darwin")
)

## END SCRIPT ## --------------------------------------------------------------
