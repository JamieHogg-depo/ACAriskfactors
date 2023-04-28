## -----------------------------------------------------------------------------
## Create plots and tables for case_study ## -----------------------------------
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

base_folder <- "C:/r_proj/two_stage_saemodel_proportions/case_study/plots"
export <- TRUE

## Functions ## ----------------------------------------------------------------

source('C:/r_proj/two_stage_saemodel_proportions/case_study/functions_ALL.R')

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
b_est <- readRDS("C:/r_proj/two_stage_saemodel_proportions/data/sf_list_bench.rds")
nb_est <- readRDS("C:/r_proj/two_stage_saemodel_proportions/data/sf_list_nobench.rds")

# Load map
map_sa2 <- st_read("C:/r_proj/two_stage_saemodel_proportions/data/2016_SA2_Shape_min/2016_SA2_Shape_min.shp") %>%  
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
ra1 <- read_csv("C:/r_proj/two_stage_saemodel_proportions/data/aust_ASGS2016_sa2_detls.csv") %>% dplyr::select(-1)
ra2 <- read_csv("C:/r_proj/two_stage_saemodel_proportions/data/Australia_SA22016_geogdet.csv") %>% dplyr::select(-1)

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
irsd <- read_excel("C:/r_proj/two_stage_saemodel_proportions/data/irsd.xlsx") %>% 
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
  st_write(., "C:/r_proj/two_stage_saemodel_proportions/data/summ_mu_shapefile/summ_mu_shapefile.shp")

## Setup ## --------------------------------------------------------------------

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

## PLOTS ## --------------------------------------------------------------------

# SA4 level - HT vs modeled ####
b_est$summ_mu_sa4 %>% 
  ggplot(aes(y = median, ymin = lower, ymax = upper,
             x = HT, xmin = HT_lower, xmax = HT_upper))+
  theme_bw()+geom_abline(col="red")+
  geom_errorbar(col = "grey")+geom_errorbarh(col = "grey")+
  geom_point()+
  facet_grid(.~model)+
  labs(y = "Modelled prevalence estimate",
       x = "Direct prevalence estimate")
if(export) jsave("sa4_directvsmodeled.png", square = F)

# SA4 level - ARB, RRMSE #### 
(b_est$summ_mu_sa4 %>% 
   dplyr::select(ps_sa4, n, RRMSE, model) %>% 
   pivot_wider(names_from = model, values_from = RRMSE) %>% 
   arrange(TSLN) %>% mutate(x = 1:nrow(.)) %>% dplyr::select(-ps_sa4) %>% 
   pivot_longer(-c(x, n)) %>% 
   ggplot(aes(y = 10*value, x = x, color = name))+
   theme_bw()+
   geom_path()+geom_point(aes(size = n))+
   scale_color_manual(values = jcol$color,
                      breaks = jcol$model)+
   labs(y = "RRMSE (x10)", x = "", col = "",
        size = "Sample size")+
   theme(legend.position = "bottom",
         legend.box = "vertical")+
   scale_size_binned(n.breaks = 5))+
  (b_est$summ_mu_sa4 %>% 
     dplyr::select(ps_sa4, n, ARB, model) %>% 
     pivot_wider(names_from = model, values_from = ARB) %>% 
     arrange(TSLN) %>% mutate(x = 1:nrow(.)) %>% dplyr::select(-ps_sa4) %>% 
     pivot_longer(-c(x, n)) %>% 
     ggplot(aes(y = 10*value, x = x, color = name))+
     theme_bw()+
     geom_path()+geom_point(aes(size = n))+
     scale_color_manual(values = jcol$color,
                        breaks = jcol$model)+
     labs(y = "ARB (x10)", x = "", col = "",
          size = "Sample size")+
     theme(legend.position = "bottom",
           legend.box = "vertical")+
     scale_size_binned(n.breaks = 5))
if(export) jsave("sa4_rrmse_arb.png", square = F)

# What about relative measures??

# Violin plots ####
b_est$summ_mu %>% 
  ggplot(aes(y = model, fill = model, x = median))+
  theme_bw()+
  geom_violin()+
  labs(y = "",
       x = "Modeled prevalence estimate")+
  theme(legend.position = "none")+
  scale_fill_manual(values = jcol$color,
                    breaks = jcol$model)
if(export) jsave("violin.png", square = F)

# Boxplot of posterior medians - IRSD ####
prev_median_wide %>%
  left_join(.,dplyr::select(aux2, ps_area, ABS_irsd_decile_nation, ra_sa2_3c),
            by = "ps_area") %>%
  pivot_longer(-c(ps_area, ABS_irsd_decile_nation, ra_sa2_3c)) %>%
  # rename IRSD categories
  mutate(ABS_irsd_decile_nation = factor(ABS_irsd_decile_nation, levels = 1:10,
                                         labels = c("Least advantaged", 
                                                    as.character(2:9), 
                                                    "Most advantaged"))) %>% 
  ggplot(aes(x = value, fill = name, y = ABS_irsd_decile_nation))+
  theme_bw()+
  geom_boxplot()+
  labs(x = "Modeled prevalence estimate",
       y = "IRSD deciles",
       fill = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = jcol$color,
                    breaks = jcol$model)
if(export) jsave("boxplot_byseifa.png", square = F)

# Boxplot of posterior medians - Remoteness ####
prev_median_wide %>%
  left_join(.,dplyr::select(aux2, ps_area, ABS_irsd_decile_nation, ra_sa2_3c),
            by = "ps_area") %>%
  pivot_longer(-c(ps_area, ABS_irsd_decile_nation, ra_sa2_3c)) %>%
  ggplot(aes(x = value, fill = name, y = ra_sa2_3c))+
  theme_bw()+
  geom_boxplot()+
  #facet_grid(.~ra_sa2_3c, labeller = label_wrap_gen(multi_line = TRUE))+
  labs(x = "Modeled prevalence estimate",
       y = "",
       fill = "")+
  theme(legend.position = "bottom")+
  scale_fill_manual(values = jcol$color,
                    breaks = jcol$model)
if(export) jsave("boxplot_byremoteness.png", square = F)

# Compare estimates from models ####
dplyr::select(b_est$summ_mu, median, lower, upper, model, ps_area) %>% 
  pivot_wider(names_from = model, values_from = c(median, lower, upper)) %>% 
  ggplot(aes(y = median_TSLN, ymin = lower_TSLN, ymax = upper_TSLN,
             x = median_ELN, xmin = lower_ELN, xmax = upper_ELN))+
  theme_bw(base_size = 20)+
  geom_abline(col = "red")+
  geom_hline(yintercept = HT_mu)+
  geom_vline(xintercept = HT_mu)+
  geom_errorbar(col="grey")+
  geom_errorbarh(col="grey")+
  geom_point()+
  xlim(0,1)+ylim(0,1)+
  labs(y = "TSLN model",
       x = "ELN model")
if(export) jsave("scatter_TSLNvsELN.png", square = T)

dplyr::select(b_est$summ_mu, median, lower, upper, model, ps_area) %>% 
  pivot_wider(names_from = model, values_from = c(median, lower, upper)) %>% 
  ggplot(aes(y = median_TSLN, ymin = lower_TSLN, ymax = upper_TSLN,
             x = median_LOG, xmin = lower_LOG, xmax = upper_LOG))+
  theme_bw(base_size = 20)+
  geom_abline(col = "red")+
  geom_hline(yintercept = HT_mu)+
  geom_vline(xintercept = HT_mu)+
  geom_errorbar(col="grey")+
  geom_errorbarh(col="grey")+
  geom_point()+
  xlim(0,1)+ylim(0,1)+
  labs(y = "TSLN model",
       x = "LOG model")
if(export) jsave("scatter_TSLNvsLOG.png", square = T)

dplyr::select(b_est$summ_mu, median, lower, upper, model, ps_area) %>% 
  pivot_wider(names_from = model, values_from = c(median, lower, upper)) %>% 
  ggplot(aes(y = median_ELN, ymin = lower_ELN, ymax = upper_ELN,
             x = median_LOG, xmin = lower_LOG, xmax = upper_LOG))+
  theme_bw(base_size = 20)+
  geom_abline(col = "red")+
  geom_hline(yintercept = HT_mu)+
  geom_vline(xintercept = HT_mu)+
  geom_errorbar(col="grey")+
  geom_errorbarh(col="grey")+
  geom_point()+
  xlim(0,1)+ylim(0,1)+
  labs(y = "ELN model",
       x = "LOG model")
if(export) jsave("scatter_ELNvsLOG.png", square = T)

# Caterpillar plots - by model - with CI  #### ---------------------------------

# Prevalence
b_est$summ_mu %>% 
  group_by(model) %>% 
  mutate(rank = rank(median, ties.method = "first")) %>%
  ggplot(aes(y = median, ymin = lower, ymax = upper,
             x = rank, 
             col = median))+theme_bw()+
  geom_errorbar(col = "grey")+
  geom_point()+
  facet_wrap(.~model)+
  scale_color_viridis_c(begin = 0.3, end = 1, 
                        direction = 1,
                        option = "B")+
  geom_hline(yintercept = HT_mu)+
  labs(y = "Modeled prevalence estimates",
       x = "")+
  theme(legend.position = "none")
if(export) jsave("cat_prev_medianci.png", square = F)

# Maps: Sample size of NHS #### ------------------------------------------------

# create map data
ss_map <- sample_agg %>% 
  dplyr::select(ps_area, HT) %>% 
  mutate(ss_dsc = ifelse(ps_area < 1263 & !is.na(HT), "Sample size > 10", "Sample size <= 10"),
         ss_dsc = ifelse(ps_area > 1262, "Nonsampled", ss_dsc),
         ss_dsc = as.factor(ss_dsc)) %>% 
  left_join(.,map_sa2, by = "ps_area") %>%
  st_as_sf() %>%
  st_transform(4326)

# color scale for map
ss_map_cols <- data.frame(model = c("Nonsampled", "Sample size <= 10", "Sample size > 10"),
                          color = c("#ffffff", "#808080", "#000000"))

# Create map
ss_pl <- ss_map %>% 
  ggplot(aes(fill = ss_dsc))+
  theme_void()+
  geom_sf()+
  scale_fill_manual(values = ss_map_cols$color,
                    breaks = ss_map_cols$model)+
  theme(legend.position = "bottom",legend.key.height = unit(0.5, "cm"))+
  guides(fill = guide_legend(nrow = 3))+
  labs(fill = "")

ss_pl
if(export) jsave("ss_map.png", square = FALSE)

# Subset to capital cities
cities <- lims[c(1,2,3,7),]
for(i in 1:nrow(cities)){
  ss_pl +
    xlim(cities$xmin[i], cities$xmax[i]) +
    ylim(cities$ymin[i], cities$ymax[i]) +
    ggtitle(label = cities$city[i])
  jsave(paste0("map_insets/ss_map_", cities$city[i], ".png"), square = F)
}

# Maps: Prevalence #### --------------------------------------------------------
# posterior medians (left)
# size of CI (right)
# grid for three models (along y axis)

'NOTE: Very uncertain and large prevalence values are for the very top of Australia.
These areas are all remote or very remote.
ps_area: 1507 1566 1567 1569 1570 1572 1573 1575 1596'

direct_est <- sample_agg %>% 
  dplyr::select(ps_area, HT, cisize) %>% 
  rename(median = HT) %>% 
  mutate(model = "Direct")
mapping_data <- b_est$summ_mu %>%
  bind_rows(direct_est) %>% 
  left_join(.,map_sa2, by = "ps_area") %>%
  bind_rows(mis_geos) %>% 
  st_as_sf() %>%
  st_transform(4326)

# Create base map for prevalence
(bm_mu <- mapping_data %>% 
    filter(model != "Direct") %>% 
    ggplot(aes(fill = median))+
    theme_void()+
    geom_sf(col = NA)+
    geom_sf(data = state_overlay, aes(geometry = geometry), 
            colour = "black", fill = NA, size = 0.3)+
    facet_grid(.~model)+
    scale_fill_viridis_c(begin = 0, end = 1, 
                         direction = -1,
                         option = "B")+
    labs(fill = "Proportion")+
    theme(legend.position = "right", legend.key.height = unit(0.5, "cm")))

# Create base map for CI prevalence
(bm_muci <- mapping_data %>% 
    filter(model != "Direct") %>% 
    ggplot(aes(fill = cisize))+
    theme_void()+
    geom_sf(col = NA)+
    geom_sf(data = state_overlay, aes(geometry = geometry), 
            colour = "black", fill = NA, size = 0.3)+
    facet_grid(.~model)+
    scale_fill_viridis_c(begin = 0, end = 0.8, 
                         direction = -1,
                         option = "D")+
    labs(fill = "Width of\nHDI")+
    theme(legend.position = "right", legend.key.height = unit(0.5, "cm"),
          strip.background = element_blank(),
          strip.text.x = element_blank()))

# Export full maps
bm_mu/bm_muci
if(export) jsave("map_mu.png")

# Subset to capital cities
cities <- lims[c(1,2,3,7),]
for(i in 1:nrow(cities)){
  mu <- bm_mu +
    xlim(cities$xmin[i], cities$xmax[i]) +
    ylim(cities$ymin[i], cities$ymax[i]) +
    ggtitle(label = cities$city[i])
  muci <- bm_muci +
    xlim(cities$xmin[i], cities$xmax[i]) +
    ylim(cities$ymin[i], cities$ymax[i])
  mu/muci
  jsave(paste0("map_insets/map_mu_", cities$city[i], ".png"), square = F)
  message(paste0("City ", i, " (of ", nrow(cities), ")"))
}

# Brisbane, Sydney, Melbourne subset
(bm_mu +
  xlim(cities$xmin[1], cities$xmax[1]) +
  ylim(cities$ymin[1], cities$ymax[1]) +
  ggtitle(label = cities$city[1])+
  theme(legend.position = "none"))/
(bm_mu +
   xlim(cities$xmin[2], cities$xmax[2]) +
   ylim(cities$ymin[2], cities$ymax[2]) +
   ggtitle(label = cities$city[2])+
   theme(legend.position = "none"))/
(bm_mu +
   xlim(cities$xmin[3], cities$xmax[3]) +
   ylim(cities$ymin[3], cities$ymax[3]) +
   ggtitle(label = cities$city[3])+
   theme(legend.position = "bottom", legend.key.width = unit(1, "cm")))
if(export) jsave("map_muonly_BriSydMel.png")

# Maps: ORs #### --------------------------------------------------------------

# SETUP
cut_offs <- c(1/1.5, 1.5)
direct_est <- sample_agg %>% 
  dplyr::select(ps_area, OR, OR_lower, OR_upper) %>% 
  rename(median = OR) %>% 
  mutate(model = "Direct",
         cisize = OR_upper - OR_lower) %>% 
  dplyr::select(-c(OR_lower, OR_upper))
mapping_data <- b_est$summ_or %>%
  bind_rows(direct_est) %>% 
  left_join(.,map_sa2, by = "ps_area") %>%
  bind_rows(mis_geos) %>% 
  st_as_sf() %>%
  st_transform(4326) %>%
  mutate(median = ifelse(median > cut_offs[2], cut_offs[2], median),
         median = ifelse(median < cut_offs[1], cut_offs[1], median))

# define fill colours
Fill.colours <- c("#2C7BB6", "#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C", "#D7191C")
End <- log(1.6)
Breaks.fill <- c(1/1.5, 1/1.25, 1, 1.25, 1.5)
Fill.values <- c(-End, log(Breaks.fill), End)

# Create base map for ORs
(bm_or <- mapping_data %>%
    filter(model != "Direct") %>% 
    ggplot(aes(fill = log(median)))+
    theme_void()+
    geom_sf(col = NA)+
    geom_sf(data = state_overlay, aes(geometry = geometry), 
            colour = "black", fill = NA, size = 0.3)+
    facet_grid(.~model)+
    scale_fill_gradientn(colors = Fill.colours,
                         values = rescale(Fill.values),
                         labels = as.character(round(Breaks.fill, 3)),
                         breaks = log(Breaks.fill),
                         limits = range(Fill.values))+
    labs(fill = "OR")+
    theme(legend.position = "right", legend.key.height = unit(0.4, "cm")))

# Create base map for cisize of ORs
(bm_orci <- mapping_data %>%
    filter(model != "Direct") %>% 
    ggplot(aes(fill = cisize))+
    theme_void()+
    geom_sf(col = NA)+
    geom_sf(data = state_overlay, aes(geometry = geometry), 
            colour = "black", fill = NA, size = 0.3)+
    facet_grid(.~model)+
    scale_fill_viridis_c(begin = 0, end = 1, 
                         direction = -1,
                         oob = squish, 
                         limits = c(0.01, 8.00), 
                         #trans = "log",
                         #breaks = c(0,0.2,1,3,20),
                         #labels = as.character(c(0,0.2,1,3,20)),
                         option = "D")+
    # scale_fill_viridis_c(begin = 0, end = 1, 
    #                      direction = -1,
    #                      oob = squish, 
    #                      limits = c(0.1, 63.7), 
    #                      trans = "log",
    #                      breaks = c(0,0.2,1,3,20),
    #                      labels = as.character(c(0,0.2,1,3,20)),
    #                      option = "D")+
    labs(fill = "Width of\nHDI")+
    theme(legend.position = "right", legend.key.height = unit(0.4, "cm"),
          strip.background = element_blank(),
          strip.text.x = element_blank()))

# Brisbane, Sydney, Melbourne subset
(bm_or +
  xlim(cities$xmin[1], cities$xmax[1]) +
  ylim(cities$ymin[1], cities$ymax[1]) +
  ggtitle(label = cities$city[1])+
  theme(legend.position = "none"))/
(bm_or +
   xlim(cities$xmin[2], cities$xmax[2]) +
   ylim(cities$ymin[2], cities$ymax[2]) +
   ggtitle(label = cities$city[2])+
   theme(legend.position = "none"))/
(bm_or +
   xlim(cities$xmin[3], cities$xmax[3]) +
   ylim(cities$ymin[3], cities$ymax[3]) +
   ggtitle(label = cities$city[3])+
   theme(legend.position = "bottom", legend.key.width = unit(1, "cm")))
if(export) jsave("map_oronly_BriSydMel.png")

# Maps: EPs for ORs #### ----------------------------------------------

# SETUP
mapping_data <- b_est$DPP_or %>%
  bind_rows(data.frame(model = "Direct", ps_area = 1:1695)) %>% 
  mutate(EP = ifelse(EP == 0, 0.001, EP),
         EP = ifelse(EP == 1, 0.999, EP)) %>% 
  left_join(.,map_sa2, by = "ps_area") %>%
  bind_rows(mis_geos) %>% 
  st_as_sf() %>%
  st_transform(4326)

# Create base map for EPs
(bm_ep <- mapping_data %>%
    filter(model != "Direct") %>% 
    ggplot(aes(fill = EP))+
    theme_void()+
    geom_sf(col = NA)+
    geom_sf(data = state_overlay, aes(geometry = geometry), 
            colour = "black", fill = NA, size = 0.3)+
    facet_grid(.~model)+
    scale_fill_distiller(palette = "PRGn",
                         limits = c(-0.0000001,1.0000001),
                         direction = -1,
                         #oob = squish,
                         #trans = "logit",
                         breaks = c(0,0.2,0.5,0.8,1),
                         labels = as.character(c(0,0.2,0.5,0.8,1))) +
    labs(fill = "EP")+
    theme(legend.position = "right", legend.key.height = unit(0.4, "cm"),
          strip.background = element_blank(),
          strip.text.x = element_blank()))

# Export full maps
bm_or/bm_ep/bm_orci
if(export) jsave("map_or.png")

# Subset to capital cities
cities <- lims[c(1,2,3,7),]
for(i in 1:nrow(cities)){
  or <- bm_or +
    xlim(cities$xmin[i], cities$xmax[i]) +
    ylim(cities$ymin[i], cities$ymax[i]) +
    ggtitle(label = cities$city[i])
  orci <- bm_orci +
    xlim(cities$xmin[i], cities$xmax[i]) +
    ylim(cities$ymin[i], cities$ymax[i])
  orep <- bm_ep +
    xlim(cities$xmin[i], cities$xmax[i]) +
    ylim(cities$ymin[i], cities$ymax[i])
  (or/orep/orci)
  jsave(paste0("map_insets/map_or_", cities$city[i], ".png"), square = F)
  message(paste0("City ", i, " (of ", nrow(cities), ")"))
}

## Table of performance metrics ## ---------------------------------------------

bind_rows(
  (b_est$summ_mu_sa4 %>% 
    mutate(overlap = overlap_v(lower, upper, HT_lower, HT_upper),
           cover = between_vec(HT, lower, upper)) %>% 
    group_by(model) %>% 
    summarise(MRRMSE = mean(RRMSE),
              MARB = mean(ARB),
              cisize = median(cisize),
              coverage = mean(cover),
              overlap1 = mean(overlap),
              overlap2 = weighted.mean(overlap, w = 1/HT_SE)) %>% 
    mutate(type = "Benchmarked") %>% relocate(type)),
  (nb_est$summ_mu_sa4 %>% 
    mutate(overlap = overlap_v(lower, upper, HT_lower, HT_upper),
           cover = between_vec(HT, lower, upper)) %>% 
    group_by(model) %>% 
    summarise(MRRMSE = mean(RRMSE),
              MARB = mean(ARB),
              cisize = median(cisize),
              coverage = mean(cover),
              overlap1 = mean(overlap),
              overlap2 = weighted.mean(overlap, w = 1/HT_SE)) %>% 
    mutate(type = "Not benchmarked") %>% relocate(type))
) %>% 
  make_numeric_decimal() %>% 
  write_excel_csv(., file = paste0(base_folder, "/comparative_performance.csv"))


## ----------------------------------------------------------------------------
#' @param sub_cities example `cities[cities$city == "Sydney",]`
#' @param map example is `map_sa2`
#' @returns Logical vector of length equal to `nrow(map)`
getIndicesInSubset <- function(sub_cities, map){
  area_of_interest <- with(sub_cities, 
                           matrix(c(xmin, ymin,
                                    xmax, ymin, 
                                    xmax, ymax,
                                    xmin, ymax,
                                    xmin, ymin), 
                                  byrow = T, ncol = 2)) %>% 
    list() %>% 
    st_polygon()
  
  st_contains(area_of_interest, map, sparse = F) %>% t() %>% c() 
}

## -----------------------------------------------------------------------------
# Very specific function
plot_comparesa4 <- function(i){
  
  overlap_tsln <- unname(unlist(b_est$summ_mu_sa4 %>% filter(ps_sa4 == i) %>% 
  mutate(overlap = overlap_v(lower, upper, HT_lower, HT_upper)) %>% 
    filter(model == "TSLN") %>% 
    dplyr::select(overlap)))
  
  temp <- bind_rows(list(
    b_est$summ_mu_sa4 %>% filter(ps_sa4 == i) %>% 
      dplyr::select(HT, HT_lower, HT_upper) %>% 
      mutate(model = "Direct") %>% 
      rename(median = HT,
             lower = HT_lower,
             upper = HT_upper) %>% slice(1),
    b_est$summ_mu_sa4 %>% 
      dplyr::select(ps_sa4, median, lower, upper, model) %>% 
      filter(ps_sa4 == i, model == "ELN") %>% 
      dplyr::select(-ps_sa4),
    b_est$summ_mu_sa4 %>% 
      dplyr::select(ps_sa4, median, lower, upper, model) %>% 
      filter(ps_sa4 == i, model == "TSLN") %>% 
      dplyr::select(-ps_sa4),
    b_est$summ_mu_sa4 %>% 
      dplyr::select(ps_sa4, median, lower, upper, model) %>% 
      filter(ps_sa4 == i, model == "LOG") %>% 
      dplyr::select(-ps_sa4)
  ))
  
  temp %>% 
    ggplot(aes(y = median, ymin = lower, ymax = upper,
               x = model)) +
    geom_errorbar()+
    geom_point()+
    labs(title = paste0("Overlap: ", round(overlap_tsln, 2)))
}
