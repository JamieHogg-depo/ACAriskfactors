## -----------------------------------------------------------------------------
## MAPS ## ---------------------------------------------------------------------
## -----------------------------------------------------------------------------

# Sample size of NHS #### ------------------------------------------------------

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

# Prevalence #### -------------------------------------------------------------
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

# ORs #### --------------------------------------------------------------------

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

# EPs for ORs #### ------------------------------------------------------------

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

## END SCRIPT ## --------------------------------------------------------------