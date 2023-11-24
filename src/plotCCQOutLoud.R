
# lookup
lookup <- data.frame(rf = names(raw_est),
                     sha = c("exercise", "exercise", "alcohol", 
                             "fruit", "obese", "overweight",
                             "smoking", "overweight"),
                     rf_full = c("Inadequate physical\nactivity (leisure)",
                                 "Inadequate physical\nactivity (all)",
                                 "Risky alcohol\n consumption",
                                 "Inadequate diet",
                                 "Obese",
                                 "Overweight/obese",
                                 "Current smoking",
                                 "Risky waist\ncircumference"))

## Explore ## ------------------------------------------------------------------

summsa2all %>% 
  left_join(.,state_name_concor) %>% 
  left_join(.,lookup, by = c("model" = "rf")) %>%
  mutate(qld_ind = ifelse(state_name_short == "QLD", "QLD", "Other"),
         state_name_short = fct_relevel(as.factor(state_name_short), "QLD")) %>% 
  ggplot(aes(y = mu_median, x = state_name_short,
             col = qld_ind))+
  theme_bw()+
  geom_boxplot()+
  geom_point(position = position_jitter(width = 0.4))+
  scale_color_manual(values = c("grey", "lightblue"),
                     breaks = c("Other", "QLD"))+
  facet_wrap(.~rf_full)+
  labs(x = "Prevlance")+
  theme(legend.position = "none")

## Areas with low prevalence ## ------------------------------------------------

# Obesity
summsa2all %>% 
  left_join(map_sa2) %>% 
  filter(str_sub(SA2, 1,1)=="3",
         model == "obesity") %>% 
  slice_min(mu_median, n = 10) %>% 
  slice_min(mu_EP, n = 2) %>% 
  mutate(est = paste0(round(mu_median, 2), " (", 
                      round(mu_lower, 2), ", ", 
                      round(mu_upper, 2), ")"),
         type = "Lowest") %>% 
  dplyr::select(model, type, SA2_NAME, est, N_persons)

summsa2all %>% 
  left_join(map_sa2) %>% 
  filter(str_sub(SA2, 1,1)=="3",
         model == "obesity") %>% 
  slice_max(mu_median, n = 10) %>% 
  slice_max(mu_EP, n = 2) %>% 
  mutate(est = paste0(round(mu_median, 2), " (", 
                      round(mu_lower, 2), ", ", 
                      round(mu_upper, 2), ")"),
         type = "Highest") %>% 
  dplyr::select(model, type, SA2_NAME, est)

# Alcohol
summsa2all %>% 
  left_join(map_sa2) %>% 
  filter(str_sub(SA2, 1,1)=="3",
         model == "alcohol") %>% 
  slice_min(mu_median, n = 10) %>% 
  slice_min(mu_EP, n = 2) %>% 
  mutate(est = paste0(round(mu_median, 2), " (", 
                      round(mu_lower, 2), ", ", 
                      round(mu_upper, 2), ")"),
         type = "Lowest") %>% 
  dplyr::select(model, type, SA2_NAME, est, ABS_irsd_decile_nation_complete)

summsa2all %>% 
  left_join(map_sa2) %>% 
  filter(str_sub(SA2, 1,1)=="3",
         model == "alcohol") %>% 
  slice_max(mu_median, n = 10) %>% 
  slice_max(mu_EP, n = 2) %>% 
  mutate(est = paste0(round(mu_median, 2), " (", 
                      round(mu_lower, 2), ", ", 
                      round(mu_upper, 2), ")"),
         type = "Highest") %>% 
  dplyr::select(model, type, SA2_NAME, est, ABS_irsd_decile_nation_complete)

# Smoking
summsa2all %>% 
  left_join(map_sa2) %>% 
  filter(str_sub(SA2, 1,1)=="3",
         model == "smoking") %>% 
  slice_min(mu_median, n = 10) %>% 
  slice_min(mu_EP, n = 2) %>% 
  mutate(est = paste0(round(mu_median, 2), " (", 
                      round(mu_lower, 2), ", ", 
                      round(mu_upper, 2), ")"),
         type = "Lowest") %>% 
  dplyr::select(model, type, SA2_NAME, est, ABS_irsd_decile_nation_complete)

summsa2all %>% 
  left_join(map_sa2) %>% 
  filter(str_sub(SA2, 1,1)=="3",
         model == "smoking",
         rr_CV < 50) %>% 
  slice_max(mu_median, n = 40) %>% 
  slice_max(mu_EP, n = 3) %>% 
  mutate(est = paste0(round(mu_median, 2), " (", 
                      round(mu_lower, 2), ", ", 
                      round(mu_upper, 2), ")"),
         type = "Highest") %>% 
  dplyr::select(model, type, SA2_NAME, est, ABS_irsd_decile_nation_complete)

# activity
summsa2all %>% 
  left_join(map_sa2) %>% 
  filter(str_sub(SA2, 1,1)=="3",
         model == "activityleis") %>% 
  slice_min(mu_median, n = 10) %>% 
  slice_min(mu_EP, n = 2) %>% 
  mutate(est = paste0(round(mu_median, 2), " (", 
                      round(mu_lower, 2), ", ", 
                      round(mu_upper, 2), ")"),
         type = "Lowest") %>% 
  dplyr::select(model, type, SA2_NAME, est)

summsa2all %>% 
  left_join(map_sa2) %>% 
  filter(str_sub(SA2, 1,1)=="3",
         model == "activityleis") %>% 
  slice_max(mu_median, n = 10) %>% 
  slice_max(mu_EP, n = 2) %>% 
  mutate(est = paste0(round(mu_median, 2), " (", 
                      round(mu_lower, 2), ", ", 
                      round(mu_upper, 2), ")"),
         type = "Highest") %>% 
  dplyr::select(model, type, SA2_NAME, est)

## Define labels ## ------------------------------------------------------------

fl <- lookup %>% 
  mutate(text = c("87.0%", "85.3%", "28.8%", "47.0%", 
                  "32.0%", "65.0%", "15.8%", "63.9%"),
         x = 146,
         y = -12)

## Filling in map ## -----------------------------------------------------------

map_sa2 %>% 
  ggplot(aes(geometry = geometry))+
  theme_void()+
  geom_sf(fill = "white", col = "black")
jsave(filename = "ccqoutloud_aus_empty.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

map_sa2 %>% 
  mutate(y = rnorm(nrow(.))) %>% 
  ggplot(aes(fill = y, geometry = geometry))+
  theme_void()+
  geom_sf(col = "black")+
  theme(legend.position = "none")
jsave(filename = "ccqoutloud_aus_filled.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

## Odds ratio ## ---------------------------------------------------------------

# SETUP - use 2 instead!
cut_offs <- c(1/2, 2)
mapping_data <- summsa2all %>% 
  left_join(map_sa2) %>% 
  mutate() %>%
  mutate(or_median = ifelse(or_median > cut_offs[2], cut_offs[2], or_median),
         or_median = ifelse(or_median < cut_offs[1], cut_offs[1], or_median)) %>% 
  mutate(or_median = ifelse(rr_CV_b > 50 | N_persons < 100, NA, or_median))

# define fill colours
Fill.colours <- c("#2C7BB6", "#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C", "#D7191C")
End <- log2(2.1)
Breaks.fill <- c(1/2, 1/1.5, 1, 1.5, 2)
Fill.values <- c(-End, log2(Breaks.fill), End)

# full map
mapping_data %>% 
  left_join(.,lookup, by = c("model" = "rf")) %>% 
  ggplot(aes(fill = log2(or_median), geometry = geometry))+
  theme_void()+
  geom_sf(col = NA)+
  scale_fill_gradientn(colors = Fill.colours,
                       values = rescale(Fill.values),
                       labels = as.character(round(Breaks.fill, 2)),
                       breaks = log2(Breaks.fill),
                       limits = range(Fill.values))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))+
  facet_wrap(.~rf_full, nrow = 2)
jsave(filename = "ccqoutloud_aus_or.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

# QLD map
mapping_data %>% 
  left_join(.,lookup, by = c("model" = "rf")) %>% 
  filter(str_sub(SA2, 1,1)=="3") %>%
  ggplot(aes(fill = log2(or_median), geometry = geometry))+
  theme_void()+
  geom_sf(col = NA)+
  scale_fill_gradientn(colors = Fill.colours,
                       values = rescale(Fill.values),
                       labels = as.character(round(Breaks.fill, 2)),
                       breaks = log2(Breaks.fill),
                       limits = range(Fill.values))+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))+
  geom_text(data = fl, aes(label = text, x = x, y = y), size = 2, inherit.aes = F)+
  facet_wrap(.~rf_full, nrow = 2)
jsave(filename = "ccqoutloud_qld_or.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

# Brisbane
mapping_data %>% 
  left_join(.,lookup, by = c("model" = "rf")) %>% 
  ggplot(aes(fill = log2(or_median), geometry = geometry))+
  theme_void()+
  geom_sf(col = NA)+
  scale_fill_gradientn(colors = Fill.colours,
                       values = rescale(Fill.values),
                       labels = as.character(round(Breaks.fill, 2)),
                       breaks = log2(Breaks.fill),
                       limits = range(Fill.values))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))+
  xlim(lims$xmin[1], lims$xmax[1]) +
  ylim(lims$ymin[1], lims$ymax[1]) +
  facet_wrap(.~rf_full, nrow = 2)
jsave(filename = "ccqoutloud_bne_or.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

## High smoking rates ## -------------------------------------------------------

summsa2all %>% 
  left_join(map_sa2) %>% 
  filter(str_sub(SA2, 1,1)=="3",
         model == "smoking") %>% 
  mutate(est = paste0(round(mu_median, 2), " (", 
                      round(mu_lower, 2), ", ", 
                      round(mu_upper, 2), ")"),
         type = "Highest",
         area = SA2_NAME %in% c("Torres", "Carpentaria")) %>%
  st_as_sf() %>% 
  ggplot(aes(y = mu_median, geometry = geometry, fill = area))+
  theme_void()+
  geom_sf()+
  scale_fill_manual(values = c("grey", "white"),
                    breaks = c(TRUE, FALSE))+
  theme(legend.position = "none")+
  coord_sf(ylim = c(-20,-10))
jsave(filename = "ccqoutloud_highsmoking.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

## ODDS RATIOS - EP #### -------------------------------------------------------

# SETUP
mapping_data <- summsa2all %>% 
  left_join(map_sa2) %>% 
  mutate(or_EP = ifelse(or_EP == 0, 0.001, or_EP),
         or_EP = ifelse(or_EP == 1, 0.999, or_EP))

# QLD map
mapping_data %>% 
  left_join(.,lookup, by = c("model" = "rf")) %>% 
  filter(str_sub(SA2, 1,1)=="3") %>%
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = or_EP, geometry = geometry), col = NA)+
  scale_fill_gradientn(colors = c("#008837", "#a6dba0", "white","white","white", "#c2a5cf", "#7b3294"),
                       limits = c(-0.0000001,1.0000001),
                       #oob = squish,
                       #trans = "logit",
                       breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                       labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))+
  facet_wrap(.~rf_full, nrow = 2)
jsave(filename = "ccqoutloud_qld_orep.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)

# BNE map
mapping_data %>% 
  left_join(.,lookup, by = c("model" = "rf")) %>% 
  filter(str_sub(SA2, 1,1)=="3") %>%
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = or_EP, geometry = geometry), col = NA)+
  scale_fill_gradientn(colors = c("#008837", "#a6dba0", "white","white","white", "#c2a5cf", "#7b3294"),
                       limits = c(-0.0000001,1.0000001),
                       #oob = squish,
                       #trans = "logit",
                       breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                       labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))+
  xlim(lims$xmin[1], lims$xmax[1]) +
  ylim(lims$ymin[1], lims$ymax[1]) +
  facet_wrap(.~rf_full, nrow = 2)
jsave(filename = "ccqoutloud_bne_orep.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      square = F,
      square_size = 1200,
      dpi = 300)


