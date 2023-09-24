## -----------------------------------------------------------------------------
## MAPS ## ---------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## SHAA estimates - obesity ## -------------------------------------------------

# Load objects
modelled_est <- readRDS("data/summary_files/obesity_b1.rds")
pha_map <- suppressMessages(modelled_est$summ$sa2_map %>% 
                              group_by(pha) %>% 
                              summarise(geometry = st_union(geometry)))

# some new datasets and objects
pha_estimates <- SHA_pha %>% 
  filter(!is.na(shaout_smoking_estimate)) %>% 
  right_join(., pha_map) %>% 
  mutate(median = shaout_obese_estimate) %>% 
  st_as_sf()

# squish the top and lower 2.5 quantiles
rar<- unname(quantile(modelled_est$summ$sa2$mu_median, p = c(0.01,0.99), na.rm = T))

# base map
base <-pha_estimates %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = median), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "F", 
                       limits = rar, oob = squish)+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Proportion")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob("Obese (SHAA)",gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("phamu_obesity.png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt,
   modelled_est, pha_map, rar, pha_estimates)
message("---- Finished SHAA obese")

## SHAA estimates - obesity - joined ## ----------------------------------------

## PHA ##

# Load objects
modelled_est <- readRDS("data/summary_files/obesity_b1.rds")
pha_map <- suppressMessages(modelled_est$summ$sa2_map %>% 
                              group_by(pha) %>% 
                              summarise(geometry = st_union(geometry)))

# some new datasets and objects
pha_estimates <- SHA_pha %>% 
  filter(!is.na(shaout_smoking_estimate)) %>% 
  right_join(., pha_map) %>% 
  mutate(median = shaout_obese_estimate) %>% 
  st_as_sf()

# squish the top and lower 2.5 quantiles
rar<- unname(quantile(modelled_est$summ$sa2$mu_median, p = c(0.01,0.99), na.rm = T))

# base map
base <-pha_estimates %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = median), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "F", 
                       limits = rar, oob = squish)+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 3.5),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Proportion")+
    guides(fill = guide_colourbar(barwidth = 9, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 3.5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
pha_full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                                  layout_matrix  = lay,
                                  top = textGrob("Obese (SHAA)",gp=gpar(fontsize=6)))


## SA2 ##

# squish the top and lower 2.5 quantiles
rar<- unname(quantile(modelled_est$summ$sa2$mu_median, p = c(0.01,0.99)))

# base map
base <- modelled_est$summ$sa2_map %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = mu_median), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "F", 
                       limits = rar, oob = squish)+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 3.5),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Proportion")+
    guides(fill = guide_colourbar(barwidth = 9, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 3.5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
sa2_full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                                  layout_matrix  = lay,
                                  top = textGrob("Obese",gp=gpar(fontsize=6)))

## JOIN and save ##
sa2pha_joined <- arrangeGrob(grobs = list(sa2_full_inset_plt,
                                          pha_full_inset_plt), 
                             layout_matrix  = matrix(c(1,2), nrow = 2))

jsave(filename = "phasa2mu_obesity.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = sa2pha_joined, square = F,
      ratio = c(9,6),
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, sa2_full_inset_plt, pha_full_inset_plt, sa2pha_joined)
message("---- Finished sa2pha joined")

## Sydney Habour Cut Out - alcohol ## ------------------------------------------

SyndeyCutOut <- data.frame(
  xmin = c(151.1242),
  xmax = c(151.317),
  ymin = -c(33.906),
  ymax = -c(33.772)
)

# SETUP
modelled_est <- readRDS(file = paste0("data/summary_files/alcohol_b1.rds"))
mapping_data <- modelled_est$summ$sa2_map %>% 
  mutate(or_EP = ifelse(or_EP == 0, 0.001, or_EP),
         or_EP = ifelse(or_EP == 1, 0.999, or_EP))

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = or_EP), col = NA)+
  scale_fill_gradientn(colors = c("#008837", "#a6dba0", "white","white","white", "#c2a5cf", "#7b3294"),
                       limits = c(-0.0000001,1.0000001),
                       #oob = squish,
                       #trans = "logit",
                       breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                       labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "right",
        panel.background = element_rect(fill = "lightblue"),
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"),
        panel.border = element_rect(colour = "black", size=1, fill=NA))+
  labs(fill = "Exceedance\nprobability")+
  xlim(SyndeyCutOut$xmin[1], SyndeyCutOut$xmax[1]) +
  ylim(SyndeyCutOut$ymin[1], SyndeyCutOut$ymax[1])

# Income
income <- modelled_est$summ$sa2_map %>% 
  left_join(.,averages, by = "SA2") %>% 
  mutate(inc = cut_number(Median_tot_prsnl_inc_weekly, 20, label = F)*5) %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = inc), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = 1,
                       option = "F")+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "right",
        panel.background = element_rect(fill = "lightblue"),
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"),
        panel.border = element_rect(colour = "black", size=1, fill=NA))+
  labs(fill = "Personal\nincome\n(percentile)")+
  xlim(SyndeyCutOut$xmin[1], SyndeyCutOut$xmax[1]) +
  ylim(SyndeyCutOut$ymin[1], SyndeyCutOut$ymax[1])

# create final list
lay <- rbind(c(1),
             c(2))
full_inset_plt <- arrangeGrob(grobs = list(base, income), 
                              layout_matrix  = lay)

# save object
jsave(filename = paste0("sydney_cutout.png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt,
      square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, income, lay, full_inset_plt)
message("---- Finished sydney cutout")

## SAMPLED #### ----------------------------------------------------------------

# base map
base <- map_sa2 %>% 
  mutate(sampled = as.factor(ifelse(ps_area < 1695, "Sampled", "Nonsampled"))) %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = sampled), col = "black", size = 0.1)+
  scale_fill_manual(values = c("grey", "white"),
                    breaks = c("Sampled", "Nonsampled"))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,1,1,1,1,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list), 
                              layout_matrix  = lay)

# save object
jsave(filename = paste0("sampled.png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, lay, full_inset_plt)
message("---- Finished sampled map")

## IRSD #### --------------------------------------------------------------------

# base map
base <- map_sa2 %>% 
  arrange(ps_area) %>% 
  left_join(.,irsd_5c, by = "ps_area") %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = irsd_5c), col = NA)+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "")+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay)

# save object
jsave(filename = paste0("irsd.png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished irsd")

## First Nations Australians #### ----------------------------------------------

# base map
base <- map_sa2 %>% 
  left_join(.,indig, by = "SA2") %>% 
  mutate(over20 = factor(case_when(
    Indigenous >= 20 ~ ">=20%",
    Indigenous >=10 &  Indigenous<20 ~ "10%-20%",
    Indigenous <10 ~ "<10%"),
    levels = c("<10%","10%-20%", ">=20%"))) %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = over20), col = NA)+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm")) + 
  scale_fill_manual(values = c("skyblue", "royalblue", "purple"),
                    breaks = c("<10%","10%-20%", ">=20%"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Proportion of\nFirst Nations\nAustralians")+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay)

# save object
jsave(filename = paste0("fna.png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished fna")

## START FOR LOOP #### ---------------------------------------------------------
for(k in 1:8){
  
  rf <- names(raw_est)[k]
  rf_full <- lookup[k,]$rf_full
  message("Started ", k, ": ", rf)
  m_s <- Sys.time()
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))

## PREVALENCE #### -------------------------------------------------------------

# squish the top and lower 2.5 quantiles
rar<- unname(quantile(modelled_est$summ$sa2$mu_median, p = c(0.01,0.99)))
  
# base map
base <- modelled_est$summ$sa2_map %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = mu_median), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "F", 
                       limits = rar, oob = squish)+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
  labs(fill = "Proportion")+
  guides(fill = guide_colourbar(barwidth = 13, 
                                title.position = "top",
                                title.hjust = 0.5))+
  theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                               layout_matrix  = lay,
                               top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("mu_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished prevalence")

## PREVALENCE - CI SIZE #### ---------------------------------------------------

# base map
base <- modelled_est$summ$sa2_map %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = mu_cisize), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "D")+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Width of 95% HPDI")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                               layout_matrix  = lay,
                               top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("mucisize_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished prevalence cisize")

## ODDS RATIOS #### ------------------------------------------------------------

# SETUP - use 2 instead!
cut_offs <- c(1/2, 2)
mapping_data <- modelled_est$summ$sa2_map %>% 
  mutate() %>%
  mutate(or_median = ifelse(or_median > cut_offs[2], cut_offs[2], or_median),
         or_median = ifelse(or_median < cut_offs[1], cut_offs[1], or_median))

# define fill colours
Fill.colours <- c("#2C7BB6", "#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C", "#D7191C")
End <- log2(2.1)
Breaks.fill <- c(1/2, 1/1.5, 1, 1.5, 2)
Fill.values <- c(-End, log2(Breaks.fill), End)

# base map
base <- mapping_data %>% 
  ggplot(aes(fill = log2(or_median)))+
  theme_void()+
  geom_sf(col = NA)+
  scale_fill_gradientn(colors = Fill.colours,
                       values = rescale(Fill.values),
                       labels = as.character(round(Breaks.fill, 3)),
                       breaks = log2(Breaks.fill),
                       limits = range(Fill.values))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Odds ratio")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                               layout_matrix  = lay,
                               top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save plot
jsave(filename = paste0("or_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished ors")

## OR - CI SIZE #### ---------------------------------------------------
  
# base map
up_lim <- unname(quantile(modelled_est$summ$sa2$or_cisize, probs = 0.98))
base <- modelled_est$summ$sa2_map %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = or_cisize), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       limit = c(0, up_lim),
                       oob = squish,
                       option = "D")+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Width of 95% HPDI")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay) #,
#top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("orcisize_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished or cisize")

## ODDS RATIOS - EP #### -------------------------------------------------------

# SETUP
mapping_data <- modelled_est$summ$sa2_map %>% 
  mutate(or_EP = ifelse(or_EP == 0, 0.001, or_EP),
         or_EP = ifelse(or_EP == 1, 0.999, or_EP))

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = or_EP), col = NA)+
  scale_fill_gradientn(colors = c("#008837", "#a6dba0", "white","white","white", "#c2a5cf", "#7b3294"),
                       limits = c(-0.0000001,1.0000001),
                       #oob = squish,
                       #trans = "logit",
                       breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                       labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Exceedance probability")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                               layout_matrix  = lay,
                               top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("orep_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished or eps")

## RR #### ---------------------------------------------------------------------

# get best range
#col_out <- getBestRRCutPoint(modelled_est$summ$sa2_map$rr_median, cut_prob = 0.05)
col_out <- getBestRRCutPoint(summsa2all$rr_median, cut_prob = 0.03)

# SETUP
cut_offs <- col_out$cut_offs
mapping_data <- modelled_est$summ$sa2_map %>% 
  mutate() %>%
  mutate(rr_median = ifelse(rr_median > cut_offs[2], cut_offs[2], rr_median),
         rr_median = ifelse(rr_median < cut_offs[1], cut_offs[1], rr_median))

# define fill colours
Fill.colours <- c("#2C7BB6", "#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C", "#D7191C")
Breaks.fill <- col_out$Breaks.fill
Fill.values <-col_out$Fill.values

# base map
base <- mapping_data %>% 
  ggplot(aes(fill = log2(rr_median)))+
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
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Relative ratio")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save plot
jsave(filename = paste0("rr_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished rrs")

## RR - same scale as OR #### --------------------------------------------------

# SETUP - use 2 instead!
cut_offs <- c(1/2, 2)
mapping_data <- modelled_est$summ$sa2_map %>% 
  mutate() %>%
  mutate(rr_median = ifelse(rr_median > cut_offs[2], cut_offs[2], rr_median),
         rr_median = ifelse(rr_median < cut_offs[1], cut_offs[1], rr_median))

# define fill colours
Fill.colours <- c("#2C7BB6", "#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C", "#D7191C")
End <- log2(2.1)
Breaks.fill <- c(1/2, 1/1.5, 1, 1.5, 2)
Fill.values <- c(-End, log2(Breaks.fill), End)

# base map
base <- mapping_data %>% 
  ggplot(aes(fill = log2(rr_median)))+
  theme_void()+
  geom_sf(col = NA)+
  scale_fill_gradientn(colors = Fill.colours,
                       values = rescale(Fill.values),
                       labels = as.character(round(Breaks.fill, 3)),
                       breaks = log2(Breaks.fill),
                       limits = range(Fill.values))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Relative ratio")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save plot
jsave(filename = paste0("rr_ss_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished rrs_ss")

## RR - CI SIZE #### ---------------------------------------------------

# base map
base <- modelled_est$summ$sa2_map %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = rr_cisize), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "D")+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Width of 95% HPDI")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay) #,
                              #top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("rrcisize_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished rr cisize")

## Counts #### -----------------------------------------------------------------

# squish the top and lower 1 quantiles
rar<- unname(quantile(summsa2all$count_median, p = c(0.01,0.99)))
#rar<- unname(quantile(modelled_est$summ$sa2$count_median, p = c(0.01,0.99)))

# base map
base <- modelled_est$summ$sa2_map %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = count_median), col = NA)+
  scale_fill_viridis_c(begin = 0.3, end = 1, 
                       direction = -1,
                       option = "B", 
                       limits = rar, oob = squish)+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Modelled population count")+
    guides(fill = guide_colourbar(barwidth = 13, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save plot
jsave(filename = paste0("count_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished counts")

## LISA #### -------------------------------------------------------------------

modelled_est$summ$sa2_map <- modelled_est$summ$sa2 %>% 
  right_join(.,map_sa2, by = c("ps_area", "SA2")) %>% sf::st_as_sf()

# base map
base <- modelled_est$summ$sa2_map %>% 
  mutate(LISA_mu = case_when(
                    LISA_mu == "HH" ~ "HC",
                    LISA_mu == "LL" ~ "LC"
                    ),
        lisa = ifelse(or_EP > 0.8, "H", 
                     ifelse(or_EP < 0.2, "L", NA)),
        LISA_c = factor(ifelse(is.na(LISA_mu) & !is.na(lisa), lisa, as.character(LISA_mu)),
                       levels = c("HC", "H", "L", "LC")),
        LISA_c = fct_explicit_na(LISA_c, na_level = "N")) %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = LISA_c), col = NA)+
  scale_fill_manual(values = c("red", "coral", "grey", "skyblue", "royalblue"),
                    labels = expression(H[cluster], H[single], N, L[single], L[cluster]),
                    breaks = c("HC", "H", "N", "L", "LC"))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 8),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "")+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob(rf_full,gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("lisa_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished LISA")

## OR and OREP joined ## -------------------------------------------------------

## OR ##

# SETUP - use 2 instead!
cut_offs <- c(1/2, 2)
mapping_data <- modelled_est$summ$sa2_map %>% 
  mutate() %>%
  mutate(or_median = ifelse(or_median > cut_offs[2], cut_offs[2], or_median),
         or_median = ifelse(or_median < cut_offs[1], cut_offs[1], or_median))

# define fill colours
Fill.colours <- c("#2C7BB6", "#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C", "#D7191C")
End <- log2(2.1)
Breaks.fill <- c(1/2, 1/1.5, 1, 1.5, 2)
Fill.values <- c(-End, log2(Breaks.fill), End)

# base map
base <- mapping_data %>% 
  ggplot(aes(fill = log2(or_median)))+
  theme_void()+
  geom_sf(col = NA)+
  scale_fill_gradientn(colors = Fill.colours,
                       values = rescale(Fill.values),
                       labels = as.character(round(Breaks.fill, 3)),
                       breaks = log2(Breaks.fill),
                       limits = range(Fill.values))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 5),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Odds ratio")+
    guides(fill = guide_colourbar(barwidth = 9, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 3.5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
or_full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                                 layout_matrix  = lay,
                                 top = textGrob(rf_full,gp=gpar(fontsize=6)))

## EP ##

# SETUP
mapping_data <- modelled_est$summ$sa2_map %>% 
  mutate(or_EP = ifelse(or_EP == 0, 0.001, or_EP),
         or_EP = ifelse(or_EP == 1, 0.999, or_EP))

# base map
base <- mapping_data %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = or_EP), col = NA)+
  scale_fill_gradientn(colors = c("#008837", "#a6dba0", "white","white","white", "#c2a5cf", "#7b3294"),
                       limits = c(-0.0000001,1.0000001),
                       #oob = squish,
                       #trans = "logit",
                       breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                       labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
  geom_sf(data = aus_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.2)+
  geom_sf(data = state_border, aes(geometry = geometry), 
          colour = "black", fill = NA, size = 0.1)+
  theme(legend.position = "none",
        text = element_text(size = 5),
        plot.title = element_text(margin = margin(0,0,2,0)),
        plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
(base_legend <- base +
    labs(fill = "Exceedance probability")+
    guides(fill = guide_colourbar(barwidth = 9, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2)
}

# Create list of insets
inset_list <- list()
for(i in 1:8){
  inset_list[[i]] <- base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    labs(title = lims$inset_labs[i])+
    theme(panel.border = element_rect(colour = "black", size=1, fill=NA),
          plot.title = element_text(margin = margin(0,0,2,0),
                                    size = 3.5),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
orep_full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                                   layout_matrix  = lay)

## JOIN and save ##
or_joined <- arrangeGrob(grobs = list(or_full_inset_plt,
                                      orep_full_inset_plt), 
                         layout_matrix  = matrix(c(1,2), nrow = 2))

jsave(filename = paste0("orjoined_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = or_joined, square = F,
      ratio = c(9,6),
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, or_full_inset_plt, orep_full_inset_plt, or_joined)
message("---- Finished OR_EP joined")

## FINISH FOR LOOP #### --------------------------------------------------------

message(paste0("Run time (mins): ", round(as.numeric(Sys.time() - m_s, units = "mins"), 2)))

}

## END SCRIPT #### -------------------------------------------------------------