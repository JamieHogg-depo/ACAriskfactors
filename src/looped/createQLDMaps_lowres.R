## -----------------------------------------------------------------------------
## MAPS ## ---------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## SHAA estimates - obesity - qld ## -------------------------------------------

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
  filter(str_sub(pha, 1,1)=="3") %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = median), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "F", 
                       limits = rar, oob = squish)+
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
for(i in 1){
  base_boxes <- base_boxes + 
    addBoxLabel(1, color = "black", size = 0.2, textsize = 2)
}

# Create list of insets
inset_list <- list()
for(i in 1){
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
lay <- rbind(c(1,2),
             c(1,1),
             c(3,3))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob("Obese (SHAA)",gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("qld_phamu_obesity.png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt, pha_map, rar, pha_estimates)
message("---- Finished SHAA obese - qld")

## PREVALENCE - obesity - qld #### ---------------------------------------------

# squish the top and lower 2.5 quantiles
rar<- unname(quantile(modelled_est$summ$sa2$mu_median, p = c(0.01,0.99)))

# base map
base <- modelled_est$summ$sa2_map %>% 
  filter(str_sub(SA2, 1,1)=="3") %>%
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = mu_median), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "F", 
                       limits = rar, oob = squish)+
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
for(i in 1){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2, textsize = 2)
}

# Create list of insets
inset_list <- list()
for(i in 1){
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
lay <- rbind(c(1,2),
             c(1,1),
             c(3,3))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay,
                              top = textGrob("Obese", gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("qld_mu_obesity.png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt, modelled_est)
message("---- Finished obesity prevalence - qld")

## COUNT - obesity - qld ## ----------------------------------------------------

# squish the top and lower 1 quantiles
rar<- unname(quantile(modelled_est$summ$sa2$count_median, p = c(0.01,0.99), na.rm = T))

# base map
base <- modelled_est$summ$sa2_map %>% 
  filter(str_sub(SA2, 1,1)=="3") %>%
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = count_median), col = NA)+
  scale_fill_viridis_c(begin = 0.3, end = 1, 
                       direction = -1,
                       option = "B", 
                       limits = rar, 
                       oob = squish)+
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
for(i in 1){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2, textsize = 2)
}

# Create list of insets
inset_list <- list()
for(i in 1){
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
lay <- rbind(c(1,2),
             c(1,1),
             c(3,3))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay)

# save plot
jsave(filename = "qld_count_us_obesity.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt, modelled_est)
message("---- Finished obesity counts - qld")

## EP - obesity - qld #### -----------------------------------------------------

# SETUP
mapping_data <- modelled_est$summ$sa2_map %>% 
  mutate(or_EP = ifelse(or_EP == 0, 0.001, or_EP),
         or_EP = ifelse(or_EP == 1, 0.999, or_EP))

# base map
base <- mapping_data %>% 
  filter(str_sub(SA2, 1,1)=="3") %>%
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = or_EP), col = NA)+
  scale_fill_gradientn(colors = c("#008837", "#a6dba0", "white","white","white", "#c2a5cf", "#7b3294"),
                       limits = c(-0.0000001,1.0000001),
                       #oob = squish,
                       #trans = "logit",
                       breaks = c(0,0.2,0.25,0.5,0.75,0.8,1),
                       labels = as.character(c(0,0.2,"",0.5,"",0.8,1)))+
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
for(i in 1){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "black", size = 0.2, textsize = 2)
}

# Create list of insets
inset_list <- list()
for(i in 1){
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
lay <- rbind(c(1,2),
             c(1,1),
             c(3,3))
full_inset_plt <- arrangeGrob(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                              layout_matrix  = lay)

# save object
jsave(filename = "qld_orep_obesity.png", 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished or eps - qld")

## END SCRIPT #### -------------------------------------------------------------