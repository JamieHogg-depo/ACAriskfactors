## -----------------------------------------------------------------------------
## MAPS ## ---------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## START FOR LOOP #### ---------------------------------------------------------
for(k in 1:8){
  
  rf <- names(raw_est)[k]
  message("Started ", k, ": ", rf)
  
  # load data
  modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))

## PREVALENCE #### -------------------------------------------------------------

# base map
base <- modelled_est$summ$sa2_map %>% 
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = mu_median), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "B")+
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
  guides(fill = guide_colourbar(barwidth = 15, 
                                title.position = "top",
                                title.hjust = 0.5))+
  theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "green", size = 0.2)
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
                                    size = 6),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- grid.arrange(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                               layout_matrix  = lay)

# save object
jsave(filename = paste0("mu_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps"),
      plot = full_inset_plt, square = F)

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
                       option = "A")+
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
    labs(fill = "Width of 95% HDI")+
    guides(fill = guide_colourbar(barwidth = 15, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "green", size = 0.2)
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
                                    size = 6),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- grid.arrange(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                               layout_matrix  = lay)

# save object
jsave(filename = paste0("mucisize_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps"),
      plot = full_inset_plt, square = F)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished prevalence cisize")

## ODDS RATIOS #### ------------------------------------------------------------

# SETUP
cut_offs <- c(1/1.5, 1.5)
mapping_data <- modelled_est$summ$sa2_map %>% 
  mutate() %>%
  mutate(or_median = ifelse(or_median > cut_offs[2], cut_offs[2], or_median),
         or_median = ifelse(or_median < cut_offs[1], cut_offs[1], or_median))

# define fill colours
Fill.colours <- c("#2C7BB6", "#2C7BB6", "#ABD9E9", "#FFFFBF", "#FDAE61", "#D7191C", "#D7191C")
End <- log(1.6)
Breaks.fill <- c(1/1.5, 1/1.25, 1, 1.25, 1.5)
Fill.values <- c(-End, log(Breaks.fill), End)

# base map
base <- mapping_data %>% 
  ggplot(aes(fill = log(or_median)))+
  theme_void()+
  geom_sf(col = NA)+
  scale_fill_gradientn(colors = Fill.colours,
                       values = rescale(Fill.values),
                       labels = as.character(round(Breaks.fill, 3)),
                       breaks = log(Breaks.fill),
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
    labs(fill = "Odds Ratio")+
    guides(fill = guide_colourbar(barwidth = 15, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "green", size = 0.2)
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
                                    size = 6),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- grid.arrange(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                               layout_matrix  = lay)

# save plot
jsave(filename = paste0("or_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps"),
      plot = full_inset_plt, square = F)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished ors")

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
  scale_fill_distiller(palette = "PRGn",
                       limits = c(-0.0000001,1.0000001),
                       direction = -1,
                       #oob = squish,
                       #trans = "logit",
                       breaks = c(0,0.2,0.5,0.8,1),
                       labels = as.character(c(0,0.2,0.5,0.8,1)))+
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
    guides(fill = guide_colourbar(barwidth = 15, 
                                  title.position = "top",
                                  title.hjust = 0.5))+
    theme(legend.position = "bottom"))
llegend <- ggpubr::get_legend(base_legend)

# Base map with boxes
base_boxes <- base
for(i in 1:8){
  base_boxes <- base_boxes + 
    addBoxLabel(i, color = "green", size = 0.2)
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
                                    size = 6),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,10,10,10,10,7))
full_inset_plt <- grid.arrange(grobs = c(list(base_boxes), inset_list, list(llegend)), 
                               layout_matrix  = lay)

# save object
jsave(filename = paste0("orep_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps"),
      plot = full_inset_plt, square = F)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished or eps")

## FINISH FOR LOOP #### --------------------------------------------------------

}

## END SCRIPT #### -------------------------------------------------------------