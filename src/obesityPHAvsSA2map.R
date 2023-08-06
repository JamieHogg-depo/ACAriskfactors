## -----------------------------------------------------------------------------
## Obesity - SHA vs ACA maps ## ------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

## Load data ## ----------------------------------------------------------------

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
rar<- unname(quantile(c(modelled_est$summ$sa2$mu_median,
                        pha_estimates$median), 
                      p = c(0.01,0.99), na.rm = T))

## PHA map ## ------------------------------------------------------------------

# base map
base <- pha_estimates %>% 
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
    guides(fill = guide_colourbar(barwidth = 15, 
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
                                    size = 6),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,1,1,1,1,7))
pha_fi <- arrangeGrob(grobs = c(list(base_boxes), inset_list), 
                              layout_matrix  = lay)

rm(base, base_boxes, base_legend, lay)

## SA2 map ## ------------------------------------------------------------------

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
                                    size = 6),
          plot.margin = unit(c(1,1,1,1), "mm"))
}
inset_list <- Filter(Negate(is.null), inset_list)

# create final list
lay <- rbind(c(9,1,1,1,1,2),
             c(5,1,1,1,1,3),
             c(6,1,1,1,1,8),
             c(4,1,1,1,1,7))
sa2_fi <- arrangeGrob(grobs = c(list(base_boxes), inset_list), 
                              layout_matrix  = lay)

rm(base, base_boxes, lay)

## Combine plots ## ------------------------------------------------------------

lay <- rbind(c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(1,1,1,2,2,2),
             c(3,3,3,3,3,3))
full_inset_plt <- arrangeGrob(grobs = list(sa2_fi, pha_fi, llegend), 
                              layout_matrix  = lay)
plot(full_inset_plt)

jsave(filename = paste0("map_phasa2.png"), 
      base_folder = paste0(base_folder, "/maps"),
      plot = full_inset_plt, square = F)
