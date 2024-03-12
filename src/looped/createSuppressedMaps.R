## -----------------------------------------------------------------------------
## MAPS ## ---------------------------------------------------------------------
## -----------------------------------------------------------------------------

source("src/ms.R")

modelled_est_all <- lapply(1:8, FUN = function(x)readRDS(file = paste0("data/summary_files/", names(raw_est)[x], "_b1.rds")))

## START FOR LOOP #### ---------------------------------------------------------
for(k in 1:8){
  
  rf <- names(raw_est)[k]
  rf_full <- lookup[k,]$rf_full
  message("Started ", k, ": ", rf)
  m_s <- Sys.time()
  
  # load data
  modelled_est <- modelled_est_all[[k]]
  #modelled_est <- readRDS(file = paste0("data/summary_files/", rf, "_b1.rds"))

## PREVALENCE #### -------------------------------------------------------------

# squish the top and lower 2.5 quantiles
rar<- unname(quantile(modelled_est$summ$sa2$mu_median, p = c(0.01,0.99)))
  
# base map
base <- modelled_est$summ$sa2_map %>% 
  mutate(suppressed = (rr_CV_b > 50 | N_persons < 100),
         mu_median = ifelse(suppressed, NA, mu_median)) %>%
  ggplot()+
  theme_void()+
  geom_sf(aes(fill = mu_median), col = NA)+
  scale_fill_viridis_c(begin = 0, end = 1, 
                       direction = -1,
                       option = "F", 
                       limits = rar, oob = scales::squish)+
  # JESS ADD: geom_fill_pattern(...)+
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
    addBoxLabel(i, color = "black", size = 0.2, textsize = 2)
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
                               top = textGrob(paste0(rf_full, "\nSuppressed ", sum(modelled_est$summ$sa2_map$rr_CV_b > 50, na.rm = T), " SA2s."),gp=gpar(fontsize=8)))

# save object
jsave(filename = paste0("musuppressed_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
message("---- Finished prevalence")

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
  mutate(rr_median = ifelse(rr_CV_b > 50, NA, rr_median)) %>% 
  ggplot(aes(fill = log2(rr_median)))+
  theme_void()+
  geom_sf(col = NA)+
  scale_fill_gradientn(colors = Fill.colours,
                       values = rescale(Fill.values),
                       labels = as.character(round(Breaks.fill, 2)),
                       breaks = log2(Breaks.fill),
                       limits = range(Fill.values))+
  # JESS ADD: geom_fill_pattern(...)+
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
    addBoxLabel(i, color = "black", size = 0.2, textsize = 2)
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
                              top = textGrob(paste0(rf_full, "\nSuppressed ", sum(modelled_est$summ$sa2_map$rr_CV_b > 50, na.rm = T), " SA2s."),gp=gpar(fontsize=8)))

# save plot
jsave(filename = paste0("rrsuppressed_", rf ,".png"), 
      base_folder = paste0(base_folder, "/maps_lowres"),
      plot = full_inset_plt, square = F,
      square_size = 1200,
      dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, mapping_data, lay, full_inset_plt)
message("---- Finished rrs")

## FINISH FOR LOOP #### --------------------------------------------------------

message(paste0("Run time (mins): ", round(as.numeric(Sys.time() - m_s, units = "mins"), 2)))

}

## END SCRIPT #### -------------------------------------------------------------