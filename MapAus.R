####
## User functions
####

# simple function to streamline the saving of plots
jsave <- function(filename, base_folder, 
                  plot = last_plot(), 
                  square = T, 
                  square_size = 5000,
                  scale = 1,
                  ratio = c(6,9),
                  dpi = 1000){
  if(square){
    ggsave(filename = filename,
           plot = plot,
           path = base_folder,
           dpi = dpi,
           width = square_size,
           height = square_size,
           scale = scale,
           units = "px")
  }else{
    total = square_size^2
    a <- sqrt((total*ratio[1])/ratio[2])
    b <- (ratio[2]*a)/ratio[1]
    ggsave(filename = filename,
           plot = plot, 
           path = base_folder,
           dpi = dpi,
           width = round(b),
           height = round(a),
           scale = scale,
           units = "px")
  }
}

# adds boxlabels to maps
addBoxLabel <- function(i, color = "white", size = 0.5, textsize = 3){
  if(lims$position[i] == "r"){
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", y = mean(c(lims$ymin[i], lims$ymax[i])), 
               x = lims$xmax[i] + lims$jitter[i], label = lims$initials[i],
               size = textsize) 
    )
  } else if(lims$position[i] == "b"){
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", x = mean(c(lims$xmin[i], lims$xmax[i])), 
               y = lims$ymin[i] - lims$jitter[i], label = lims$initials[i],
               size = textsize) 
    )
  } else if(lims$position[i] == "l"){
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", y = mean(c(lims$ymin[i], lims$ymax[i])), 
               x = lims$xmin[i] - lims$jitter[i], label = lims$initials[i],
               size = textsize) 
    )
  }else{
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", x = mean(c(lims$xmin[i], lims$xmax[i])), 
               y = lims$ymax[i] + lims$jitter[i], label = lims$initials[i],
               size = textsize) 
    )
  }
}

lims <- data.frame(
  xmin = c(152.6, 150.35, 144.5, 115.45, 138.1, 146.8, 148.6, 130.3),
  xmax = c(153.6, 151.35, 145.5, 116.45, 139.1, 147.8, 149.6, 131.3),
  ymin = -c(28, 34.4, 38.4, 32.5, 35.4, 43.4, 35.8, 13),
  ymax = -c(27, 33.4, 37.4, 31.5, 34.4, 42.4, 34.8, 12),
  city = c("Brisbane", "Sydney", "Melbourne", "Perth", "Adelaide", "Hobart", "Canberra", "Darwin"),
  position = c("r", "r", "b", "l", "b", "b", "r", "l"),
  inset_labs = c("B - Brisbane (Qld)", "S - Sydney (NSW)",
                 "M - Melbourne (Vic)", "P - Perth (WA)",
                 "A - Adelaide (SA)", "Ho - Hobart (Tas)",
                 "C - Canberra (ACT)", "D - Darwin (NT)"),
  jitter = c(1,1,1,1,1,1,2,1)
) %>% 
  mutate(initials = c("B", "S", "M", "P", "A", "Ho", "C", "D"))

####
## CREATE MAP 
####
  
# base map
	base <- sa2_map %>% 
		ggplot()+
		theme_void()+
		geom_sf(aes(fill = y), col = NA)+
		scale_fill_viridis_c(begin = 0, end = 1, 
						   direction = -1,
						   option = "F", 
						   oob = squish)+
		# add border of Australia
		geom_sf(data = aus_border, aes(geometry = geometry), 
			  colour = "black", fill = NA, size = 0.2)+
		# add state borders
		geom_sf(data = state_border, aes(geometry = geometry), 
			  colour = "black", fill = NA, size = 0.1)+
		theme(legend.position = "none",
			text = element_text(size = 8),
			plot.title = element_text(margin = margin(0,0,2,0)),
			plot.margin = unit(c(1,1,1,1), "mm"))

# Base map with legend
	(base_legend <- base +
		labs(fill = "Color Bar Title")+
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
								   top = textGrob("Main Title",gp=gpar(fontsize=8)))

# save object
	jsave(filename = "map.png", 
		  base_folder = base_folder,
		  plot = full_inset_plt, square = F,
		  square_size = 1200,
		  dpi = 300)

# cleanup
rm(base, base_boxes, llegend, base_legend, lay, full_inset_plt)
