# More funs

# simple function to streamline the saving of plots
jsave <- function(filename, base_folder, 
                  plot = last_plot(), 
                  square = T, square_size = 5000, 
                  ratio = c(6,9)){
  if(square){
    ggsave(filename = filename,
           plot = plot,
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
           plot = plot, 
           path = base_folder,
           dpi = 1000,
           width = round(b),
           height = round(a),
           scale = 1,
           units = "px")
  }
}

# used in pipes to enforce rounding
make_numeric_decimal <- function(.data, digits = 2){
  df <- .data
  cols_to_format <- unlist(lapply(df, is.numeric))
  df[,cols_to_format] <- bind_cols(lapply(df[,cols_to_format], sprintf, fmt = paste0('%#.', digits, 'f')))
  return(df)
}

# adds boxlabels to maps
addBoxLabel <- function(i, color = "white", size = 0.5){
  if(lims$position[i] == "r"){
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", y = mean(c(lims$ymin[i], lims$ymax[i])), 
               x = lims$xmax[i] + 1, label = lims$initials[i],
               size = 3) 
    )
  } else if(lims$position[i] == "b"){
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", x = mean(c(lims$xmin[i], lims$xmax[i])), 
               y = lims$ymin[i] - 1, label = lims$initials[i],
               size = 3) 
    )
  } else if(lims$position[i] == "l"){
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", y = mean(c(lims$ymin[i], lims$ymax[i])), 
               x = lims$xmin[i] - 1, label = lims$initials[i],
               size = 3) 
    )
  }else{
    list(
      annotate("rect", 
               xmin = lims$xmin[i], xmax = lims$xmax[i],
               ymin = lims$ymin[i], ymax = lims$ymax[i],
               color = color, fill = NA, size = size),
      annotate("text", x = mean(c(lims$xmin[i], lims$xmax[i])), 
               y = lims$ymax[i] + 1, label = lims$initials[i],
               size = 3) 
    )
  }
}

# small function
apa <- function(.data, .m_n = FALSE, model_name = NULL){
  if(.m_n){
    temp <- .data %>% 
      mutate(ps_area = 1:nrow(.),
             model = model_name)
  }else{
    temp <- .data %>% 
      mutate(ps_area = 1:nrow(.))
  }
}

# state names
state_name_concor <- data.frame(ps_state = 1:8,
                                state_name_short = c("NSW", "VIC", "QLD",
                                                     "SA", "WA", "TAS",
                                                     "NT", "ACT"),
                                state_name = c("New South Wales",
                                               "Victoria",
                                               "Queensland",
                                               "South Australia",
                                               "Western Australia",
                                               "Tasmania",
                                               "Northern Territory",
                                               "Australian Capital Territory"))

# To be used in `mutate()` to relabel rf 
getRFFullNames <- function(x){
  temp <- case_when(
    x == "waist_circum" ~ "Risky waist\ncircumference",
    x == "smoking" ~ "Current smoking",
    x == "overweight" ~ "Overweight",
    x == "obesity" ~ "Obesity",
    x == "diet" ~ "Diet",
    x == "alcohol" ~ "Alcohol",
    x == "activityleis" ~ "Leisure physical\nactivity",
    x == "activityleiswkpl" ~ "All physical\nactivity"
  )
  return(factor(temp, 
                levels = c("Current smoking",
                "Alcohol",
                "Diet",
                "Obesity",
                "Overweight",
                "Risky waist\ncircumference",
                "Leisure physical\nactivity",
                "All physical\nactivity")))
}

# Adds correct color scale to ra_sa2
addRemotenessColor <- function(){
  scale_fill_manual(breaks = c("Major Cities", "Inner Regional",
                               "Outer Regional", "Remote", "Very Remote"),
                    values = c('#fef0d9','#fdcc8a','#fc8d59','#e34a33','#b30000'))
}

# Adds correct color scale to irsd_5c
addIRSDColor <- function(){
  scale_fill_manual(breaks = c("1 - least\nadvantaged", "2", "3", "4",
                               "5 - most\nadvantaged"),
                    values = rev(c('#edf8e9','#bae4b3','#74c476','#31a354','#006d2c')))
}

# converts numbers to proportions
returnProps <- function(x, y){
  out <- as.numeric(NA)
  for(i in 1:length(x)){
    if(y[i] == 0){
      out[i] <- 0
    }else{
      out[i] <- 100*(x[i]/y[i])
    }
  }
  return(out)
}

# check percentages are between zero and one
checkBounds <- function(x){
  ifelse(x > 100, 100, ifelse(x < 0, 0, x))
}
