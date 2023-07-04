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

## Reference from https://rdrr.io/github/tidymodels/tune/src/R/coord_obs_pred.R

CoordObsPred <-
  ggplot2::ggproto(
    "CoordObsPred",
    ggplot2::CoordFixed,
    setup_panel_params = function(self, scale_x, scale_y, params = list()) {
      # coord limits take precedence over scale limits
      rngs <- range(
        self$limits$x %||% scale_x$get_limits(),
        self$limits$y %||% scale_y$get_limits(),
        na.rm = TRUE
      )
      self$limits$y <- rngs
      self$limits$x <- rngs
      ggplot2::ggproto_parent(ggplot2::CoordFixed, self)$setup_panel_params(scale_x, scale_y, params)
    },
    aspect = function(self, ranges) {
      1 / self$ratio
    }
  )

#' Use same scale for plots of observed vs predicted values
#'
#' For regression models, `coord_obs_pred()` can be used in a ggplot to make the
#' x- and y-axes have the same exact scale along with an aspect ratio of one.
#' @param ratio	Aspect ratio, expressed as `y / x`. Defaults to 1.0.
#' @param xlim,ylim Limits for the x and y axes.
#' @param expand Not currently used.
#' @param clip Should drawing be clipped to the extent of the plot panel? A setting
#' of "on" (the default) means yes, and a setting of "off" means no. In most
#' cases, the default of "on" should not be changed, as setting `clip = "off"`
#' can cause unexpected results. It allows drawing of data points anywhere on
#' the plot, including in the plot margins. If limits are set via `xlim` and
#' `ylim` and some data points fall outside those limits, then those data points
#' may show up in places such as the axes, the legend, the plot title, or the
#' plot margins.
#' @return A `ggproto` object.
#' @examples
#' data(solubility_test, package = "modeldata")
#'
#' library(ggplot2)
#' p <- ggplot(solubility_test, aes(x = solubility, y = prediction)) +
#'   geom_abline(lty = 2) +
#'   geom_point(alpha = 0.5)
#'
#' p
#'
#' p + coord_fixed()
#'
#' p + coord_obs_pred()
#' @export
coord_obs_pred <-
  function(ratio = 1,
           xlim = NULL,
           ylim = NULL,
           expand = TRUE,
           clip = "on") {
    ggplot2::ggproto(
      NULL,
      CoordObsPred,
      limits = list(x = xlim, y = ylim),
      ratio = ratio,
      expand = expand,
      clip = clip
    )
  }