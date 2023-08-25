# Create the geo file for Tango's MEET
library(sf)
library(dplyr)
library(ggplot2)

concord <- read.csv("O:/Population-based data/Projects/National Cancer Atlas/Atlas 2.0/Modelling/Adjacency matrix/Based on adult ERP 2010-2019/Data/Shapefile concordance.csv")
map <- st_read("O:/Population-based data/Projects/National Cancer Atlas/Atlas 2.0/Modelling/Mapping/SA2_2016_AUST.shp")

# Remove remote islands
concord <- 
  concord %>%
  subset(!is.na(ID_2238)) %>%
  mutate(SA2_5DIG16 = paste0(SA2_Code_Short))

map <- 
  map %>%
  subset(SA2_5DIG16 %in% concord$SA2_Code_Short) %>%
  arrange(as.numeric(SA2_5DIG16))

# Check the map is in the same order as the concordance file
all(order(map$SA2_5DIG16) == order(concord$SA2_Code_Short))

# Obtain centroids for each area
sf_centers <- 
  map %>%
  # st_minimum_rotated_rectangle() %>%
  st_centroid() %>% # head()
  st_geometry()

ctrs <- do.call("rbind", lapply(sf_centers,
                                 function(x) {
                                   d <- data.frame(t(as.numeric(gsub("^POINT \\(| [[:punct:]][[:digit:]]+[[:punct:]][[:digit:]]+\\)$", "", x))))
                                   names(d) = c("long", "lat")
                                   return(d)
                                 }
))

ctrs$SA2_5DIG16 <- map$SA2_5DIG16

plot.dat <- left_join(map, ctrs, by = "SA2_5DIG16")

gg.base <- ggplot(plot.dat) +
  geom_sf() +
  geom_point(aes(x = long, y = lat)) +
  # coord_fixed() +
  theme_bw()

# In case you want to create insets of the state and territory capitals
# City inset limits
lims <- data.frame(
  xmin = c(152.6, 150.35, 144.5, 115.45, 138.1, 146.8, 148.6, 130.3),
  xmax = c(153.6, 151.35, 145.5, 116.45, 139.1, 147.8, 149.6, 131.3),
  ymin = -c(28, 34.4, 38.4, 32.5, 35.4, 43.4, 35.8, 13),
  ymax = -c(27, 33.4, 37.4, 31.5, 34.4, 42.4, 34.8, 12),
  city = c("Brisbane", "Sydney", "Melbourne", "Perth", "Adelaide", "Hobart", "Canberra", "Darwin")
)

gg.inset <- vector("list", 8)

for (i in 1:nrow(lims)) {
  gg.inset[[i]] <- gg.base +
    xlim(lims$xmin[i], lims$xmax[i]) +
    ylim(lims$ymin[i], lims$ymax[i]) +
    ggtitle(label = lims$city[i])
}

library(gridExtra)

layout <- rbind(c(8,9,9,1),
                c(4,9,9,2),
                c(5,3,6,7)
)

all.maps <- grid.arrange(grobs = list(gg.inset[[1]], gg.inset[[2]], gg.inset[[3]], gg.inset[[4]],
                                      gg.inset[[5]], gg.inset[[6]], gg.inset[[7]], gg.inset[[8]], gg.base),
                         layout_matrix = layout)


ggsave("O:/Population-based data/Projects/National Cancer Atlas/Atlas 2.0/Modelling/Mapping/Tangos MEET/Map of centroids with insets.png",
       plot = all.maps, 
       dpi = 150, width = 580/72, height = 600/72)

dat <- data.frame(id = concord$ID_2238[match(ctrs$SA2_5DIG16, 
                                             concord$SA2_Code_Short)],
                  y = ctrs$lat,
                  x = ctrs$long,
                  yrad = ctrs$lat * pi / 180,
                  xrad = ctrs$long * pi / 180)

# colnames(dat) = NULL

write.csv(dat, "O:/Population-based data/Projects/National Cancer Atlas/Atlas 2.0/Modelling/Data/2016 ASGS/Other/SA2_2016_Aust_lat_long.csv",
          row.names = F)
