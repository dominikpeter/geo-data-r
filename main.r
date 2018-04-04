
rm(list = ls())

library(rgdal)
library(maptools)
library(dplyr)
library(broom)
library(ggplot2)
library(viridis)
library(raster)


theme_map <- function(...) {
  theme_minimal() +
    theme(
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      # panel.grid.minor = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      panel.border = element_blank(),
      ...
    )
}

shape <- readOGR("./geodata/map/swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET.shp",
                 layer="swissBOUNDARIES3D_1_3_TLM_HOHEITSGEBIET")

crs(shape) <- "+proj=somerc +lat_0=46.95240555555556 
+lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 
+ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"

map_data <- broom::tidy(shape)

relief <- raster("./geodata/topo/PK1M_LV95_RELI_1000_2016_1.tif")

crs(relief) <-  "+proj=somerc +lat_0=46.95240555555556 
+lon_0=7.439583333333333 +k_0=1 +x_0=600000 +y_0=200000 
+ellps=bessel +towgs84=674.374,15.056,405.346,0,0,0,0 +units=m +no_defs"

relief <- relief %>%
  as("SpatialPixelsDataFrame") %>%
  as_data_frame()

names(relief) <- c("relief", "long", "lat")

relief <- relief %>% filter(relief>0)

relieff <- relief[sample(nrow(relief), 100000), ]


p <- ggplot() +
  geom_raster(data=relieff, aes(x = long, y = lat, alpha = relief)) +
  geom_polygon(data=map_data, aes(x = long, y = lat, group = group)) +
  geom_path(data=map_data, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.1) +
  coord_equal() +
  theme_map() +
  labs(x = NULL, 
       y = NULL, 
       title = "Switzerland's regional demographics", 
       subtitle = "Average age in Swiss municipalities, 2015", 
       caption = "Geometries: ThemaKart, BFS; Data: BFS, 2016")
p












