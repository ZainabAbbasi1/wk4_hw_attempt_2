#homework - wk4


library(sf)
library(tidyverse)
library(dplyr)
library(raster)
library(terra)
library(tmap)

install.packages("terra")
install.packages("usegit")
library(usegit)


#set working directiory

setwd("~/Documents/GIS/Wk_4/homework")

ggi <-  read_csv("~/Documents/GIS/Wk_4/homework/ggi.csv")

world_data <- st_read("~/Documents/GIS/Wk_4/homework/World_Countries_(Generalized)_9029012925078512962.geojson")


#plot outline

plot(world_data)


#clean


ggi$indexCode <- NULL
ggi$index <- NULL
ggi$dimension <- NULL
ggi$note <- NULL
ggi$indicatorCode <- NULL
ggi$COUNTRYAFF <- NULL
ggi$AFF_ISO <- NULL
ggi$ISO <- NULL
ggi$indicator <- NULL
ggi$indicator <- NULL
ggi$countryIsoCode <- NULL


world_data$ISO <- NULL
world_data$COUNTRYAFF <- NULL
world_data$AFF_ISO <- NULL
world_data$FID <- NULL

world_data <- world_data %>%
  dplyr::rename(country = COUNTRY)


#merge

spatial_data <- merge(world_data,ggi, by.x= "country", by.y = "country", all.x = TRUE)



#filter

spatial_data_filter = spatial_data %>%filter(year %in% c(2010, 2019))


#pivot long

wide_df <- spatial_data_filter %>%
  pivot_wider(
    names_from = year,                     # Column to use for new column names
    values_from = value,       # Column to fill the new columns
    names_prefix = "gender_inequality"                 # Optional: prefix for new column names
  )


#find difference and create new column

final_df <- wide_df %>%
  mutate(difference_in_ggi = (gender_inequality2019 - gender_inequality2010))


#map

tmap_mode("view")
tm_shape(final_df) + 
  tm_polygons("difference_in_ggi", 
              style="pretty",
              palette="-RdYlBu",
              midpoint=0,
              title="Gender Inequality Index",
              alpha = 0.9)  +
  
  tm_scale_bar(position = c("right", "bottom")) +
  tm_layout(title = "Difference in Gender Inequality Index", title.size = 1)



#countries with low value is doing the best
#countries with high value is doing bad

tmap_mode("plot")
tm_shape(final_df) + 
  tm_polygons("difference_in_ggi", 
              style="pretty",
              palette="-RdYlBu",
              midpoint=0,
              alpha = 0.9)  +
  
  tm_scale_bar(position = c("left", "bottom")) +
  tm_layout(
    legend.position = c("left", "bottom")  # Center the legend at the bottom
  )



