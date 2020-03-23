library(tidyverse)
library(dplyr)
library(tigris)
library(sf)
library(sp)
library(GISTools)

# get gis data about census tracts
phil1 <- tracts(42, county = 101)
phil <- fortify(phil1)

# load in public GSI project data
public_gsi <- st_read("Desktop/HonorsScholar/DataSets/GSI_Public_Projects_Point/GSI_Public_Projects_Point.shp")
public_gsi <- fortify(public_gsi)

# plot public GIS projects and census tracts
ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), data = phil) +
  geom_sf(aes(), data = public_gsi)

# convert public_gsi to spatial points dataframe to be able to use open()
public_sp <- as_Spatial(public_gsi)
public_sp <- spTransform(public_sp, crs(phil1))

# count number of public project points per census tract
public_count <- over(public_sp, phil1)
count <- poly.counts(public_sp, phil1)
count <- setNames(count, phil1@data$NAMELSAD)
count <-as.data.frame(count)
colnames(count)[1] <- "points"
colnames(count)[0] <- "NAMELSAD"
public_count <- merge(x = public_count, y = count, "NAMELSAD", all.x = TRUE)
View(public_count)
