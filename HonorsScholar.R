library(tidyverse)
library(dplyr)
library(tigris)
library(sf)
library(sp)
library(GISTools)

# get gis data about census tracts
philly_sp <- tracts(42, county = 101) # units for aland and awater are m^2
philly_df <- fortify(phil1)

# load in public GSI project data
public_gsi <- st_read("Desktop/HonorsScholar/DataSets/GSI_Public_Projects_Point/GSI_Public_Projects_Point.shp")
public_gsi <- fortify(public_gsi)

# plot public GIS projects and census tracts
ggplot() + 
  geom_polygon(aes(x = long, y = lat, group = group), data = philly_df) +
  geom_sf(aes(), data = public_gsi)

# convert public_gsi to spatial points dataframe to be able to use poly.counts()
public_sp <- as_Spatial(public_gsi)
public_sp <- spTransform(public_sp, crs(philly_sp))

# count number of public project points per census tract
count <- poly.counts(public_sp, philly_sp)
count <- setNames(count, philly_sp@data$NAMELSAD)
count <-as.data.frame(count)
colnames(count)[1] <- "points"
count <- tibble::rownames_to_column(count, "NAMELSAD")

# join together counts and shapes of census tracts as dataframe and calculate density of points for public
public_count <- left_join(as.data.frame(philly_sp), count, by = "NAMELSAD")
public_count$ALAND <- as.numeric(public_count$ALAND)
public_count <- transform(public_count, density = points / ALAND)


