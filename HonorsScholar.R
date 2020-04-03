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
# load in private GSI project data
private_gsi_regs <- st_read("Desktop/HonorsScholar/DataSets/GSI_Private_Projects_Regs/GSI_Private_Projects_Regs.shp")
private_gsi_regs <- fortify(private_gsi_regs)
private_gsi_retro <- st_read("Desktop/HonorsScholar/DataSets/GSI_Private_Projects_Retrofit/GSI_Private_Projects_Retrofit.shp")
private_gsi_retro <- fortify(private_gsi_retro)

# convert public_gsi to spatial points dataframe to be able to use poly.counts()
public_sp <- as_Spatial(public_gsi)
public_sp <- spTransform(public_sp, crs(philly_sp)) # converts to same coordinate system as philly_sp
# convert private_gsi_regs to spatial points dataframe
private_regs_sp <- as_Spatial(private_gsi_regs)
private_regs_sp <- spTransform(private_regs_sp, crs(philly_sp))
# convert private_gsi_retro to spatial points dataframe
private_retro_sp <- as_Spatial(private_gsi_retro)
private_retro_sp <- spTransform(private_retro_sp, crs(philly_sp))

# count number of public project points per census tract
public_count <- poly.counts(public_sp, philly_sp)
public_count <- setNames(public_count, philly_sp@data$NAMELSAD)
public_count <-as.data.frame(public_count)
colnames(public_count)[1] <- "public_points"
public_count <- tibble::rownames_to_column(public_count, "NAMELSAD")
# count number of private regulation project points per census tract
private_regs_count <- poly.counts(private_regs_sp, philly_sp)
private_regs_count <- setNames(private_regs_count, philly_sp@data$NAMELSAD)
private_regs_count <- as.data.frame(private_regs_count)
colnames(private_regs_count)[1] <- "regs_points"
private_regs_count <- tibble::rownames_to_column(private_regs_count, "NAMELSAD")
# count number of private retrofit project points per census tract
private_retro_count <- poly.counts(private_retro_sp, philly_sp)
private_retro_count <- setNames(private_retro_count, philly_sp@data$NAMELSAD)
private_retro_count <- as.data.frame(private_retro_count)
colnames(private_retro_count)[1] <- "retro_points"
private_retro_count <- tibble::rownames_to_column(private_retro_count, "NAMELSAD")

# join together counts and shapes of census tracts as dataframe
count <- left_join(as.data.frame(philly_sp), public_count, by = "NAMELSAD")
count <- left_join(count, private_regs_count, by = "NAMELSAD")
count <- left_join(count, private_retro_count, by = "NAMELSAD")

# add up points and calculate densities for individual and together
count <- transform(count, private_points = regs_points + retro_points)
count <- transform(count, all_points = private_points + public_points)

count$ALAND <- as.numeric(count$ALAND)
count <- transform(count, public_density = public_points / ALAND)
count <- transform(count, regs_density = regs_points / ALAND)
count <- transform(count, retro_density = retro_points / ALAND)
count <- transform(count, private_density = private_points / ALAND)
count <- transform(count, all_density = all_points / ALAND)

# deleting unneeded columns
count$STATEFP <- NULL
count$COUNTYFP <- NULL
count$TRACTCE <- NULL
count$GEOID <- NULL
count$NAME <- NULL
count$MTFCC <- NULL
count$FUNCSTAT <- NULL
count$INTPTLAT <- NULL
count$INTPTLON <- NULL

# joining count and spatial data to use ggplot
philly_count <- geo_join(philly_sp, count, "NAMELSAD", "NAMELSAD")
philly_count_sf <- st_as_sf(philly_count)

# maps with density coded in as fill
ggplot() +
  geom_sf(aes(fill = public_density), data = philly_count_sf) 
ggplot() +
  geom_sf(aes(fill = regs_density), data = philly_count_sf)
ggplot() +
  geom_sf(aes(fill = retro_density), data = philly_count_sf)
ggplot() +
  geom_sf(aes(fill = private_density), data = philly_count_sf)
ggplot() +
  geom_sf(aes(fill = all_density), data = philly_count_sf)

# import census data
income <- read.csv("Desktop/HonorsScholar/DataSets/ACS_17_5YR_S1903/ACS_17_5YR_S1903_with_ann.csv", header = TRUE)
demographics <- read.csv("Desktop/HonorsScholar/DataSets/ACS_17_5YR_S0601/ACS_17_5YR_S0601_with_ann.csv", header = TRUE)
colnames(income)[2] <- "GEOID"
colnames(demographics)[2] <- "GEOID"

# join demographic data with points data
philly_count_sf$GEOID <-as.numeric(philly_count_sf$GEOID)
count_demographics <- geo_join(philly_count_sf, demographics, "GEOID", 'GEOID')

