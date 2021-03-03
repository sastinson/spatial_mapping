##06252019 sampling site maps
#https://sccwrp.github.io/CABW2018_R_training/index.html

# part 1 wrangling data ---------------------------------------------------


# install packages from CRAN
# install.packages("tidyverse")
# install.packages("sf")
# install.packages("mapview")
# install.packages("viridis")
# install.packages("USAboundaries")

#load libraries
library("tidyverse")
library("sf")
library("mapview")
library("viridis")
library("USAboundaries")
library(ggplot2)
library(dplyr)

#load the data
latlon <- read.csv('latlon.csv', stringsAsFactors = F)

# making maps part 1 ------------------------------------------------------

summary(latlons)

# make data sf object: 
df_sf <- st_as_sf(latlon,
                  # coords = c("Lon", "Lat"), # can use numbers here too
                  coords = c(3, 2), # can use numbers here too
                  remove = F, # don't remove these lat/lon cols from df
                  crs = 4326) # add projection (this is WGS84)

class(df_sf)

# check CRS first:
st_crs(df_sf)

plot(df_sf)

# single layer
plot(df_sf$geometry)

# spatial games -----------------------------------------------------------

library(USAboundaries)

# Pick a State
state_names <- c("california") # notice we can add more states to this list if we want

# Download STATE data and add projection
CA<-us_states(resolution = "high", states = state_names) # what does resolution do?

st_crs(CA) # double check the CRS

# make a quick plot!
plot(CA$geometry)

# add data from above? use "add=TRUE"
plot(df_sf$geometry, col="blue", add=TRUE)


###ggplot example
# ggplot map
ggplot() + # can include data here, if only using one dataset
  geom_sf(data=CA, color="gray") +
  geom_sf(data=df_sf, aes(color=New_Long)) +
  labs(x="Longitude", y="Latitude", title= "Map of stuff")

##save your dataaaa
ggsave(filename = "figures/map_of_cscsi_points.png", width = 8.5, height = 11, units = "in", dpi = 200)

# crop and join -----------------------------------------------------------
# Download outlines of all the counties for California
CA_counties<-us_counties(resolution = "high", states = "CA") # what does resolution do?
plot(CA_counties$geometry)

# Pick a county of interest
co_names <- c("San Luis Obispo", "Monterey") # notice we can add more states to this list if we want

# filter to just that county:
monslo_co <- subset(CA_counties, name=="San Luis Obispo"|name=="Monterey")

# quick map
ggplot() +
  geom_sf(data=CA_counties, color="gray")+
  geom_sf(data=monslo_co, color="blue")

monslo_pts <- st_intersection(df_sf, monslo_co)

# ggplot of cropped data
ggplot() +
  geom_sf(data=monslo_co, color="gray")+
  geom_sf(data=monslo_pts, fill="orange", pch=21) +
  theme_bw()


library(raster)
bre2 <- c(1, 2, 3, 4, 5, 6)

colors <- c("orange", "yellow", 
            "lightgreen","green",
            "firebrick", "forestgreen")

mapview(monslo_pts, zcol="Sample", col.regions = colors, at = bre2,
        layer="Sample", cex = 10,legend = mapviewGetOption("legend"), 
        legend.opacity = 0) + mapview(monslo_co, map.types = "Esri.WorldImagery", legend = FALSE) 


##save your dataaaa
ggsave(filename = "/Users/WeeRedLass/Box Sync/DPR_2019/metabarcoding/data/figures/2019_map_of_samplesites.png", width = 8.5, height = 11, units = "in", dpi = 200)


# join with CSCI data -----------------------------------------------------

monslo_csci <- left_join(monslo_pts, cscidat, by=c("StationCode"="id"))

class(monslo_csci)

bre <- c(1.4, 1.2, 1.0, 0.8, 0.6, 0.4, 0.2)
bre2 <- c(1, 2, 3, 4, 5, 6)
library(raster)

colors <- c("firebrick", "orange",
            "yellow", "lightgreen",
            "green",
            "forestgreen")

mapview(monslo_csci, zcol="NAME", col.regions = colors, at = bre2,
        layer="NAME", cex = 10,legend = mapviewGetOption("legend"), 
        legend.opacity = 0) + mapview(monslo_co, map.types = "Esri.WorldImagery", legend = FALSE) 

# save out spatial data ---------------------------------------------------

st_write(obj = monslo_csci, "/Users/WeeRedLass/Box Sync/DPR_2018/data/slo_county.csci.shp")

##save your dataaaa
ggsave(filename = "/Users/WeeRedLass/Box Sync/DPR_2018/data/figures/monslo_map_of_csci.png", width = 8.5, height = 11, units = "in", dpi = 200)

