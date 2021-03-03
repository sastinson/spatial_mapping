
# install.packages("USAboundaries")

#load libraries
library("tidyverse")
library("sf")
library("mapview")
library("viridis")
library("USAboundaries")
library(ggplot2)
library(dplyr)


summary(latlons)

# make data sf object: 
df_sf <- st_as_sf(latlons,
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
co_names <- c("Sacramento") # notice we can add more states to this list if we want

# filter to just that county:
sacto_co <- filter(CA_counties, name==co_names)

# quick map
ggplot() +
  geom_sf(data=CA_counties, color="gray")+
  geom_sf(data=sacto_co, color="purple")

sacto_pts <- st_intersection(df_sf, sacto_co)

# ggplot of cropped data
ggplot() +
  geom_sf(data=sacto_co, color="gray")+
  geom_sf(data=sacto_pts, fill="orange", pch=21) +
  theme_bw()


# join with CSCI data -----------------------------------------------------

sacto_csci <- left_join(sacto_pts, cscidat, by=c("StationID"="StationCode"))

class(sacto_csci)

mapview(sacto_csci, zcol="CSCI", layer="CSCI") + mapview(sacto_co)


# save out spatial data ---------------------------------------------------

st_write(obj = sacto_csci, "data/sacramento_county.csci.shp")


