##R workshop 10/24/2018
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

#load the data
cscidat <- read.csv('data/cscidat.csv', stringsAsFactors = F)
ascidat <- read.csv('data/ascidat.csv', stringsAsFactors = F)

# get the dimensions
dim(cscidat)
dim(ascidat)

# get the column names
names(cscidat)
names(ascidat)

# see the first six rows
head(cscidat)
head(ascidat)

# get the overall structure
str(cscidat)

#wrangling

# first, select some columns
dplyr_sel1 <- select(cscidat, SampleID_old, New_Lat, New_Long, CSCI)
head(dplyr_sel1)

# select everything but CSCI and COMID
dplyr_sel2 <- select(cscidat, -CSCI, -COMID)
head(dplyr_sel2)

# select columns that contain the letter c
dplyr_sel3 <- select(cscidat, matches('c'))
head(dplyr_sel3)

#filtering
# get CSCI scores greater than 0.79
dplyr_filt1 <- filter(cscidat, CSCI > 0.79)
head(dplyr_filt1)

# get CSCI scores above latitude 37N
dplyr_filt2 <- filter(cscidat, New_Lat > 37)
head(dplyr_filt2)

# use both filters
dplyr_filt3 <- filter(cscidat, CSCI > 0.79 & New_Lat > 37)
head(dplyr_filt3)

# get observed taxa
dplyr_mut1 <- mutate(cscidat, observed = OE * E)
head(dplyr_mut1)

# add a column for lo/hi csci scores
dplyr_mut2 <- mutate(cscidat, CSCIcat = ifelse(CSCI <= 0.79, 'lo', 'hi'))
head(dplyr_mut2)

# arrange by CSCI scores
dplyr_arr <- arrange(cscidat, CSCI)
head(dplyr_arr)

# rename lat/lon (note the multiple arguments)
dplyr_rnm <- rename(cscidat, 
                    lat = New_Lat,
                    lon = New_Long
)
head(dplyr_rnm)

cscidat <- select(cscidat, SampleID_old, New_Lat, New_Long, CSCI)    

cscidat <- rename(cscidat, 
                  id = SampleID_old, 
                  lat = New_Lat, 
                  lon = New_Long
)

library(dplyr)
alldat <- inner_join(cscidat, ascidat, by = "id")
colnames(cscidat)

##ggplot
ggplot(data = alldat) +
  geom_point(mapping = aes(x = CSCI, y = ASCI))

# making maps part 1 ------------------------------------------------------

#read the data
ascidat <- read_csv("data/ascidat.csv")
cscidat <- read_csv("data/cscidat.csv")
latlons <- read_csv("data/latlon.csv")

#load libraries
library("tidyverse")
library("sf")
library("mapview")
library("viridis")
library("USAboundaries")

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


