library(raster)
library(rgdal)
library(dismo)
library(rJava)
library(jsonlite)
library(maptools)
library(maps)
library(tiff)
library(dplyr)
library(spatialEco)

# I used this script to pull occurrences from GBIF directly in order to examine
# them in R alone. We have a cleaned and verified occurrence dataset elsewhere

# Creating world map for plotting purposes
world <- map("world", fill = T)
class(world)
ids <- world$names
world <- map2SpatialPolygons(world, IDs = ids, proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
plot(world)
proj4string(world)

# Pulling Coelocanth occurrences from GBIF
coelacanth_raw <- gbif('Latimeria', 'chalumnae', geo = T, download = T, sp = T,
                       removeZeros = T)

# Cleaning occurrence records, first by removing duplicates
class(coelacanth_raw)
coelacanth_raw@proj4string
View(coelacanth_raw@coords)

un <- (unique(coelacanth_raw@coords))
uni <- data.frame(un)
coordinates(uni) <- ~lon+lat
class(uni)
uni
proj4string(uni) <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
plot(uni, col = "red")
plot(world, add = T)
nrow(uni@coords)

# Now further cleaning by removing points that are on land
coelacanth_cleaner <- (erase.point(uni, world, inside = T))
plot(coelacanth_cleaner, col = "red")
plot(world, add = T)
length(coelacanth_cleaner@coords)
View(coelacanth_cleaner@coords)
