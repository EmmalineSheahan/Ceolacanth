library(utils)
library(dplyr)
library(raster)
library(rgdal)

# the epsg code for the WOA and sediment data is 4326
epsg <- make_EPSG()
wanted_crs <- epsg %>% filter(code == 4326)
wanted_crs <- wanted_crs$prj4

# create a raster layer to use as a base, EPSG 4326, 1 degree resolution
raster_base <- raster(ncols = 360, nrows = 180, xmn = -180, xmx = 180, 
                      ymn = -90, ymx = 90, crs = wanted_crs)

# Should I pull in a continent shapefile to use as a mask?
continent <- shapefile('./coelacanth_data/continent/continent.shp')
continent <- spTransform(continent, CRSobj = CRS(wanted_crs))

# unzipping the sediment data
untar('./coelacanth_data/sediment/sediment.tar.gz', list=TRUE)
untar('./coelacanth_data/sediment/sediment.tar.gz', exdir = './coelacanth_data/sediment')

# exploring the sediment data
sediment1 <- read.delim('./coelacanth_data/sediment/g00127sample.txt', header = F)
class(sediment1)
head(sediment1)

sediment2 <- read.delim('./coelacanth_data/sediment/g00127cruise.txt', header = F)
class(sediment2)
head(sediment2)

sediment3 <- read.delim('./coelacanth_data/sediment/g00127interval.txt', header = F)
head(sediment3)

sediment4 <- read.delim('./coelacanth_data/sediment/g00127phi.txt', header = F)

# the interval and the sample seem to be what we want, the sample has the coordinates
# and the interval has relevant data. It looks like replicants may have to be averaged
# for each coordinate?

dim(sediment1)
dim(sediment3)

colnam_sed1 <- c("mggid", "ship", "cruise", "sample", "device_code", "device", 
                 "year", "month", "day", "time", "latdeg", "latmin", "latminhun",
                 "ns", "londeg", "lonmin", "lonminhun", "lonew", "latlon_orig", 
                 "lat", "lon", "water_depth", "core_length", "device2", "comments", "what")
colnames(sediment1) <- colnam_sed1
head(sediment1)
sediment1 <- sediment1[-1,]
sediment1 <- sediment1[,-26]
head(sediment1)

colnam_sed3 <- c("mggid", "ship", "cruise", "sample", "device_code", "device", 
                 "subcore", "interval", "replicate", "analysis_type", "depth_top_cm",
                 "depth_top_mm", "depth_bot_cm", "depth_bot_mm", "test_date",
                 "test_time", "total_weight", "coarse_meth", "fine_meth", 
                 "coarse_fine_boundary", "coarse_boundary", "fine_boundary",
                 "pct_coarser", "pct_finer", "pct_gravel", "pct_sand", "pct_silt",
                 "pct_clay", "pct_mud", "usc_gravel", "usc_sand", "usc_fines",
                 "meth_description", "mean_mm", "mean_phi", "median_phi", "modal_phi",
                 "skewness", "kurtosis", "std_dev", "sort_coeff", "interval_comments",
                 "what")
colnames(sediment3) <- colnam_sed3
head(sediment3)
sediment3 <- sediment3[-1,]
sediment3 <- sediment3[,-43]

# combining sediment1 and sediment3 for a data frame with coordinates and measurements
use4merge <- c("mggid", "ship", "cruise", "sample", "device_code", "device")
sediment_df <- merge(sediment3, sediment1, by = use4merge, all.x = T)
sediment_df$mean_mm <- as.numeric(as.character(sediment_df$mean_mm))
sediment_df$mean_phi <- as.numeric(as.character(sediment_df$mean_phi))
save(sediment_df, file = './coelacanth_data/sediment/sediment_df.Rdata')

# converting the sediment dataframe into spatial points
sed_lon <- as.numeric(as.character(sediment_df$lon))
sed_lat <- as.numeric(as.character(sediment_df$lat))
sed_coords <- cbind(sed_lon, sed_lat)
class(sed_coords)
sed_coords <- data.frame(sed_coords)
colnames(sed_coords) <- c("lon", "lat")
head(sed_coords)
which(is.na(sed_coords))
sediment_sp <- SpatialPointsDataFrame(coords = sed_coords, data = sediment_df,
                                           proj4string = CRS(wanted_crs))

# converting spatial points to raster for sediment
sediment_phi_raster <- rasterize(sediment_sp, raster_base, field = sediment_df$mean_phi)

# Temperature data
untar('./coelacanth_data/temperature/woa18_decav_t01mn01_shape.tar.gz', 
      exdir = './coelacanth_data/temperature')
temp_jan <- shapefile('./coelacanth_data/temperature/monthly/woa18_decav_t01mn01.shp')
class(temp_jan)

# function to open temperature shapefiles 
# month_num must be 2 digits in character form, 13 is winter and 15 is summer
# which_dir must be a character string, corresponds to monthly or seasonal
open_temperature <- function(month_num, which_dir) {
  untar(paste0('./coelacanth_data/temperature/', which_dir, '/woa18_decav_t', 
               month_num, 'mn01_shape.tar.gz'), 
        exdir = paste0('./coelacanth_data/temperature/', which_dir))
  t <- shapefile(paste0('./coelacanth_data/temperature/', which_dir,
                '/woa18_decav_t', month_num, 'mn01.shp'))
  return(t)
}

temp_feb <- open_temperature('02', "monthly")
temp_mar <- open_temperature('03', "monthly")
temp_apr <- open_temperature('04', "monthly")
temp_may <- open_temperature('05', "monthly")
temp_jun <- open_temperature('06', "monthly")
temp_jul <- open_temperature('07', "monthly")
temp_aug <- open_temperature('08', "monthly")
temp_sep <- open_temperature('09', "monthly")
temp_oct <- open_temperature('10', "monthly")
temp_nov <- open_temperature('11', "monthly")
temp_dec <- open_temperature('12', "monthly")

temp_winter <- open_temperature('13', "seasonal")
temp_summer <- open_temperature('15', "seasonal")

# replacing -999.999 with NA
for (i in 1:57) {
     temp_jan@data[,i] <- gsub(pattern = '-999.999', replacement = 'NA', 
                               x = temp_jan@data[,i])
}

for (i in 1:102) {
  temp_winter@data[,i] <- gsub(pattern = '-999.999', replacement = 'NA', 
                            x = temp_winter@data[,i])
}

for (i in 1:102) {
  temp_summer@data[,i] <- gsub(pattern = '-999.999', replacement = 'NA', 
                            x = temp_summer@data[,i])
}

# all of the data are annoyingly in character mode, so they have to be changed 
# to numeric
for (i in 1:102) {
  temp_winter@data[,i] <- as.numeric(temp_winter@data[,i])
}

for (i in 1:102) {
  temp_summer@data[,i] <- as.numeric(temp_summer@data[,i])
}

# want to be rasterized based on mean depth ranging from 40 - 400 m
temp_winter@data$meandepth <- rowMeans(temp_winter@data[,9:33])
temp_summer@data$meandepth <- rowMeans(temp_summer@data[,9:33])

# assign CRS to spatial polygons
proj4string(temp_winter) <- CRS(wanted_crs)
proj4string(temp_summer) <- CRS(wanted_crs)

# rasterizing Winter and Summer temperature values
temp_winter_raster <- rasterize(temp_winter, raster_base, field = "meandepth")
mode(temp_winter@data$meandepth)




