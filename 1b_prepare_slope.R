library(raster)
library(sf)
library(magrittr)
library(mapview)
library(terra)

path2data <- './data'
path2temp_results <- './temp_results'

# load Indonesian boarder as shapefiles (as provided by Rio)
indonesia <- st_read(file.path(path2data, 'IDN_adm0', 'IDN_adm0.shp'))

# load altitude raster for indonesia and neigbouring countries
r_list <- lapply(c('Indonesia', 'Malaysia', 'East Timor', 'Papua New Guinea'), 
                 function(x) {
                   getData('alt', country = x)
                 })
r1 <- do.call('merge', r_list)

# st_crs(indonesia)
# raster::crs(r1)
# raster::projectRaster(r1, crs = st_crs(indonesia))
# set all NA values (should all be within ocean) to 0. Reason: avoid getting ring of NA cells when calc the slope
r1[is.na(r1)] <- 0

# calculate slope 
r2 <- raster::terrain(r1, 'slope', 'tangent')

# clip RasterStack to indonesian borders
r3 <- raster::crop(r2, indonesia)
r4 <- raster::mask(r3, indonesia)


#TODO: there is an outer 'ring' with missing (NA) cells between the raster and 
# the indonesian borders. Comes from the 'terrain' function that uses 
# 8 neighbouring cells to compute the slope 
# -> does not work for boundary cells. Solution: create buffer around the raster
#r5 <- rast(r4)
#r6 <- buffer(r5, width = res(r5)[1]) 

mapview(r4)

raster::writeRaster(r4, file.path(path2temp_results, 'slope_tangent.tif'), 
            overwrite = TRUE)
