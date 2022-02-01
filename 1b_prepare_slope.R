library(raster)
library(sf)
library(magrittr)
library(mapview)
library(terra)

path2data <- './data'
path2temp_results <- './temp_results'

# load Indonesian boarder as shapefiles (as provided by Rio)
indonesia <- st_read(file.path(path2data, 'IDN_adm0', 'IDN_adm0.shp'))

# load altitude raster for indonesia
r1 <- getData('alt', country = 'Indonesia')

# clip RasterStack to indonesian borders
r2 <- raster::crop(r1, indonesia)
r3 <- raster::mask(r2, indonesia)

# calculate slope 
r4 <- raster::terrain(r3, 'slope', 'tangent')

r5 <- rast(r4)

# begin test
grid <- st_make_grid(indonesia, n = 20)
grid <- grid[indonesia]

r6 <- raster::crop(r4, st_bbox(grid[1,]))

mapview(r6)
r7 <- raster::buffer(r6, width =3000) 
r7 <- terra::buffer(rast(r6), width = res(r5)[1]*2) 
mapview(r6) + mapview(r7)
res(r5)

# end 


#TODO: there is an outer 'ring' with missing (NA) cells between the raster and 
# the indonesian borders. Comes from the 'terrain' function that uses 
# 8 neighbouring cells to compute the slope 
# -> does not work for boundary cells. Solution: create buffer around the raster
r6 <- buffer(r5, width = res(r5)[1]) 

mapview(r4)

writeRaster(r4, file.path(path2temp_results, 'slope_tangent.tif'), 
            overwrite = FALSE)
