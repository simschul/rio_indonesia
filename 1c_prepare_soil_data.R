library(terra)
library(hwsdr)
library(raster)
library(magrittr)
library(sf)
library(mapview)

# get LON LAT extend of Indonesia
indonesia <- st_read(file.path('data', 'IDN_adm0', 'IDN_adm0.shp'))
extend <- st_bbox(indonesia)
extend <- c(extend[2], extend[1], extend[4], extend[3])

# specify parameters here, see: https://daac.ornl.gov/SOILS/guides/HWSD.html, table 1
params <- c("REF_DEPTH", 'T_PH_H2O', 'S_PH_H2O') 

# Download soil data from HWSDR
soil_data <- ws_subset(
  site = "HWSD",
  location = extend,
  param = params, 
  path = file.path('data', 'HWSD'),
  internal = FALSE
)
names(soil_data) <- params

# plot results
mapview(soil_data)

# write raster files sepeartely to disk
for (i in 1:length(params)) {
  writeRaster(soil_data[[i]], file.path('temp_results', paste0('soil_',
                                                               params[i],
                                                               '.tif')), 
              overwrite = TRUE)
}

# END 