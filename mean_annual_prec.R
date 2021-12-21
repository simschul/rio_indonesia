library(raster)
library(sf)
library(magrittr)


path2data <- './data'


get_data_for_indonesia <- function(root_dir, type = c('prec', 'tavg')) {
  # load Indonesian boarder as shapefiles (as provided by Rio)
  indonesia <- st_read(file.path(root_dir, 'IDN_adm0', 'IDN_adm0.shp'))
  
  # transform to the required CRS (extracted from other worldclim data)
  indonesia <- st_transform(indonesia, crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  
  # load raster data with monthly data from Worldclim
  r1 <- list.files(file.path(root_dir, paste0('wc2.1_30s_', type)), 
                   '.tif',
                   full.names = TRUE) %>% 
    lapply(function(x) raster(x, ext = extent(indonesia)))
  
  # convert list of rasters -> RasterStack
  r1 <- stack(r1)
  
  # clip RasterStack to indonesian borders
  r2 <- crop(r1, indonesia)
  r3 <- mask(r2, indonesia)
  
  # calculate the sum/mean over the 12 raster layers (=12 months)
  if (type == 'prec')  r4 <- sum(r3)
  if (type == 'tavg')  r4 <- mean(r3)
  
  return(r4)
}

r <- get_data_for_indonesia(path2data, 'prec')

# plot results
plot(r)
plot(indonesia, add = TRUE, alpha = 0.3, fill = NA, col = NA)

# nice interactive plot 
mapview::mapview(r, maxpixels =  11318050)

# save final raster
writeRaster(r, file.path('temp_results', 'prec_sum.tif'), 
            overwrite = FALSE)


# END