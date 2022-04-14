#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2021-12-21 10:17:56
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(raster)
library(magrittr)
library(mapview)
library(units)
library(stars)
library(tictoc)
library(data.table)
library(job)
library(sf)
library(terra)
############################################################################## # 
##### settings #################################################################
############################################################################## # 
options("datatable.print.class" = TRUE)
theme_set(theme_bw())

############################################################################## # 
##### load data #############################################################
############################################################################## # 

path2rasters <- './temp_results'

# filter list ==================================================================
filter <- list(
  tavg_mean = c(24,34),
  prec_sum = c(2000,3500),
  slope_tangent = c(0, 0.16), 
  soil_REF_DEPTH = c(75, Inf), # 150
  soil_T_PH_H2O = c(4.5, 6.5),
  soil_S_PH_H2O = c(4.5, 6.5)
)

# filter all rasters according to filterlist ===================================
r_list <- vector('list', length = length(filter)) %>% 
  setNames(names(filter))

for (i in 1:length(filter)) {
  path2file <- list.files(path = path2rasters, 
                          pattern = paste0(names(filter)[i], '.tif'), 
                          full.names = TRUE)
  cat(paste0('loading ', path2file, '\n'))
  r_list[[i]] <-  raster(path2file) # load data
  
  # filter ranges
  lower <- filter[[i]][1]
  upper <- filter[[i]][2]
  
  r_list[[i]][r_list[[i]] < lower | r_list[[i]] > upper] <- NA
  r_list[[i]][!is.na(r_list[[i]])] <- 1
  
  # convert raster -> polygons (sf)
  # shp_list[[i]] <- as(r_list[[i]],'SpatialPolygonsDataFrame') %>% 
  #   st_as_sf
  # 
  # # combine all features into one (to speed up the intersect-calculations)
  # shp_list[[i]] <- st_union(shp_list[[i]])
  # 
}

# create one raster filter that combines all 3 other filters ===================

# convert to terra raster
#r_list2 <- lapply(r_list, rast) # not working somehow

# quick and dirty fix: 
tempdir <- tempdir()
for (i in 1:length(r_list)) {
  raster::writeRaster(r_list[[i]], file.path(tempdir, paste0(names(r_list)[i], '.tif')), 
                      overwrite = TRUE)
  cat(i, '')
}
r_list2 = lapply(1:length(r_list), function(x) {
    rast(file.path(tempdir, paste0(names(r_list)[x], '.tif')))
  })
names(r_list2) = names(r_list)


# extend the slope raster
# r_list2$slope_tangent <- terra::extend(r_list2$slope_tangent,
#                                        r_list$tavg_mean)
# 
# 
for (i in 1:(length(r_list2)-1)) {
  r_list2[[i]] <- resample(r_list2[[i]], r_list2[[length(r_list2)]])
  cat(i, '')
}

lapply(r_list2, ext)
# combine all 3 as stack
r_stack <- rast(r_list2)

# create 4th raster that combines the other 3
r_stack2 <- terra::app(r_stack, function(x) {
  ifelse(sum(is.finite(x)) == length(x), 
         1, 
         NA)
})
names(r_stack2) <- 'combined'

# combine into one stack
r_stack3 <- c(r_stack, r_stack2)


# save stack
terra::writeRaster(r_stack3, './temp_results/filters_HeveaBrasiliensis_S1S2.grd', 
                   overwrite = TRUE)



# Convert rasters to polygons ==================================================
shp_list <- vector('list', length = dim(r_stack3)[3]) %>% 
  setNames(names(r_stack3))


for (i in 1:length(shp_list)) {
  cat(i, '')
  #convert SpatRaster -> SpatVector
  temp_shp <- as.polygons(r_stack3[[i]])
  tempdir <- tempdir(check = TRUE)
  # save as .shp
  writeVector(temp_shp, filename = file.path(tempdir, 'temp.shp'), 
              overwrite = TRUE)
  # read as sf
  shp_list[[i]] <- st_read(file.path(tempdir, 'temp.shp'))
  
  # combine all features into one (to speed up the intersect-calculations)
  #shp_list[[i]] <- st_union(shp_list[[i]])
}


# save results
saveRDS(shp_list, './temp_results/filters_HeveaBrasiliensis_S1S2.RData')

mapview(shp_list)

# for (i in 1:length(v)) {
#   st_write(shp_list[[i]], paste0('./temp_results/filter_', 
#                                  names(shp_list)[i], 
#                                  '_',
#                                  paste(filter[[i]], collapse = '-'),
#                                  '.shp'))
#   
# }
# 











