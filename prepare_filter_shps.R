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
library(sf)
library(magrittr)
library(mapview)
library(units)
library(stars)
library(tictoc)
library(data.table)
library(job)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
options("datatable.print.class" = TRUE)
theme_set(theme_bw())

############################################################################## # 
##### load data #############################################################
############################################################################## # 

path2rasters <- './temp_results'

# filter list
filter <- list(
  tavg_mean = c(24,34),
  prec_sum = c(2000,3500),
  slope_tangent = c(NA, 16)  
)


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
  
  # convert raster -> polygons (sf)
  r_list[[i]] <- as(r_list[[i]],'SpatialPolygonsDataFrame') %>% 
    st_as_sf
  
  # combine all features into one (to speed up the intersect-calculations)
  r_list[[i]] <- st_union(r_list[[i]])
  
}


# save results
for (i in 1:length(r_list)) {
  st_write(r_list[[i]], paste0('./temp_results/filter_', 
                               names(r_list)[i], 
                               '_',
                               paste(filter[[i]], collapse = '-'),
                               '.shp'))
  
}

saveRDS(r_list, './temp_results/filters_HeveaBrasiliensis_S1S2.RData')













