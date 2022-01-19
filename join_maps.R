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
library(mapedit)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
options("datatable.print.class" = TRUE)
theme_set(theme_bw())

############################################################################## # 
##### load data #############################################################
############################################################################## # 


filters <- readRDS('./temp_results/filters_HeveaBrasiliensis_S1S2.RData')
dl <- st_read('./temp_results/cstocks35_crsset_cleaned100m2.shp')
#dl <- st_make_valid(dl)
#st_precision(dl) <- 0

# crop to test code
point <- c(107.84166667, -7.69689625)
buffer <- 0.5
dl <- st_crop(dl, 
              xmin = point[1] - buffer, 
              xmax = point[1] + buffer, 
              ymin = point[2] - buffer, 
              ymax = point[2] + buffer)
for (i in 1:length(filters)) {
  filters[[i]] <- st_crop(filters[[i]], 
                          xmin = point[1] - buffer, 
                          xmax = point[1] + buffer, 
                          ymin = point[2] - buffer, 
                          ymax = point[2] + buffer)
  
}

# end crop

filtered_maps <- vector('list', length = length(filters)) %>% 
  setNames(names(filters))

for (i in 1:length(filters)) {
  cat('Filtering with ', names(filters)[i],'\n')
  # st_precision(filters[[i]]) <- 0
  tic('intersection')
  filtered_maps[[i]] <- st_intersection(dl, filters[[i]]) 
  toc()
}
# Evaluation error: TopologyException: Input geom 1 is invalid: 
# Too few points in geometry component at or near point 
# 107.84166667 -7.69689625 at 107.84166667 -7.69689625.

mapview(filtered_maps$prec_sum)

saveRDS(filtered_maps, './temp_results/filtered_maps.RData')
saveRDS(r_list, './temp_results/r_list.RData')
saveRDS(dl_union, './temp_results/dl_union.RData')

#filtered_maps <- readRDS('./temp_results/filtered_maps.RData')


# junk =================================================================

# zoom to problem ============
point <- c(107.84166667, -7.69689625)
buffer <- 0.05
class(dl)
zoom_area <- st_crop((dl), 
                     xmin = point[1] - buffer, 
                     xmax = point[1] + buffer, 
                     ymin = point[2] - buffer, 
                     ymax = point[2] + buffer)
mapview(zoom_area) %>% editMap

st_area(zoom_area)
plot <- zoom_area %>% 
  st_cast('MULTILINESTRING') %>%
  st_cast('LINESTRING') %>% 
  st_cast('POLYGON') %>%
  st_area()
mapview
st_cast('MULTIPOINT') %>% 
  plot + mapview(zoom_area, add = TRUE)

st_buffer(-1E-6) %>% 
  st_area
st_is_valid()
st_cast('POLYGON')  



### #



filter_degraded_land_for <- function(shp, path2rasters, 
                                     tavg_mean = NULL, prec_sum = NULL, slope_tangent = NULL) {
  path2rasters <- './temp_results'
  
  filter <- list(
    tavg_mean = c(24,34),
    prec_sum = c(2000,3500),
    slope_tangent = c(NA, 16)  
  )
  
  # load rasters
  r_list <- vector('list', length = length(filter)) %>% 
    setNames(names(filter))
  for (i in 1:length(filter)) {
    path2file <- list.files(path = path2rasters, 
                            pattern = names(filter)[i], 
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
  
  # create one filter layer from tavg, prec and slope
  # for (i in 2:length(r_list)) {
  #   r <- st_intersection(r_list[[i-1]], r_list[[i]])
  # }
  
  filtered_maps <- vector('list', length = length(filter)) %>% 
    setNames(names(filter))
  
  
  dl2 <- st_make_valid(dl)
  dl_union <- st_union(dl2)
  
  for (i in 1:length(filter)) {
    cat('Filtering with ', names(r_list)[i],'\n')
    st_precision(r_list[[i]]) <- 0
    tic('intersection')
    filtered_maps[[i]] <- st_intersection(dl_union, r_list[[i]]) 
    toc()
  }
  
  saveRDS(filtered_maps, './temp_results/filtered_maps.RData')
  saveRDS(r_list, './temp_results/r_list.RData')
  saveRDS(dl_union, './temp_results/dl_union.RData')
  
  return(filtered_maps)
  
}



# analyse results -========================
filtered_maps <- readRDS('./temp_results/filtered_maps.RData')
map1 <- st_cast(filtered_maps$tavg_mean, 'POINT', group_or_split = TRUE)
map2 <- st_simplify(filtered_maps$tavg_mean, preserveTopology = TRUE)
map3 <- st_collection_extract(filtered_maps$tavg_mean, type = 'POLYGON')
mapview(map3[1:10000])


dl_union <- readRDS('./temp_results/dl_union.RData')

for (i in seq_along(filtered_maps)) {
  map <- st_collection_extract(filtered_maps[[i]], type = 'POLYGON')
  st_write(map, paste0('./temp_results/',
                       'DL35_', 
                       'HeveaBrasiliensis_',
                       'S1S2_',
                       names(filtered_maps)[i],
                       '.shp'
  ))
}



# select ranges
tavg2[tavg2 > 26] <- NA
tavg2[tavg2 < 18] <- NA
plot(tavg2)

plot(tavg4[1:100,])

# get the intersection between the degraded land map and the temperature mask
s4 <- st_intersection(dl35, tavg4)

# plot map
mapview(tavg) + 
  mapview(tavg4, col.regions = 'grey') + 
  mapview(s4[1:1000,], col.regions = 'red')  
#mapview(dl35[1:1000,]) 

# calculate area
area_DL <- st_area(s4) %>% 
  sum %>% 
  set_units(ha)

# save shapefile
st_write(st_collection_extract(s4, 'POLYGON'), 'map_35_temp_intersect.shp')


###
map_35 <- st_read('/home/simon/Documents/Projects/Rio/GIS Map_Rio-20211123T145039Z-001/GIS Map_Rio/cstocks35_crsset.shp')
vect_temp <- st_read('/home/simon/Documents/Projects/Rio/GIS Map_Rio-20211123T145039Z-001/GIS Map_Rio/Vect_Temp_IDN.shp')
province <- st_read('/home/simon/Documents/Projects/Rio/IDN_adm0/IDN_adm0.shp')

map_35_2 <- st_make_valid(map_35)

DL <- st_intersection(map_35_2, province)
mapview(list(DL, map_35[1:10,]))
DL_temp <- st_intersection(map_35, vect_temp)
#mapview(list(DL_temp, map_35[1:1000,]), color = c('blue', 'red'))



DL_area <- st_area(DL)
DL_temp_area <- st_area(DL_temp) 
DL_temp_area %>% 
  sum %>% 
  set_units(ha)



pol = st_sfc(st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0)))) ) %>% st_as_sf
plot(pol, col = 'red')

pol2 <- st_sfc(st_polygon(list(rbind(c(-1,-1), c(1,-1), c(1,1), c(-1,1), c(-1,-1))))) %>% st_as_sf
plot(pol2, add = TRUE, col = 'blue', alpha = 0.3)

a <- st_join(pol, pol2, join = st_within)
plot(a, add = TRUE, col = 'green1')

b <- st_join(pol, pol2)
plot(b, add = TRUE, col = 'green2')

c <- st_join(pol2, pol)
plot(c, add = TRUE, col = 'green3')

d <- st_intersection(pol2, pol)
plot(d, add = TRUE, col = 'green4')


st_as_sf(pol)








