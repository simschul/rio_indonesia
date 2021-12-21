library(raster)
library(sf)
library(magrittr)
library(mapview)
library(units)
library(stars)
library(tictoc)
library(data.table)

# TODO: 
# - first clean


filter_degraded_land_for <- function(shp, path2rasters, 
                                     tavg_mean = NULL, prec_sum = NULL, slope_tangent = NULL) {
  shp <- './data/cstocks35_crsset.shp'
  path2rasters <- './temp_results'
  
  filter <- list(
    tavg_mean = c(24,34),
    prec_sum = c(2000,3500),
    slope_tangent = c(NA, 16)  
  )
  
  # load degraded land map (shapefile)
  dl <- st_read(shp)
  
  # check validity
  valid <- st_is_valid(dl)
  
  # 845 iout of 131713 features are invalid
  invalid_dl <- dl[!valid,]
  valid_dl <- dl[valid == 'Valid Geometry',]
  
  # check reason for invalidity
  invalid_reasons <- st_is_valid(invalid_dl, reason = TRUE)
  
  mapview(invalid_dl[grepl('Hole lies outside shell', invalid_reasons), ])
  mapview(invalid_dl[38823,] %>% st_buffer(0))
  
  # zero buffer
  invalid_dl_fix1 <- st_buffer(invalid_dl, 0)
  valid2 <- st_is_valid(invalid_dl_fix1, reason = TRUE)
  
  mapview(invalid_dl_fix1)
  
  # convert multipolygons to polygons
  invalid_dl2 <- st_cast(invalid_dl_fix1, 'POLYGON')
  valid2 <- st_is_valid(invalid_dl2, reason = FALSE) # aha, some polygons got valid now
  invalid_reasons2 <- st_is_valid(invalid_dl2, reason = TRUE) 
  
  invalid_dl3 <- invalid_dl2[!valid2,] # 845 invalid features left
  
  # kick out all geometries with an area 0 or negativ
  invalid_dl3_area <- st_area(invalid_dl3)
  invalid_dl4 <- invalid_dl3[invalid_dl3_area > set_units(0, m^2),] # 574 invalid features left
  
  # repair remaining
  st_make_valid(invalid_dl4) %>% 
    st_is_valid()
  
  
  mapview(invalid_dl4)
  invalid_dl4[420,]$geometry[[1]]
  st_make_valid(invalid_dl4)[420,]$geometry[[1]]
  
  
  mapview(invalid_dl2[valid2,])
  
  invalid_dl3_repaired <- st_simplify(invalid_dl3)
  mapview(invalid_dl3_repaired)
  valid3r <- st_is_valid(invalid_dl3_repaired)
  
  
  
  
  invalid_dl_repaired <- st_make_valid(invalid_dl)
  mapview(invalid_dl_repaired)
  
  
  # check empty geometries
  empty <- st_dimension(dl)
  
  # ===
  dl <- valid_dl
  dl <- st_set_precision(dl, 1E8)
  #dl <- st_buffer(dl, 0)
  #dl <- st_make_valid(dl)
  
  
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








