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

# set path to shapefile
shp <- './data/cstocks35_crsset.shp'

# load degraded land map 
dl <- st_read(shp)
colnames(dl)

# kick out all unnecessary fields
dl <- dl[, c('DN', 'geometry')]

# remove Z dimension
dl <- st_zm(dl, what = 'ZM')


# split multipolygons into single polygons, then linestrings, then polygons again
# aim: cut out linestrings that formally belonged to a polygon but fall outside it

  dl3 <- st_cast(dl, 'POLYGON') %>% 
    st_cast('MULTILINESTRING') %>%
    st_cast('LINESTRING') %>% 
    st_cast('POLYGON')  

# check validity of each polygon
  valid <- st_is_valid(dl3) # takes long time  

# check area of each polygon
  area <- st_area(dl3) 

summary(area)

dl3 <- st_zm(dl3, what = 'ZM')

# retrieve all invalid geometries (not valid OR area smaller than 0.5m2)
dl2_invalid <- dl3[!valid | area <= set_units(100, m^2),] %>%
  as.data.table %>% 
  .[, list(geometry = st_combine(geometry)), by = DN] %>% 
  st_as_sf

mapview(dl2_invalid)

# retrieve all valid geometires
dl2_valid <- dl3[valid & area > set_units(100, m^2),] %>%
  as.data.table %>% 
  .[, list(geometry = st_combine(geometry)), by = DN] %>% 
  st_as_sf


# compare areas
area_unvalid <- st_area(dl2_invalid) %>% sum
area_valid <- st_area(dl2_valid) %>% sum
area_total <- st_area(dl) %>% sum

area_unvalid / area_valid # --> negligible 
area_valid / area_total
area_valid + area_unvalid

# save results
st_write(dl2_valid, './temp_results/cstocks35_crsset_cleaned100m2.shp')
st_write(dl2_invalid, './temp_results/cstocks35_crsset_artefacts100m2.shp')

# plot results
mapview(dl2_valid) # takes long time
mapview(dl2_invalid) # takes long time


point <- c(107.84166667, -7.69689625)
buffer <- 0.05
zoom_area <- st_crop((dl2_valid), 
                     xmin = point[1] - buffer, 
                     xmax = point[1] + buffer, 
                     ymin = point[2] - buffer, 
                     ymax = point[2] + buffer)
mapview(zoom_area)
#### junk ======================================================================

dl2_invalid2 <- dl2_invalid %>% 
  st_cast('MULTILINESTRING') %>%
  st_cast('LINESTRING') %>% 
  st_cast('POLYGON') 
  
valid2 <- st_is_valid(dl2_invalid2)
mapview(dl2_invalid2)

area <- st_area(dl2_invalid2)
dl2_invalid2[area > set_units(0.5, m^2),] %>% mapview


st_make_valid(dl2_invalid3) %>% mapview
  
dl2_invalid4 <- dl2_invalid3 %>% 
  as_Spatial %>% 
  cleangeo::clgeo_Clean()

dl2_invalid2[1019,]$geometry[[1]] %>% 
  st_cast('MULTIPOINT') %>% 
  #st_cast('POLYGON') %>% 
  mapview

%>% mapview
area[1019]

st_simplify(dl2_invalid, preserveTopology = TRUE) %>% mapview
st_make_valid(dl2_invalid) %>% mapview

dl2_invalid[dl2_invalid$DN == 12,] %>% 
  st_combine()


st_cast(dl2_invalid, 'GEOMETRYCOLLECTION') %>% 
  st_collection_extract('POLYGON')


dl4 <- dl3 %>% 
  as.data.table %>% 
  .[, st_union(geometry), by = DN] %>% 
  st_as_sf

dl3 <- st_make_valid(dl2)
dl4 <- st_union(dl2, by_feature = TRUE)




dl_dt <- as.data.table(dl)
dl_dt$DN %>% unique %>% sort

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


map1 <- st_cast(filtered_maps$tavg_mean, 'POINT', group_or_split = TRUE)
map2 <- st_simplify(filtered_maps$tavg_mean, preserveTopology = TRUE)
map3 <- st_collection_extract(filtered_maps$tavg_mean, type = 'POLYGON')



# THE END ---------------------------------------------------------------------
