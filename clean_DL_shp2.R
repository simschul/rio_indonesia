#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2021-12-21 14:31:12
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

shp <- './data/GIS Map_Rio_200122-20220125T164332Z-001/GIS Map_Rio_200122/cstocks35_fixed_200122.shp'


# load degraded land map (shapefile)
dl <- st_read(shp)
colnames(dl)


# kick out all unecessary fields
dl <- dl[, c('DN', 'geometry')]

# remove Z dimension
dl <- st_zm(dl, what = 'ZM')

sf_use_s2(FALSE)
temp <- st_area(dl)
temp %>% sum %>% set_units(ha)

# split multipolygons into single polygons, then linestrings, then polygons again
# aim: cut out linestrings that formally belonged to a polygon but fall outside it
# 191785
dl2 <- st_cast(dl, 'POLYGON') %>% 
    st_cast('MULTILINESTRING') %>%
    st_cast('LINESTRING') %>% 
    st_cast('POLYGON')  


# THE END ---------------------------------------------------------------------
