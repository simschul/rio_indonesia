############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(sf)
library(magrittr)
library(mapview)
library(tictoc)
library(data.table)
library(pbmcapply)
library(parallel)
library(future.apply)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
options("datatable.print.class" = TRUE)


############################################################################## # 
##### functions #################################################################
############################################################################## # 


############################################################################## # 
##### load data #############################################################
############################################################################## # 

# use s2 geometries (then the st_make_valid process works better)
sf_use_s2(TRUE)

# 1. load degraded land shp and make it valid ==================================

shp <- '/home/simon/Documents/Projects/Rio/rio_indonesia/data/GIS Map_Rio_200122_v1.1-20220414T084909Z-001/GIS Map_Rio_200122_v1.1/cstocks35_fixed_200122_v1.1.shp'

# load degraded land map 
dl <- st_read(shp)
colnames(dl)

# kick out all unnecessary fields
dl <- dl[, c('DN', 'geometry')]

# remove Z dimension
dl <- st_zm(dl, what = 'ZM')

# cast geometries from Multipolygon to polygon
dl <- st_cast(dl, 'POLYGON')
dl <- st_make_valid(dl)

#dl[26494,] <- st_make_valid(dl[26494,])
#dl[16087,] <- st_make_valid(dl[16087,])

st_area(dl) %>% sum %>% units::set_units(ha)





#' Title
#'
#' @param dl degraded land shapefile
#' @param filter list of filter shapefiles
#'
#' @return
#' @export
#'
#' @examples
filter_degraded_land_map <- function(dl, filter, save_dir = NULL, n_cores = 1) {
  
  results <- vector('list', length = length(filter)) %>% 
    setNames(names(filter))  
  
  for (i in 1:length(filter)) {
    cat('Filtering with ', names(filter)[i],'\n')
    # st_precision(filters[[i]]) <- 0
    tic('intersection')
    
    results[[i]] <- tryCatch({
      st_intersection(dl, filter[[i]])
    }, error = function(e) print(e))
    
    results[[i]] <- st_collection_extract(results[[i]], 'POLYGON')
    results[[i]] <- st_cast(results[[i]], 'POLYGON')
    
    if (!is.null(save_dir)){
      if (!dir.exists(save_dir)) dir.create(save_dir)
      saveRDS(results[[i]], 
              file.path(save_dir, paste0('DL_filtered_', names(filter)[i], '.RData')))
      st_write(results[[i]], 
              file.path(save_dir, paste0('DL_filtered_', names(filter)[i], '.shp')), 
              append = FALSE)
      
      
    }
    toc()
  }
  return(results)
}


# 2. run function for all species and S classes ===============================
species <- c("Acacia_auriculiformis", 'Tectona_grandis', 'Hevea_brasiliensis')
suitability_classes <- c('S1_S2')

par_comb <- expand.grid(specie = species, suitability_class = suitability_classes)

dl_filtered <- pbmclapply(1:nrow(par_comb), function(x) {
  path <- file.path('results', par_comb[x,1], par_comb[x,2])
  filter <- readRDS(file.path(path, 
                              'filters.RData'))
  result <- filter_degraded_land_map(dl, filter, 
                                     save_dir = file.path(path, 'DL35'))
  return(result)
}, mc.cores = min(6, nrow(par_comb)))


# analyse results ===============================

files <- list.files('./results', 'DL_filtered_combined.RData', recursive = TRUE, 
           full.names = TRUE)

data <- lapply(files, readRDS)
names(data) <- files
lapply(data, class)

lapply(data, function(x) sum(st_area(x)) %>% units::set_units(ha))



# end  









