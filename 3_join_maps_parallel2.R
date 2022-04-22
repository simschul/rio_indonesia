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
    
    if (!is.null(save_dir)){
      if (!dir.exists(save_dir)) dir.create(save_dir)
      saveRDS(results[[i]], 
              file.path(save_dir, paste0('DL_filtered_', names(filter)[i], '.RData')))
    }
    toc()
  }
  return(results)
}


# 2. run function for all species and S classes ===============================
species <- c("Acacia_auriculiformis", 'Tectona_grandis', 'Hevea_brasiliensis')
suitability_classes <- c('S1', 'S2')

par_comb <- expand.grid(specie = species, suitability_class = suitability_classes)

dl_filtered <- pbmclapply(1:nrow(par_comb), function(x) {
  path <- file.path('results', par_comb[x,1], par_comb[x,2])
  filter <- readRDS(file.path(path, 
                              'filters.RData'))
  result <- filter_degraded_land_map(dl, filter, 
                                     save_dir = file.path(path, 'DL35'))
  return(result)
}, mc.cores = 6)


# analyse results ===============================

files <- list.files('./results', 'DL_filtered_combined.RData', recursive = TRUE, 
           full.names = TRUE)

data <- lapply(files, readRDS)
lapply(data, class)

#### jun k ===============================
# make a grid over shp and select all grid cells that intersect the shp
grid <- st_make_grid(dl, n = 9)
grid <- grid[dl]

# stop using s2, otherwise will cause topology error (see: )
sf_use_s2(TRUE)

# run in parallel
n_cores <- 7

filtered_maps <- vector('list', length = length(filters)) %>% 
  setNames(names(filters))

tic('parallel')
# plan(multisession(workers = 6))
# 
# split_results <- future_lapply(
#   1:length(grid), function(x) {
#     within <- st_intersects(dl, grid[x]) %>% 
#       sapply(any)
#     
#     for (i in 1:length(filters)) {
#       filtered_maps[[i]] <- tryCatch({
#         st_intersection(dl[within == 1,], 
#                         filters[[i]])
#       }, error = function(e) print(e))
#       
#     }
#     return(filtered_maps)
#   }  
# )

split_results <- pbmcapply::pbmclapply(
  1:length(grid), function(x) {
    within <- st_intersects(dl, grid[x]) %>% 
      sapply(any)
    
    for (i in 1:length(filters)) {
      filtered_maps[[i]] <- tryCatch({
        st_intersection(dl[within == 1,], 
                        filters[[i]])
      }, error = function(e) print(e))
      
    }
    return(filtered_maps)
  }, mc.cores = n_cores)
toc()
# 579.891  sec

# check for error messages
sapply(split_results, function(x) {
  sapply(x, function(y) {
    'sf' %in% class(y)    
  })
}) %>% mean
# should be all TRUE

## begin test
# grids_broken <- c(25, 35, 36, 42, 44)
# x <- grids_broken[2]
# within <- st_intersects(dl, grid[x]) %>% 
#   sapply(any)
# 
# test2 <- dl[within == 1,] %>% st_is_valid()
# mapview(dl[within ==1,])
# 
# sf_use_s2(TRUE)
# 
# test3 <- vector('list', nrow(dl[within ==1,]))
# for (i in 1:nrow(dl[within ==1,])){
#   cat(i, '')
#   test3[[i]] <- tryCatch({
#     st_intersection(dl[within == 1, ][i,], filters$combined)
#   }, error = function(e) print(e))
# }
# allok <- sapply(test3, function(x) 'sf' %in% class(x))
# mean(allok)
# mapview(dl[within ==1, ][!allok,])

## end test



# combine results
results <- pbmcapply::pbmclapply(names(filters), function(x) {
  temp <- sapply(split_results, '[', x)
  temp <- temp[sapply(temp, function(x) {'sf' %in% class(x)})]
  temp <- do.call('rbind', temp)
  st_make_valid(temp)
}, mc.cores = 7) %>% 
  setNames(names(filters))

# [1] "Error : arguments have different crs\n"
# attr(,"class")
# [1] "try-error"
# attr(,"condition")
# <simpleError: arguments have different crs>


#mapview(results)


# union geometries
results2 <- pbmcapply::pbmclapply(results, function(x) {
  x[, c('DN', 'geometry')] %>% 
    as.data.table %>% 
    .[, list(geometry = st_combine(geometry)), by = DN] %>% 
    st_as_sf
}, mc.cores = 7)



lapply(results, function(x) sum(st_area(x)) %>% units::set_units(ha))

# my output: 
# $tavg_mean
# 6200079 [ha]
# 
# $prec_sum
# 4483993 [ha]
# 
# $slope_tangent
# 6175453 [ha]
# 
# $soil_REF_DEPTH
# 5987475 [ha]
# 
# $soil_T_PH_H2O
# 5769478 [ha]
# 
# $soil_S_PH_H2O
# 5691945 [ha]
# 
# $combined
# 3871329 [ha]


# save results
saveRDS(results2, './temp_results/cstocks35_fixed_200122_cleaned100m2_filtered_v5.RData')

mapview(results2$combined)
# as shp
for (i in 1:length(results)) {
  st_write(results[[i]], paste0('./temp_results/',
                                'DL35_', 
                                'HeveaBrasiliensis_',
                                'S1S2_',
                                names(results)[i],
                                '_v5.shp'), 
           append = FALSE
  )
}

# calc area by DN

results3 <- lapply(results, function(x) {
  x <- as.data.table(x)
  x[, area := st_area(geometry)]
  x <- x[, list(area = sum(area)), by = DN]
  setorderv(x, 'DN')
  return(x)
})


rio::export(
  results3, file = './temp_results/area_by_DN_and_variable.xlsx'
)


# end  









