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

# load filter shapefiles and make them valid
filters <- readRDS('./temp_results/filters_HeveaBrasiliensis_S1S2.RData')
filters <- lapply(filters, st_make_valid) %>% 
  setNames(names(filters))
filters <- lapply(filters, st_buffer, dist = 0) %>% 
  setNames(names(filters))

# load degraded land shp and make it valid
dl <- st_read('./temp_results/cstocks35_fixed_200122_cleaned100m2.shp')

# cast geometries from Multipolygon to polygon
dl <- st_cast(dl, 'POLYGON')
dl <- st_make_valid(dl)

# make a grid over shp and select all grid cells that intersect the shp
grid <- st_make_grid(dl, n = 9)
grid <- grid[dl]

# stop using s2, otherwise will cause topology error (see: )
sf_use_s2(FALSE)

# run in parallel
n_cores <- 6

tic('parallel')
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
# 260 sec

# check for error messages
sapply(split_results, function(x) {
  sapply(x, function(y) {
    'sf' %in% class(y)    
  })
})

# combine results
results <- pbmcapply::pbmclapply(names(filters), function(x) {
  sapply(split_results, '[', x) %>% 
    do.call('rbind', .)
}, mc.cores = 4) %>% setNames(names(filters))

#mapview(results)

# union geometries
results2 <- pbmcapply::pbmclapply(results, function(x) {
  x[, c('DN', 'geometry')] %>% 
  as.data.table %>% 
    .[, list(geometry = st_combine(geometry)), by = DN] %>% 
    st_as_sf
}, mc.cores = 4)


# save results
saveRDS(results2, './temp_results/cstocks35_fixed_200122_cleaned100m2_filtered.RData')

# as shp
for (i in 1:length(results)) {
  st_write(results2[[i]], paste0('./temp_results/',
                                'DL35_', 
                                'HeveaBrasiliensis_',
                                'S1S2_',
                                names(results)[i],
                                '2.shp'), 
           append = FALSE
  )
}


# end  

