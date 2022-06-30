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

#library(raster)
library(magrittr)
library(mapview)
library(units)
library(stars)
library(tictoc)
library(data.table)
library(job)
library(sf)
library(terra)
library(pbmcapply)
############################################################################## # 
##### settings #################################################################
############################################################################## # 
options("datatable.print.class" = TRUE)
theme_set(theme_bw())

species <- c("Acacia_auriculiformis", 'Tectona_grandis', 'Hevea_brasiliensis')
suitability_classes <- c('S1_S2')
############################################################################## # 
##### load data #############################################################
############################################################################## # 

path2rasters <- './temp_results'

# filter list ==================================================================
# filter <- list(
#   tavg_mean = c(24,34),
#   prec_sum = c(2000,3500),
#   slope_tangent = c(0, 0.16), 
#   soil_REF_DEPTH = c(75, Inf), # 150
#   soil_T_PH_H2O = c(4.5, 6.5),
#   soil_S_PH_H2O = c(4.5, 6.5)
# )

pars <- vector('list', length(species)) %>% 
  setNames(species)

pars$Acacia_auriculiformis$S1_S2 <- list(
  tavg_mean = c(21, 34),
  prec_sum = c(1000,4000),
  slope_tangent = c(0, 0.15), 
  soil_REF_DEPTH = c(75, Inf), # check this out
  soil_T_PH_H2O = c(6, 8),
  #soil_S_PH_H2O = c(7, 7.5), 
  soil_T_CEC_CLAY = c(5, Inf), 
  soil_T_OC = c(0, Inf)
)

# pars$Acacia_auriculiformis$S2 <- list(
#   tavg_mean = c(21, 23, 30, 34),
#   prec_sum = c(1000, 1300, 2500, 4000),
#   slope_tangent = c(0.08, 0.15), 
#   soil_REF_DEPTH = c(75, Inf), # check this out
#   soil_T_PH_H2O = c(6, 7, 7.5, 8),
#   #soil_S_PH_H2O = c(6, 7, 7.5, 8), 
#   soil_T_CEC_CLAY = c(5, 16), 
#   soil_T_OC = c(0, 0.4)
# )

pars$Tectona_grandis$S1_S2 <- list(
  tavg_mean = c(21, 35),
  prec_sum = c(1250,2250),
  slope_tangent = c(0, 0.15), 
  soil_REF_DEPTH = c(75, Inf), # check this out
  soil_T_PH_H2O = c(5, 7.5),
  #soil_S_PH_H2O = c(5.5, 7), 
  soil_T_CEC_CLAY = c(5, Inf), 
  soil_T_OC = c(0, Inf)
)

# pars$Tectona_grandis$S2 <- list(
#   tavg_mean = c(21, 25, 30, 35),
#   prec_sum = c(1250, 1500,2000, 2250),
#   slope_tangent = c(0.08, 0.15), 
#   soil_REF_DEPTH = c(75, Inf), # check this out
#   soil_T_PH_H2O = c(5, 5.5, 7, 7.5),
#   #soil_S_PH_H2O = c(5, 5.5, 7, 7.5), 
#   soil_T_CEC_CLAY = c(5, 16), 
#   soil_T_OC = c(2, 3)
# )


pars$Hevea_brasiliensis$S1_S2 <- list(
  tavg_mean = c(24, 34),
  prec_sum = c(2000,3500),
  slope_tangent = c(0, 0.16), 
  soil_REF_DEPTH = c(75, Inf), # check this out
  soil_T_PH_H2O = c(4.5, 6.5),
  #soil_S_PH_H2O = c(5, 6), 
  soil_T_CEC_CLAY = c(5, Inf), 
  soil_T_OC = c(0, Inf)
)

# pars$Hevea_brasiliensis$S2 <- list(
#   tavg_mean = c(24, 26, 30, 34),
#   prec_sum = c(2000, 2500, 3000, 3500),
#   slope_tangent = c(0.08, 0.16), 
#   soil_REF_DEPTH = c(75, Inf), # check this out
#   soil_T_PH_H2O = c(4.5, 5, 6, 6.5),
#   #soil_S_PH_H2O = c(4.5, 5, 6, 6.5), 
#   soil_T_CEC_CLAY = c(5, 16), 
#   soil_T_OC = c(0, 0.8)
# )

pars

  
# load raster files
parameter_names <- names(pars[[1]][[1]])

r_list <- vector('list', length = length(parameter_names)) %>% 
  setNames(parameter_names)


for (i in 1:length(parameter_names)) {
  
  # load raster file
  path2file <- list.files(path = path2rasters, 
                          pattern = paste0(parameter_names[i], '.tif'), 
                          full.names = TRUE)
  cat(paste0('loading ', path2file, '\n'))
  r_list[[i]] <-  rast(path2file) # load data
}

# filter all rasters according to filterlist ===================================




create_filters <- function(
  parameters, 
  raster_files
) {

  
  for (i in 1:length(parameters)) {
    
   
    cat('Set filter range\n')
    # specifiy filter ranges: set all between lower and upper bound to 1, the rest to NA
    lower <- parameters[[i]][1]
    upper <- parameters[[i]][2]
    
    if (length(parameters[[i]]) == 2) {
      raster_files[[i]][raster_files[[i]] < lower | raster_files[[i]] > upper] <- NA
    }
    
    if (length(parameters[[i]]) == 4) {
      lower2 <- parameters[[i]][3]
      upper2 <- parameters[[i]][4]  
      raster_files[[i]][raster_files[[i]] < lower 
                  | raster_files[[i]] > upper2 
                  | (raster_files[[i]] > upper & raster_files[[i]] < lower2)] <- NA
    }
    
    raster_files[[i]][!is.na(raster_files[[i]])] <- 1
    
    # resample so that all rasters have same extent
    if (i > 1 & !isTRUE(all.equal(ext(raster_files[[1]]), ext(raster_files[[i]])))) {
      cat('Resample \n')
      raster_files[[i]] <- resample(raster_files[[i]], raster_files[[1]])
    }
    
  }
  
  # combine all raster files as raster stack
  r_stack <- rast(raster_files)
  
  # create another raster that combines the other 
  cat('Combine \n')
  r_stack2 <- terra::app(r_stack, function(x) {
    ifelse(sum(is.finite(x)) == length(x), 
           1, 
           NA)
  })
  names(r_stack2) <- 'combined'
  
  # combine into one stack
  r_stack3 <- c(r_stack, r_stack2)
  
  return(r_stack3)
  
  
}

#test <- create_filters(pars$Acacia_auriculiformis$S1, raster_files = r_list)
#plot(test)

# apply function for each parameter combination and save results
par_comb <- expand.grid(specie = species, suitability_class = suitability_classes)


filters <- pbmclapply(1:nrow(par_comb), function(i) {
  # create directory for results if not already existing
  dir <- file.path('results', par_comb[i,1], par_comb[i,2])
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  
  # run create filter function
  filter_rasters <- create_filters(pars[[par_comb[i,1]]][[par_comb[i,2]]], 
                           raster_files = r_list)

  # Convert rasters to polygons
  cat('convert to shapefile and save \n')
  filter_shps <- lapply(filter_rasters, function(j) {
    j <- as.polygons(j)
    j <- st_as_sf(j)
    j <- st_make_valid(j)
    j <- st_buffer(j, 0)
    
    st_write(j, file.path(dir, paste0('filter_', names(j)[1], '.shp')), 
             append = FALSE)
    return(j)
  })
  names(filter_shps) <- names(filter_rasters)
  
  # save results
  saveRDS(filter_shps, file.path(dir, 'filters.RData'))
  
  return(filter_shps)
}, mc.cores = 3)


### end 





