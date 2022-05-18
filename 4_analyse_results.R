#' 
#' 
#'  
#' @author Simon Schulte
#' Date: 2022-05-18 11:59:59
#' 
#' Content:
#'  


############################################################################## # 
##### load packages ############################################################
############################################################################## # 

library(data.table)
library(units)
library(sf)
library(magrittr)
library(mapview)
library(tictoc)
library(pbmcapply)

############################################################################## # 
##### settings #################################################################
############################################################################## # 
options("datatable.print.class" = TRUE)
theme_set(theme_bw())

############################################################################## # 
##### load data #############################################################
############################################################################## # 

# analyse results ===============================

# only filtered maps with combined biophysical pars
files <- list.files('./results', 'DL_filtered_combined.RData', recursive = TRUE, 
                    full.names = TRUE)
# only S1 and S2 classes together
files <- files[grepl('S1_S2',files)]

# load data
data <- lapply(files, readRDS)

# set species name
species <- stringr::str_split(files, '/', simplify = TRUE)[,c(3, 4)]
species <- paste0(species[,1], '-', species[,2])
names(data) <- species
lapply(data, class)

# just to be sure: make valid again...
sf_use_s2(TRUE)
data <- lapply(data, st_make_valid) 

# calculate areas
lapply(data, function(x) sum(st_area(x)) %>% units::set_units(ha))




# THE END ---------------------------------------------------------------------
