library(dplyr)
library(geobr)
library(sf)
library(ggplot2)

set.seed(1)

# ------

# available years: 
# 1872 1900 1911 1920 1933 1940 1950 1960 1970 1980 1991 2000 2001 2005 2007 
# 2010 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
raw_all_muni <- 
  read_municipality(simplified = FALSE) 

all_muni <-
  raw_all_muni %>%
  mutate(name_muni = 
           stringi::stri_trans_general(str = .$name_muni,
                                       id = "Latin-ASCII") %>% 
           toupper(.))

# --

# st_write(all_muni, here::here('data', 'all_muni.shp'))

rj_muni <- 
  all_muni %>% 
  filter(abbrev_state == 'RJ')

sp_muni <- 
  all_muni %>% 
  filter(abbrev_state == 'SP')

sample_muni <-
  all_muni %>% 
  filter(abbrev_state == 'RJ' |
           abbrev_state == 'BA' |
           abbrev_state == 'PE' |
           abbrev_state == 'PA' |
           abbrev_state == 'SP')

# --

rj_muni_grid <-
  rj_muni %>%
  st_make_grid(., cellsize = 0.01) %>%
  st_sf()

rj_centroids <-
  rj_muni_grid %>%
  st_centroid

# -

all_muni_grid <-
  all_muni %>%
  st_make_grid(., cellsize = 0.1) %>%
  st_sf()

all_muni_centroids <-
  all_muni_grid %>%
  st_centroid

lapply(all_muni, function(x){
  x %>%
  st_make_grid(., cellsize = 0.1) %>%
  st_sf() %>%
  st_centroid
})

# -----

# Function to compute the disconnection index for a given polygon
compute_disconnection_index <- function(polygon, n_points, n_samples) {
  
  indices <- 
    purrr::map_dbl(1:n_samples, ~{
    # Sample n random points inside the polygon
    points <- st_sample(polygon, n, type = "random")
    
    # Compute pairwise Euclidean distances
    distance_matrix <- st_distance(points)
    
    # Compute disconnection index
    sum(distance_matrix) / (n_points * (n_points - 1))
  })
  
  return(mean(indices))  # Return the mean across the 30 samples
}


# Set parameters
n <- 50  # Number of random points per sample
s <- 5  # Number of samples per city

# ------

start.time <- Sys.time()
#
rio_muni_results <- # Apply the function to all cities
  rj_muni %>%
  rowwise() %>%
  mutate(disconnection_index = compute_disconnection_index(.$geom, n, s))
#
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
saveRDS(rio_muni_results, here::here('out', 'rio_muni_results.Rds'))


# ------


start.time <- Sys.time()
#
sp_muni_results <- # Apply the function to all cities
  sp_muni %>%
  rowwise() %>%
  mutate(disconnection_index = compute_disconnection_index(.$geom, n, s))
#
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
saveRDS(sp_muni_results, here::here('out', 'sp_muni_results.Rds'))


# ------
start.time <- Sys.time()
#
sample_muni_results <- # Apply the function to all cities
  sample_muni %>%
  rowwise() %>%
  mutate(disconnection_index = compute_disconnection_index(.$geom, n, s))
#
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
saveRDS(sample_muni_results, here::here('out', 'sample_muni_results.Rds'))

# -----