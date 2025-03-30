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
  read_municipality(simplified = FALSE) %>%
  st_make_valid()

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
n <- 100  # Number of random points per sample
s <- 10  # Number of samples per city

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

normalized_indices <-
  data.frame(geom=rio_muni_results$geom,
           norm=as.numeric(rio_muni_results$disconnection_index / 
                             (sqrt(st_area(rio_muni_results$geom) / 
                                     pi))
                           )
           ) %>%
  st_as_sf()

ggplot() +
  geom_sf(data = normalized_indices,
          aes(fill = norm)) +
  scale_fill_gradient(low='navy', high='magenta')

# ---

rio_campos_muni <-
  rj_muni %>%
  filter(name_muni == 'RIO DE JANEIRO' | name_muni == 'CAMPOS DOS GOYTACAZES')


get_across_samples_disconnection <-
  
  function(muni_sf=rio_campos_muni, n_samples=3, n_points=5){
    
    samples <-
      
      lapply(1:n_samples, 
            
             function(i){
               
               draws <-
                 lapply(muni_sf$geom,
                        function(x){
                          st_sample(x, size = n_points) %>%
                            st_set_crs(., st_crs(muni_sf))})
               
               dists <-
                 lapply(draws, FUN=st_distance)
               
               indices <-
                 lapply(dists, function(x){sum(x) / (n_points * (n_points - 1))})
               
               unlist(indices)
               
             }
        )
    
    avg_across_samples <-
      rowSums(matrix(unlist(samples), nrow=nrow(muni_sf))) / n_samples
    
    return(avg_across_samples)
    
  }

# for n_sample=3, two cities (campos and rio in that order) :
#
# > a <- get_across_samples_disconnection(muni_sf=rio_campos_muni)
# > a
# [[1]]
# [1] 35094.17 13726.74
# 
# [[2]]
# [1] 33021.66 25672.59
# 
# [[3]]
# [1] 45941.27 20718.73
# 
# this one is wrong
# > matrix(unlist(a), nrow=3)
# [,1]     [,2]
# [1,] 35094.17 25672.59
# [2,] 13726.74 45941.27
# [3,] 33021.66 20718.73
# 
# this is nice
# > matrix(unlist(a), nrow=2)
# [,1]     [,2]     [,3]
# [1,] 35094.17 33021.66 45941.27
# [2,] 13726.74 25672.59 20718.73


rj_muni_norm <-
  rj_muni %>%
  mutate(index = 
           get_across_samples_disconnection(muni_sf = .)) %>%
  mutate(area = as.numeric(st_area(geom))) %>%
  mutate(index_norm = index / sqrt(area/pi) )

ggplot() +
  geom_sf(data = rj_muni_norm, aes(fill = index)) +
  scale_fill_gradient(low='navy', high='magenta') +
  ggtitle('Raw index')

ggplot() +
  geom_sf(data = rj_muni_norm, aes(fill = index_norm)) +
  scale_fill_gradient(low='navy', high='magenta') +
  ggtitle('Normalized index')

saveRDS(rio_muni_results, here::here('out', 'rio_muni_results_n100s10.Rds'))

# --

br_s2p5 <-
  all_muni %>%
  mutate(index = 
           get_across_samples_disconnection(muni_sf = .,
                                            n_samples = 2,
                                            n_points = 5)) %>%
  mutate(area = as.numeric(st_area(geom))) %>%
  mutate(index_norm = index / sqrt(area/pi) )


ggplot() +
  geom_sf(data = br_s2p5, aes(fill = index, color = NA)) +
  scale_fill_gradient(low='navy', high='magenta') +
  ggtitle('Raw index')

ggplot() +
  geom_sf(data = br_s2p5, aes(fill = index_norm)) +
  scale_fill_gradient(low='navy', high='magenta') +
  ggtitle('Normalized index')

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