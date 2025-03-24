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

# st_write(all_muni, here::here('data', 'all_muni.shp'))

mock_muni <- all_muni %>% filter(name_muni == 'RIO DE JANEIRO' |
                             name_muni == 'NOVA IGUACU' |
                             name_muni == 'SAO GONCALO')

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
n <- 10  # Number of random points per sample
num_samples <- 3  # Number of samples per city

# Apply the function to all cities
mock_muni %>%
  rowwise() %>%
  mutate(disconnection_index = compute_disconnection_index(.$geom, n, num_samples))

# View results
print(results)

# -----


sample_gen <-
  function(x){ # x = all_muni
    
    # get 30 batches of 10,000 points for each municipality
    sampled_points <-
      lapply(1:2, function(i){
        st_sample(x, size = c(3, 3), type = "random")
      })
    
    return(sampled_points)
    
  }


sample_gen(mock_muni)


vu <- lapply(1:2, function(i){
  st_sample(mock_muni, size = c(3, 3), type = "random")
})

st_distance(vu)

S <- function(x){
  
  
  
  st_distance(mock_muni) %>% rowSums() %>% sum / n*(n-1)
}

# ggplot() +
#   geom_sf(data = mock_muni, fill='gray8', color='gray80') +
#   geom_sf(data = sampled_points, color='blue')


