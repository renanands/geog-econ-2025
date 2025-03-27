
library(dplyr)
library(sf)
library(ggplot2)

# ----------------------------------------------------------------

all_muni_sf <-
  read_sf(here::here('data', 'all_muni.shp')) %>%
  st_make_valid()

rj_muni_sf <-
  all_muni_sf %>%
  filter(cod_stt == 33)

# -

CISP_sf <- 
  read_sf(here::here('data','CISPshp','lm_cisp_bd.shp')) %>%
  st_make_valid() %>%
  st_transform(., st_crs(rj_muni_sf))

# -
  
shootings_rj <-
  read.csv(here::here('data', 'shootings_metropol_rio.csv')) %>%
  tidyr::drop_na(id) %>%
  st_as_sf(., coords = c('longitude', 'latitude'), crs = 'EPSG:4326') %>%
  st_transform(crs=st_crs(rj_muni_sf))

CISP_crimes <-
  read.csv(here::here('data', 'BaseDPEvolucaoMensalCisp.csv'))

disconnection_rj <-
  readRDS(here::here('data', 'rio_muni_results.Rds'))

# ----------------------------------------------------------------

ggplot() +
  theme_minimal() + 
  # geom_sf(rio_muni_sf, fill = 'gray8', color = 'gray50') +
  geom_sf(data = disconnection_rj,
          mapping = aes(fill=disconnection_index)) +
  scale_fill_gradient(low='navy', high='magenta')

# ----------------------------------------------------------------

disconnection_by_grid_centroids <-
  function(cs = 0.1, muni_sf){
    rj_grid <-
      lapply(rj_muni_sf$geometry,
             function(x){
               all_cells <- 
                 st_make_grid(x, cellsize = cs) %>% 
                 st_sf() %>%
                 st_set_crs(st_crs(rj_muni_sf))
                 
               intersecting_cells <- 
                 which(lengths(st_intersects(all_cells, x)) > 0)
                 
               intersected_grid <-
                 all_cells[intersecting_cells,] %>%
                 mutate(cell = row_number())
             }
      )
    
    # Compute pairwise centroid distances for each municipality's grid
    distance_matrices <- 
      lapply(rj_grid, function(muni) {
        # Extract centroids from the grid polygons
        centroids <- st_centroid(muni)
        
        # Calculate distance matrix between all centroids
        dist_matrix <- st_distance(centroids)
        n <- nrow(dist_matrix)
        sum(dist_matrix) / (n*(n - 1))
      })
    
    sf_w_indices <-
      rj_muni_sf %>%
      mutate(disconnection_index = unlist(distance_matrices)) %>%
      mutate(disconnection_index_normalized = 
               as.numeric(disconnection_index / (sqrt(st_area(disconnection_sf) / pi))))
    
    return(sf_w_indices)
  }

# ----------------------------------------------------------------

disconnection_sf <-
  disconnection_by_grid_centroids(cs = 0.01, rj_muni_sf)

ggplot() +
  theme_minimal() + 
  geom_sf(data = disconnection_sf,
          mapping = aes(fill=disconnection_index_normalized)) +
  scale_fill_gradient(low='navy', high='magenta')
          
# ----------------------------------------------------------------

CISP_w_shootings <-
  CISP_sf %>% 
  st_join(shootings_rj, ., join = st_intersects)

ggplot() +
  theme_minimal() + 
  # geom_sf(rio_muni_sf, fill = 'gray8', color = 'gray50') +
  geom_sf(data = )

# ----------------------------------------------------------------



