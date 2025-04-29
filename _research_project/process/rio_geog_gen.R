
# =========================================================================== 
#
# This script merges the iFood data with the rain and shootings data
# and estimates reduced-form specifications.
#
# Please refer to `./rp_docs.html` for more info
# 
# =========================================================================== 

library(dplyr)
library(ggplot2)
library(sf)
library(dplyr)

# ------------------------------------------------------------------------

rio_neighborhoods <- 
  geobr::read_neighborhood(year = 2010,
                           simplified = FALSE) %>%
  filter(code_muni == 3304557) %>%
  mutate(name_neighborhood =
           stringi::stri_trans_general(str = .$name_neighborhood,
                                       id = "Latin-ASCII") %>% toupper()) %>%
  st_transform(., crs = 'EPSG:31983')

saveRDS(rio_neighborhoods,
        here::here('data', 'rio_neighborhoods.Rds'))

# ------------------------------------------------------------------------

all_cells_rio_grid <-
  st_make_grid(rio_neighborhoods, cellsize = 1000) %>%
  st_sf()

intersecting_cells <- 
  which(lengths(st_intersects(all_cells_rio_grid, rio_neighborhoods)) > 0)

rio_grid <-
  all_cells_rio_grid[intersecting_cells,] %>%
  mutate(cell = row_number())

saveRDS(rio_grid,
        here::here('data', 'rio_grid.Rds'))

# ------------------------------------------------------------------------

rio_grid_plot <-
  ggplot() +
  theme_minimal() +
  geom_sf(data = rio_neighborhoods, fill = "gray80", color = "gray8") +
  geom_sf(data = rio_grid, color = "blue", alpha = 0.3) +
  ggtitle('cs = 1km^2')

ggsave(plot = rio_grid_plot,
       here::here('out', 'rio_grid_plot.png'))
