
# =========================================================================== 
#
# This script pre-processes the shootings dataset 
#
# Current range: 01-01-2020 ~ 31/07/2020 
#
# =========================================================================== 

library(dplyr)
library(ggplot2)
library(sf)

# ---------------------------------------------------------------------------

raw_shootings <-
  read.csv(here::here('data', 'raw', 'shootings_rio_jan_jul_2020.csv')) %>%
  rename(shooting_id = id)

# ---------------------------------------------------------------------------


shootings_ts_formatted <-
  lubridate::dmy_hms(raw_shootings$data,
                     tz = "America/Sao_Paulo")

raw_shootings_formatted <-
  raw_shootings %>%
  mutate(shooting_ts = shootings_ts_formatted) %>%
  mutate(year = lubridate::year(shooting_ts)) %>%
  mutate(month = lubridate::month(shooting_ts)) %>%
  mutate(day = lubridate::day(shooting_ts)) %>%
  mutate(hour = lubridate::hour(shooting_ts)) %>%
  mutate(day_period = cut(hour,
                          breaks = c(0, 6, 12, 18, 24),
                          labels = c(1, 2, 3, 4))
         ) %>%
  mutate(day_period = ifelse(hour == 0, 3, day_period))

# ---------------------------------------------------------------------------

#
# get shootings data ready for estimation:
#   - intersect with neighborhoods and grid cells
#   - measure them at 15 min intervals
#
# be sure to have run `./process/rio_geog_gen.R` first
#

rio_neighborhoods_path <-
  here::here('data', 'rio_neighborhoods.Rds')

rio_grid_path <-
  here::here('data', 'rio_grid.Rds')

# -

shootings_data_ready <- # ready for estimation
  
  if (file.exists(rio_neighborhoods_path) &
      file.exists(rio_grid_path)) {
    
    rio_neighborhoods <- readRDS(rio_neighborhoods_path)
    rio_grid <- readRDS(rio_grid_path)
    
    # -
    
    raw_shootings_formatted %>%
      tidyr::drop_na(shooting_id) %>%
      st_as_sf(., coords = c('longitude', 'latitude'), crs='EPSG:4326') %>%
      st_transform(crs=st_crs(rio_neighborhoods)) %>%
      st_join(.,
              rio_neighborhoods['name_neighborhood'],
              join = st_intersects) %>%
      st_join(.,
              rio_grid,
              join = st_intersects)
    
  } else {
    
    stop("Spatial data not found. 
         --> Be sure to run `./process/rio_geog_gen.R` first")
  }

# ---------------------------------------------------------------------------

saveRDS(shootings_data_ready,
        here::here('data', 'shootings_data.Rds'))

# ---------------------------------------------------------------------------

library(dplyr)
library(ggplot2)

shootings_qa <-
  shootings_data_ready %>%
  filter(!is.na(name_neighborhood)) %>%
  filter(shooting_ts > as.POSIXct('2020-02-01 00:00:01') &
           shooting_ts < as.POSIXct('2020-04-30 23:59:59')) %>%
  mutate(
    month_day = paste(month, day, sep = "-"),
    highlight_311 = ifelse(month_day == "3-11", "3-11", "Other")
  ) %>%
  mutate(quarantine_approved = ifelse(month_day>'3-10', 1, 0))

# - 

shootings_qa %>%
  st_drop_geometry() %>%
  summarise(.by = quarantine_approved,
            n = n())

# -

ggplot(data = shootings_qa, 
       aes(x = month_day, fill = highlight_311)) +
  geom_bar() +
  scale_fill_manual(values = c("3-11" = "red", "Other" = "steelblue")) +
  labs(
    x = "Month-Day",
    y = "Number of Shootings",
    title = "Shootings by Neighborhood, Date and Day Period"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  guides(fill = "none")
