
library(dplyr)
library(sf)
library(geobr)
library(ggplot2)

raw_shootings <-
  read.csv('shootings_metropol_rio.csv')

shootings_ts_formatted_no_tz <-
  as.POSIXct(raw_shootings$data,
             format = "%d/%m/%Y, %H:%M:%S")

shootings_ts_formatted_tz <-
  lubridate::ymd_hms(shootings_ts_formatted_no_tz, tz = "America/Sao_Paulo")

sf_shootings <-
  raw_shootings %>%
  mutate(shooting_ts = shootings_ts_formatted_tz) %>%
  tidyr::drop_na(shooting_ts) %>%
  st_as_sf(., coords = c('longitude', 'latitude'), crs = 4326)

#
## ---------------------- shapefiles
#

greater_rio_cities <- 
  read_metro_area(
    year = 2018,
    code_state = 33,
    simplified = FALSE,
    showProgress = TRUE,
    cache = TRUE
  )

all_neighborhoods <- 
  read_neighborhood(
    year = 2022,
    simplified = FALSE,
    showProgress = TRUE,
    cache = TRUE
  )

rio_proper_neighborhoods <-
  all_neighborhoods %>% 
  filter(name_muni == 'Rio de Janeiro')

# rio_census_tract <-
#   geobr::read_census_tract(
#   3304557,
#   year = 2010,
#   zone = "urban",
#   simplified = TRUE,
#   showProgress = TRUE,
#   cache = TRUE
# )
#      
# saveRDS(unique(rio_census_tract$code_tract),
#         here::here('census_tracts_from_shapefile.Rds'))
# 
# census_tract_plot <-
#   ggplot() +
#   geom_sf(data = rio_census_tract, fill = "gray80", color = "gray8")
# 
# ggsave(filename = 'census_tract_plot.png',
#        plot = census_tract_plot,
#        width = 8, height = 6, dpi = 300)

#
## ---------------------- plot
#

#
## just 2019
ggplot() +
  geom_sf(data = greater_rio_cities, fill = "gray8", color = "gray80") +
  geom_sf(data = rio_proper_neighborhoods, fill = "gray8", color = "gray80") +
  geom_sf(data = sf_shootings, aes(color = shooting_ts), size = 1, alpha = 0.1)  +
  scale_color_gradientn(
    colors = c("magenta", "cyan"),
    breaks = quantile(sf_shootings$shooting_ts),  # Define evenly spaced breaks
    labels = scales::date_format("%Y-%m-%d"),     # Format dates as "YYYY-MM-DD"
  ) +
  labs(color = "") +
  ggtitle("Shootings in Greater Rio: 01-01-2019 ~ 31-12-2020") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
  )
