
library(dplyr)
library(sf)
library(ggplot2)
library(geobr)

# -----------------------------------------------------------------

raw_shootings <-
  read.csv(here::here('data', 'shootings_rio_proper.csv'))

# --

# Sample data frame
pre_sf_teleferico_alemao <- data.frame(
  Sigla = c("BCO", "ADS", "BNA", "ALO", "IRE", "PMS"),
  Nome = c("Bonsucesso", "Adeus", "Baiana", "Alemão", "Itararé", "Palmeiras"),
  Inauguracao = c("7 de julho de 2011", "7 de julho de 2011", "7 de julho de 2011",
                  "7 de julho de 2011", "7 de julho de 2011", "7 de julho de 2011"),
  Comunidade = c("Morro do Adeus", "Morro do Adeus", "Morro da Baiana",
                 "Morro do Alemão", "Itararé", "Favela das Palmeiras"),
  Integracao = c("Morro da Baiana", "Morro do Alemão", "Morro da Baiana",
                 "Morro do Alemão", "Itararé", "Favela das Palmeiras"), 
  Posicao = c("Superfície", "Elevada", "Elevada", "Elevada", "Elevada", "Elevada"),
  Latitude = c("22°52'03\" S", "22°51'59\" S", "22°51'43\" S",
               "22°51'30\" S", "22°51'47\" S", "22°51'39\" S"),
  Longitude = c("43°15'19\" O", "43°15'40\" O", "43°15'39\" O",
                "43°15'15\" O", "43°14'59\" O", "43°14'50\" O"),
  stringsAsFactors = FALSE
)

# If you haven't already, set the locale to Portuguese for date conversion
Sys.setlocale("LC_TIME", "pt_BR.UTF-8")
pre_sf_teleferico_alemao$Inauguracao <- as.Date(pre_sf_teleferico_alemao$Inauguracao, format = "%d de %B de %Y")

# Function to convert DMS to decimal degrees
dms_to_decimal <- function(dms) {
  # Remove any whitespace for consistency
  dms_clean <- gsub(" ", "", dms)
  # Extract degrees, minutes, seconds, and the direction (N, S, E, W)
  pattern <- "([0-9]+)°([0-9]+)'([0-9]+)\"([NSWO])"
  matches <- regmatches(dms_clean, regexec(pattern, dms_clean))[[1]]
  
  if(length(matches) == 0) return(NA)
  
  d <- as.numeric(matches[2])
  m <- as.numeric(matches[3])
  s <- as.numeric(matches[4])
  direction <- matches[5]
  
  dec <- d + m / 60 + s / 3600
  # For South and West, the decimal should be negative
  if (direction %in% c("S", "W", "O")) dec <- -dec
  
  return(dec)
}

# Apply conversion to Latitude and Longitude columns
pre_sf_teleferico_alemao$Latitude_decimal <- sapply(pre_sf_teleferico_alemao$Latitude, dms_to_decimal)
pre_sf_teleferico_alemao$Longitude_decimal <- sapply(pre_sf_teleferico_alemao$Longitude, dms_to_decimal)

# Create an sf object using the decimal coordinates.
# Note: The order of coordinates is (longitude, latitude)
teleferico_alemao <- 
  st_as_sf(pre_sf_teleferico_alemao, coords = c("Longitude_decimal", "Latitude_decimal"), crs = 4326)

# Check the data frame



# --

shootings_ts_formatted_no_tz <-
  as.POSIXct(raw_shootings$data,
             format = "%d/%m/%Y, %H:%M:%S")

shootings_ts_formatted_tz <-
  lubridate::ymd_hms(shootings_ts_formatted_no_tz, tz = "America/Sao_Paulo")

raw_shootings <-
  raw_shootings %>%
  mutate(shooting_ts = shootings_ts_formatted_tz)

# --

rio_neighborhoods <- 
  geobr::read_neighborhood(year = 2010,
                           simplified = FALSE) %>%
  filter(code_muni == 3304557) %>%
  mutate(name_neighborhood =
           stringi::stri_trans_general(str = .$name_neighborhood,
                                       id = "Latin-ASCII") %>% toupper())

all_cells_rio_grid <-
  st_make_grid(rio_neighborhoods, cellsize = 0.01) %>%
  st_sf()

intersecting_cells <- 
  which(lengths(st_intersects(all_cells_rio_grid, rio_neighborhoods)) > 0)

rio_grid <-
  all_cells_rio_grid[intersecting_cells,] %>%
  mutate(cell = row_number())

# --

rio_shootings <-
  raw_shootings %>%
  rename(shooting_id = id) %>%
  tidyr::drop_na(shooting_id) %>%
  st_as_sf(., coords = c('longitude', 'latitude'), crs = 'EPSG:4326') %>%
  st_transform(crs=st_crs(rio_neighborhoods)) %>%
  st_join(.,
          rio_neighborhoods['name_neighborhood'],
          join = st_intersects) %>%
  st_join(.,
          rio_grid,
          join = st_intersects)

rio_shootings <-
  st_transform(rio_shootings, st_crs(rio_neighborhoods))

teleferico_alemao <- 
  st_transform(teleferico_alemao, st_crs(rio_neighborhoods))

alemao_bbox <- st_bbox(teleferico_alemao)

# --

alemao_bbox

ggplot() +
  geom_sf(data = rio_neighborhoods %>% 
            filter(name_neighborhood == 'COMPLEXO DO ALEMAO'),
          fill='gray8', color='gray50') +
  geom_sf(data = 
            rio_shootings %>% 
            st_crop(alemao_bbox), color = 'magenta')

rio_shootings$shooting_ts
