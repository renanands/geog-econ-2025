# --------------------------------------------------------------------------- #
# ------------------------- < generate rain files > ------------------------- #
# --------------------------------------------------------------------------- #


# =========================================================================== 
#
# This script pre-processes the shootings dataset 
#
# Needs cleaning to raw > pluv files only for 2020
#
# =========================================================================== 

library(dplyr)
library(sf)
library(ggplot2)

#
## ---------------------- get file paths
#

pluv_files_2019 <- 
  list.files(here::here('data', 'raw', 'DadosPluviometricos2019'),
             pattern = "*.txt",
             full.names = TRUE)

pluv_files_2020 <- 
  list.files(here::here('data', 'raw', 'DadosPluviometricos2020'),
             pattern = "*.txt",
             full.names = TRUE)

pluv_files <- c(pluv_files_2019, 
                pluv_files_2020)

# --

rio_neighborhoods <-
  readRDS(here::here('data', 'rio_neighborhoods.Rds'))

raw_meta_station_info <- 
  data.frame(
    station = c("Vidigal", "Urca", "Rocinha", "Tijuca", "Santa Teresa",
                "Copacabana", "Grajaú", "Ilha do Governador", "Penha",
                "Madureira", "Irajá", "Bangu", "Piedade", "Tanque",
                "Saúde", "Jardim Botânico", "Barrinha", "Cidade de Deus",
                "Riocentro", "Guaratiba", "Grajaú/Jacarepaguá", "Santa Cruz",
                "Grande Méier", "Anchieta", "Grota Funda", "Campo Grande", "Sepetiba",
                "Alto da Boa Vista", "Av Brasil/Mendanha", "Recreio",
                "Laranjeiras", "São Cristóvão", "Tijuca/Muda"),
    latitude = c(-22.99250, -22.95583, -22.98583, -22.93194, -22.93167,
                 -22.98639, -22.92222, -22.81806, -22.84444, -22.87333,
                 -22.82694, -22.88028, -22.89182, -22.91250, -22.89606,
                 -22.97278, -23.00849, -22.94556, -22.97721, -23.05028,
                 -22.92556, -22.90944, -22.89056, -22.82694, -23.01444,
                 -22.90361, -22.96889, -22.96583, -22.85694, -23.01000,
                 -22.94056, -22.89667, -22.93278),
    longitude = c(-43.23306, -43.16667, -43.24500, -43.22167, -43.19639,
                  -43.18944, -43.26750, -43.21028, -43.27528, -43.33889,
                  -43.33694, -43.46583, -43.31005, -43.36472, -43.18786,
                  -43.22389, -43.29965, -43.36278, -43.39155, -43.59472,
                  -43.31583, -43.68444, -43.27806, -43.40333, -43.52139,
                  -43.56194, -43.71167, -43.27833, -43.54111, -43.44056,
                  -43.18750, -43.22167, -43.24333)
  )


meta_station_info <- 
  raw_meta_station_info %>% 
  mutate(station = 
           stringi::stri_trans_general(str = .$station,
                                       id = "Latin-ASCII") %>% 
           toupper(.) %>% 
           gsub(' ', '_', .) %>% 
           gsub('/', '_', .) %>% 
           gsub('_', ' ', .)) %>%
  st_as_sf(., coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(., st_crs(rio_neighborhoods))

# --

#
## ---------------------- read files + implement CPI 2019 formula
# 

# diagnostics
# catches ill-formatted observations
faulty_lines <- list()

# --

read_rainfall_data <- function(file, flavor) {
  
  # identify if forecasting (_Plv) or observation (_Met) file
  
  if (flavor=='pluv'){
    station_name <- stringr::str_remove(basename(file), "_\\d{6}_Plv\\.txt$")
  }
  
  if (flavor=='meteo'){
    station_name <- stringr::str_remove(basename(file), "_\\d{6}_Met\\.txt$")
  }
  
  raw_lines <- readLines(file)
  
  # --
  
  if (flavor=='pluv'){
    df <- tryCatch({
      read.table(text = raw_lines,
                 skip = 5,
                 fill = TRUE,
                 header = FALSE,
                 na.strings = "ND")
    }, error = function(e) {
      message(paste("Error reading file:", file, "- Skipping faulty lines"))
      return(NULL)
    })
  }
  

  
  if (flavor=='meteo'){
    df <- tryCatch({
      read.table(text = raw_lines,
                 skip = 6,
                 fill = TRUE,
                 header = FALSE,
                 na.strings = "ND")
    }, error = function(e) {
      message(paste("Error reading file:", file, "- Skipping faulty lines"))
      return(NULL)
    })
  }
  
  # --
  
  # assigns correct col names
  # secures numeric formatting
  
  if (!is.null(df)) {
    
    if (flavor == 'pluv'){
      if (ncol(df) == 8){
        df <- df %>% dplyr::select(-V3)
      }
      colnames(df) <- 
        c("Dia", "Hora", "15 min", "01 h", "04 h", "24 h", "96 h")
      df[,3] <- as.numeric(df[,3])
      df[,4] <- as.numeric(df[,4])
      df[,5] <- as.numeric(df[,5])
      df[,6] <- as.numeric(df[,6])
      df[,7] <- as.numeric(df[,7])
      
    }
    
    if (flavor == 'meteo'){
      if (ncol(df) == 9){
        df <- df %>% dplyr::select(-V3)
      }
      colnames(df) <- 
        c('Dia','Hora','Chuva','DirVento','VelVento','Temperatura','Pressao','Umidade')
      df[,3] <- as.numeric(df[,3])
      df[,4] <- as.numeric(df[,4])
      df[,5] <- as.numeric(df[,5])
      df[,6] <- as.numeric(df[,6])
      df[,7] <- as.numeric(df[,7])
      df[,8] <- as.numeric(df[,8])
      
    }
    
    # --
    
    df <- df %>% mutate(station = station_name)
  
  }
  
  return(df)
}

# --

pluv_data <-
  purrr::map_df(c(pluv_files), read_rainfall_data, flavor='pluv') %>%
  mutate(time_15min = 
           lubridate::dmy_hms(paste(Dia, Hora), tz = 'America/Sao_Paulo')) %>%
  mutate(station = 
           stringi::stri_trans_general(str = .$station,
                                       id = "Latin-ASCII") %>% 
           toupper(.) %>% 
           gsub('_', ' ', .)) %>%
  left_join(., meta_station_info, by = 'station')

# -

pluv_data_roll <- 
  pluv_data %>%
  mutate(`15 min` = as.numeric(`15 min`)) %>%
  tidyr::drop_na(`15 min`) %>%
  mutate(
    sum_15 = zoo::rollsumr(`15 min`, 2, fill = NA),
    sum_30 = zoo::rollsumr(`15 min`, 3, fill = NA),
    sum_60 = zoo::rollsumr(`15 min`, 5, fill = NA)
  )

# -

pluv_data_alerted <-
  pluv_data_roll %>%
  mutate(SA_ind = ifelse((sum_15 > 15.0) | (sum_30 > 20.0) | (sum_60 > 25.0), 1, 0),
         SC_ind = ifelse((sum_30 > 40.0) | (sum_60 > 55.0), 1, 0)) %>%
  dplyr::summarise(
    .by = time_15min,
    SA_sum = sum(SA_ind),
    SC_sum = sum(SC_ind),
    rain_sum = sum(`15 min`)) %>%
  mutate(SC = ifelse(SC_sum > 1, 1, 0)) %>%
  mutate(SA = ifelse((SA_sum > 0) & (SC != 1), 1, 0))

saveRDS(pluv_data_alerted, here::here('data', 'pluv_data_alerted.Rds'))

# --

start_date <- as.POSIXct('2020-01-31 23:59:59')
end_date <- as.POSIXct('2020-05-01 00:00:01')
  
pluv_data_summ_data <-
  pluv_data_alerted %>%
  filter(time_15min > start_date &
           time_15min < end_date) %>%
  mutate(year = lubridate::year(time_15min)) %>%
  mutate(month = lubridate::month(time_15min)) %>%
  mutate(day = lubridate::day(time_15min)) %>%
  mutate(hour = lubridate::hour(time_15min)) %>%
  mutate(day_period = cut(hour,
                          breaks = c(0, 6, 12, 18, 24),
                          labels = c(1, 2, 3, 4))
  ) %>%
  mutate(day_period = ifelse(hour == 0, 3, day_period))

pluv_data_summ_data %>%
  summarise(.by = c(year, month, day, day_period),
            sum_SA_sum = sum(SA_sum),
            sum_SC_sum = sum(SC_sum),
            mean_rain_sum = mean(rain_sum),
            sum_SA = sum(SA),
            sum_SC = sum(SC))

# -----------------------------

# get monthly counts

pluv_monthly_counts <-
  pluv_data_alerted %>%
  tidyr::drop_na() %>%
  mutate(month = format(as.POSIXct(.$time_15min), "%m-%Y")) %>%
  summarise(.by = month,
            SA_cumm = sum(SA),
            SC_cumm = sum(SC))


saveRDS(pluv_monthly_counts, here::here('data', 'pluv_monthly_counts.Rds'))

# --

meta_station_info <-
  meta_station_info %>%
  mutate(type =
           ifelse(
             station %in% (meteo_data$station %>% unique),
             'METEO',
             'PLUV')
  )