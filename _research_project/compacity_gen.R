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

get_across_samples_disconnection <-
  
  function(muni_sf, n_samples=3, n_points=5){
    
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


# start.time <- Sys.time()
# #
# all_muni_s10p100 <-
#   all_muni %>%
#   mutate(index = 
#            get_across_samples_disconnection(muni_sf = .,
#                                             n_samples = 10,
#                                             n_points= 100)) %>%
#   mutate(area = as.numeric(st_area(geom))) %>%
#   mutate(index_norm = index / sqrt(area/pi) )
# #
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# 
# saveRDS(all_muni_s10p100, here::here('out', 'all_muni_s10p100.Rds'))

# --

all_muni_s10p100 <-
  readRDS(here::here('out', 'all_muni_s10p100.Rds'))

non_normalized_PE <-
  ggplot() +
  geom_sf(data = all_muni_s10p100 %>% 
            filter(abbrev_state == 'PE') %>%
            filter(name_muni != 'FERNANDO DE NORONHA'), 
          aes(fill = index), color = NA) +
  scale_fill_gradient(low='navy', high='magenta') +
  ggtitle('Raw index')

ggsave(plot = non_normalized_PE,
       here::here('out', 'non_normalized_PE.png'))

normalized_PE <-
  ggplot() +
  geom_sf(data = all_muni_s10p100 %>% 
            filter(abbrev_state == 'PE') %>%
            filter(name_muni != 'FERNANDO DE NORONHA'),
          aes(fill = index_norm), , color = NA) +
  scale_fill_gradient(low='navy', high='magenta') +
  ggtitle('Normalized index')

ggsave(plot = normalized_PE,
       here::here('out', 'normalized_PE.png'))