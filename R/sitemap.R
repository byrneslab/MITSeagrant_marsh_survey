library(tidyverse)
library(ggplot2)
library(readr)
library(rnaturalearth)
library(sf)



my_map <- ne_states(country = "united states of america", returnclass = "sf") %>%
  filter(name_tr == "Massachusetts")

crp <- c(xmin = -71.5, st_bbox(my_map)[2], xmax = -69.5, st_bbox(my_map)[4])
mass_map <- st_crop(my_map, crp)

sites <- read_csv("../clean_data/sites.csv") %>%
  mutate(Site = ifelse(Site=="M", "Med", Site)) %>%
  st_as_sf(coords = c("Longitude", "Latitude")) %>%
  st_set_crs(st_crs(mass_map))

ggplot() +
  geom_sf(data = mass_map, fill = "lightgreen") +
  xlim(c(-71.5, -69.75)) +
  ylim(c(41.2,43)) +
  geom_sf(data = sites,  size = 5,color = "red") +
  #ggthemes::theme_map(base_size = 18) +
  #theme_void(base_size = 18, base_line_size=0) +
  theme(plot.background = element_rect(fill = "#BFD5E3"),
        panel.background = element_rect(fill = "#BFD5E3"),
        axis.text = element_text(size = 18)) 
  
