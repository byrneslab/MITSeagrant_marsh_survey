library(tidyverse)
library(ggplot2)
library(readr)
library(ggmap)

deep <- read_csv("../clean_data/deep_pitfall_marsh_sampling.csv")
sites <- read_csv("../clean_data/sites.csv") %>%
  mutate(Site = ifelse(Site=="M", "Med", Site))

ses <- deep %>%
  filter(Species=="SES")  %>%
  left_join(sites) %>%
  group_by(Site, Latitude, Longitude) %>%
  summarize(`Total Sesarma` = sum(Avg_Count))




myMap <- get_map(location=c(-71.321359, 42.421448), source="google", maptype="roadmap", crop=FALSE, zoom=8, color="bw") 

base_map <- ggmap(myMap) +
  xlim(c(-71.5, -69.75)) +
  ylim(c(41.2,43)) 


base_map +
  geom_point(sites,  mapping = aes(x = Longitude, y = Latitude),
             shape=2) +
  geom_point(data = ses, 
             mapping = aes(x = Longitude, y = Latitude,
                           color = `Total Sesarma`,
                           size = `Total Sesarma`)) +
  scale_color_gradient(low = "blue", high = "red")
