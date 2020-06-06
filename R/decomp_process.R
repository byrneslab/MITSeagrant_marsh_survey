#################################################
#'
#' @Title Script to take Seagrant Byrnes lab decomp
#' data and turn them into 
#' clean data for analysis
#
#' @by Jarrett Byrnes
#'
#' @Notes Different protocols have very different data sheets
#' so,first task is to generate clean data files
#' and save. Once clean data is generated, then merge.
#################################################

#Load Libraries
library(tidyverse)

decomp <- read.csv("../COMPLETE DATA/deco/deco_allsites.csv") %>%
  select(site:final.mass) %>%
  rename(Site = site, Year = year, Transect = transect) %>%
  mutate(mass_lost_g = initial.mass - final.mass) %>%
  filter(!is.na(Site))

write.csv(decomp, "../clean_data/decomp_marsh_sampling.csv",
          row.names=FALSE)
