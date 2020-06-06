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

cores <- read.csv("../COMPLETE DATA/core/Core_allsites.csv") %>%
  select(-site.code) %>%
  rename(Site = site, Transect = transect, Year = year) %>%
  gather(Species, Count, -Site, -Transect, -Year)  

write.csv(cores, "../clean_data/cores_marsh_sampling.csv",
          row.names=FALSE)
