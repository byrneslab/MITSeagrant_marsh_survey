#################################################
#'
#' @Title Script to take Seagrant Byrnes lab fish
#' and crab trap data and turn them into 
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

sedi <- read.csv("../COMPLETE DATA/sedi/sedi_allsites.csv",
                 na.strings=c("", "NA", "N/A")) %>%
  select(site, transect, year, final.mass) %>%
  rename(Site = site, Transect = transect, Year = year, sediment_dry_mass_g = final.mass)


write.csv(sedi, "../clean_data/sediment_marsh_sampling.csv",
          row.names=FALSE)