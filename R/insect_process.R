#################################################
#'
#' @Title Script to take Seagrant Byrnes lab insect
#' rap data and turn them into 
#' clean data for analysis
#
#' @by Jarrett Byrnes
#'
#' @Notes Different protocols have very different data sheets
#' so,first task is to generate clean data files
#' and save. Once clean data is generated, then merge.
#' Counts of squares are out of 36
#################################################

#Load Libraries
library(tidyverse)
library(readxl)

#Read in and clean
insects_orig <- read.csv("../COMPLETE DATA/insects/Vacuum data_FINAL.csv",
                           na.strings=c("NA", "", "`", "Na"),
                         stringsAsFactors=FALSE) %>%
  select(-X, -X.1, -X.2, -notes, -Date.started, -Date.finished, sample.code) %>%
  rename(Recorder = Recorder..initials., Site = site) %>%
  filter(!is.na(Site)) %>%
  group_by(Site, Transect, Quadrat) %>%
  mutate(N_Squares = length(unique(Square..))) %>%
  group_by(Site, Transect, Quadrat, Species, N_Squares) %>%
  summarize(Count = sum(Count, na.rm=TRUE))

## check for issues
library(assertr)


#Add taxonomy


#write out
write.csv(insects_orig, "../clean_data/insects_marsh_sample.csv", row.names=FALSE)

