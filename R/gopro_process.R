#################################################
#'
#' @Title Script to take Seagrant Byrnes lab gopro
#' raw data and turn them into 
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
library(googlesheets)
library(readxl)
library(lubridate)

##### Initialize GoogleSheet ####
GoPro <- gs_title("GoPro Video Data 2014/15")

##### Download working copy #####
GoPro %>% 
  gs_download(to = "../COMPLETE DATA/GoPro/gopro_from_googlesheets.xlsx")


##### Fetch from Google ####
gp_data <- lapply(1:8, function(i){Sys.sleep(5); gs_read(GoPro, ws = i)})

##### Get cols we want ####
gp_data_table <- lapply(gp_data, function(adf)
  adf %>% 
    select(Site, Transect, Video_Date, `Video #`, TimeStamp, Species, Quantity, Behaviors, Clarity, Recorder)
  )

##### bind it up ####
gp_data_table <- bind_rows(gp_data_table) %>%
  mutate(Site = ifelse(is.na(Site), "Nantucket", Site)) %>%
  mutate(TimeStamp = parse_time(TimeStamp))

## NEED TO GET LENGTH OF VIDEO INFO
gp_summary <- gp_data_table %>%
  mutate(Year = year(parse_date_time(Video_Date, orders="mdy"))) %>%
  group_by(Site, Transect, `Video #`, Year) %>%
  summarize(Video_Duration = max(TimeStamp, na.rm=TRUE) - min(TimeStamp, na.rm=TRUE))
  

gp_species <- gp_data_table %>%
  group_by(Site, Transect, `Video #`, Species) %>%
  summarize(Abundance = sum(Quantity, na.rm=TRUE)) %>%
  filter(!is.na(Species))

gp_clean <- left_join(gp_species, gp_summary)

#### Write it out ####
write.csv(gp_clean, "../clean_data/video_marsh_sampling.csv", row.names=FALSE)
