#################################################
#'
#' @Title Script to take Seagrant Byrnes lab deep
#' pitfall datasheets and turn them into 
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

pitfall_dir <- "../COMPLETE DATA/pitfall/"


#### Shallow Pitfalls First ####
shallow <- read.csv(paste0(pitfall_dir, "ShallowPitfall_allsites.csv"),
                    na.strings=c("", "NA", "not done")) %>%
  rename(Site = site, Transect = transect, Year = year) %>%
  select(-site.code) %>%
  gather(Species, Count, -Site, -Transect, -Year)

write.csv(shallow, "../clean_data/shallow_pitfall_marsh_sampling.csv", row.names=FALSE)


#### Deep Pitfalls ####
pitfalls <- list.files(pitfall_dir)
pitfalls <- pitfalls[grep("Deep", pitfalls)]

read_pitfalls <- function(afile){
  #afile <- pitfalls[1]
  print(afile)
  
  apitfall <- read.csv(paste0(pitfall_dir, afile),
                       na.strings=c("", "NA", "-", "`"),
                       stringsAsFactors = FALSE)
  site <- gsub("DeepPitfall_", "", afile)
  site <- gsub("\\.csv", "", site)
  apitfall$Site <- site
  names(apitfall)[grep("Day", names(apitfall))] <- "Day.of.Sampling"
  
  #print(summary(apitfall)) #debug
  
  
  apitfall_out <- apitfall %>%
    rename(Day = Day.of.Sampling) %>%
    gather(Species, Count, -Site, -Transect, -Quadrat, -Day) %>%
    group_by(Site, Transect, Quadrat, Species)  %>%
    summarize(Avg_Count = mean(Count, na.rm=TRUE)) %>%
    ungroup () %>%
    mutate(Avg_Count = ifelse(is.nan(Avg_Count), 0, Avg_Count))
  
  
  
  apitfall_out
}

pitfalls_data <- lapply(pitfalls, read_pitfalls) %>%
  bind_rows()

write.csv(pitfalls_data, "../clean_data/deep_pitfall_marsh_sampling.csv", row.names=FALSE)
