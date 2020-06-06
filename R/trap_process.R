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

trap_dir <- "../COMPLETE DATA/fishcrabtrap/"
trap_files <- list.files(trap_dir)

fish <- trap_files[grep("fish", trap_files)]
crab <- trap_files[grep("crab", trap_files)]

#### Function to read trials ####
read_trap <- function(afile){
 # afile <- fish[1]
  print(afile)
  atrap <- read.csv(paste0(trap_dir, afile), 
                    stringsAsFactors = FALSE,
                    na.strings=c("NA", "", " ", "`", "-", "MISSING"))
  
  names(atrap)[grep("Day", names(atrap))] <- "Day"
  names(atrap) <- gsub("[T,t]rans$", "Transect", names(atrap))
  
  site <- gsub("fish_", "", afile)
  site <- gsub("crab_", "", site)
  site <- gsub("\\.csv", "", site)
  atrap$Site <- site
  
  atrap_clean <- atrap %>%
    gather(Species, Count, -Site, -Transect, -Day) %>%
    group_by(Site, Transect, Species) 
  
  print(class(atrap_clean$Transect)) #debug
  atrap_clean <- atrap_clean %>%
    summarize(Avg_Count = mean(Count, na.rm=T)) %>%
    ungroup () %>%
    mutate(Avg_Count = ifelse(is.nan(Avg_Count), 0, Avg_Count))
  
  atrap_clean
}

trap_data <- lapply(trap_files, read_trap) %>%
  bind_rows()

write.csv(trap_data, "../clean_data/traps_marsh_sampling.csv", row.names=FALSE)
