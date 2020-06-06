#################################################
#'
#' @Title Script to take Seagrant Byrnes lab marsh bird datasheets
#' and turn them into clean data for analysis
#
#' @by Jarrett Byrnes
#'
#' @Notes Different protocols have very different data sheets
#' so,first task is to generate clean data files
#' and save. Once clean data is generated, then merge.
#################################################

#Load Libraries
library(tidyverse)
library(lubridate)

birds <- read.csv("../COMPLETE DATA/bird/bird_ALLSITES.csv") %>%
  select(-X, -X.1) %>%
  mutate(Species = toupper(Species),
         Species = gsub("AMERICAN ROBIN", "AMRO", Species),
         Species = gsub("CATBIRD", "CAT", Species),
         Species = gsub("WILLET", "WILLETT", Species),
         Species = gsub("ROBIN", "AMRO", Species),
         Species = gsub("SNOWYEGRET", "SNOWY", Species),
         Species = gsub("TREESWALLOW", "TSWAL", Species),
         Species = gsub("TREE SWALLOW", "TSWAL", Species),
         Species = gsub("WHIMREL", "WHIMBREL", Species),
         Species = gsub("TURKEY VULTURE", "TUVU", Species),
         Species = gsub("TREESWALLOW", "TSWAL", Species),
         Species = gsub("SANDPIPERS", "SANDPIP", Species),
         Species = gsub("RE TAILED HAWK", "REDHAWK", Species),
         Species = gsub("MOURNING DOVE", "MOURNINGDOVE", Species),
         Species = gsub("MERLIN", "MERL", Species),
         Species = gsub("GREY CARBIRD", "CAT", Species),
         Species = gsub("CEDAR W.", "CEWA", Species), 
         Species = gsub("CHICKADEE", "CHIC", Species), 
         Species = gsub("GOLDFINCH", "GOLD", Species), 
         Species = gsub("GREAT BB GULL", "GBBG", Species),
         Species = gsub("HERG", "HEG", Species),
         Species = gsub("LEAST SANDPIPER", "LSPIP", Species), 
         Species = gsub("OSPREY", "OSP", Species), 
         Species = gsub("PIPLOV", "PPLOV", Species), 
         Species = gsub("SALTMARSH SPARROW", "SMSPAR", Species), 
         Species = gsub("WILLETTT", "WILLETT", Species), 
         Species = gsub("WHIMBREL", "WHIM", Species), 
         Species = gsub("CHICK", "CHIC", Species), 
         Species = gsub("CREWA", "CEWA", Species), 
         Species = gsub("GOLD FINCH", "GOLD", Species), 
         Species = gsub("HRRG", "HEG", Species), 
         Species = gsub("GRCA", "GRAC", Species), 
         Species = gsub("GREY CAT", "CAT", Species), 
         Species = gsub("GREY CATBIRD", "CAT", Species), 
         Species = gsub("HARRIER", "HAR", Species), 
         Species = gsub("HERRING GULL", "HEG", Species), 
         Species = gsub("KILLDEER", "KILL", Species), 
         Species = gsub("LGULL", "LAG", Species), 
         Species = gsub("MALLARD", "MAL", Species), 
         Species = gsub("OSPR", "OSP", Species), 
         Species = gsub("RDOIN", "AMRO", Species), 
         Species = gsub("SANDERLING", "SLING", Species), 
         Species = gsub("SMSP", "SMSPAR", Species), 
         Species = gsub("SM SPATR", "SMSPAR", Species), 
         Species = gsub("SNEG", "SNOWY", Species), 
         Species = gsub("SPAR", "SMSPAR", Species), 
         Species = gsub("SPOTTED SANDPIPER", "SPPIP", Species), 
         Species = gsub("SSP", "SPPLOV", Species), 
         Species = gsub("SWALLOW SPP.", "SWAL", Species), 
         Species = gsub("TSPAR", "TSWAL", Species), 
         Species = gsub("TSWAP", "TSWAL", Species), 
         Species = gsub("TRSW", "TSWAL", Species), 
         Species = gsub("SMSMSPARAR", "SMSPAR", Species), 
         Species = gsub("SMSMSPAR", "SMSPAR", Species), 
         Species = gsub("ROB", "AMRO", Species), 
         Species = gsub("COMMON TERN", "COTE", Species), 
         Species = gsub("GREAT YELLOW LEGS", "GRYE", Species), 
         Species = gsub("LSAND", "LESA", Species),
         Species = gsub("VESAND", "LESA", Species),
         Species = gsub("SANDPIP", "SPPIP", Species)
  ) %>%
  group_by(Site, Zone, Tide, Species) %>%
  summarize(Avg_Count = sum(Number, na.rm=TRUE))
###NOTE: I named this "Avg_Count even tho its a sum because when we merge them all together the bird numbers arent getting in there

write.csv(birds, "../clean_data/birds_marsh_sampling.csv", row.names=FALSE)
