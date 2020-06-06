#################################################
#'
#' @Title Script to take Seagrant Byrnes lab veg 
#' and invert quad datasheets and turn them into 
#' clean data for analysis
#
#' @by Jarrett Byrnes
#'
#' @Notes 
#################################################

#Load Libraries
library(tidyverse)

###Note 2/1/19 I found an error and for some reason Squantum Quads was the same as PI15.  Real squantum quads were found and are in teh COMPLETE data folder now.  Probably didnt affect too much in the past analysis but I need it for my paper! (mh) 

data_dir <- "../COMPLETE DATA/quadrat/"
na_possible <- "(NA)|(na)|(N\\/A)|(n\\/a)|-" #for gsub/stringr use
na_strings = c("", "NA", "n/a", "N/A", "N/a", "na", " ", "-", "``", "Na")

#Get files
files <- list.files(data_dir)

#function to read one file
read_quad <- function(afile){
  print(afile)
 # afile <- files[8] #debug
  aquad <- read.table(paste0(data_dir, afile), sep=",",
                      na.strings=na_strings,
                      stringsAsFactors = FALSE)
  lastIDX <- which(aquad[,1]=="SPA Height10")
  aquad <- aquad[-c(lastIDX:nrow(aquad)),]
  #aquad <- aquad[,-which(is.na(aquad[1,]))]
  
  #reshape
  aquad_clean <- aquad %>%
    gather(key, value, -V1) %>%
    #clean
    filter(!is.na(V1)) %>%
    #make na into 0
    mutate(value = ifelse(is.na(value), 0, value)) %>%
    
    #Strip identifier off of subsamples and clean
    mutate(V1 = gsub("SPA Height[0-9]", "SPA Height", V1),
           V1 = gsub("SPA Density[0-9]", "SPA Density", V1),
           V1 = gsub("Melampus [0-9]", "Melampus", V1),
           V1 = gsub("Amphipods [0-9]", "Amphipods", V1),
           V1 = gsub("upsp", "UPSP", V1),
           V1 = gsub("uua", "UUA", V1),
           V1 = gsub("upsp", "UPSP", V1),
           V1 = gsub("Trans$", "Transect", V1),
           V1 = gsub("Quad$", "Quadrat", V1)) %>% 
    
    #fix the repeat name error
    group_by(V1, key) %>%
    summarize(value = sum(as.numeric(value), na.rm=TRUE)) %>%
    ungroup() %>%
    
    #reshape into something wide
    spread(V1, value, fill=0) %>%
    select(-key) 
  
  site <- gsub("quadrat_", "", afile)
  site <- gsub("\\.csv", "", site)
  aquad_clean$Site <- site
  
  aquad_clean
 
}

quad_list <- lapply(files, read_quad)
all_auads <- bind_rows(quad_list)

#### Harmonize Subsampling #### Estimating NPP using formula from Nantucket regressions, added SPA term to account for per.cover ###
all_auads <- all_auads %>%
  ungroup() %>%
  mutate(`SPA Density` = (`SPA Density`/3)*8, #per sq m
         `SPA Height` = (`SPA Height`)/10) %>%
  mutate(Burrows = Burrows*8) %>%
  mutate(live.per = (100 - `% Bare` - (`% Detritus`/4))) %>%
  mutate(dead.per = (`% Detritus`)) %>%
  mutate(NPP_est = ((`SPA Density`^.8)*(`SPA Height`)/6)* (SPA/100))
  #rename(SPA_Density_sq_m = SPA Density,
  #       Avg_SPA_Height_cm = SPA Height)
  


##NOTE!!!!!!!!!! I took out the Melampus*100 because the scale gets so big. Im just going to explain the differences in scale in the words of the figure.

#all_auads <- all_auads %>%
#  mutate(`SPA Density` = `SPA Density`/3*8, #per sq m
#         `SPA Height` = `SPA Height`/10) %>%
#  mutate(Burrows = Burrows*8, Melampus = Melampus*100) %>%
#  rename(SPA_Density_sq_m = `SPA Density`,
#         Avg_SPA_Height_cm = `SPA Height`)


####

#### Reshape to Long ####
all_auads <- all_auads %>%
  gather(Measurement, Value, -Site, -Transect, -Quadrat)

write_csv(all_auads, "../clean_data/quadrat_marsh_sample.csv")





###Filter out just animals for ease of operation####
#Realized thie isnt needed but the data table is kinda nice so i'm leaving it in for reference

quad_animalsTog <- all_auads %>%
  filter(Measurement %in% c("Burrows", "Littorina", "Melampus", "Mussels")) %>%
  group_by(Measurement, Quadrat, Transect, Site) %>%
  mutate(row_id=1:n()) %>% ungroup() %>%
  spread(key = Measurement, value = Value) %>%
  select(-row_id)

write_csv(quad_animalsTog, "../clean_data/quadanimals_marsh_sample.csv")


##these get copy-pasted over to the main merge_marsh_data_MH but i left themhere too 
burrows_Q <- all_auads %>%
  filter(Measurement == "Burrows") %>%
  rename(Count = Value) %>%
  rename(Protocol = Measurement)

littorina <- all_auads %>%
  filter(Measurement == "Littorina") %>%
  rename(Count = Value) %>%
  rename(Protocol = Measurement)

melampus <- all_auads %>%
  filter(Measurement == "Melampus") %>%
  rename(Count = Value) %>%
  rename(Protocol = Measurement)

mussels <- all_auads %>%
  filter(Measurement == "Mussels") %>%
  rename(Count = Value) %>%
  rename(Protocol = Measurement)

abund_w_quad_animals <- full_join(abund_protocol_site_func, quad_animals)




