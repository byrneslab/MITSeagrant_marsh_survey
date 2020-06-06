#################################################
#'
#' @Title Script to take Seagrant Byrnes lab veg 
#' and invert quad datasheets and turn them into 
#' clean data for analysis
#
#' @by Jarrett Byrnes
#'
#' @Notes Different protocols have very different data sheets
#' so,first task is to generate clean data files
#' and save. Once clean data is generated, then merge.
#################################################

#Load Libraries
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)

data_dir <- "../COMPLETE DATA/WHOLE datasheets/"
#na_possible <- c("NA", "na", "N/A", "n/a", "-")
na_possible <- "(NA)|(na)|(N\\/A)|(n\\/a)|-" #for gsub/stringr use
#test
#gsub(na_possible, "two", c("na", "n/a", "N/A", "NA", "chicken", "-"))

####### 
# Functions to parse data 
# put in in data sheet format
#######

#function to take 1 sheet and turn it into useful data
get_raw_quad_data <- function(start, end, adf, gap=2){
  #get metadata
  sample_date <- parse_date_time(gsub("Date:", "", adf[start,1]), order="mdy")
  site <- gsub("Site:", "", adf[start,3])
  obs <- gsub("Observer:", "", adf[start,5])
  
  #get data subset
  subdf <- adf[(start+gap):(end),]
  names(subdf) <- c("Measurement", apply(subdf[1:2,-1], 2, paste, collapse=";"))
  subdf[2,] <- gsub(" ", "", subdf[2,])
  subdf[3,] <- gsub(" ", "", subdf[3,])
  
  #reshape it
  ret <- subdf[-c(1:2),] %>%
    gather(tq, value, -Measurement) %>%
    spread(Measurement, value) %>%
    separate(tq, into=c("Transect", "Quadrat"), ";") %>%
    mutate(Transect = gsub("Trans:", "", Transect),
           Quadrat = gsub("Quad:", "", Quadrat))
  
  #merge metadata and data
  ret$year = year(sample_date)
  ret$month = month(sample_date)
  ret$day = day(sample_date)
  ret$Site = site
  ret$Observer = obs
  
  #return the data
  ret %>%
    select(Site, Transect, Quadrat, day, month, year, Observer, everything())
  
}


make_processed_from_raw <- function(adf, gap=2){
  
  #Get start and end index of each "sheet"
  adf_sheet_start <-   grep("Date.*",adf$X1)
  adf_sheet_end <-   grep("SPA Height10", adf$X1)
  
  #process the data given start and end
  adf_dat_list <- lapply(1:length(adf_sheet_start), 
                         function(i) {print(i); get_raw_quad_data(adf_sheet_start[i], 
                                                                  adf_sheet_end[i],
                                                                  adf, gap=gap)})
  
  adf_dat <- bind_rows(adf_dat_list)
  
  #make NAs where needed
  adf_dat <- purrr::map_df(adf_dat, ~gsub(na_possible, NA, .))
  
  #Some light string cleaning
  adf_dat <- purrr::map_df(adf_dat, ~gsub("_", "", .))
  adf_dat <- purrr::map_df(adf_dat, str_trim)
  
  
  #fill 0s down for NAs
  adf_dat[is.na(adf_dat)] <- 0
  
  adf_dat
}

############
# Glades Corps	2014
############

gc <- as.data.frame(read_excel(paste0(data_dir, "Glades Corp 2014 NOT FINAL.xls"), sheet="Quadrat Sampling", col_names=FALSE))

gc_dat <- make_processed_from_raw(gc, gap = 2)

# Rock Island	2014
############
# Plum Island	2014
############

plum_14 <- as.data.frame(read_excel(paste0(data_dir, "Plum 2014 FINAL.xls"), sheet="Quadrat Sampling", col_names=FALSE))

#line up the two columnsinto 1 df
pcol1 <-plum_14[,1:6]
pcol2 <-   plum_14[,7:12]
names(pcol2) <- names(pcol1)
plum_14 <- bind_rows(pcol1, pcol2)

#now make the cleaned data
plum_14_dat <- make_processed_from_raw(plum_14, gap=3)

########
# Nantucket	2014
# In a semi-processed form
########

#We know about separate warnings
nfs_veg <- as.data.frame(read_excel(paste0(data_dir, "Nantucket.xlsx"), sheet="Vegetation Percent Cover")) %>%
  select(-`41834`) %>%
  mutate(Observer = NA, day=NA, month=NA, year=2014) %>%
  rename(Transect = trans, Quadrat = quad, 
         `% Bare` = `% BARE`,
         `% Detritus` = `% DETRITUS`,
         `% Wrack`  = `% W`,
         LIM = lim,
         UPSP = upsp,
         SEDGE = sedge) %>%
    separate(`SPA DENSITY (X3)`, into=c("SPA Density1", "SPA Density2", "SPA Density3"), sep=",") %>%
    separate(`HEIGHT (X10)`, into=paste("SPA Height", 1:10, sep=""), sep=",")

# Plum Island	2015
# Medouie	2015
# Waquoit	2015
# Squantum	2015
# Eel Point	2015
