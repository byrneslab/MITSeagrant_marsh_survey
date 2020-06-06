#################################################
#'
#' @Title Script to take Seagrant Byrnes lab bite 
#' map datasheets and turn them into 
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

bite_dir <- "../COMPLETE DATA/bitemap/"
files <- list.files(bite_dir)
squid <- files[grep("SQUID", files)]
kelp <- files[grep("KELP", files)]

parse_bitemap_squid <- function(afile){
 # afile <- kelp[1]
  print(afile)#debug
  bitemap <- read.csv(paste0(bite_dir, afile), stringsAsFactors=FALSE) 
  names(bitemap) <- gsub("[T,t]rans$", "Transect", names(bitemap))
  names(bitemap) <- gsub("[d,D]ay$", "Day", names(bitemap))
  
  bitemap <- bitemap %>%
    gather(Quadrat, Record, -Day, -Transect) %>%
    arrange(Day)
  
  site <- gsub("biteSQUID_", "", afile)
  #site <- gsub("biteKELP_", "", site)
  site <- gsub("\\.csv", "", site)
  bitemap$Site <- site
  
  
  #m: missing
  #p: partial bites taken out
  #s: scraped
  #w: whole
  
  #simplify to # missing over experiment
  ##REDONE- just missing and partials count for squid
  bitemap %>%
    group_by(Site, Transect, Quadrat, Record) %>%
    tally %>%
    ungroup() %>%
    group_by(Site, Transect, Quadrat) %>%
    filter(Record %in% c("m" , "M", "p", "P")) %>%
    summarize(Number_Missing = sum(n)) %>%
    ungroup()
  
}

parse_bitemap_kelp <- function(afile){
  # afile <- kelp[1]
  print(afile)#debug
  bitemap <- read.csv(paste0(bite_dir, afile), stringsAsFactors=FALSE) 
  names(bitemap) <- gsub("[T,t]rans$", "Transect", names(bitemap))
  names(bitemap) <- gsub("[d,D]ay$", "Day", names(bitemap))
  
  bitemap <- bitemap %>%
    gather(Quadrat, Record, -Day, -Transect) %>%
    arrange(Day)
  
  #site <- gsub("biteSQUID_", "", afile)
  site <- gsub("biteKELP_", "", afile)
  site <- gsub("\\.csv", "", site)
  bitemap$Site <- site
  
  #m: missing
  #p: partial bites taken out
  #s: scraped
  #w: whole
  
  #simplify to # missing over experiment
  ##REDONE- just scraped and partial counts for kelp.  Any M was from the tide ripping the thing off, nothing could eat that much kelp
  bitemap %>%
    group_by(Site, Transect, Quadrat, Record) %>%
    tally %>%
    ungroup() %>%
    group_by(Site, Transect, Quadrat) %>%
    filter(Record %in% c("s", "S", "p", "P")) %>%
    summarize(Number_Missing = sum(n)) %>%
    ungroup()
  
}

##### Turn into data ####
squid_list <- lapply(squid, parse_bitemap_squid) %>%
  bind_rows()


kelp_list <- lapply(kelp, parse_bitemap_kelp) %>%
  bind_rows()

write.csv(squid_list, "../clean_data/squid_bitemap_marsh_sampling.csv", row.names=FALSE)
write.csv(kelp_list, "../clean_data/kelp_bitemap_marsh_sampling.csv", row.names=FALSE)
