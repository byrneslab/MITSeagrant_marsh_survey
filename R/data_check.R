library(dplyr)
library(readr)
library(stringr)
library(readxl)

unique_sp <- read_csv("../clean_data/unique_sp.csv") %>%
  rename(Code = Species)%>%
  mutate(upper_Code = toupper(Code))

codes <- read_excel("../COMPLETE DATA/spp and site codes.xlsx") %>%
  filter(Category !="site") %>%
  select(Code, `Actual name`) %>%
  rename(Species = `Actual name`)

insect_codes <- read_excel("../COMPLETE DATA/insects/Vacuum data_FINAL.xlsx", sheet=2) %>%
  select(Species) %>%
  mutate(Code = Species)

gopro_codes <- read_excel("../COMPLETE DATA/GoPro/gopro_from_googlesheets.xlsx", sheet=11)[,1:3] %>%
  rename(Code = `Spp Code`)

all_codes <- bind_rows(codes, insect_codes, gopro_codes) %>%
  mutate(upper_Code = toupper(Code))

#anti_join(all_codes, unique_sp)
bad_codes <- anti_join(unique_sp, all_codes, by="upper_Code") %>%
  group_by(Code) %>%
  slice(1L) %>%
  ungroup() %>%
  select(-upper_Code)

write.csv(bad_codes, "../clean_data/bad_codes.csv", row.names=FALSE)

#####
sp_code_correct <- function(species_code_vec){
  bad_codes <- ...
  good_codes <- ...
  str_replace_all(species_code_vec, bad_codes, good_codes)
}

####
insects <- read.csv("../clean_data/insects_marsh_sample.csv", stringsAsFactors = FALSE)
good_codes <- read.csv("../clean_data/bad_codes_fixed_20170828.csv", stringsAsFactors = FALSE) %>%
  filter(Protocol=="Vacuum Sampling")

insects_fix <- left_join(insects, good_codes, by=c("Species" = "Code")) %>%
  mutate(Species_Code = ifelse(is.na(Fix), Species, Fix)) %>%
  mutate(Species_Code = str_replace(Species_Code, "AMPH", "Amphpod")) %>%
  mutate(Species = Species_Code) %>%
  select(-Protocol, -Fix, -Remove, -Species_Code) %>%
  mutate(N_Sq_M = Count*N_Squares/36)

write.csv(insects_fix, "../codefixed_data/insects_clean_codefixed.csv", row.names=FALSE)
write.csv(data.frame(Species_Code = unique(insects_fix$Species)), 
                     "../codefixed_data/insect_sp_code.csv", row.names=FALSE)

