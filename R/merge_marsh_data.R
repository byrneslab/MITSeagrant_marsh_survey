#################################################
#'
#' @Title SeaGrant marsh merge
#' 
#' 
#' 
#' 
#################################################
library(tidyverse)
library(readr)

setwd("~/Dropbox (Byrnes Lab)/Byrnes Lab Shared Folder/Marsh 2013 SeaGrant/Data/clean_data")

clean_dir <- "../clean_data/"

#site info
sites <- read_csv(paste0(clean_dir, "sites.csv"))

#species
birds <- read_csv(paste0(clean_dir, "birds_marsh_sampling.csv"))  %>%
  mutate(Site = gsub("MED", "M", Site), Protocol = "Birds")

deep_pitfall <- read_csv(paste0(clean_dir, "deep_pitfall_marsh_sampling.csv")) %>%
  mutate(Site = gsub("Med", "M", Site), Protocol = "Deep Pitfall")

insects <- read_csv(paste0(clean_dir, "insects_marsh_sample.csv")) %>%
  mutate(Protocol = "Vacuum Sampling")

quads <- read_csv(paste0(clean_dir, "quadrat_marsh_sample.csv"))  %>%
  rename(Species = Measurement) %>%
  mutate(Site = gsub("Med", "M", Site), Protocol = "Quadrat")

shallow_pitfall <- read_csv(paste0(clean_dir, "shallow_pitfall_marsh_sampling.csv")) %>%
  mutate(Protocol = "Shallow Pitfall")

traps <- read_csv(paste0(clean_dir, "traps_marsh_sampling.csv")) %>%
  mutate(Site = gsub("Med", "M", Site), Protocol = "Traps")

video<- read_csv(paste0(clean_dir, "video_marsh_sampling.csv"))  %>%
  filter(!grepl("N[e,E][t,T]", Site)) %>%
  filter(!grepl("D[1,2]", Site))  %>%
  mutate(Site = gsub("Medouie", "M", Site),
         Site = gsub("Glades", "GC", Site),
         Site = gsub("Nantucket", "N", Site),
         Site = gsub("PLUM", "PI", Site),
         Site = gsub("PP", "PI15", Site),
         Site = gsub("Waq", "W", Site),
         Protocol = "Video Sampling")

cores <- read_csv(paste0(clean_dir, "cores_marsh_sampling.csv")) %>% 
  mutate(Protocol = "Cores")

#functions
squid_bitemap <- read_csv(paste0(clean_dir, "squid_bitemap_marsh_sampling.csv")) 
kelp_bitemap <- read_csv(paste0(clean_dir, "kelp_bitemap_marsh_sampling.csv")) 

decomp <- read_csv(paste0(clean_dir, "decomp_marsh_sampling.csv")) %>%
  mutate(decomp_s = mass_lost_g/max(mass_lost_g, na.rm=T))

sediments <- read_csv(paste0(clean_dir, "sediment_marsh_sampling.csv")) %>%
  mutate(sediments_s = sediment_dry_mass_g/max(sediment_dry_mass_g, na.rm=T))

#### Merge Functions ####
squid_bitemap_transect <- squid_bitemap %>%
  group_by(Site, Transect) %>%
  summarize(Missing = sum(Number_Missing)) %>%
  ungroup() %>%
  mutate(squid_s = Missing/max(Missing))

kelp_bitemap_transect <- kelp_bitemap %>%
  group_by(Site, Transect) %>%
  summarize(Missing = sum(Number_Missing)) %>%
  ungroup() %>%
  mutate(kelp_s = Missing/max(Missing))

get_mf <- function(mf_vec){
 # mf_vec <- c(0.4, 0.5, 0.1, 0.2)
  mf_vec <- mf_vec[!is.na(mf_vec)]
  if(length(mf_vec)==0) return(NA)
  p_vec <- mf_vec/sum(mf_vec)
  h_vec <- sapply(p_vec, function(p) p*log(p))
  h <- -sum(h_vec)
  j <- h/log(length(mf_vec))
  
  j*sum(mf_vec)
}

all_func <- full_join(decomp, sediments) %>%
  select(-initial.mass, final.mass) %>%
  full_join(squid_bitemap_transect) %>%
  select(-Missing) %>%
  full_join(kelp_bitemap_transect) 

mf <- sapply(1:nrow(all_func), function(i){
  with(all_func, get_mf(c(decomp_s[i], sediments_s[i], squid_s[i], kelp_s[i])))
})

all_func$multifunc <- mf
cor(all_func[,c(6,8,9,11)], use="pairwise.complete.obs")

#### Merge Biological Data for Richness ####
all <- bind_rows(deep_pitfall, insects, quads, shallow_pitfall, traps, video, cores, birds) #something wrong with birds (it wouldnt bind rows w birds cuz the birds column was "Count" not "Avg_Count" I renamed it in birds for now)

all_sp <- all %>%
  filter(Abundance > 0 | Avg_Count >0 | Value > 0 | Count > 0) %>%
    left_join(sites) %>%
  select(Site, Transect, Species, Protocol, Latitude, Longitude)


#unique species codes (w LOCATION data)
all_sp %>%
  select(Species, Protocol) %>%
  group_by(Species) %>%
  slice(1L) %>%
  ungroup() %>%
  filter(!grepl("\\%", Species)) %>%
  write.csv(file="../clean_data/unique_sp.csv", row.names=FALSE)

rich_protocol <- all_sp %>%
  group_by(Site, Transect, Protocol, Latitude, Longitude) %>%
  summarize(Richness = n_distinct(Species))


rich_protocol_site <- all_sp %>%
  group_by(Site, Protocol, Latitude, Longitude) %>%
  summarize(Richness = n_distinct(Species))


rich_transect <- all_sp %>%
  group_by(Site, Transect, Latitude, Longitude) %>%
  summarize(Richness = n_distinct(Species))

rich_site <- all_sp %>%
  group_by(Site, Latitude, Longitude) %>%
  summarize(Richness = n_distinct(Species))


###Plotting ####
library(ggplot2)
library(ggmap)
qplot(Site, Richness, data=rich) + stat_summary( color = "red")
qplot(Site, Richness, data=rich_site) 
qplot(Site, Richness, data=rich_protocol) + stat_summary(color="red") + 
  facet_wrap(~Protocol, scale="free_y")

qplot(Site, Richness, data=rich_protocol_site)  + 
  facet_wrap(~Protocol, scale="free_y")

#### Maps ####
myMap <- get_map(location=c("Massachusetts"), source=c("google"), maptype="roadmap", crop=FALSE, zoom=8, color="bw") #this isnt working anymore???

base_map <- ggmap(myMap) +
  xlim(c(-71.5, -69.75)) +
  ylim(c(41.2,43)) 

base_map +
  geom_point(data=rich_site, mapping=aes(x=Longitude, y=Latitude, size=Richness, color=Richness, group=Site),
             position=position_dodge(width=0.1)) +
  scale_color_gradient(low = "blue", high="red") +
  theme_bw(base_size=12) +
  xlab("") +
  ylab("")

base_map +
  geom_point(data=rich_protocol_site, mapping=aes(x=Longitude, y=Latitude, size=Richness, color=Richness, group=Site),
             position=position_dodge(width=0.1)) +
  facet_wrap(~Protocol) +
  scale_color_gradient(low = "blue", high="red") +
  theme_bw(base_size=12) +
  xlab("") +
  ylab("")

#One protocol only
base_map +
  geom_point(data=rich_protocol_site %>%
               filter(Protocol == "Vacuum Sampling"), mapping=aes(x=Longitude, y=Latitude, size=Richness, color=Richness, group=Site),
             position=position_dodge(width=0.1)) +
  scale_color_gradient(low = "blue", high="red") +
  theme_bw(base_size=12) +
  xlab("") +
  ylab("")


base_map +
  geom_point(data=rich_protocol_site %>%
               filter(Protocol %in% c("Birds", "Traps", "Video Sampling"))
             , mapping=aes(x=Longitude, y=Latitude, size=Richness, color=Richness, group=Site),
             position=position_dodge(width=0.1)) +
  facet_wrap(~Protocol) +
  scale_color_gradient(low = "blue", high="red") +
  theme_bw(base_size=12) +
  xlab("") +
  ylab("")


#### merge with functions ####
#get rid of MF values measured with <4 functions
rich_func <- full_join(rich_transect, all_func) %>%
  filter(!is.na(final.mass))

rich_protocol_func <- left_join(rich_protocol, all_func)%>%
  filter(!is.na(final.mass))


rich_protocol_site_func <- left_join(rich_protocol_site, all_func)%>%
  filter(!is.na(final.mass))

#all on one
qplot(Richness, multifunc, data=rich_func, color=Site) +
  stat_smooth(method="lm", fill=NA) +
  theme_bw(base_size=17) +
  ylab("Multifunctionality\n (Effective Number of Functions)") +
  xlab("Species Richness")

#broken up by protocol
qplot(Richness, multifunc, data=rich_protocol_func, color=Site) +
  stat_smooth(method="lm", fill=NA) +
  facet_wrap(~Protocol, scale="free_x")+
  ylab("Multifunctionality\n (Effective Number of Functions)")+
  xlab("Species Richness")

#Birds only
qplot(Richness, multifunc, data=rich_protocol_site_func %>% filter(Protocol=="Birds"), color=Site) +
  stat_smooth(method="lm", fill=NA, mapping=aes(group=1)) +
  ylab("Multifunctionality\n (Effective Number of Functions)")+
  xlab("Species Richness") +
  theme_bw(base_size=17) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Birds only, by LAT
qplot(Latitude, Richness, data=rich_protocol_site_func %>% filter(Protocol=="Birds"), color=Site) +
  stat_smooth(method="lm", fill=NA, mapping=aes(group=1)) +
  ylab("Bird Richness")+
  xlab("Latitude") +
  theme_bw(base_size=17) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Traps only
qplot(Richness, multifunc, data=rich_protocol_site_func %>% filter(Protocol=="Traps"), color=Site) +
  stat_smooth(method="lm", fill=NA, mapping=aes(group=1)) +
  facet_wrap(~Protocol, scale="free_x")+
  ylab("Multifunctionality\n (Effective Number of Functions)")+
  xlab("Species Richness") +
  xlim(0,12) +
  theme_bw(base_size=17) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#Videos only
qplot(Richness, multifunc, data=rich_protocol_site_func %>% filter(Protocol=="Video Sampling"), color=Site) +
  stat_smooth(method="lm", fill=NA, mapping=aes(group=1)) +
  facet_wrap(~Protocol, scale="free_x")+
  ylab("Multifunctionality\n (Effective Number of Functions)")+
  xlab("Species Richness") +
  theme_bw(base_size=17) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#### Analysis ####
library(lme4)
library(lmerTest)
library(car)

#mod1 richness only
mod1 <- lmer(multifunc ~ Richness + (1+Richness | Site), data=rich_func)
Anova(mod1)


#mod2 richness by protocol
mod2 <- lmer(multifunc ~ Richness*Protocol + (1+Richness*Protocol | Site) , data=rich_protocol_func)
summary(mod2)

#mod3 site seperate ricness
mod3 <- lmer(multifunc ~ Richness*Protocol + (1| Site) , data=rich_protocol_func)
summary(mod3)
Anova(mod3)

library(rstanarm)
mc.cores = parallel::detectCores()

mod2_stan <- stan_lmer(multifunc ~ Richness*Protocol + (1+Richness*Protocol | Site) , data=rich_protocol_func)
summary(mod2_stan)





