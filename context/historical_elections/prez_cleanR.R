############################################################################################
########### POTUS Election Returns CleanR ##############################################
##########################################################################################
library(dplyr)
library(stringi)
library(foreign)
library(stringr)
library(tidyverse)
options(stringsAsFactors = FALSE)
#######################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
list.files()
county_prez_long <- read.csv("countypres_2000-2016long.csv")
names(county_prez_long)
sort(unique(county_prez_long$party))
county_prez_long$party2 <- county_prez_long$party
county_prez_long$party2[county_prez_long$party!="democrat" & county_prez_long$party!="republican"] <- "other"
county_prez_long$party2[is.na(county_prez_long$party2)==T] <- "other"

###now will reshape. 
county_prez_longb <- subset(county_prez_long, select=-c(candidate,party,totalvotes,version))
county_prez_longb <- county_prez_longb %>% group_by(party2,year,state,state_po,county,FIPS,office) %>%
  summarise(candidatevotes=sum(candidatevotes,na.rm=T))
county_prez_wide <- spread(county_prez_longb, party2, candidatevotes)
###string pad 
county_prez_wide$FIPS <- str_pad(county_prez_wide$FIPS, width=5, pad="0",side="left")
###now write out 
saveRDS(county_prez_wide, "county_prez_wide.rds")
write.csv(county_prez_wide, "county_prez_wide.csv", row.names = FALSE)
