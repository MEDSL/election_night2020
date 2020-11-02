############################################################################################
########### Complete County Variable CleanR ##############################################
##########################################################################################
library(dplyr)
library(stringi)
library(stringr)
options(stringsAsFactors = FALSE)
#####################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
list.files()
county_acs_demos <- read.csv("county_acs_demos.csv")
econ_acs_cleaned <- readRDS("econ_acs_cleaned.rds")
###now read in the metro data 
metro_counties <- read.csv("raw/county_metro_coding.csv")
metro_counties$county14 <- str_pad(metro_counties$county14,side="left",pad="0",width=5)
sort(unique(metro_counties$cbsatype))
###### now combine all of these 
nrow(metro_counties)
nrow(county_acs_demos)
nrow(econ_acs_cleaned)
### the metro counties is shorter; will want to merge that on last 
#head(county_acs_demos)
county_acs_demos$Geo_FIPS <- str_pad(county_acs_demos$Geo_FIPS,width=5,side="left",pad="0")
econ_acs_cleaned <- subset(econ_acs_cleaned, select=-c(Geo_GEOID,Geo_NAME,Geo_QName,Geo_STUSAB))
#head(econ_acs_cleaned[1:5])
#merge on now 
complete_county_data <- merge(county_acs_demos, econ_acs_cleaned, by="Geo_FIPS")
nrow(complete_county_data)## no data lost 

### merge on the metro data 
complete_county_data <- merge(complete_county_data, metro_counties, by.x="Geo_FIPS", by.y="county14", all.x=T )
nrow(complete_county_data)
###let's check missing data 
sum(is.na(complete_county_data$cbsatype)) # 80 are missing. 

###Will go with the NCHA coding: 
complete_county_data$metro_type <- NA
complete_county_data$metro_type[complete_county_data$cbsatype=="Metro" & complete_county_data$total_pop>=1000000] <- "Large Metro"
complete_county_data$metro_type[complete_county_data$cbsatype=="Metro" & complete_county_data$total_pop<1000000 & 
                                  complete_county_data$total_pop >= 250000] <- "Medium Metro"
complete_county_data$metro_type[complete_county_data$cbsatype=="Metro" & complete_county_data$total_pop<250000] <- "Small Metro"
complete_county_data$metro_type[complete_county_data$cbsatype=="Micro"] <- "Micro"
complete_county_data$metro_type[is.na(complete_county_data$metro_type)==T] <- "Noncore"
table(complete_county_data$metro_type)
##appears good. Let's see if we can't read in some segregation data 
seg_matrix <- readRDS("raw/county_seg_mat.Rdata")
seg_matrix <- as.data.frame(seg_matrix)
names(seg_matrix)
colnames(seg_matrix)[1] <- "duncan_index"
seg_matrix$duncan_index <- as.numeric(seg_matrix$duncan_index)
seg_matrix$county_vec <- str_pad(seg_matrix$county_vec, width=5,side="left",pad="0")
###let's merge now 
complete_county_data <- merge(complete_county_data,seg_matrix, all.x=T, by.x="Geo_FIPS",by.y="county_vec" )
sum(is.na(complete_county_data$duncan_index)) # 80 counties missing. Let's check.
miss_seg_data <- subset(complete_county_data, is.na(duncan_index)==T)
###looking at these data, there is one observation from Alaska, one from South Dakota, and 78 from Puerto Rico. For today, this is fine. 
#While PR should be a state, for the purpose of analysis of the 2020 presidential election outcomes, it is not necessary. 
complete_county_data <- subset(complete_county_data, select=-c(cbsatype))
saveRDS(complete_county_data, "complete_county_data.rds")
write.csv(complete_county_data, "complete_county_data.csv", row.names = FALSE)


