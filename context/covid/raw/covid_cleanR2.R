##################################################
########### Data CleanR for COVID data  #####
######################################################
library(dplyr)
library(stringi)
library(stringr)
library(lubridate)
library(foreign)
library(tidyverse)
options(stringsAsFactors = FALSE)

#####################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
main_wd <- dirname(rstudioapi::getActiveDocumentContext()$path)

list.files()
###reading in the county data nd cleaning 
covid_counties <- read.csv("us-counties-nyt.csv")
covid_counties$fips <- str_pad(covid_counties$fips, width=5,side="left",pad="0")
covid_counties$state <- str_to_upper(covid_counties$state)
covid_counties$county <- str_to_upper(covid_counties$county)
covid_counties$st_fips <- substr(covid_counties$fips,1,2)
covid_counties <- covid_counties[order(covid_counties$fips,covid_counties$date), ]
covid_counties <- covid_counties %>% group_by(state,fips) %>% mutate(lag_cases=dplyr::lag(cases,default=0))
covid_counties <- covid_counties %>% group_by(state,fips) %>% mutate(lag_deaths=dplyr::lag(deaths,default=0))
covid_counties$new_cases <- covid_counties$cases - covid_counties$lag_cases
covid_counties$new_cases[covid_counties$new_cases<0] <- 0
covid_counties$new_deaths <- covid_counties$deaths - covid_counties$lag_deaths
covid_counties$new_deaths[covid_counties$new_deaths<0] <- 0
summary(covid_counties$new_cases)
covid_weeks <- covid_counties  %>% group_by(week = week(date), state,fips,county) %>% dplyr::summarise(cases = sum(new_cases,na.rm=T), 
                                                                                                       deaths=sum(new_deaths,na.rm=T))
####creating true time series df 
sort(unique(covid_weeks$week))
county_df <- sort(unique(covid_weeks$fips))
week_seq <- as.data.frame(seq(1,max(covid_weeks$week),by=1))
county_df2 <- merge(county_df,week_seq,by=NULL)
colnames(county_df2) <- c("fips","week")
dim(county_df2) # 141460 rows by 10/30
county_df2 <- merge(county_df2, covid_weeks, by=c("fips","week"), all.x=T)
dim(county_df2) # 141460 rows by 10/30

county_df2$cases[is.na(county_df2$cases)==T] <- 0
county_df2$deaths[is.na(county_df2$deaths)==T] <- 0
###need to fill here 
county_df2 <- county_df2 %>% group_by(fips) %>% fill(state, .direction = "up")
county_df2 <- county_df2 %>% group_by(fips) %>% fill(state)
county_df2 <- county_df2 %>% group_by(fips) %>% fill(county, .direction = "up")
county_df2 <- county_df2 %>% group_by(fips) %>% fill(county)

###Dropping territories 
county_df2 <- subset(county_df2, state!="NORTHERN MARIANA ISLANDS" & state!="VIRGIN ISLANDS" & state != "PUERTO RICO")
county_df2$st_fips <- substr(county_df2$fips,1,2)
dim(county_df2) # 137808 rows by 10/30

###########read in acs data 
state_codes <- read.csv("merge_on_statecodes.csv")
state_codes$state_fips <- str_pad(state_codes$state_fips,width=2,side="left",pad="0")
acs_data <- read.csv("county_acs_demos.csv")
acs_data$Geo_FIPS <- str_pad(acs_data$Geo_FIPS, width=5,pad="0",side = "left")
acs_data$state_fips <- substr(acs_data$Geo_FIPS,1,2)
acs_data <- merge(acs_data, state_codes, by="state_fips")
acs_data <- subset(acs_data, select=c(Geo_FIPS,total_pop,pop_ppsm,gini_index,white_pct,state_po))
###let's get pop info on the covid side 
county_df2 <- merge(county_df2,acs_data, by.x="fips",by.y="Geo_FIPS")
sum(is.na(county_df2$total_pop))#good, nothing missing 
county_df2$deaths_per_cap <- county_df2$deaths/county_df2$total_pop
county_df2$cases_per_cap <- county_df2$cases/county_df2$total_pop
county_df2$cases_per100k <- county_df2$cases_per_cap*100000 
county_df2$deaths_per100k <- county_df2$deaths_per_cap*100000 
###saving data 
saveRDS(county_df2, "covid_county_ts.rds")
write.csv(county_df2, "covid_county_ts.csv",row.names = FALSE)
min(county_df2$week)
names(county_df2)

###Let's get state data now
state_df2 <- county_df2 %>% group_by(state,week,st_fips,state_po) %>% 
  summarise(total_pop=sum(total_pop),cases=sum(cases),deaths=sum(deaths))
###Now let's get the vars coded 
state_df2$deaths_per_cap <- state_df2$deaths/state_df2$total_pop
state_df2$cases_per_cap <- state_df2$cases/state_df2$total_pop
state_df2$cases_per100k <- state_df2$cases_per_cap*100000 
state_df2$deaths_per100k <- state_df2$deaths_per_cap*100000 
saveRDS(state_df2, "covid_state_ts.rds")


###Let's create an index now 
state_index <- matrix(NA, ncol=9, nrow=4)
state_index[1,1] <- "Deaths per capita"
state_index[2,1] <- "Cases per capita"
state_index[3,1] <- "Deaths per 100k"
state_index[4,1] <- "Cases per 100k"
##var names 
state_index[1,2] <- colnames(state_df2)[8]
state_index[2,2] <- colnames(state_df2)[9]
state_index[3,2] <- colnames(state_df2)[10]
state_index[4,2] <- colnames(state_df2)[11]
###now let's get the jenks for each var 
library(BAMMtools)
deathpercap_jenks <- getJenksBreaks(state_df2$deaths_per_cap, 6)
casespercap_jenks <- getJenksBreaks(state_df2$cases_per_cap, 6)
deathper100k_jenks <- getJenksBreaks(state_df2$deaths_per100k, 6)
casesper100k_jenks <- getJenksBreaks(state_df2$cases_per100k, 6)
###now let's assign the vars 
for(i in 1:6){
  state_index[1,i+2] <- deathpercap_jenks[i]
}
for(i in 1:6){
  state_index[2,i+2] <- casespercap_jenks[i]
}
for(i in 1:6){
  state_index[3,i+2] <- deathper100k_jenks[i]
}
for(i in 1:6){
  state_index[4,i+2] <- casesper100k_jenks[i]
}
###now let's get the date 
state_index[,9]  <- as.character(Sys.Date())
state_index <- as.data.frame(state_index)
##column names 
colnames(state_index) <- c("title","var","jenks1","jenks2","jenks3","jenks4","jenks5","jenks6","date_updated")
###write it out 
write.csv(state_index, "state_covid_index.csv",row.names = FALSE)


####Now counties 


county_index <- matrix(NA, ncol=9, nrow=4)
county_index[1,1] <- "Deaths per capita"
county_index[2,1] <- "Cases per capita"
county_index[3,1] <- "Deaths per 100k"
county_index[4,1] <- "Cases per 100k"
##var names 
county_index[1,2] <- colnames(county_df2)[8]
county_index[2,2] <- colnames(county_df2)[9]
county_index[3,2] <- colnames(county_df2)[10]
county_index[4,2] <- colnames(county_df2)[11]
###now let's get the jenks for each var 
deathpercap_jenks <- getJenksBreaks(county_df2$deaths_per_cap, 6)
casespercap_jenks <- getJenksBreaks(county_df2$cases_per_cap, 6)
deathper100k_jenks <- getJenksBreaks(county_df2$deaths_per100k, 6)
casesper100k_jenks <- getJenksBreaks(county_df2$cases_per100k, 6)
###now let's assign the vars 
for(i in 1:6){
  county_index[1,i+2] <- deathpercap_jenks[i]
}
for(i in 1:6){
  county_index[2,i+2] <- casespercap_jenks[i]
}
for(i in 1:6){
  county_index[3,i+2] <- deathper100k_jenks[i]
}
for(i in 1:6){
  county_index[4,i+2] <- casesper100k_jenks[i]
}
###now let's get the date 
county_index[,9]  <- as.character(Sys.Date())
county_index <- as.data.frame(county_index)
##column names 
colnames(county_index) <- c("title","var","jenks1","jenks2","jenks3","jenks4","jenks5","jenks6","date_updated")
###write it out 
write.csv(county_index, "county_covid_index.csv",row.names = FALSE)


