###################################################################################
####################### Election Night Results CleanR #################################
################################################################################
library(ggplot)
library(dplyr)
library(stringi)
library(stringr)
library(zoo)
library(lubridate)
library(readxl)
library(chron)
library(grid)
library(ggthemes)
library(ggalt)
library(zoo)
library(tidyverse)

options(stringsAsFactors = FALSE)
################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
caption_date <- paste0("Data source: New York Times Election Night reporting", 
                       "\nGraph Source: MIT Elections Data and Science Lab\nGraph date:",
                       sep=" ", format(Sys.Date(),format="%m/%d/%Y"))
list.files()
medsl_brands <- c("#3791FF","#59CBF5","#C0BA79","#F6573E","#156DD0","#C72654","#FF6878")

####This script will be used to create ggplot style figures for election night for counties. 


####Read in the NYT over time data 
list.files("results")
nyt_time <- read.csv("results/complete_nyt_by_time3.csv")

###check structure 
str(nyt_time) #
names(nyt_time)
###hours.from.close already present.  
#cutting out data where 0 percent reported 
nyt_time<- subset(nyt_time, total>0)
nyt_time<- subset(nyt_time, hours.from.close>0)

summary(nyt_time$hours.from.close)# 108 is max approx
###let's find out the two party vote pct, and then the prop of ballots arriving at a given time 
nyt_time <- nyt_time %>% group_by(state) %>% mutate(max_dem=max(dem,na.rm=T))
nyt_time <- nyt_time %>% group_by(state) %>% mutate(max_gop=max(rep,na.rm=T))
###getting proportions 
nyt_time$prop_dem <- (nyt_time$dem/nyt_time$max_dem)*100
nyt_time$prop_gop <- (nyt_time$rep/nyt_time$max_gop)*100

###Now lete's get dem pct 
nyt_time$dem_pct <- (nyt_time$dem/(nyt_time$dem+nyt_time$rep))*100

###let's get the prop chg vote 

###now let's find out the diff before and after election night. Let's get change in vote 
nyt_time <- nyt_time %>% group_by(state) %>% mutate(lag_dem=lag(dem,default=0))
nyt_time <- nyt_time %>% group_by(state) %>% mutate(lag_gop=lag(rep,default=0))
nyt_time$dem_change <- nyt_time$dem - nyt_time$lag_dem
nyt_time$gop_change <- nyt_time$rep - nyt_time$lag_gop
nyt_time$dem_diff_pct <- ((nyt_time$dem-nyt_time$rep)/(nyt_time$dem+nyt_time$rep))*100
nyt_time$dem_diff <- nyt_time$dem - nyt_time$rep



summary(nyt_time$dem_diff_pct)
##total diff 
nyt_time <- nyt_time %>% group_by(state) %>% mutate(lag_total=lag(total,default=0))
nyt_time$total_change <- nyt_time$total - nyt_time$lag_total
##getting final vote by state 
nyt_time <- nyt_time %>% group_by(state) %>% mutate(final_vote_total=max(total))
###lwt's retry the overturn calc 
nyt_time$dem_total_prop <- (nyt_time$dem/nyt_time$total)
nyt_time$prop_total <- nyt_time$total/nyt_time$final_vote_total
nyt_time$overturn_dem_count <- nyt_time$dem + 
  (0.5- nyt_time$prop_total*nyt_time$dem/nyt_time$total)*(nyt_time$outstanding_total)*(1-nyt_time$prop_total)^-1
nyt_time$overturn_dem_pct  <- (nyt_time$overturn_dem_count / nyt_time$outstanding_total)

###let's try something different to see if we can fix the calc 
nyt_time$abs2party_diff <- abs(nyt_time$dem-nyt_time$rep)
nyt_time$magnitude_diff_overturn <- nyt_time$abs2party_diff/nyt_time$outstanding_total

##let's create elec status dummy 
nyt_time$before_elec_vbm <- 1
# https://www.ncsl.org/research/elections-and-campaigns/absentee-and-early-voting.aspx
sort(unique(nyt_time$state))
nyt_time$before_elec_vbm[nyt_time$state=="alabama"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="south carolina"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="district-of-columbia"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="new-hampshire"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="idaho"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="connecticut"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="new-york"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="pennsylvania"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="west-virginia"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="kentucky"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="michigan"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="wisconsin"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="south-dakota"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="wyoming"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="massachusetts"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="maryland"] <- 0
nyt_time$before_elec_vbm[nyt_time$state=="mississippi"] <- 0



#nyt_time$win_majority <- (nyt_time$final_vote_total/2)+1
#nyt_time$overturn_dem_pct <- ((nyt_time$win_majority - nyt_time$dem)/(nyt_time$outstanding_total))*100
summary(nyt_time$overturn_dem_pct)###good, this gives us what we need 

###let's read in the winner data 
nyt_called <- read.csv("results/timestamp_results_nyt_call.csv")
names(nyt_called)
nyt_called <- subset(nyt_called, select=c(winnerCalledTimestamp,state,absentee_votes,poll_time,precincts_total,tot_exp_vote,
                                          leader_margin_votes,leader_party_id,margin2016,margin2012,clinton2016,trump2016))
nyt_called <- nyt_called %>% group_by(state) %>% slice(which.max(absentee_votes))
nyt_called$winner_called2 <- as.POSIXct(nyt_called$winnerCalledTimestamp, format=c("%m/%d/%Y %H:%M"))
nyt_called$poll_time2 <- as.POSIXct(nyt_called$poll_time, format=c("%m/%d/%Y %H:%M"))
#nyt_called$poll_time2[nyt_called$state=="north carolina"] <- as.POSIXct("2020-11-12 23:59:59")
###needed to change poll time for NC, given that they accepted ballots until the 12th 


###now let's get the time till call 
nyt_called$time2call <- (as.numeric(nyt_called$winner_called2) - as.numeric(nyt_called$poll_time2))/3600
###now that we have this, let's get the data based on when 95% of returns would have to be of one party 
nyt_time_95pct_overturn <- subset(nyt_time, magnitude_diff_overturn > 1)
# nyt_time_95pct_overturn <- subset(nyt_time, overturn_dem_pct >= 100 | overturn_dem_pct <= 0)

##now get min hours from close 
nyt_time_95pct_overturn <- nyt_time_95pct_overturn %>% group_by(state) %>% slice(which.min(hours.from.close))
nyt_time_95pct_overturn$state <- gsub("-", " ", nyt_time_95pct_overturn$state)
#View(nyt_time_95pct_overturn)
###ok, now we can get the data merged 
nyt_time_95pct_overturn <- merge(nyt_time_95pct_overturn, nyt_called, by="state")
nyt_time_95pct_overturn$call_difference <- nyt_time_95pct_overturn$hours.from.close - nyt_time_95pct_overturn$time2call

###let's now go with certification dates 
nyt_time_95pct_overturn$certification_date <- NA
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="alabama"] <- "2020-11-25 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="alaska"]<- "2020-11-25 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="arizona"]<- "2020-11-30 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="arkansas"]<- "2020-11-18 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="california"] <- "2020-12-11 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="colorado"] <- "2020-11-30 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="connecticut"] <- "2020-12-03 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="delware"] <- "2020-11-05 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="district of columbia"] <- "2020-12-02 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="florida"] <- "2020-11-17 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="georgia"] <- "2020-11-20 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="hawaii"] <- NA
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="idaho"] <- "2020-11-18 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="illinois"] <- "2020-12-04 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="indiana"] <- "2020-11-24 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="iowa"] <- "2020-11-30 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="kansas"] <- "2020-12-01 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="kentucky"] <- "2020-11-23 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="louisiana"] <- "2020-11-10 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="maine"] <- "2020-11-23 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="maryland"] <- "2020-12-08 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="massachusetts"] <- "2020-11-18 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="michigan"] <- "2020-11-23 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="minnesota"] <- "2020-11-25 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="mississippi"] <- "2020-11-13 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="missouri"] <- "2020-12-08 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="montana"] <- "2020-11-30 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="nebraska"] <- "2020-11-30 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="nevada"] <- "2020-11-24 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="new hampshire"] <- "2020-12-02 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="new jersey"] <- "2020-12-08 23:59" 
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="new mexico"] <- "2020-11-24 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="new york"] <- "2020-12-07 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="north carolina"] <- "2020-11-24 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="north dakota"] <- "2020-11-20 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="ohio"] <- "2020-11-24 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="oklahoma"] <- "2020-11-10 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="oregon"] <- "2020-12-03 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="pennsylvania"] <- "2020-11-23 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="south carolina"] <- "2020-11-11 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="south dakota"] <- "2020-11-10 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="tennessee"] <- NA
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="texas"] <- "2020-12-03 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="utah"] <- "2020-11-23 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="vermont"] <- "2020-11-10 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="virginia"] <- "2020-11-16 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="washington"] <- "2020-12-03 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="wisconsin"] <- "2020-12-01 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="west virginia"] <- "2020-12-03 23:59"
nyt_time_95pct_overturn$certification_date[nyt_time_95pct_overturn$state=="wyoming"] <- "2020-11-11 23:59"
# https://ballotpedia.org/Election_results_certification_dates,_2020

###Let's find difference in time to call 
library(e1071) 

summary(nyt_time_95pct_overturn$call_difference)

nyt_time_95pct_overturn %>% group_by(before_elec_vbm) %>% summarise(mean(time2call,na.rm=T), sd(time2call,na.rm=T),
                                                                    skewness(time2call,na.rm=T), kurtosis(time2call))
##for mathematical impossibility
nyt_time_95pct_overturn %>% group_by(before_elec_vbm) %>% summarise(mean(hours.from.close,na.rm=T), sd(hours.from.close,na.rm=T),
                                                                    skewness(hours.from.close,na.rm=T), kurtosis(hours.from.close))


###Let's find the difference in when called 



#when over 100, impossible for GOP to in. When under 0, impossible for Dems to win. Let's now subset
# to wide battleground 
###subsetting here 
##let's make the additions now 
wide_battleground <- subset(nyt_time_95pct_overturn, state=="georgia" | state=="nevada" | state=="florida" | state=="ohio" 
                            | state=="michigan" | 
                              state=="north carolina" | state=="wisconsin" | state=="pennsylvania" | state=="arizona" )
wide_battleground$dem_pct_poll <- wide_battleground$dem_pct
wide_battleground$dem_pct_poll[wide_battleground$state=="arizona"] <- 
  (wide_battleground$dem_pct_poll+3)[wide_battleground$state=="arizona"]

wide_battleground$dem_pct_poll[wide_battleground$state=="pennsylvania"] <- 
  (wide_battleground$dem_pct_poll+3)[wide_battleground$state=="pennsylvania"]

wide_battleground$dem_pct_poll[wide_battleground$state=="wisconsin"] <- 
  (wide_battleground$dem_pct_poll+7)[wide_battleground$state=="wisconsin"]

wide_battleground$dem_pct_poll[wide_battleground$state=="north carolina"] <- 
  (wide_battleground$dem_pct_poll+3)[wide_battleground$state=="north carolina"]

wide_battleground$dem_pct_poll[wide_battleground$state=="michigan"] <- 
  (wide_battleground$dem_pct_poll+5)[wide_battleground$state=="michigan"]

wide_battleground$dem_pct_poll[wide_battleground$state=="ohio"] <- 
  (wide_battleground$dem_pct_poll+3)[wide_battleground$state=="ohio"]

wide_battleground$dem_pct_poll[wide_battleground$state=="florida"] <- 
  (wide_battleground$dem_pct_poll+6)[wide_battleground$state=="florida"]

wide_battleground$dem_pct_poll[wide_battleground$state=="nevada"] <- 
  (wide_battleground$dem_pct_poll+2)[wide_battleground$state=="nevada"]

wide_battleground$dem_pct_poll[wide_battleground$state=="georgia"] <- 
  (wide_battleground$dem_pct_poll+1)[wide_battleground$state=="georgia"]
####now that we have these numbers, what we can do is find the new projected total at a given time 
long_battleground <- subset(nyt_time, state=="georgia" | state=="nevada" | state=="florida" | state=="ohio" 
                            | state=="michigan" | 
                              state=="north carolina" | state=="wisconsin" | state=="pennsylvania" | state=="arizona" )
long_battleground$dem_pct_poll <- long_battleground$dem_pct
long_battleground$dem_pct_poll[long_battleground$state=="arizona"] <- 
  (long_battleground$dem_pct_poll+3)[long_battleground$state=="arizona"]

long_battleground$dem_pct_poll[long_battleground$state=="pennsylvania"] <- 
  (long_battleground$dem_pct_poll+3)[long_battleground$state=="pennsylvania"]

long_battleground$dem_pct_poll[long_battleground$state=="wisconsin"] <- 
  (long_battleground$dem_pct_poll+7)[long_battleground$state=="wisconsin"]

long_battleground$dem_pct_poll[long_battleground$state=="north carolina"] <- 
  (long_battleground$dem_pct_poll+3)[long_battleground$state=="north carolina"]

long_battleground$dem_pct_poll[long_battleground$state=="michigan"] <- 
  (long_battleground$dem_pct_poll+5)[long_battleground$state=="michigan"]

long_battleground$dem_pct_poll[long_battleground$state=="ohio"] <- 
  (long_battleground$dem_pct_poll+3)[long_battleground$state=="ohio"]

long_battleground$dem_pct_poll[long_battleground$state=="florida"] <- 
  (long_battleground$dem_pct_poll+6)[long_battleground$state=="florida"]

long_battleground$dem_pct_poll[long_battleground$state=="nevada"] <- 
  (long_battleground$dem_pct_poll+2)[long_battleground$state=="nevada"]

long_battleground$dem_pct_poll[long_battleground$state=="georgia"] <- 
  (long_battleground$dem_pct_poll+1)[long_battleground$state=="georgia"]
###Let's add corrected magnitude var 
long_battleground <- as.data.frame(long_battleground)
long_battleground$abs2party_diff_polls <- abs((( long_battleground$dem_pct_poll/100)*long_battleground$total) - 
                                                (1 - (long_battleground$dem_pct_poll/100) ) )
long_battleground$magnitude_diff_overturn_polls <- (long_battleground$abs2party_diff_polls/long_battleground$outstanding_total)
summary(long_battleground$magnitude_diff_overturn_polls)


long_battleground <- subset(long_battleground, magnitude_diff_overturn_polls > 1)
# nyt_time_95pct_overturn <- subset(nyt_time, overturn_dem_pct >= 100 | overturn_dem_pct <= 0)

##now get min hours from close 
long_battleground <- long_battleground %>% group_by(state) %>% slice(which.min(hours.from.close))
long_battleground$state <- gsub("-", " ", long_battleground$state)
###Let's subset the fields 
long_battleground <- subset(long_battleground, select=c(state,magnitude_diff_overturn_polls,hours.from.close))
colnames(long_battleground)[3] <- "hours_from_close_pright"
wide_battleground2 <- merge(wide_battleground,long_battleground,by="state")
wide_battleground2$pright_time_diff <- wide_battleground2$hours.from.close - wide_battleground2$hours_from_close_pright 
View(wide_battleground2)
wide_battleground2[,c(1,13,56,57)]


###now we will want to subset 
wide_battleground_finalcall <- subset(wide_battleground,overturn_dem_pct < 15 | overturn_dem_pct > 85 )
wide_battleground_finalcall <- wide_battleground_finalcall %>% group_by(state) %>% slice(which.min(hours.from.close))
View(wide_battleground_finalcall)
wide_battleground_finalcall_projected <- subset(wide_battleground,overturn_dem_pct_polls < 15 | overturn_dem_pct_polls > 85)
wide_battleground_finalcall_projected <- wide_battleground_finalcall_projected %>% group_by(state) %>% slice(which.min(hours.from.close))
View(wide_battleground_finalcall_projected)


###Let's subset corrections here 
nyt_time_corrections <- subset(nyt_time, total_change < 0)
View(nyt_time_corrections)
summary(nyt_time_corrections$dem_change)
summary(nyt_time_corrections$gop_change)
##
nyt_time_corrections$dem_favor_correction <- 1
nyt_time_corrections$dem_favor_correction[nyt_time_corrections$dem_change < nyt_time_corrections$gop_change] <- 0
summary(nyt_time_corrections$dem_favor_correction)
table(nyt_time_corrections$state)
###Let's now create the long data 
# select=c(state,rep,dem,time_counter,prop_dem,prop_gop,dem_pct)
dem_time <- subset(nyt_time, select=c(time, state,dem,prop_dem,dem_pct,dem_change,dem_diff_pct,dem_diff,hours.from.close,total_change))
gop_time <- subset(nyt_time, select=c(time, state,rep,prop_gop,dem_pct,gop_change,dem_diff_pct,dem_diff,hours.from.close,total_change))



###renaming 
colnames(dem_time) <- c("time", "state","votes","prop_vote","dem_pct","vote_change", "dem_diff_pct","dem_diff","time_counter","total_change")
colnames(gop_time) <- c("time", "state","votes","prop_vote","dem_pct","vote_change", "dem_diff_pct","dem_diff","time_counter","total_change")
###party assignment
dem_time$party <- "DEM"
gop_time$party <- 'GOP'
###binding data
nyt_time2 <- rbind(dem_time,gop_time)
nyt_time2 <- nyt_time2 %>% group_by(state,party) %>% mutate(final_vote=max(votes,na.rm=T))
nyt_time2$prop_vote_point <- (nyt_time2$vote_change/nyt_time2$final_vote)*100
names(nyt_time2)
###let's save 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("results")
saveRDS(nyt_time2, "nyt_enr_long3.rds")
nyt_time2 <- readRDS("nyt_enr_long3.rds")






View(nyt_time2)
###appear to be some corrections; let's cut these out, but make note of them 
nyt_time2neg_chg <- subset(nyt_time2, vote_change < 0)
nyt_time2 <- subset(nyt_time2, vote_change >= 0)

####Now we want to create plots by state 
state_vec <- sort(unique(nyt_time$state))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("results/plots")
for(i in 1:length(unique(nyt_time$state))){
  temp_sub <- subset(nyt_time2, state==state_vec[i])
  temp_plot <- ggplot(temp_sub, aes(x=time_counter,y=prop_vote, color=party)) +
    geom_line(size=1.2, alpha=0.8) +
    geom_point(aes(size=vote_change),alpha=0.6) + 
     scale_color_manual(values = medsl_brands[c(1,6)],drop=F) + theme_minimal() + guides(size=FALSE) +
    labs(title=paste0(str_to_title(state_vec[i]),sep=" ", "proportion of \nparty's votes over time"),x="Hours from polls closing",
         y="Proportion of total votes", color="Party",
         caption = caption_date) + 
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
    theme(plot.caption = element_text(hjust=0),
                                                  title = element_text(size = rel(1.2), family="Styrene B")) 
  
  ###non cumulative plot 
  temp_plot_noncum <- ggplot(temp_sub, aes(x=time_counter,y=prop_vote_point, color=party)) +
    geom_line(size=1.2, alpha=0.8) +
    geom_point(aes(size=vote_change),alpha=0.6) + 
    scale_color_manual(values = medsl_brands[c(1,6)],drop=F) + theme_minimal() + guides(size=FALSE) +
    labs(title=paste0(str_to_title(state_vec[i]),sep=" ", "proportion of \nparty's votes over time"),x="Hours from polls closing",
         y="Proportion of total votes", color="Party",
         caption = caption_date) + 
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
    theme(plot.caption = element_text(hjust=0),
          title = element_text(size = rel(1.2), family="Styrene B")) 
  #temp_plot_noncum
  
  ###now Let's do pct of vote 
  # scale_x_continuous(limits = c(0,130)) 
  dem_sub <- subset(temp_sub, party=="DEM")
  dem_sub$dem_pct[is.na(dem_sub$dem_pct)==T] <- 0
  temp_plot_pct <- ggplot(dem_sub, aes(x=time_counter,y=dem_pct)) +
    geom_line(size=1.2, alpha=0.8, color=medsl_brands[c(1)]) +
    geom_point(aes(size=vote_change),alpha=0.6,  color=medsl_brands[c(1)])  + theme_minimal() + guides(size=FALSE) +
    labs(title=paste0(str_to_title(state_vec[i]),sep=" ", "Democratic percent of \ntwo party vote over time"),x="Hours from polls closing",
         y="Democratic % of vote",
         caption = caption_date) + 
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
    theme(plot.caption = element_text(hjust=0),
                                                  title = element_text(size = rel(1.2), family="Styrene B")) 
  ###Let's create a plot of when the state parties get most of their votes 

####now save the data 
  ggsave(paste0(state_vec[i],sep="","enr_prop",sep="", ".png"), plot = temp_plot, scale = 1,
         width = 9, height = 6, units = c("in"), dpi = 600) 
  ggsave(paste0(state_vec[i],sep="","enr_noncum_prop",sep="", ".png"), plot = temp_plot_noncum, scale = 1,
         width = 9, height = 6, units = c("in"), dpi = 600) 
  ggsave(paste0(state_vec[i],sep="","dem_pct",sep="", ".png"), plot = temp_plot_pct, scale = 1,
         width = 9, height = 6, units = c("in"), dpi = 600) 
  

}
###Let's get the total 


###Let's get around 6 hours after polls closed 
nyt_time2start <- subset(nyt_time2, time_counter >=6)
nyt_time2start <- nyt_time2start %>%
  group_by(state,party) %>%
  slice(which.min(time_counter))
sort(unique(nyt_time2start$state))
summary(nyt_time2start$time_counter)
#now let's get the last 
nyt_time2end <- nyt_time2 %>% group_by(state,party) %>%
  slice(which.max(prop_vote))
####now let's condense to three columns 
nyt_time2end <- subset(nyt_time2end, select=c(state,party,time_counter,dem_pct))
colnames(nyt_time2end)[3:4] <- c("time_end","dem_pct_end")
###merge 
wide_nyt_data <- merge(nyt_time2start, nyt_time2end, by=c("state","party"))
###now let's get time diff 
wide_nyt_data$time_diff <- wide_nyt_data$time_end - wide_nyt_data$time_counter
wide_nyt_data <- subset(wide_nyt_data, party=="DEM")
wide_nyt_data$dem_pct_diff <- wide_nyt_data$dem_pct_end - wide_nyt_data$dem_pct

###Reading in the state codes data 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
state_codes <- read.csv("context/merge_on_statecodes.csv")
head(state_codes)
state_codes$state <- str_to_lower(state_codes$state)
#now we want to remove hyphens 
wide_nyt_data$state <- gsub("-", " ", wide_nyt_data$state)
sort(unique(wide_nyt_data$state))
nrow(wide_nyt_data)
wide_nyt_data <- merge(wide_nyt_data,state_codes, by="state")
nrow(wide_nyt_data)

####let's create a dumbell plot
#setting up annotation
grob_start <- grobTree(textGrob("Starting \ntally", x=0.85,  y=0.9, hjust=0,
                                gp=gpar(col=medsl_brands[6], fontsize=15, fontface="bold")))
grob_end <- grobTree(textGrob("End \ntally", x=0.85,  y=0.75, hjust=0,
                                gp=gpar(col=medsl_brands[1], fontsize=15, fontface="bold")))
###dumbell plot
#for color , I should be able to change to blue or red depending on var 
wide_nyt_data$dem_shift <- 0
wide_nyt_data$dem_shift[wide_nyt_data$dem_pct_diff>0] <- 1
dumbell_plot_dempct <- ggplot() +
  geom_dumbbell(data = wide_nyt_data, aes(y= reorder(state_po,dem_pct_diff), x=dem_pct, 
                                    xend=dem_pct_end, color=as.factor(dem_shift)),
                size=1.9, colour_x=medsl_brands[6], colour_xend=medsl_brands[1], size_x = 3.5, size_xend = 3.5) +
  theme_minimal() +  labs(x = "Democratic %",
                          y = "",
                          title =  "Change in Democratic vote share of final tally", caption = caption_date) + 
  annotation_custom(grob_start) + annotation_custom(grob_end) + theme(title = element_text(size = rel(1.4), family="Styrene B"),
                                                                      plot.caption = element_text(hjust=0)) +
  geom_vline(xintercept = 50, linetype="dashed", 
             color = "#37C256", size=0.5) + scale_color_manual(values = medsl_brands[c(6,1)],drop=F) + guides(color=FALSE)

dumbell_plot_dempct

# Create a text
setwd("results/plots")

ggsave("blueshift_dumbell.png", plot = dumbell_plot_dempct, scale = 1, 
       width = 8, height = 12, units = c("in"), dpi = 400)

####Let's do the nyt counties here ; read directly from dropbox
nyt_counties <- read.csv("results/nyt_counties.csv")
# https://www.dropbox.com/home/ElectionNightResults/newspapers/counties
head(nyt_counties)
nyt_counties$fips <- str_pad(nyt_counties$fips, width=5,pad="0",side="left")
nyt_counties$name <- str_to_upper(nyt_counties$name)
nyt_counties <- nyt_counties[,-c(10:15,17:23)]
names(nyt_counties)
nyt_counties$state <- str_to_upper(nyt_counties$state)

###Let's now get the velocity, which will need change in time. Or grouping of times into 15 min intervals 
nyt_counties$time_num <- as.numeric(as.POSIXct(nyt_counties$time))
head(nyt_counties$time_num)
###good; now let's normalize, finding earliest time by state 
nyt_counties$time <- as.POSIXct(nyt_counties$time)
nyt_counties <- nyt_counties %>% group_by(state) %>% mutate(first_time=min(time_num,na.rm=T))
nyt_counties$time_count <- nyt_counties$time_num - nyt_counties$first_time
nyt_counties <- nyt_counties %>% group_by(fips) %>% mutate(lag_timecount = lag(time_count))
nyt_counties$time_count_chg <- (nyt_counties$time_count-nyt_counties$lag_timecount)/3600
###now let's get the vote chg 
nyt_counties <- nyt_counties %>% group_by(fips) %>% mutate(lage_vote_total=lag(votes))
nyt_counties$vote_chg <- nyt_counties$votes - nyt_counties$votes
###now let's find vote velocity 
nyt_counties$vote_velocity <- (nyt_counties$vote_chg/1000)/nyt_counties$time_count_chg

summary(nyt_counties$vote_velocity)

library(zoo)
library(tidyr)
###We will want to get the loop of states in order to run the analysis that we need 
state_vec <- sort(unique(nyt_counties$state))
nyt_counties2 <- data.frame(stringsAsFactors = FALSE)
for(i in 1:length(state_vec)){
  print(state_vec[i])
  temp_nyt <- subset(nyt_counties, state==state_vec[i])
  temp_nyt$interval15min <- cut(temp_nyt$time, breaks="15 min")
  temp_nyt$interval15min_num <- as.numeric(temp_nyt$interval15min)
  ###we will then want to collapse data, then merge onto time series style df 
  temp_nyt <- temp_nyt %>% group_by(fips,name,state,interval15min,interval15min_num) %>% 
    summarise(tot_exp_vote=max(tot_exp_vote), total.votes=max(votes,na.rm=T),trumpd=max(trumpd,na.rm=T),bidenj=max(bidenj,na.rm=T),
    abs_trumpd=max(abs_trumpd,na.rm=T),abs_bidenj=max(abs_bidenj,na.rm=T))
  ###create time seq
  min_seq <- as.data.frame(seq(min(temp_nyt$interval15min_num),max(temp_nyt$interval15min_num),by=1))
  county_seq <- as.data.frame(sort(unique(temp_nyt$fips)))
  temp_nyt2 <- merge(county_seq,min_seq,by=NULL)
  colnames(temp_nyt2) <- c("fips","interval15min_num")
  temp_nyt2 <- merge(temp_nyt2, temp_nyt, by=c("fips","interval15min_num"),all.x=T)
  temp_nyt2 <- temp_nyt2 %>% group_by(fips) %>% 
    fill(name,state,tot_exp_vote, total.votes,trumpd,bidenj,abs_trumpd,abs_bidenj) %>% 
    fill(name,state,tot_exp_vote, total.votes,trumpd,bidenj,abs_trumpd,abs_bidenj, .direction="down")
  if(nrow(nyt_counties2)==0){
    nyt_counties2 <- temp_nyt2
  }else{
    nyt_counties2 <- rbind(nyt_counties2,temp_nyt2)
  }
                                                               
  
}
nyt_counties2 <- nyt_counties2 %>% group_by(fips) %>% mutate(lag_total_votes=lag(total.votes, default=0),
                                                             lag_bidenj=lag(bidenj,default=0),
                                                             lag_trumpd=lag(trumpd,default=0))
nyt_counties2$total_vote_chg <- nyt_counties2$total.votes-nyt_counties2$lag_total_votes
nyt_counties2$biden_chg <- nyt_counties2$bidenj - nyt_counties2$lag_bidenj
nyt_counties2$trump_chg <- nyt_counties2$trumpd - nyt_counties2$lag_trumpd
##proportion of expected returns 
nyt_counties2$prop_expect_returned <- (nyt_counties2$total.votes/nyt_counties2$tot_exp_vote)*100


summary(nyt_counties2$trump_chg)
length(which(nyt_counties2$trump_chg<0)) # 338
length(which(nyt_counties2$biden_chg<0)) # 331

###Let's get acceleration and dem pct vars 
nyt_counties2$dem_pct <- (nyt_counties2$bidenj/nyt_counties2$total.votes)*100
nyt_counties2$dem2party_pct <- (nyt_counties2$bidenj/(nyt_counties2$bidenj+nyt_counties2$trumpd))*100
nyt_counties2 <- nyt_counties2 %>% group_by(fips) %>% mutate(final_vote=max(total.votes,na.rm=T))
nyt_counties2$returned_pct <- (nyt_counties2$total.votes/nyt_counties2$final_vote)*100

###acceleration 
nyt_counties2 <- nyt_counties2 %>% group_by(fips) %>% mutate(lag_vote_chg = lag(total_vote_chg))
nyt_counties2$tabulate_acceleration <- ((nyt_counties2$total_vote_chg - nyt_counties2$lag_vote_chg)/1000)*4 # this gets us the change in 
nyt_counties2$tabulate_acceleration_prop <- ((nyt_counties2$total_vote_chg - nyt_counties2$lag_vote_chg))/nyt_counties2$final_vote
# this gets us the change in 
nyt_counties2 <- as.data.frame(nyt_counties2)
###Let's try the gini coefficients 
library(DescTools)
nyt_counties2 <- nyt_counties2 %>% group_by(state,interval15min_num) %>% mutate(gini_votes=Gini(returned_pct))
nyt_counties2$gini_votes[is.na(nyt_counties2$gini_votes)==T] <- 0
summary(nyt_counties2$gini_votes) #seems to be working; let's check the non-zero entries, which are complete equality. 
test_sub <- subset(nyt_counties2, state=="PENNSYLVANIA" & gini_votes > 0 & is.na(gini_votes)==FALSE)
test_sub2 <- subset(test_sub, interval15min_num==50)
###analyzing these, as expected the gini coefficient is the same for all states for a given time period. This allows us to determine the 
#proportion of votes. This means that we could get the collapsed data by time 
state_nyt_gini <- nyt_counties2 %>% 
  group_by(state,interval15min_num) %>% 
  summarise(gini_reported=mean(gini_votes,na.rm=T),total.votes=sum(total.votes,na.rm=T), final_vote=sum(final_vote,na.rm=T),
            bidenj=sum(bidenj,na.rm=T), trumpd=sum(trumpd,na.rm=T))
state_nyt_gini$prop_reported <- (state_nyt_gini$total.votes/state_nyt_gini$final_vote)*100
state_nyt_gini$dem_pct <- (state_nyt_gini$bidenj/state_nyt_gini$total.votes)*100
state_nyt_gini$dem2party_pct <- (state_nyt_gini$bidenj/(state_nyt_gini$trumpd+state_nyt_gini$bidenj))*100
state_nyt_gini$dem_majority <- 0
state_nyt_gini$dem_majority[state_nyt_gini$dem2party_pct > 50 ] <- 1
state_nyt_gini$lead <- "Trump"
state_nyt_gini$lead[state_nyt_gini$dem_majority==1] <- "Biden"
#### Let's save data here 
write.csv(state_nyt_gini, 'results/state_nyt_gini.csv', row.names = FALSE)
saveRDS(state_nyt_gini, 'results/state_nyt_gini.rds')

### Let's get the time series plots by 
#note: will need to run theme minimal first in order to prevent the alignment from the other theme cmd from being overwritten 
##note: adding in alpha within the geom_line section gets rid of teh legend. Can we fix this by placing it in the aes ggplot section? ?
###Seems like the best path forward is to overlay the text with the geom
grob_biden <- grobTree(textGrob("Biden", x=0.8,  y=0.7, hjust=0,
                                gp=gpar(col=medsl_brands[1], fontsize=12, fontface="bold")))
grob_trump <- grobTree(textGrob("Trump", x=0.8,  y=0.6, hjust=0,
                                gp=gpar(col=medsl_brands[6], fontsize=12, fontface="bold")))
grob_title <- grobTree(textGrob("Lead in results", x=0.7,  y=0.8, hjust=0,
                                gp=gpar(col="black", fontsize=14, fontface="bold")))
gini_state_plot <- ggplot(data=state_nyt_gini, aes(x=interval15min_num,y=gini_reported,group=state, colour=lead)) +
  geom_line(lwd=1.05,alpha=0.3) +
  theme_minimal() + 
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  scale_color_manual(values = medsl_brands[c(1,6)]) + 
  guides(colour = FALSE) +
  scale_x_continuous(breaks = seq(0,576,by=96), 
                     labels=c("Nov-4", "Nov-5", "Nov-6", "Nov-7", "Nov-8", "Nov-9", "Nov-10"), limits = c(0,576)) + ylim(0.05,1)+
  labs(title="Inequality in county reporting over time, by state",
       x="Time from polls closing",y="Inequality in reports (gini coef.)",color="Lead",
       caption = caption_date)  + annotation_custom(grob_biden) + annotation_custom(grob_trump) + annotation_custom(grob_title)
  
gini_state_plot 
ggsave("results/plots/state_gini_ts_plot_party.png", plot = gini_state_plot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600) 

###Let's get means by for period between nov 4 and 5 
nov4to5data <- subset(state_nyt_gini, interval15min_num >= 96 & interval15min_num  <= 192)
nov4to5data_sum <- nov4to5data %>% group_by(state) %>% summarise(mean(gini_reported))
###



summary(nov4to5data_sum$`mean(gini_reported)`)
View(nov4to5data_sum)

#1000 votes per hour tabulated by county 

###now let's subset to a smaller window of time 
summary(nyt_counties2$interval15min_num)
nyt_counties2$time_num <- as.numeric(as.POSIXct(as.character(nyt_counties2$interval15min)))
nyt_counties2 <- nyt_counties2 %>% group_by(fips) %>% fill(time_num, .direction="down") 
nyt_counties2$time_num[is.na(nyt_counties2$interval15min)==T] <- 
  (nyt_counties2$time_num+(nyt_counties2$interval15min_num*900))[is.na(nyt_counties2$interval15min)==T]
nyt_counties2$chr_time <- as.POSIXlt(nyt_counties2$time_num, origin = "1970-01-01")
nyt_counties2$chr_time2 <- format(strptime(as.character(nyt_counties2$chr_time), "%Y-%m-%d %H:%M:%S"),"%b-%d %H:%M" )
###Let's get final lead var 
nyt_counties2$dem_majority <- 0
nyt_counties2$dem_majority[nyt_counties2$dem2party_pct>50] <- 1 
nyt_counties2$lead <- "Trump"
nyt_counties2$lead[nyt_counties2$dem_majority==1] <- "Biden"
nyt_counties2 <- nyt_counties2 %>% group_by(fips) %>% mutate(last_lead=last(lead))
#View(nyt_counties2)

###Let's save the data 

saveRDS(nyt_counties2,"results/nyt15minute_county_data.rds")
write.csv(nyt_counties2,"results/nyt15minute_county_data.csv",row.names=FALSE)

##reading in 
nyt_counties2 <- readRDS("results/nyt15minute_county_data.rds")
#test_Date <- "2020-11-03 6:56:00"
#format(strptime(as.character(test_Date), "%Y-%m-%d %H:%M"),"%b-%d %H:%M" )





###Let's get final results 
nyt_counties_final <- nyt_counties2 %>% group_by(fips) %>% slice(which.max(interval15min_num))
nrow(nyt_counties_final)
nyt_counties_final <- as.data.frame(nyt_counties_final)
View(nyt_counties_final)

class(nyt_counties_final)
###Let's get the data exported 
write.csv(nyt_counties_final, "nyt_counties_final12042020.csv",row.names = FALSE)


#### we will now want to move onto figures, in line with what we talked about
grob_biden2 <- grobTree(textGrob("Biden", x=0.8,  y=0.4, hjust=0,
                                gp=gpar(col=medsl_brands[1], fontsize=12, fontface="bold")))
grob_trump2 <- grobTree(textGrob("Trump", x=0.8,  y=0.3, hjust=0,
                                gp=gpar(col=medsl_brands[6], fontsize=12, fontface="bold")))
grob_title2 <- grobTree(textGrob("Lead in results", x=0.7,  y=0.5, hjust=0,
                                gp=gpar(col="black", fontsize=14, fontface="bold")))
setwd("results/plots/county_tabulate")

state_vec <- sort(unique(nyt_counties2$state))
library(scales)
nyt_counties2$posix_time <- as.POSIXct.POSIXlt(nyt_counties2$chr_time)
str(nyt_counties2$posix_time)
#setwd("F:/MEDSL/election_night2020/results/plots/county_acceleration")
for(i in 1:length(state_vec)){
  svMisc::progress((i/51)*100)
  temp_state <- subset(nyt_counties2, state==state_vec[i])
  temp_state <- as.data.frame(temp_state)
  temp_state <- subset(temp_state, interval15min_num >= 48)
  #let's just go with manual 
  temp_ts_plot <- ggplot(temp_state, aes(x= interval15min_num, y=returned_pct, group=fips, col=last_lead)) +
    geom_line(alpha=0.4,lwd=1.2) + theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)]) +
    guides(colour = FALSE) +
    theme(title = element_text(size = rel(1.2), family="Styrene B"), 
          plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45,vjust=0.5)) +
    labs(title=paste(str_to_title(state_vec[i]),sep=" ", "time to tabulate ballots, by county"),
         x="Time from polls closing",y="% tabulated",
         caption = caption_date)  + ylim(0,100) + annotation_custom(grob_title2) + annotation_custom(grob_biden2) +
    annotation_custom(grob_trump2)  +
    scale_x_continuous(breaks = seq(48,624,by=96), 
                       labels=c("Nov-3 \n 6 PM", "Nov-4 \n 6 PM", "Nov-5 \n 6 PM", "Nov-6 \n 6 PM",
                                "Nov-7 \n 6 PM", "Nov-8 \n 6 PM", "Nov-9 \n 6 PM"), 
                       limits = c(48,624)) 
  temp_ts_plot

  ggsave(paste0(state_vec[i],sep="_", "tabulate_plot",sep="", ".png"), plot = temp_ts_plot, scale = 1,
         width = 9, height = 6, units = c("in"), dpi = 600) 
  
  
  
  ###acceleration plot 
 grob_biden3 <- grobTree(textGrob("Biden", x=0.8,  y=0.7, hjust=0,
                                   gp=gpar(col=medsl_brands[1], fontsize=12, fontface="bold")))
  grob_trump3 <- grobTree(textGrob("Trump", x=0.8,  y=0.6, hjust=0,
                                   gp=gpar(col=medsl_brands[6], fontsize=12, fontface="bold")))
 grob_title3 <- grobTree(textGrob("Lead in results", x=0.7,  y=0.8, hjust=0,
                                   gp=gpar(col="black", fontsize=14, fontface="bold")))
  
  temp_ts_accel_plot <- ggplot(temp_state, aes(x=interval15min_num, y=tabulate_acceleration, group=fips,col=last_lead)) +
    geom_line(alpha=0.4, lwd=1.2) + theme_minimal()  + scale_color_manual(values = medsl_brands[c(1,6)]) + 
    theme(title = element_text(size = rel(1.2), family="Styrene B"), 
          plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45,vjust=0.5)) + guides(color=FALSE) +
    annotation_custom(grob_title3) + annotation_custom(grob_biden3) + annotation_custom(grob_trump3) + 
    labs(title=paste(str_to_title(state_vec[i]),sep=" ", "acceleration in ballots counted, by county"),
         x="Time from polls closing",y="1000 ballots counted per hour",
         caption = caption_date)  +
    scale_x_continuous(breaks = seq(48,624,by=96), 
                       labels=c("Nov-3 \n 6 PM", "Nov-4 \n 6 PM", "Nov-5 \n 6 PM", "Nov-6 \n 6 PM",
                                "Nov-7 \n 6 PM", "Nov-8 \n 6 PM", "Nov-9 \n 6 PM"), 
                       limits = c(48,624))  +
    coord_cartesian(ylim = c(21, 400), xlim = c(0,576))
 temp_ts_accel_plot
  ggsave(paste0(state_vec[i],sep="_", "acceleration_plot",sep="", ".png"), plot = temp_ts_accel_plot, scale = 1,
         width = 9, height = 6, units = c("in"), dpi = 600) 
  
}

###Let's create the 6 states of interest: AZ, MI, PA, WI , NC, FL . Let's do a 4 by 4 and 






###Let's now add in the wbm_merge_counties data
#nyt_counties <- read.csv("wbm_merge_counties.csv")
View(nyt_counties)



###how much Biden was ahead: 
#PA: Biden by 1.2 points
# Ave across battleground states: 2.7 points
#https://www.cnbc.com/2020/11/02/election-2020-biden-leads-trump-in-polls-as-swing-states-tighten.html
# 538 article on poll averages: https://fivethirtyeight.com/features/the-polls-werent-great-but-thats-pretty-normal/



###look more closely at PA 
state_vecbg <- sort(unique(battleground_nyt$state))
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("results/plots")
for(i in 1:length(unique(battleground_nyt$state))){
  temp_sub <- subset(battleground_nyt, state==state_vecbg[i])
  ###Let's make long now 
  temp_sub <- subset(temp_sub, select=c(time_counter,vote_change,dem_pct_poll,dem_pct))
  temp_sub <- temp_sub %>% gather("calculation","dem_pct",dem_pct_poll:dem_pct)
  temp_sub$type[temp_sub$calculation=="dem_pct"] <- "Actual results"
  temp_sub$type[temp_sub$calculation=="dem_pct_poll"] <- "Polls correct"

    ###now Let's do pct of vote 
  temp_plot_pct <- ggplot(temp_sub, aes(x=time_counter,y=dem_pct,color=as.factor(type))) +
    geom_line(size=1.2, alpha=0.8) +
    geom_point(aes(size=vote_change),alpha=0.6)  + theme_minimal() + guides(size=FALSE) +
    labs(title=paste0(str_to_title(state_vecbg[i]),sep=" ", "Democratic percent of two party \nvote over time"),
         x="Hours from polls closing",
         y="Democratic % of vote",color="",
         caption = caption_date) + 
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
    theme(plot.caption = element_text(hjust=0),
          title = element_text(size = rel(1.2), family="Styrene B")) +
    geom_hline(yintercept = 50, linetype="dashed", 
               color = "#37C256", size=0.5) +
    scale_color_manual(values = medsl_brands[c(3,1)],drop=F) 
  ###Let's create a plot of when the state parties get most of their votes 
  ####now save the data 
  ggsave(paste0(state_vecbg[i],sep="","poll_ushift_dem_pct",sep="", ".png"), plot = temp_plot_pct, scale = 1,
         width = 9, height = 6, units = c("in"), dpi = 600) 
  
}

###here is where we will have the plots by first 24 hours 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd("results/plots/first24hrs")
for(i in 1:length(unique(nyt_time$state))){
  temp_sub <- subset(nyt_time2, state==state_vec[i])
  temp_plot <- ggplot(temp_sub, aes(x=time_counter,y=prop_vote, color=party)) +
    geom_line(size=1.2, alpha=0.7) +
    geom_point(size=2.4,alpha=0.5) + 
    scale_color_manual(values = medsl_brands[c(1,6)],drop=F) + theme_minimal() + guides(size=FALSE) +
    labs(title=paste0(str_to_title(state_vec[i]),sep=" ", "proportion of \nparty's votes over time"),x="Hours from polls closing",
         y="Proportion of total votes", color="Party",
         caption = caption_date) + scale_x_continuous(limits=c(0,24)) + 
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
    theme(plot.caption = element_text(hjust=0),
          title = element_text(size = rel(1.2), family="Styrene B")) 
  temp_plot

  ###Let's create a plot of when the state parties get most of their votes 
  
  ####now save the data 
  ggsave(paste0(state_vec[i],sep="","enr24prop",sep="", ".png"), plot = temp_plot, scale = 1,
         width = 9, height = 6, units = c("in"), dpi = 600) 

}


temp_plot_pct


pa_sub <- subset(nyt_time2, state=="pennsylvania")
az_sub <- subset(nyt_time2, state=="arizona")
View(pa_sub)
View(az_sub)






