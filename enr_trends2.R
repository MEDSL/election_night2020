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
nyt_time <- nyt_time %>% group_by(state) %>% mutate(final_vote_total=last(total))
nyt_time$party2vote <- nyt_time$dem+nyt_time$rep
nyt_time <- nyt_time %>% group_by(state) %>% mutate(final2party=last(party2vote))
nyt_time <- as.data.frame(nyt_time)
###outstanding 2 party 
nyt_time$outstanding2party <- nyt_time$final2party - nyt_time$party2vote
nyt_time$dem2party_prop <- nyt_time$dem/nyt_time$party2vote
nyt_time$prop2party <- nyt_time$party2vote/nyt_time$final2party
head(nyt_time)
###Let's get the two party final total 
nyt_time$outstanding_total <- (nyt_time$final_vote_total - nyt_time$total)
###lwt's retry the overturn calc 
nyt_time$dem_total_prop <- (nyt_time$dem/nyt_time$total)
nyt_time$prop_total <- nyt_time$total/nyt_time$final_vote_total
nyt_time$overturn_dem_count <- nyt_time$dem + 
  (0.5- (nyt_time$dem/nyt_time$final_vote_total) )*(nyt_time$outstanding_total)*(1-nyt_time$prop_total)^-1
nyt_time$overturn_dem_pct  <- (nyt_time$overturn_dem_count / nyt_time$outstanding_total)
summary(nyt_time$overturn_dem_count)

###Let's do the 2 party version 
nyt_time$overturn_dem_count2 <- nyt_time$dem + 
  (0.5- nyt_time$dem/nyt_time$final2party)*(nyt_time$outstanding2party)*(1-nyt_time$prop2party)^-1
nyt_time$overturn_dem_count2pct <- (nyt_time$overturn_dem_count2/nyt_time$outstanding2party)
summary(nyt_time$overturn_dem_count2pct)

###let's try something different to see if we can fix the calc 
nyt_time$abs2party_diff <- abs(nyt_time$dem-nyt_time$rep)
nyt_time$magnitude_diff_overturn <- nyt_time$abs2party_diff/nyt_time$outstanding_total
summary(nyt_time$magnitude_diff_overturn)
###Let's get the data subsetted now 
nyt_time_call1 <- subset(nyt_time, overturn_dem_pct > 100 | overturn_dem_pct < -100   )
nyt_time_call1 <- nyt_time_call1 %>% group_by(state) %>% slice(which.min(hours.from.close))###This eqn is definitely off for places like GA
#####
nyt_time_call2 <- subset(nyt_time, overturn_dem_count2pct > 100 | overturn_dem_count2pct < -100   )
nyt_time_call2 <- nyt_time_call2 %>% group_by(state) %>% slice(which.min(hours.from.close))

###ok, let's get the magnitude for v3 
nyt_time_call3 <- subset(nyt_time, magnitude_diff_overturn > 1   )
nyt_time_call3 <- nyt_time_call3 %>% group_by(state) %>% slice(which.min(hours.from.close))

###Let's subset the data for 2 
nyt_time_call2 <- subset(nyt_time_call2, select=c(state, percent.reported, dem_pct,hours.from.close,overturn_dem_count2pct))
colnames(nyt_time_call2)[2:5] <- c("pct_reported_tcf","dem_pct_tcf","hours_from_close_tcf","overturn_pct")
nyt_time_call3 <- merge(nyt_time_call3,nyt_time_call2, by="state",all.x=T )
###Let's find diff in time 
nyt_time_call3$call_time_diff1 <- nyt_time_call3$hours.from.close - nyt_time_call3$hours_from_close_tcf
summary(nyt_time_call3$call_time_diff1)
View(nyt_time_call3)

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
nyt_called$poll_time2[nyt_called$state=="north carolina"] <- as.POSIXct("2020-11-12 23:59:59")
###needed to change poll time for NC, given that they accepted ballots until the 12th 

###need to figure out what's going on with west virginia 
wv_sub <- subset(nyt_time, state=="west-virginia")
wv_sub$overturn_dem_count <- wv_sub$dem + 
  (0.5- (wv_sub$total/wv_sub$final_vote_total)*wv_sub$dem/wv_sub$total)*(wv_sub$outstanding_total)*(1-(wv_sub$total/wv_sub$final_vote_total))^-1
wv_sub$overturn_dem_pct  <- (wv_sub$overturn_dem_count / wv_sub$outstanding_total)

###looking at GA
ga_sub <- subset(nyt_time, state=="georgia")
View(ga_sub)
###now let's get the time till call 
nyt_called$time2call <- (as.numeric(nyt_called$winner_called2) - as.numeric(nyt_called$poll_time2))/3600
###now that we have this, let's get the data based on when 95% of returns would have to be of one party 
nyt_time_95pct_overturn <- subset(nyt_time, magnitude_diff_overturn > 1)
# nyt_time_95pct_overturn <- subset(nyt_time, overturn_dem_pct >= 100 | overturn_dem_pct <= 0)

##now get min hours from close 
nyt_time_95pct_overturn <- nyt_time_95pct_overturn %>% group_by(state) %>% slice(which.min(hours.from.close))
nyt_time_95pct_overturn$state <- gsub("-", " ", nyt_time_95pct_overturn$state)
View(nyt_time_95pct_overturn)
###ok, now we can get the data merged 
nyt_time_95pct_overturn <- merge(nyt_time_95pct_overturn, nyt_called, by="state")
nyt_time_95pct_overturn$call_difference <- nyt_time_95pct_overturn$hours.from.close - nyt_time_95pct_overturn$time2call

summary(nyt_time_95pct_overturn$call_difference)
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


View(nyt_time_95pct_overturn)
?strptime

##let's create elec status dummy 
nyt_time$before_elec_vbm <- 1
# https://www.ncsl.org/research/elections-and-campaigns/absentee-and-early-voting.aspx
sort(unique(nyt_time$state))
nyt_time$before_elec_vbm[nyt_time$state=="alabama"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="south carolina"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="district-of-columbia"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="new-hampshire"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="idaho"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="connecticut"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="new-york"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="pennsylvania"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="west-virginia"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="kentucky"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="michigan"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="wisconsin"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="south-dakota"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="wyoming"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="massachusetts"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="maryland"] <- 1
nyt_time$before_elec_vbm[nyt_time$state=="mississippi"] <- 1


#when over 100, impossible for GOP to in. When under 0, impossible for Dems to win. Let's now subset
# to wide battleground 
###subsetting here 
##let's make the additions now 
wide_battleground <- subset(nyt_time, state=="georgia" | state=="nevada" | state=="florida" | state=="ohio" | state=="michigan" | 
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
wide_battleground$dem_projected <- (wide_battleground$dem_pct_poll/100)*wide_battleground$total
wide_battleground$overturn_dem_pct_polls <- ((wide_battleground$win_majority - wide_battleground$dem_projected)/
                                               (wide_battleground$outstanding_total))*100
summary(wide_battleground$overturn_dem_pct_polls)


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

###Let's now add in the wbm_merge_counties data
nyt_counties <- read.csv("wbm_merge_counties.csv")
nyt_counties$fips <- str_pad(nyt_counties$fips, width=5,pad="0",side="left")
nyt_counties <- nyt_counties[,-c(10:15,17:23)]
View(nyt_counties)




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

###how much Biden was ahead: 
#PA: Biden by 1.2 points
# Ave across battleground states: 2.7 points
#https://www.cnbc.com/2020/11/02/election-2020-biden-leads-trump-in-polls-as-swing-states-tighten.html
# 538 article on poll averages: https://fivethirtyeight.com/features/the-polls-werent-great-but-thats-pretty-normal/


###subsetting here 
battleground_nyt <- subset(nyt_time2, state=="pennsylvania" | state=="arizona" | state=="georgia" | state=="north carolina" |
                             state=="ohio" | state=="michigan" | state=="wisconsin" | state=="florida" | state=="nevada")
battleground_nyt <- subset(battleground_nyt, party=="DEM")

##let's make the additions now 
battleground_nyt$dem_pct_poll <- battleground_nyt$dem_pct
battleground_nyt$dem_pct_poll[battleground_nyt$state=="arizona"] <- 
  (battleground_nyt$dem_pct_poll+3)[battleground_nyt$state=="arizona"]

battleground_nyt$dem_pct_poll[battleground_nyt$state=="pennsylvania"] <- 
  (battleground_nyt$dem_pct_poll+3)[battleground_nyt$state=="pennsylvania"]

battleground_nyt$dem_pct_poll[battleground_nyt$state=="wisconsin"] <- 
  (battleground_nyt$dem_pct_poll+7)[battleground_nyt$state=="wisconsin"]

battleground_nyt$dem_pct_poll[battleground_nyt$state=="north carolina"] <- 
  (battleground_nyt$dem_pct_poll+3)[battleground_nyt$state=="north carolina"]

battleground_nyt$dem_pct_poll[battleground_nyt$state=="michigan"] <- 
  (battleground_nyt$dem_pct_poll+5)[battleground_nyt$state=="michigan"]

battleground_nyt$dem_pct_poll[battleground_nyt$state=="ohio"] <- 
  (battleground_nyt$dem_pct_poll+3)[battleground_nyt$state=="ohio"]

battleground_nyt$dem_pct_poll[battleground_nyt$state=="florida"] <- 
  (battleground_nyt$dem_pct_poll+6)[battleground_nyt$state=="florida"]

battleground_nyt$dem_pct_poll[battleground_nyt$state=="nevada"] <- 
  (battleground_nyt$dem_pct_poll+2)[battleground_nyt$state=="nevada"]

battleground_nyt$dem_pct_poll[battleground_nyt$state=="georgia"] <- 
  (battleground_nyt$dem_pct_poll+1)[battleground_nyt$state=="georgia"]

####now let's do the loop for the plots ; we will want to add in the eqn that is present in the paper, as 
# number of votes for Democrats at time t, 
# Dt + (0.5 - Pt*Dt/Tt)(T - Tt)(1 - Pt)^-1


View(battleground_nyt)
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






