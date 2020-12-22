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
####Let's do the nyt counties here ; read directly from dropbox
nyt_counties <- read.csv("results/nyt_counties.csv")
# https://www.dropbox.com/home/ElectionNightResults/newspapers/counties
head(nyt_counties)
nyt_counties$fips <- str_pad(nyt_counties$fips, width=5,pad="0",side="left")
nyt_counties$name <- str_to_upper(nyt_counties$name)
nyt_counties <- nyt_counties[,-c(10:15,17:23)]
names(nyt_counties)
nyt_counties$state <- str_to_upper(nyt_counties$state)
###Right here, let's drop all obs before the polls close 
#get unique vals for state 
state_sub <- nyt_counties %>% group_by(state) %>% slice(which.min(votes))
nyt_counties$polls_close <- NA
nyt_counties$polls_close[nyt_counties$state=="ALABAMA"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="ALASKA"] <- "2020-11-04 01:00:00"
nyt_counties$polls_close[nyt_counties$state=="ARIZONA"] <- "2020-11-03 21:00:00"
nyt_counties$polls_close[nyt_counties$state=="ARKANSAS"] <- "2020-11-03 20:30:00"
nyt_counties$polls_close[nyt_counties$state=="CALIFORNIA"] <- "2020-11-03 23:00:00"
nyt_counties$polls_close[nyt_counties$state=="COLORADO"] <- "2020-11-03 21:00:00"
nyt_counties$polls_close[nyt_counties$state=="CONNECTICUT"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="DELAWARE"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="DISTRICT OF COLUMBIA"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="FLORIDA"] <- "2020-11-03 19:00:00"
nyt_counties$polls_close[nyt_counties$state=="GEORGIA"] <- "2020-11-03 19:00:00"
nyt_counties$polls_close[nyt_counties$state=="HAWAII"] <- "2020-11-03 24:00:00"
nyt_counties$polls_close[nyt_counties$state=="IDAHO"] <- "2020-11-03 22:00:00"
nyt_counties$polls_close[nyt_counties$state=="ILLINOIS"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="INDIANA"] <- "2020-11-03 18:00:00"
nyt_counties$polls_close[nyt_counties$state=="IOWA"] <- "2020-11-03 22:00:00"
nyt_counties$polls_close[nyt_counties$state=="KANSAS"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="KENTUCKY"] <- "2020-11-03 18:00:00"
nyt_counties$polls_close[nyt_counties$state=="LOUISIANA"] <- "2020-11-03 21:00:00"
nyt_counties$polls_close[nyt_counties$state=="MAINE"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="MARYLAND"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="MASSACHUSETTS"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="MICHIGAN"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="MINNESOTA"] <- "2020-11-03 21:00:00"
nyt_counties$polls_close[nyt_counties$state=="MISSISSIPPI"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="MISSOURI"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="MONTANA"] <- "2020-11-03 22:00:00"
nyt_counties$polls_close[nyt_counties$state=="NEBRASKA"] <- "2020-11-03 21:00:00"
nyt_counties$polls_close[nyt_counties$state=="NEVADA"] <- "2020-11-03 22:00:00"
nyt_counties$polls_close[nyt_counties$state=="NEW HAMPSHIRE"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="NEW JERSEY"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="NEW MEXICO"] <- "2020-11-03 21:00:00"
nyt_counties$polls_close[nyt_counties$state=="NEW YORK"] <- "2020-11-03 21:00:00"
nyt_counties$polls_close[nyt_counties$state=="NORTH CAROLINA"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="NORTH DAKOTA"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="OHIO"] <- "2020-11-03 19:30:00"
nyt_counties$polls_close[nyt_counties$state=="OKLAHOMA"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="OREGON"] <- "2020-11-03 23:00:00"
nyt_counties$polls_close[nyt_counties$state=="PENNSYLVANIA"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="RHODE ISLAND"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="SOUTH CAROLINA"] <- "2020-11-03 19:00:00"
nyt_counties$polls_close[nyt_counties$state=="SOUTH DAKOTA"] <- "2020-11-03 21:00:00"
nyt_counties$polls_close[nyt_counties$state=="TENNESSEE"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="TEXAS"] <- "2020-11-03 20:00:00"
nyt_counties$polls_close[nyt_counties$state=="UTAH"] <- "2020-11-03 22:00:00"
nyt_counties$polls_close[nyt_counties$state=="VERMONT"] <- "2020-11-03 19:00:00"
nyt_counties$polls_close[nyt_counties$state=="VIRGINIA"] <- "2020-11-03 19:00:00"
nyt_counties$polls_close[nyt_counties$state=="WASHINGTON"] <- "2020-11-03 23:00:00"
nyt_counties$polls_close[nyt_counties$state=="WEST VIRGINIA"] <- "2020-11-03 19:30:00"
nyt_counties$polls_close[nyt_counties$state=="WISCONSIN"] <- "2020-11-03 21:00:00"
nyt_counties$polls_close[nyt_counties$state=="WYOMING"] <- "2020-11-03 21:00:00"

###now let's get the poll time num 
###Let's now get the velocity, which will need change in time. Or grouping of times into 15 min intervals 
nyt_counties$time_num <- as.numeric(as.POSIXct(nyt_counties$time))
head(nyt_counties$time_num)
str(nyt_counties$time)
nyt_counties$polls_close_num <- as.numeric(as.POSIXct(nyt_counties$polls_close))
head(nyt_counties$polls_close_num)



nyt_counties <- subset(nyt_counties, time_num  > polls_close_num )
###Let's create hours from closing 
nyt_counties$hours_from_close <- (nyt_counties$time_num-nyt_counties$polls_close_num)/3600
summary(nyt_counties$hours_from_close)
#View(state_sub)
#View(nyt_counties)




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
    summarise(tot_exp_vote=last(tot_exp_vote), total.votes=last(votes),trumpd=last(trumpd),bidenj=last(bidenj),
    abs_trumpd=last(abs_trumpd),abs_bidenj=last(abs_bidenj), polls_close=first(polls_close), polls_close_num=first(polls_close_num),
    hours_from_close=last(hours_from_close))
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
                                                             lag_trumpd=lag(trumpd,default=0),lag_trump_abs=lag(abs_trumpd,default=0),
                                                             lag_biden_abs = lag(abs_bidenj, default=0))
nyt_counties2$total_vote_chg <- nyt_counties2$total.votes-nyt_counties2$lag_total_votes
nyt_counties2$biden_chg <- nyt_counties2$bidenj - nyt_counties2$lag_bidenj
nyt_counties2$trump_chg <- nyt_counties2$trumpd - nyt_counties2$lag_trumpd
nyt_counties2$biden_abs_Chg <- nyt_counties2$abs_bidenj - nyt_counties2$lag_biden_abs
nyt_counties2$trump_abs_chg <- nyt_counties2$abs_trumpd - nyt_counties2$lag_trump_abs
##proportion of expected returns 
nyt_counties2$prop_expect_returned <- (nyt_counties2$total.votes/nyt_counties2$tot_exp_vote)*100


summary(nyt_counties2$trump_chg)
length(which(nyt_counties2$trump_chg<0)) # 338
length(which(nyt_counties2$biden_chg<0)) # 331

###Let's get acceleration and dem pct vars 
nyt_counties2$dem_pct <- (nyt_counties2$bidenj/nyt_counties2$total.votes)*100
nyt_counties2$dem2party_pct <- (nyt_counties2$bidenj/(nyt_counties2$bidenj+nyt_counties2$trumpd))*100
###Let's get abs numbers 
nyt_counties2$dem2party_abs_pct <- (nyt_counties2$abs_bidenj/(nyt_counties2$abs_bidenj+nyt_counties2$abs_trumpd))*100
nyt_counties2$prop_abs_ballots <- ((nyt_counties2$abs_bidenj+nyt_counties2$abs_trumpd)/(nyt_counties2$trumpd+nyt_counties2$bidenj))*100

nyt_counties2 <- nyt_counties2 %>% group_by(fips) %>% mutate(final_vote=last(total.votes))
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
#test_sub <- subset(nyt_counties2, state=="PENNSYLVANIA" & gini_votes > 0 & is.na(gini_votes)==FALSE)
#test_sub2 <- subset(test_sub, interval15min_num==50)
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
state_nyt_gini <- state_nyt_gini %>% group_by(state) %>% mutate(last_lead=last(lead))

#### Let's save data here 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

write.csv(state_nyt_gini, 'results/state_nyt_gini.csv', row.names = FALSE)
saveRDS(state_nyt_gini, 'results/state_nyt_gini.rds')
state_nyt_gini <- readRDS('results/state_nyt_gini.rds')
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
###let's get last lead 


gini_state_plot <- ggplot(data=state_nyt_gini, aes(x=interval15min_num,y=gini_reported,group=state, colour=last_lead)) +
  geom_line(lwd=1.05,alpha=0.3) +
  theme_minimal() + 
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  scale_color_manual(values = medsl_brands[c(1,6)]) + 
  guides(colour = FALSE) +
  scale_x_continuous(breaks = seq(0,624,by=96), 
                     labels=c("0", "24", "48", "72",
                              "96", "120", "144"), 
                     limits = c(0,624))  + ylim(0.0,1)+
  labs(title="Inequality in county reporting over time, by state",
       x="Hours from polls closing",y="Inequality in reports (gini coef.)",color="Lead",
       caption = caption_date)  + annotation_custom(grob_biden) + annotation_custom(grob_trump) + annotation_custom(grob_title)
  
gini_state_plot 
ggsave("results/plots/state_gini_ts_plot_party.png", plot = gini_state_plot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600) 

###Let's get means by for period between nov 4 and 5 
#nov4to5data <- subset(state_nyt_gini, interval15min_num >= 96 & interval15min_num  <= 192)
#nov4to5data_sum <- nov4to5data %>% group_by(state) %>% summarise(mean(gini_reported))
###



#summary(nov4to5data_sum$`mean(gini_reported)`)
#View(nov4to5data_sum)

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
nyt_counties2$lead <- NA
nyt_counties2$lead[nyt_counties2$dem2party_pct>=50] <- "Biden"
nyt_counties2$lead[nyt_counties2$dem2party_pct<50] <- "Trump"

###sorting data, just to be sure 
interval15min_num <- nyt_counties2[
  with(nyt_counties2, order(fips, interval15min_num)),
  ]
###Let's get the last vote 
nyt_counties2 <- nyt_counties2 %>% group_by(state,fips) %>% mutate(last_dem2party_pct=last(dem2party_pct)) 
nyt_counties2$last_lead <- NA
nyt_counties2$last_lead[nyt_counties2$last_dem2party_pct>=50] <- "Biden"
nyt_counties2$last_lead[nyt_counties2$last_dem2party_pct<50] <- "Trump"
nyt_counties2 <- subset(nyt_counties2, is.na(name)==FALSE)
nyt_counties2 <- nyt_counties2 %>% group_by(fips) %>% mutate(hours_from_close2=first(hours_from_close))

nyt_counties2$hours_from_close2 <- nyt_counties2$hours_from_close2 + (nyt_counties2$interval15min_num*0.25)

#View(nyt_counties2)
#old code here 
#nyt_counties2 <- nyt_counties2 %>% group_by(fips) %>% mutate(last_lead=last(lead))
#Let's get the last data 
#final_sub <- nyt_counties2 %>% group_by(fips) %>% slice(which.max(interval15min_num))
#That seems to work. 


#View(nyt_counties2)
###test sub 
#trst_sub <- subset(final_sub, last_lead=="Trump" & dem2party_pct > 50 )
#trst_sub <- subset(nyt_counties2, last_lead=="Trump" & returned_pct >=100 & dem2party_pct > 50 )
#table(trst_sub$fips)
#dim(trst_sub)#odd 
###Let's get the data for the county of interest 
#montour_sub <- subset(nyt_counties, fips== "42093" )

#View(montour_sub)
####ok, let's add in manual 




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
#View(nyt_counties_final)

class(nyt_counties_final)
###Let's get the data exported 
write.csv(nyt_counties_final, "results/nyt_counties_final12152020.csv",row.names = FALSE)


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
setwd("F:/MEDSL/election_night2020/results/plots/county_acceleration")
for(i in 1:length(state_vec)){
  svMisc::progress((i/51)*100)
  temp_state <- subset(nyt_counties2, state==state_vec[i])
  temp_state <- as.data.frame(temp_state)
  #temp_state <- subset(temp_state, interval15min_num >= 48)
  #let's just go with manual 
  temp_ts_plot <- ggplot(temp_state, aes(x= hours_from_close2, y=returned_pct, group=fips, col=last_lead)) +
    geom_line(alpha=0.4,lwd=1.2) + theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)]) +
    guides(colour = FALSE) +
    theme(title = element_text(size = rel(1.2), family="Styrene B"), 
          plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45,vjust=0.5)) +
    labs(title=paste(str_to_title(state_vec[i]),sep=" ", "time to tabulate ballots, by county"),
         x="Hours from polls closing",y="% tabulated",
         caption = caption_date)  + ylim(0,100) + annotation_custom(grob_title2) + annotation_custom(grob_biden2) +
    annotation_custom(grob_trump2)  +
    scale_x_continuous(breaks = seq(0,144,by=24), 
                       limits = c(0,144)) 
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
         caption = caption_date) + 
    ylim(0,100) + annotation_custom(grob_title3) + annotation_custom(grob_biden3) + annotation_custom(grob_trump3) + 
    scale_x_continuous(breaks = seq(0,144,by=24), 
                       limits = c(0,144)) 
 temp_ts_accel_plot
  ggsave(paste0(state_vec[i],sep="_", "acceleration_plot",sep="", ".png"), plot = temp_ts_accel_plot, scale = 1,
         width = 9, height = 6, units = c("in"), dpi = 600) 
  
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###Let's create the 6 states of interest: AZ, MI, PA, WI , NC, FL . Let's do a 4 by 4 and 
#let's just go with manual 
az_tabulate <- ggplot(data = subset(nyt_counties2, state=="ARIZONA" &  (interval15min_num %% 8 )==0), 
                       aes(x= hours_from_close2, y=returned_pct, group=fips, col=last_lead, size=total.votes)) +
  geom_line(alpha=0.4,lwd=1.2) +
  geom_point(alpha=0.4) + guides(size=FALSE) +
  theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)],name="Winner") +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Arizona",
       x="Hours from polls closing", y="% reported")  + ylim(0,100)  +
  scale_x_continuous(breaks = seq(0,144,by=24), 
                     limits = c(0,144)) 
az_tabulate
mi_tabulate <- ggplot(data = subset(nyt_counties2, state=="MICHIGAN" &  (interval15min_num %% 8 )==0), 
                      aes(x= hours_from_close2, y=returned_pct, group=fips, col=last_lead, size=total.votes)) +
  geom_line(alpha=0.4,lwd=1.2) +
  geom_point(alpha=0.4)  +theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)],name="Winner") +
  guides(size = FALSE) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Michigan",
       x="Time from polls closing", y="% reported") + ylim(0,100)  +
  scale_x_continuous(breaks = seq(0,144,by=24), 
                     limits = c(0,144)) 
mi_tabulate
pa_tabulate <- ggplot(data = subset(nyt_counties2, state=="PENNSYLVANIA" & (interval15min_num %% 8 )==0), 
                      aes(x= hours_from_close2, y=returned_pct, group=fips, col=last_lead, size=total.votes)) +
  geom_line(alpha=0.4,lwd=1.2) + 
  geom_point(alpha=0.4) +theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)],name="Winner") +
  guides(size = FALSE) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Pennsylvania",
       x="Hours from polls closing", y="% reported")  + ylim(0,100)  +
  scale_x_continuous(breaks = seq(0,144,by=24), 
                     limits = c(0,144)) 
pa_tabulate
wi_tabulate <- ggplot(data = subset(nyt_counties2, state=="WISCONSIN" &  (interval15min_num %% 8 )==0), 
                      aes(x= hours_from_close2, y=returned_pct, group=fips, col=last_lead, size=total.votes)) +
  geom_line(alpha=0.2,lwd=1.2) +
  geom_point(alpha=0.4) + theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)], name="Winner") +
  guides(size = FALSE) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Wisconsin",
       x="Hours from polls closing", y="% reported")  + ylim(0,100)  +
  scale_x_continuous(breaks = seq(0,144,by=24), 
                     limits = c(0,144)) 
wi_tabulate

#save
library(grid)
library(gridExtra)
library(ggpubr)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

g_reported <- ggpubr::ggarrange(az_tabulate, mi_tabulate, pa_tabulate, wi_tabulate,  
                                common.legend=TRUE, legend="bottom" ) #generates g
g_reported <- annotate_figure(g_reported, top=text_grob("Time to report ballots by county",size=14,face="bold"), 
                              bottom= text_grob(caption_date, hjust=1) )

ggsave("results/plots/battle_ground_slowreports.png",  g_reported,width=12,height=12,units=c("in")) #saves g
g_reported
#pa_sub <- subset(nyt_counties2, state=="PENNSYLVANIA" & interval15min_num >= 48 & (interval15min_num %% 8 )==0)
#pa_sub <- subset(pa_sub, interval15min_num==624)
#View(pa_sub)

###Let's do fl and nc now 
fl_tabulate <- ggplot(data = subset(nyt_counties2, state=="FLORIDA" & (interval15min_num %% 8 )==0), 
                      aes(x= hours_from_close2, y=returned_pct, group=fips, col=last_lead, size=total.votes)) +
  geom_line(alpha=0.4,lwd=1.2) +
  geom_point(alpha=0.4) + theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)], name="Winner") +
  guides(size = FALSE) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Florida",
       x="Hours from polls closing", y="% reported", caption=caption_date)  + ylim(0,100) + 
  scale_x_continuous(breaks = seq(0,144,by=24), 
                     limits = c(0,144)) 
nc_tabulate <- ggplot(data = subset(nyt_counties2, state=="NORTH CAROLINA" &  (interval15min_num %% 8 )==0), 
                      aes(x= hours_from_close2, y=returned_pct, group=fips, col=last_lead, size=total.votes)) +
  geom_line(alpha=0.4,lwd=1.2) +
  geom_point(alpha=0.4) + theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)], name="Winner") +
  guides(size = FALSE) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="North Carolina",
       x="Hours from polls closing", y="% reported")  + ylim(0,100) + 
  scale_x_continuous(breaks = seq(0,144,by=24), 
                     limits = c(0,144)) 
nc_tabulate
fl_tabulate
g_reported2 <- ggpubr::ggarrange(fl_tabulate, nc_tabulate,  
                                common.legend=TRUE, legend="bottom" ) #generates g
g_reported2 <- annotate_figure(g_reported2, top=text_grob("Time to report ballots by county",size=14,face="bold") )

ggsave("results/plots/battle_ground_fastreports.png",  g_reported2,width=12,height=6,units=c("in"))

###Check for PA and other states in comparison
az_sub <- subset(nyt_counties2, state=="ARIZONA" & interval15min_num ==96)
summary(az_sub$returned_pct)
mi_sub <- subset(nyt_counties2, state=="MICHIGAN" & interval15min_num ==96)
summary(mi_sub$returned_pct)
pa_sub <- subset(nyt_counties2, state=="PENNSYLVANIA" & interval15min_num ==96)
summary(pa_sub$returned_pct)
wi_sub <- subset(nyt_counties2, state=="WISCONSIN" & interval15min_num ==96)
summary(wi_sub$returned_pct)


####Let's now create a plot of the states of interest and the dem shift over the night ; both the individual counties and the state ave. 

###Let's now do FL and NC 
###FL


###NC


###export 
flnc_demshift <- ggpubr::ggarrange(fl_demshift, nc_demshift, 
                                common.legend=TRUE, legend="bottom" ) #generates g
flnc_demshift2 <- annotate_figure(flnc_demshift, top=text_grob("Democratic vote share by county over time",size=14,face="bold") )
ggsave("results/plots/battle_groundsouth_demshift.png",  flnc_demshift,width=12,height=6,units=c("in")) #saves g

###Let's looks at chg in em pct 
az_state <- az_state %>% mutate(lag_dem = lag(dem2party_pct))
mi_state <- mi_state %>% mutate(lag_dem = lag(dem2party_pct))
pa_state <- pa_state %>% mutate(lag_dem = lag(dem2party_pct))
wi_state <- wi_state %>% mutate(lag_dem = lag(dem2party_pct))
fl_state <- fl_state %>% mutate(lag_dem = lag(dem2party_pct))
nc_state <- nc_state %>% mutate(lag_dem = lag(dem2party_pct))
####now chg
az_state$dem_pctchg <- az_state$dem2party_pct - az_state$lag_dem
mi_state$dem_pctchg <- mi_state$dem2party_pct - mi_state$lag_dem
pa_state$dem_pctchg <- pa_state$dem2party_pct - pa_state$lag_dem
wi_state$dem_pctchg <- wi_state$dem2party_pct - wi_state$lag_dem
fl_state$dem_pctchg <- fl_state$dem2party_pct - fl_state$lag_dem
nc_state$dem_pctchg <- nc_state$dem2party_pct - nc_state$lag_dem
#View(az_state)

###we can identify the changes 
az_state <- subset(az_state, is.na(dem2party_pct)==FALSE)
((az_state$interval15min_num[az_state$dem_pctchg == min(az_state$dem_pctchg,na.rm=T)])*15 - (48*15) )/60
((mi_state$interval15min_num[mi_state$dem_pctchg == min(mi_state$dem_pctchg,na.rm=T)])*15 - (48*15) )/60
((pa_state$interval15min_num[pa_state$dem_pctchg == min(pa_state$dem_pctchg,na.rm=T)])*15 - (48*15) )/60
((wi_state$interval15min_num[wi_state$dem_pctchg == min(wi_state$dem_pctchg,na.rm=T)])*15 - (48*15) )/60
((fl_state$interval15min_num[fl_state$dem_pctchg == min(fl_state$dem_pctchg,na.rm=T)])*15 - (48*15) )/60
((nc_state$interval15min_num[nc_state$dem_pctchg == min(nc_state$dem_pctchg,na.rm=T)])*15 - (48*15) )/60

####Let's see if we can do a pct reported and hours plot 
nyt_counties2$twoparty_vote <- nyt_counties2$bidenj+nyt_counties2$trumpd
###Arizona

###Let's try the plots grouped by color , last_lead 

####Color plots for % reported on Dem vote share 

### ARizona 
az_demcounty_cor_col <- ggplot(data = subset(nyt_counties2, state=="ARIZONA"), 
                               aes(x= returned_pct, y=dem2party_pct, group=fips, col=last_lead) ) +
  geom_line(alpha=0.6,lwd=1.2)  + guides(size=FALSE) + 
  geom_point(aes(size=total.votes), alpha=0.6) +theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)],drop=F) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Arizona",color="Winner",
       x="% Reported", y="Democratic 2-party vote %")  + ylim(0,100) + xlim(0,100) 
az_demcounty_cor_col # I like this. Let's recreate for the other states 

###Michigan 
mi_demcounty_cor_col <- ggplot(data = subset(nyt_counties2, state=="MICHIGAN"), 
                               aes(x= returned_pct, y=dem2party_pct, group=fips, col=last_lead) ) +
  geom_line(alpha=0.6,lwd=1.2)  + guides(size=FALSE) + 
  geom_point(aes(size=total.votes), alpha=0.6) +theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)],drop=F) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Michigan",color="Winner",
       x="% Reported", y="Democratic 2-party vote %")  + ylim(0,100) + xlim(0,100) 
mi_demcounty_cor_col # I like this. Let's recreate for the other states 

###Let's get the abs ballots 
sum(nyt_counties_final$abs_bidenj,na.rm=T)/sum(nyt_counties_final$bidenj) # 0.3952951
sum(nyt_counties_final$abs_trumpd,na.rm=T)/sum(nyt_counties_final$trumpd) # 0.3400335
(sum(nyt_counties_final$abs_trumpd)+sum(nyt_counties_final$trumpd))/sum(nyt_counties_final$total.votes)

####Let's create a hexagonal map 

###Pennsylvania 
##need to figure out why one point is so democratic yet red 
#test_pa <- subset(nyt_counties2, state=="PENNSYLVANIA")
#test_pa <- subset(test_pa, returned_pct >= 100 & dem2party_pct > 75  )
#View(test_pa)
####
pa_demcounty_cor_col <- ggplot(data = subset(nyt_counties2, state=="PENNSYLVANIA"), 
                               aes(x= returned_pct, y=dem2party_pct, group=fips, col=last_lead) ) +
  geom_line(alpha=0.6,lwd=1.2)  + guides(size=FALSE) + 
  geom_point(aes(size=total.votes), alpha=0.6) +theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)],drop=F) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Pennsylvania",color="Winner",
       x="% Reported", y="Democratic 2-party vote %")  + ylim(0,100) + xlim(0,100) 
pa_demcounty_cor_col # I like this. Let's recreate for the other states 

### Wisconsin
wi_demcounty_cor_col <- ggplot(data = subset(nyt_counties2, state=="WISCONSIN"), 
                               aes(x= returned_pct, y=dem2party_pct, group=fips, col=last_lead) ) +
  geom_line(alpha=0.6,lwd=1.2)  + guides(size=FALSE) + 
  geom_point(aes(size=total.votes), alpha=0.6) +theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)],drop=F) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Wisconsin",color="Winner",
       x="% Reported", y="Democratic 2-party vote %")  + ylim(0,100) + xlim(0,100) 
wi_demcounty_cor_col # I like this. Let's recreate for the other states 

## Florida 
fl_demcounty_cor_col <- ggplot(data = subset(nyt_counties2, state=="FLORIDA"), 
                               aes(x= returned_pct, y=dem2party_pct, group=fips, col=last_lead) ) +
  geom_line(alpha=0.6,lwd=1.2)  + guides(size=FALSE) + 
  geom_point(aes(size=total.votes), alpha=0.6) +theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)],drop=F) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="Florida",color="Winner",
       x="% Reported", y="Democratic 2-party vote %")  + ylim(0,100) + xlim(0,100) 
fl_demcounty_cor_col # I like this. Let's recreate for the other states 


###North Carolina 
nc_demcounty_cor_col <- ggplot(data = subset(nyt_counties2, state=="NORTH CAROLINA"), 
                           aes(x= returned_pct, y=dem2party_pct, group=fips, col=last_lead) ) +
  geom_line(alpha=0.6,lwd=1.2)  + guides(size=FALSE) + 
  geom_point(aes(size=total.votes), alpha=0.6) +theme_minimal() + scale_color_manual(values = medsl_brands[c(1,6)],drop=F) +
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  labs(title="North Carolina",color="Winner",
       x="% Reported", y="Democratic 2-party vote %")  + ylim(0,100) + xlim(0,100) 
nc_demcounty_cor_col # I like this. Let's recreate for the other states 

setwd(getwd())
####Let's create the grid plots now for th
gg_pct_rpt_midw_col <- ggpubr::ggarrange(az_demcounty_cor_col, mi_demcounty_cor_col, pa_demcounty_cor_col, wi_demcounty_cor_col,  
                                     common.legend=TRUE, legend="bottom" ) #generates g

gg_pct_rpt_midw_col <- annotate_figure(gg_pct_rpt_midw_col, 
                                       top=text_grob("Democratic voteshare by county percent reported",size=14,face="bold"))

ggsave("results/plots/battle_groundmidw_pctreport_by_dem_col.png",  gg_pct_rpt_midw_col,width=12,height=12,units=c("in")) #saves g

gg_pct_rpt_south_col <- ggpubr::ggarrange(fl_demcounty_cor_col, nc_demcounty_cor_col,  
                                      common.legend=TRUE, legend="bottom" ) #generates g

gg_pct_rpt_south_col <- annotate_figure(gg_pct_rpt_south_col,
                                        top=text_grob("Democratic voteshare by county percent reported",size=14,face="bold"))

ggsave("results/plots/battle_groundsouth_pctreport_by_dem_col.png",  gg_pct_rpt_south_col,width=12,height=6,units=c("in")) #saves g


###Looking at the counties that shifted 

wi_state <- subset(nyt_counties2, state=="WISCONSIN")
wi_state$round_report_pct <- round(wi_state$returned_pct,0)
wi_test_sub <- subset(wi_state, lead!=last_lead)
table(wi_test_sub$name)
##PA 
pa_state <- subset(nyt_counties2, state=="PENNSYLVANIA")
pa_state$round_report_pct <- round(pa_state$returned_pct,0)
pa_test_sub <- subset(pa_state, lead!=last_lead)
table(pa_test_sub$name)


###Let's check the convergence of FL and NC 

fl_sub <- subset(nyt_counties2, state=="FLORIDA" & interval15min_num ==96)
summary(fl_sub$returned_pct)
nc_sub <- subset(nyt_counties2, state=="NORTH CAROLINA" & interval15min_num ==96)
summary(nc_sub$returned_pct)
View(fl_sub)
###Let's now add in the wbm_merge_counties data
#nyt_counties <- read.csv("wbm_merge_counties.csv")
View(nyt_counties)

(5 %% 3)==0



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






