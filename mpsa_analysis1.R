###################################################################################
####################### MPSA Blue Shift Analysis #################################
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

### read in the gini results 
state_nyt_gini <- readRDS('results/state_nyt_gini.rds')

##read in the county data for every 15 minutes 
nyt_counties2 <- readRDS("results/nyt15minute_county_data.rds")
nyt_counties2 <- as.data.frame(nyt_counties2)
##check the code for the demo vote share 
head(nyt_counties2)
###now let's subset the data so its less sparse; 
nyt_counties2 <- subset(nyt_counties2, total_vote_chg > 0)
#fields are dem_pct and dem2party_pct ; we will now ant to predict the change in demvote share, then the change in change. 
nyt_counties2 <- nyt_counties2 %>%
  group_by(fips) %>%
  mutate(lag_dem2party_pct = lag(dem2party_pct))
###get change in time 
nyt_counties2 <- nyt_counties2 %>%
  group_by(fips) %>%
  mutate(lag_hours_from_close = lag(hours_from_close2))
nyt_counties2$hours_from_close2chg <- nyt_counties2$hours_from_close2-nyt_counties2$lag_hours_from_close
##now calculate the change 
nyt_counties2$dem2party_pct_chg <- (nyt_counties2$dem2party_pct - nyt_counties2$lag_dem2party_pct)/nyt_counties2$hours_from_close2chg
#now find the change in the velocity (acceleration)
nyt_counties2 <- nyt_counties2 %>%
  group_by(fips) %>%
  mutate(lag_dem2party_pct_chg = lag(dem2party_pct_chg))
nyt_counties2$dem2party_pct_accel <- (nyt_counties2$dem2party_pct_chg - nyt_counties2$lag_dem2party_pct_chg)/nyt_counties2$hours_from_close2chg

###next we will want to calculate the average vote total at a given time 
nyt_counties2$rounded_returned_pct <- round(nyt_counties2$returned_pct, 1)
nyt_counties2 <- nyt_counties2 %>% group_by(state,rounded_returned_pct) %>%
  mutate(state_total=max(total.votes,na.rm=T)) #this gives us the max vote at a given time. Now its possible to compute a county's share
#of the state's vote at ret_pct t 
nyt_counties2$county_share <- nyt_counties2$total.votes/nyt_counties2$state_total
###should always be less than or equal to 1 
summary(nyt_counties2$county_share) # checks out 

###let's change the drawing order such that red doesn't dominate 
nyt_counties2$last_lead_fac <- factor(nyt_counties2$last_lead, c("Trump", "Biden"))

#################################################33 TEST ####################################################################
###we now have the numbers. So let's test plot a state 
nc_sub <- subset(nyt_counties2, state=="NORTH CAROLINA")

###try diff one, with the ave plotted first 
#nc_sub$rounded_returned_pct <- round(nc_sub$returned_pct, 1)
nc_plot1 <- ggplot(nc_sub %>% group_by(rounded_returned_pct) %>% summarise(dem2party_pct_chg=sum(dem2party_pct_chg*county_share, na.rm=T)),
                   aes(x=rounded_returned_pct,y=dem2party_pct_chg)) +
  geom_line(data=nc_sub,aes(x=returned_pct,y=dem2party_pct_chg,group=fips, color=last_lead_fac),alpha=0.4,lwd=1.2) + 
  geom_line(color="gray",lwd=1.1,alpha=0.5 ) + 
  geom_point(data=nc_sub,aes(x=returned_pct,y=dem2party_pct_chg,group=fips, color=last_lead_fac,size=total.votes),alpha=0.4) + 
  labs(title="",
       y="2-Party vote share velocity",x="% Tabulated",
       caption = paste0(caption_date,sep="\n","Positive values equate to change favorable to Dem. candidate"))+ 
  scale_color_manual(values = medsl_brands[c(6,1)], "Winner") + xlim(50,100) + theme_minimal() + ylim(-50,50)+
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) + guides(size=FALSE)
nc_plot1
###############################################3 END OF TEST#################################################################
###seems like we should be able to run a loop now 
state_vec <- sort(unique(nyt_counties2$state))
for(i in 1:length(state_vec)){
  svMisc::progress((i/51)*100)
  temp_state <- subset(nyt_counties2, state==state_vec[i])
  temp_state <- as.data.frame(temp_state)
  title_obj <- paste0(str_to_title(state_vec[i]), sep=" ", "2-party vote share convergence by county")
  temp_vel <- ggplot(temp_state %>% group_by(rounded_returned_pct) %>% 
                       summarise(dem2party_pct_chg=sum(dem2party_pct_chg*county_share, na.rm=T)),
                     aes(x=rounded_returned_pct,y=dem2party_pct_chg)) +
    geom_line(data=temp_state,aes(x=returned_pct,y=dem2party_pct_chg,group=fips, color=last_lead_fac),alpha=0.4,lwd=1.2) + 
    geom_line(color="gray",lwd=1.1,alpha=0.5 ) + 
    geom_point(data=temp_state,aes(x=returned_pct,y=dem2party_pct_chg,group=fips, color=last_lead_fac,size=total.votes),alpha=0.4) + 
    labs(title=title_obj,
         y="2-Party vote share velocity",x="% Tabulated",
         caption = paste0(caption_date,sep="\n","Positive values equate to change favorable to Dem. candidate"))+ 
    scale_color_manual(values = medsl_brands[c(6,1)], "Winner") + xlim(50,100) + theme_minimal() + ylim(-50,50)+
    theme(title = element_text(size = rel(1.2), family="Styrene B"), 
          plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 45,vjust=0.5)) + guides(size=FALSE)
  temp_vel
  ggsave(paste0("mpsa_output/velocity",sep="/",state_vec[i],sep="_", "velocity_plot",sep="", ".png"), plot = temp_vel, scale = 1,
         width = 9, height = 6, units = c("in"), dpi = 600) 
}

####good. This seems to work. Now let's try the above, but with acceleration 






