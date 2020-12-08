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
nyt_time <- read.csv("results/complete_nyt_by_time2.csv")

###check structure 
str(nyt_time) # see that time is a character class, and therefore needs to be converted to time. Let's not make a stupid mistake like Nooruddin,
#and cause a coup through crap data analysis.
nyt_time$time <- str_squish(nyt_time$time)
nyt_time$time2 <- as.POSIXct(nyt_time$time, format=c("%m/%d/%Y %H:%M"))
nyt_time$time_num <- as.numeric(nyt_time$time2)
#cutting out data where 0 percent reported 
nyt_time<- subset(nyt_time, total>0)
###now find starting time 
nyt_time <- nyt_time %>% group_by(state) %>% mutate(start_time= min(time_num))
nyt_time$time_counter <- (nyt_time$time_num - nyt_time$start_time)/3600 # getting by hours 
summary(nyt_time$time_counter)# 108 is max approx
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



###Let's now create the long data 
# select=c(state,rep,dem,time_counter,prop_dem,prop_gop,dem_pct)
dem_time <- subset(nyt_time, select=c(time, state,dem,prop_dem,dem_pct,dem_change,dem_diff_pct,dem_diff,time_counter,total_change))
gop_time <- subset(nyt_time, select=c(time, state,rep,prop_gop,dem_pct,gop_change,dem_diff_pct,dem_diff,time_counter,total_change))



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
saveRDS(nyt_time2, "nyt_enr_long2.rds")
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


###Let's get the greatest jump in data 
nyt_time2start <- nyt_time2 %>%
  group_by(state,party) %>%
  slice(which.max(prop_vote_point))
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
wide_nyt_data <- merge(wide_nyt_data,state_codes, by="state")


####let's create a dumbell plot
#setting up annotation
grob_start <- grobTree(textGrob("Starting \ntally", x=0.85,  y=0.9, hjust=0,
                                gp=gpar(col=medsl_brands[3], fontsize=15, fontface="bold")))
grob_end <- grobTree(textGrob("End \ntally", x=0.85,  y=0.75, hjust=0,
                                gp=gpar(col=medsl_brands[6], fontsize=15, fontface="bold")))
###dumbell plot
dumbell_plot_dempct <- ggplot() +
  geom_dumbbell(data = wide_nyt_data, aes(y= reorder(state_po,dem_pct_diff), x=dem_pct, 
                                    xend=dem_pct_end),
                size=1.5, color=medsl_brands[1], colour_x=medsl_brands[3], colour_xend=medsl_brands[6], size_x = 3.5, size_xend = 3.5) +
  theme_minimal() +  labs(x = "Democratic %",
                          y = "",
                          title =  "Change in Democratic vote share of final tally", caption = caption_date) + 
  annotation_custom(grob_start) + annotation_custom(grob_end) + theme(title = element_text(size = rel(1.4), family="Styrene B"),
                                                                      plot.caption = element_text(hjust=0)) +
  geom_vline(xintercept = 50, linetype="dashed", 
             color = "#37C256", size=0.5)

dumbell_plot_dempct

# Create a text
setwd("results/plots")

ggsave("blueshift_dumbell.png", plot = dumbell_plot_dempct, scale = 1, 
       width = 8, height = 12, units = c("in"), dpi = 400)

###look more closely at PA 
pa_sub <- subset(nyt_time2, state=="pennsylvania")
View(pa_sub)







