###################################################################################
####################### Monkey Cage Election Results CleanR #################################
################################################################################
library(ggplot)
library(dplyr)
library(stringi)
library(stringr)
options(stringsAsFactors = FALSE)
library(zipWRUext2)
library(readxl)

################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
caption_date <- paste0("Graph Source: MIT Elections Data and Science Lab\nGraph date:",
                       sep=" ", format(Sys.Date(),format="%m/%d/%Y"))
list.files()
medsl_brands <- c("#3791FF","#59CBF5","#C0BA79","#F6573E","#156DD0","#C72654","#FF6878")

####This script will be used to create ggplot style figures for election night for counties. 


#county_results <- read.csv()  #specify election results here

county_results <- readRDS("F:/MEDSL/election_night2020/context/historical_elections/county_prez_wide.rds")
county_results$dem_pct <- (county_results$democrat/(county_results$democrat+county_results$republican))*100
county_results <- as.data.frame(county_results)
county_results$total2vote <- county_results$democrat + county_results$republican 
county_results <- subset(county_results, year==2016)
county_results$county <- str_to_upper(county_results$county)
###county results from prior years split Richmond. Need to combine these 
county_results$county[county_results$FIPS=="51760"] <- "RICHMOND CITY"
county_results$county[county_results$FIPS=="51770"] <- "ROANOKE CITY"
county_results$county[county_results$FIPS=="51600"] <- "FAIRFAX CITY"
county_results$county[county_results$FIPS=="51620"] <- "FRANKLIN CITY"
###now leip data read in 
list.files("results")
#leip_counties <- read.csv("results/leip_natl_data.csv")
leip_counties <- read_xlsx("results/leip_data2020-11-30.xlsx",sheet="County")
names(leip_counties)
##we want the following col nums: 1 (county name), 2 (state_abb), 3 (total vote), 14 (Biden vote), 15 (trump vote), 68 (FIPS), 69 (FIPS2010),
#70 (LSAD_TRANS)
leip_counties <- leip_counties[,c(1:3,14:15,68:70)]
leip_counties <- leip_counties[-1,]
colnames(leip_counties) <- c("county",	"state_po",	"Total Vote",	"Biden",	"Trump",	"FIPS",	"FIPS2010","Geographic.Subtype") 
leip_counties$FIPS <- str_pad(leip_counties$FIPS, width=5,side="left",pad="0")
leip_counties$FIPS2010 <- str_pad(leip_counties$FIPS2010, width=5,side="left",pad="0")
leip_counties <- subset(leip_counties, is.na(`Total Vote`)==F)
###do check for oddities 
length(which(leip_counties$FIPS!=leip_counties$FIPS2010))#1; let's check these
###subsetting to check oddities 
#leip_odd <- subset(leip_counties, FIPS != FIPS2010)###there is one SD county, but otherwise all AK. Should be fine to use 
#leip_odd <- subset(leip_counties, is.na(`Total Vote`)==T)###there is one SD county, but otherwise all AK. Should be fine to use 
#View(leip_odd)
sum(is.na(leip_counties$`Total Vote`))
leip_counties$st_fips <- substr(leip_counties$FIPS, 1,2)
sort(unique(leip_counties$Geographic.Subtype))
table(leip_counties$st_fips, leip_counties$Geographic.Subtype )


nrow(leip_counties)
head(leip_counties)
colnames(leip_counties)[3:5] <- c("total2020", "biden2020","trump2020")

###

##merge elec results here 
county_results2 <- merge(county_results, leip_counties, by=c("FIPS","state_po"),all.x=T )
table(leip_counties$state_po)
table(county_results2$state_po)###seems good; the leip data had totals that were excluded 
sum(is.na(county_results2$total2020))
###Let's check missing 
miss_counties <- subset(county_results2, is.na(total2020)==T)
###Everything in Alaska is missing. Additionally, Kansas City, mO (odd FIPS code), Bedford VA (no longer exists), Federal PRecinct RI,
#STATEWIDE writein CT, ,and ME UOCAVA 
View(miss_counties)
###let's get pct present 
nrow(county_results2)
county_results2 <- subset(county_results2, is.na(total2020)==F)
nrow(leip_counties)
county_results2$pct2020 <- (county_results2$total2020/county_results2$total2vote)*100
summary(county_results2$pct2020)
###looks like there is still a lot of missing data in the Leip data. Let's check what's going on. 
below2016 <- subset(county_results2,pct2020 < 90 | is.na(pct2020)==T )
View(below2016)
###With updated data, the only missing states are now CA, NY, and IL. 

###########################################
#### Let's bind the data here for the missing states and clean names 
##Read in the NBC data: 
leip_others <- read_xlsx("results/missing_leip_data2.xlsx",sheet = "all")
#leip_ma <- read_xlsx("results/missing_leip_data2.xlsx",sheet = "ma")
#leip_me <- read_xlsx("results/missing_leip_data2.xlsx",sheet = "me")
#leip_ak <- read_xlsx("results/missing_leip_data2.xlsx",sheet = "ak")

##merging on the FIPS 
county_dict <- subset(county_results, select=c(state_po,county,FIPS ))
leip_others$county <- str_to_upper(leip_others$county)
leip_others <- merge(leip_others, county_dict, by.x=c("state","county"),by.y=c("state_po" ,"county"), all.x=T)
###WE will now want to manually assign fips for following :
leip_others$FIPS[leip_others$state=="AR" & leip_others$county=="SAINT FRANCIS"] <- "05123"
leip_others$FIPS[leip_others$state=="IL" & leip_others$county=="DEWITT"] <- "17039"
leip_others$FIPS[leip_others$state=="IL" & leip_others$county=="JODAVIESS"] <- "17085"
leip_others$FIPS[leip_others$state=="NY" & leip_others$county=="BROOKLYN"] <- "36047"
leip_others$FIPS[leip_others$state=="NY" & leip_others$county=="MANHATTAN"] <- "36061"
leip_others$FIPS[leip_others$state=="NY" & leip_others$county=="SAINT LAWRENCE"] <- "36089"
leip_others$FIPS[leip_others$state=="NY" & leip_others$county=="STATEN ISLAND"] <- "36085"
leip_others$total2020 <- leip_others$biden+leip_others$trump
###good, everything is fixed. 


###let's now drop the other data from the leip data 
leip_counties2 <- subset(leip_counties, state_po!="CA" & state_po!="IL" & state_po!="NY" )
colnames(leip_others)[colnames(leip_others)=="state"] <- "state_po"
leip_others$FIPS[leip_others$state_po=="IL" & leip_others$county=="JODAVIESS"] <- "17085"
leip_others$st_fips <- substr(leip_others$FIPS,1,2)
###good, now let's get the data bound
colnames(leip_others)[colnames(leip_others)=="trump"] <- "trump2020"
colnames(leip_others)[colnames(leip_others)=="biden"] <- "biden2020"
leip_counties2 <- subset(leip_counties2, select=-c(FIPS2010,Geographic.Subtype))
leip_counties2 <- rbind(leip_counties2,leip_others )
###good, now we should have the complete data to merge onto 
county_results2 <- merge(county_results, leip_counties2, by=c("FIPS","state_po"),all.x=T )
sum(is.na(county_results2$biden2020))#47 missing 


###checking for returns under 90% of 2016
county_results2$pct2020 <- (county_results2$total2020/county_results2$total2vote)*100
below2016 <- subset(county_results2,pct2020 < 90 | is.na(pct2020)==T )
View(below2016)###Still the case that NYC associated counties have yet to fully report all of their results. 












######################################333

###55 counties missing. These are AR, CA, IL, MA, ME, NY, and OH 
table(below2016$state_po)

county_results2 <- subset(county_results2, is.na(FIPS)==F)
##find out where duplicates arise 
county_results2$dum=1 
county_results2 <- county_results2 %>% group_by(FIPS) %>% mutate(dup1=sum(dum))
county_results2dups <- subset(county_results2, dup1>1)#no duplicates

county_results2 <- subset(county_results2, pct2020 <= 1000 )
summary(county_results2$pct2020)


View(county_results2)
###let's see if we can get the data for missing states 
county_results_missing <- subset(county_results2, pct2020 < 90 | is.na(pct2020)==T)
View(county_results_missing)#473 entries. Let's check by state 
table(county_results_missing$state_po) # not enough at all for ME , NH, 
###Let's get CA, IL, IN, MA, MD, ME, MI, MS, NH, NJ, NY, PA, VA, WA, WI 
##Ah well, did what I could 
county_results2$time_stamp <- Sys.time()
county_results2master <- merge(county_results2, county_metro,by.x="FIPS",by.y="Geo_FIPS" )
###ok. Now that we have these data , we should find out the state aves and such, as Charles requested 
View(county_results2master)
county_results2master$dem_pct2020 <- (county_results2master$biden2020/(county_results2master$biden2020+county_results2master$trump2020))*100
county_results2master <- county_results2master %>% group_by(state_po) %>% mutate(state2016dem_ave = mean(dem_pct,na.rm=T))
county_results2master <- county_results2master %>% group_by(state_po) %>% mutate(state2020dem_ave = mean(dem_pct2020,na.rm=T))
###now let's get deviation from the mean
county_results2master$deviance2016 <- county_results2master$dem_pct - county_results2master$state2016dem_ave
county_results2master$deviance2020 <- county_results2master$dem_pct2020 - county_results2master$state2020dem_ave
##now the difference 
county_results2master$deviance_change <- county_results2master$deviance2020 - county_results2master$deviance2016

### let's save here 
saveRDS(county_results2master, "county_results2master.rds")
###now let's get the aves
summary(county_results2master$deviance_change)
##ok, let's do the collapse here for totals 

county_metro_sums2 <- county_results2master %>% group_by(metro_type) %>% 
  summarise(total2016vote = sum(total2vote,na.rm=T), total2020vote=sum(total2020,na.rm=T), democrat2016=sum(democrat,na.rm=T),
            democrat2020=sum(biden2020,na.rm=T))
county_metro_sums2$dem2016pct = (county_metro_sums2$democrat2016/county_metro_sums2$total2016vote)*100
county_metro_sums2$dem2020pct = (county_metro_sums2$democrat2020/county_metro_sums2$total2020vote)*100
##now chg in pct 
county_metro_sums2$dem_pct_chg <- county_metro_sums2$dem2020pct-county_metro_sums2$dem2016pct 
county_metro_sums2$metro_factor <- factor(county_metro_sums2$metro_type, levels=c("Large Metro","Medium Metro","Small Metro",
                                                                                 "Micro","Noncore"))
summary(county_metro_sums2$dem_pct_chg)
###plot for county chg 
natl_metro_chgplot_count <- ggplot(county_metro_sums2, aes(y=dem_pct_chg,x=metro_factor,fill=metro_factor)) + 
  geom_bar(position="stack", stat="identity") + theme_minimal() + 
  scale_fill_manual(values = medsl_brands[c(1:4,6)],drop=F)  +  scale_y_continuous( limits = c(-2,2))  +
  labs(title="Democratic Vote Share Change by Metro Designation",x="Metro Type",y="Democratic vote % change", fill="Metro Type",
       caption = paste0(caption_date ,sep=" ", "\nData not normalized by state averages", sep=" ",
                        "\nMetro areas categorized via National Center for Health Statistics coding.")) +
  theme(plot.caption = element_text(hjust=0),title = element_text(size = rel(1.2), family="Styrene B")) 
natl_metro_chgplot_count
#### collapse here for the %s 
county_metro_aves <- county_results2master %>% group_by(metro_type) %>% 
  summarise(vote_sum=sum(total2020,na.rm=T), mean2020=weighted.mean(deviance2020,total2020,na.rm=T), 
            mean2016=weighted.mean(deviance2016,total2vote,na.rm=T), 
            mean_change=weighted.mean(deviance_change,total2020,na.rm=T))
# 139,616,752 present in data 
View(county_metro_aves)

###setting factors 
county_metro_aves$metro_factor <- factor(county_metro_aves$metro_type, levels=c("Large Metro","Medium Metro","Small Metro",
                                                                                "Micro","Noncore"))

natl_metro_chgplot <- ggplot(county_metro_aves, aes(y=mean_change,x=metro_factor,fill=metro_factor)) + 
  geom_bar(position="stack", stat="identity") + theme_minimal() + 
  scale_fill_manual(values = medsl_brands[c(1:4,6)],drop=F)  +  scale_y_continuous( limits = c(-2,2))  +
  labs(title="Democratic Vote Share Change by Metro Designation",x="Metro Type",y="Democratic vote % change", fill="Metro Type",
       caption = paste0(caption_date ,sep=" ", "\nData normalized by state averages", sep=" ",
                        "\nMetro areas categorized via National Center for Health Statistics coding."))+
  theme(plot.caption = element_text(hjust=0),title = element_text(size = rel(1.2), family="Styrene B")) 
natl_metro_chgplot
##good. 
ggsave(paste0("natl_metro_chgplot_count",sep="", ".png"), plot = natl_metro_chgplot_count, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)
ggsave(paste0("natl_metro_chgplot",sep="", ".png"), plot = natl_metro_chgplot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)

####now lets create the dm pct corr plot 
county_results2master$metro_factor <- factor(county_results2master$metro_type, levels=c("Large Metro","Medium Metro","Small Metro",
                                                                                  "Micro","Noncore"))
large_metro <- subset(county_results2master, metro_type=="Large Metro")
large_metro_neg <- subset(large_metro, dem_pct_chg < 0)
large_metro_neg$county.x
(large_metro_neg[c(5,4,90)])

View(large_metro_neg)
# Fulton: 2020 = 72.66809, 2016 = 71.60720
# Cobb: 2016 = 51.15148, 2020= 56.704305
#Gwinnett 2020= 59.04370, 2016 = 53.059954
county_results2master$dem_pct_chg <- county_results2master$dem_pct2020 - county_results2master$dem_pct
ga_sub <- subset(county_results2master, state_po=="GA")
View(county_results2master)
names(county_results2master)
demo_results_plotpt_all <- ggplot(county_results2master,aes(y=dem_pct2020,x=dem_pct,size=total2020,color=metro_factor)) +
  geom_point(alpha=0.4) + scale_color_manual(values = medsl_brands[c(1:4,6)],drop=F) + theme_minimal() +
  guides(size=FALSE)  + labs(title="Correlation between Democratic vote share",x="Clinton %",y="Biden %", color="Metro Type",
                             caption = paste0(caption_date , "\nMetro areas categorized via National Center for Health Statistics coding."))+
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  theme(plot.caption = element_text(hjust=0),title = element_text(size = rel(1.2), family="Styrene B"))+
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5) +
  geom_text(aes(x=63.67511, y=53.68786, label="Miami"),  size=5, color="black",alpha=0.4) +
  geom_text(aes(x=53.059954, y=59.04370, label="Gwinnett"),  size=5, color="black", alpha=0.4)
demo_results_plotpt_all
ggsave(paste0("demo_results_plotpt_all",sep="", ".png"), plot = demo_results_plotpt_all, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)
county_results2masterb <- subset(county_results2master, is.na(dem_pct2020)==F)
nrow(county_results2masterb)
sum(is.na(county_results2master$dem_pct2020))
cor(x = county_results2masterb$dem_pct, y=county_results2masterb$dem_pct2020) # cor of 0.9794016

###save data 
setwd("monkey_cage")
saveRDS(county_results2master, "county_results2master.rds")
saveRDS(large_metro, "large_metro_data.rds")

####let's do a basic analysis of correlation 
names(county_results2masterb)
county_results2masterb$hispanic_pct_squared <- county_results2masterb$hispanic_pct^2
hispanic_trump_model <- lm(dem_pct_chg ~ hispanic_pct+ hispanic_pct_squared + duncan_index + median_income + as.factor(metro_type) + as.factor(st_fips),
                           data=county_results2masterb, weights = total2020 )
summary(hispanic_trump_model)
coef(hispanic_trump_model)[1:8]

(67.966677*6.509005e-02) - (2.510818e-03*67.966677)

###let's get null model w/states alone 
trump_null_model <- lm(dem_pct_chg ~ as.factor(st_fips),
                       data=county_results2masterb )

###takes too long to run the ggplot, so let's just do a correlation plot 

demo_results_plotpt <- ggplot(county_results2masterb,aes(y=dem_pct_chg,x=hispanic_pct,size=total2020,color=metro_factor)) +
  geom_point(alpha=0.6) + scale_color_manual(values = medsl_brands[c(1:4,6)],drop=F) + theme_minimal() +
  guides(size=FALSE)  + labs(title="County Correlation between Democratic Change",x="Latino %",
                             y="Democratic Percentage Point Change", color="Metro Type",
                             caption = paste0(caption_date , "\nMetro areas categorized via National Center for Health Statistics coding."))+
  
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + theme(plot.caption = element_text(hjust=0)) +
  geom_text(aes(x=67.966677, y=-11.4532205, label="Miami"),  size=5, color="black",alpha=0.4)  + 
  theme(plot.caption = element_text(hjust=0),title = element_text(size = rel(1.2), family="Styrene B"))
  
demo_results_plotpt


ggsave(paste0("democrat_latino_corr",sep="", ".png"), plot = demo_results_plotpt, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)

