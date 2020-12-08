###################################################################################
####################### Monkey Cage Election Results CleanR #################################
################################################################################
library(ggplot)
library(dplyr)
library(stringi)
library(stringr)
options(stringsAsFactors = FALSE)
################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
caption_date <- paste0("Graph Source: MIT Elections Data and Science Lab\nGraph date:",
                       sep=" ", format(Sys.Date(),format="%m/%d/%Y"))
list.files()
medsl_brands <- c("#3791FF","#59CBF5","#C0BA79","#F6573E","#156DD0","#C72654","#FF6878")

####This script will be used to create ggplot style figures for election night for counties. 


#Step 1: Read in data 
county_metro <- read.csv("F:/MEDSL/election_night2020/context/acs_demos/complete_county_data.csv")
county_metro$Geo_FIPS <- str_pad(county_metro$Geo_FIPS, width=5,side="left",pad="0")


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
leip_counties <- read.csv("results/leip_natl_data.csv")
leip_counties$FIPS <- str_pad(leip_counties$FIPS, width=5,side="left",pad="0")
leip_counties$st_fips <- substr(leip_counties$FIPS, 1,2)
sort(unique(leip_counties$Geographic.Subtype))
table(leip_counties$st_fips, leip_counties$Geographic.Subtype )
###non-county states: AK, LA (parishes; should be fine), MD (1 city), MO (1 city), NV (1 city), VA ( 38 cities), NV good. Should be good with 
#VA with name changes. Will need to collapse data for MA 
problem_leip <- subset(leip_counties, st_fips=="24" | st_fips=="29" | st_fips=="32" | st_fips=="51" )
###Baltimore needs to be recoded as city if fip is 24510 
leip_counties$Geographic.Name[leip_counties$Geographic.Name=="Baltimore" & leip_counties$Geographic.Subtype=="City" ] <- "Baltimore City"
leip_counties$Geographic.Name[leip_counties$Geographic.Name=="St. Louis" & leip_counties$Geographic.Subtype=="City" ] <- "St. Louis City"

nrow(leip_counties)
head(leip_counties)
##subset data 
leip_counties <- subset(leip_counties, select=c(FIPS,Geographic.Name, Total.Vote, Joseph.R..Biden.Jr., Donald.J..Trump))
leip_counties$FIPS <- str_pad(leip_counties$FIPS, width=5,side="left",pad="0")
leip_counties$Geographic.Name <- str_to_upper(leip_counties$Geographic.Name)
names(leip_counties)
colnames(leip_counties)[2:5] <- c("geog_name" ,"total2020", "biden2020","trump2020")
###let's subset out the data here, then merge on 
leip_others <- read_xlsx("results/missing_leip_data.xlsx",sheet = "all")
leip_ma <- read_xlsx("results/missing_leip_data.xlsx",sheet = "ma")
length(which(county_results$state_po=="MA"))# 14 counties; need to simplify 
##reading in MA towns 
ma_towns <- read.csv("context/acs_demos/ma_towns.csv")
names(ma_towns)
ma_towns <- subset(ma_towns, select=c(Geo_NAME,Geo_FIPS))
ma_towns$Geo_NAME <- str_to_upper(ma_towns$Geo_NAME)
leip_ma<- str_to_upper(leip_ma$county)
###let's create dictionary 
ma_towns2 <- merge(ma_towns,leip_ma)
ma_towns_acs <- ma_towns$Geo_NAME
ma_towns_nbc <- leip
match_list <- list()
town_vec <- ma_towns_nbc
acs_town_vec <- ma_towns_acs 
for(i in 1:length(town_vec)){
  for(j in 1:length(acs_town_vec)){
    temp_store <- agrep(town_vec[i],acs_town_vec[j])
    if (length(temp_store) > 0) {
      match_list[[length(match_list) + 1]] <- c(town_vec[i], 
                                                acs_town_vec[j])
    }
  }
  
}
df <- data.frame(matrix(unlist(match_list), nrow=length(match_list), byrow=T),stringsAsFactors=FALSE)
colnames(df) <- c("elec_name","acs_name")
df$same_name = 0
df$same_name[df$elec_name==df$acs_name] <- 1
df <- df %>% group_by(elec_name) %>% mutate(town_match=sum(same_name))
df$acs_name2 <- str_remove(df$acs_name, " CITY")
df$acs_name2 <- str_remove(df$acs_name2, " TOWN")
df$same_name2 = 0
df$same_name2[df$elec_name==df$acs_name2] <- 1
df <- df %>% group_by(elec_name) %>% mutate(town_match=sum(same_name2))
summary(df$same_name2)
df2a <- subset(df, (same_name2==1))

nrow(df2a)
df2b <- subset(df, same_name2==0 &  town_match ==0)
df2 <- rbind(df2a,df2b)
###now merging 
leip_ma <- read_xlsx("results/missing_leip_data.xlsx",sheet = "ma")
leip_ma$county<- str_to_upper(leip_ma$county)

df3 <- merge(df2, ma_towns, by.x="acs_name",by.y="Geo_NAME")
df3 <- merge(df3, leip_ma, by.x="elec_name", by.y="county")
df3$county_fips <- substr(df3$Geo_FIPS,1,5)
ma_results <- df3 %>% group_by(county_fips) %>% summarise(trump=sum(trump,na.rm=T),biden=sum(biden,na.rm=T))
ma_results$total2020 <- ma_results$biden+ma_results$trump
###let's see if we can't merge the data 
head(leip_counties)
head(leip_others)
head(ma_results)
####let's exclude the relevant states here 
leip_counties$st_fips <- substr(leip_counties$FIPS, 1,2)
sort(unique(leip_others$state))
leip_counties <- subset(leip_counties,st_fips!="06" & st_fips!="17" & st_fips!="18" & st_fips!="24" & st_fips!="25" & 
                          st_fips!="34" & st_fips!="36" & st_fips!="42" & st_fips!="51" & st_fips!="53" & st_fips!="53" & st_fips!="55")
leip_counties <- subset(leip_counties, st_fips!="26" & st_fips!="28")
nrow(leip_counties)
leip_counties <- subset(leip_counties, select=-c(geog_name,total2020))
leip_counties$total2020 <- leip_counties$biden2020+leip_counties$trump2020
colnames(ma_results)[2:3] <- c("trump2020","biden2020")
colnames(ma_results)[1] <- c("FIPS")
ma_results$st_fips <- substr(ma_results$FIPS,1,2)
ma_results <- as.data.frame(ma_results)
head(ma_results)
head(leip_counties)
leip_counties2 <- rbind(leip_counties, ma_results)
###now let's get FIPS onto this 
head(county_results)
county_results_sub <- subset(county_results, select=c(FIPS, county, state_po))
head(leip_others)
leip_others <- as.data.frame(leip_others)
leip_others$county <- str_to_upper(leip_others$county)
###now let's merge 
head(leip_others)
leip_others$county[leip_others$county=="DE WITT" & leip_others$state=="IL"] <- "DEWITT"
leip_others$county[leip_others$county=="JO DAVIESS" & leip_others$state=="IL"] <- "JODAVIESS"
leip_others$county[leip_others$county=="JEFFERSON DAVIS" & leip_others$state=="MS"] <- "JEFF DAVIS"
leip_others$county[leip_others$county=="ST. LAWRENCE" & leip_others$state=="NY"] <- "SAINT LAWRENCE"
leip_others$county[leip_others$county=="MANHATTAN" & leip_others$state=="NY"] <- "NEW YORK"
leip_others$county[leip_others$county=="BROOKLYN" & leip_others$state=="NY"] <- "KINGS"
leip_others$county[leip_others$county=="STATEN ISLAND" & leip_others$state=="NY"] <- "RICHMOND"



leip_others2 <- merge(leip_others, county_results_sub, by.x=c("county","state"), by.y=c("county","state_po"),all.x=T)
leip_others2$total2020 <- leip_others2$biden+leip_others2$trump
leip_others2$st_fips <- substr(leip_others2$FIPS,1,2)
leip_others2 <- subset(leip_others2, select=-c(county,state))
colnames(leip_others2)[1:2] <- c("trump2020","biden2020")
###binding data here 
leip_counties2 <- rbind(leip_counties2, leip_others2)
nrow(leip_counties2)
saveRDS(leip_counties2,"leip_corrected1.rds")


##merge elec results here 
county_results2 <- merge(county_results, leip_counties2, by="FIPS",all.x=T )
###let's get pct present 
nrow(county_results2)
county_results2$pct2020 <- (county_results2$total2020/county_results2$total2vote)*100
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

