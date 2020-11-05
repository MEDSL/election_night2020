###################################################################################
####################### Election Results CleanR #################################
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
county_results$dem_pct <- (county_results$democrat/(county_results$democrat+county_results$republican+county_results$other))*100
county_results <- as.data.frame(county_results)
county_results$total_vote <- county_results$democrat + county_results$republican + county_results$other
county_results <- subset(county_results, year==2016)
county_results <- subset(county_results, state_po=="OH")
county_results$county <- str_to_upper(county_results$county)
###import election 2020 results 
list.files()
oh2020 <- read_xlsx("oh_results.xlsx", sheet="11051230pm")
oh2020$county <- str_to_upper(oh2020$county)
oh2020$dem_pct2020 <- (oh2020$biden/(oh2020$biden+oh2020$trump))*100
oh2020$total2020 <- oh2020$biden+oh2020$trump
county_results <- merge(county_results,oh2020, by="county",all.x=T )


##Step 2: Specify county FIPs field 

county_field <- readline(prompt="Enter name of column field with the county FIPs code, length of 5 : ")
county_results[,which(colnames(county_results)==county_field)] <- 
  str_pad(county_results[,which(colnames(county_results)==county_field)], width=5,side="left",pad="0")
##merging data 
county_metro <- subset(county_metro, Geo_STUSAB=="oh")
county_metro2 <- merge(county_metro, county_results, by.x="Geo_FIPS", by.y=county_field, all.x=T)
county_metro2$metro_factor <- factor(county_metro2$metro_type, levels=c("Large Metro","Medium Metro","Small Metro",
                                                                                "Micro","Noncore"))


##step 4: Select democrat and total vote fields 
dem_count_field <- readline(prompt="Enter name of column field with the Democratic 2 party vote share data, as count : ")
total_vote_field <- readline(prompt="Enter name of column field with the total 2 party vote share data, as count : ")
dem_tally_num <- which(colnames(county_metro2)==dem_count_field)
total_tally_num <- which(colnames(county_metro2)==total_vote_field)
###adding fields just in case 
county_metro2$dem_total <- county_metro2[,dem_tally_num]
county_metro2$total_vote_field <- county_metro2[,total_tally_num]

#### Now, the following are plots that can be created by demographics and the like. 
county_metro_sums <- county_metro2 %>% group_by(metro_type) %>% 
  summarise(dem_vote=sum(dem_total,na.rm=T),
            total_vote=sum(total_vote_field,na.rm=T))
county_metro_sums$dem_pct <- (county_metro_sums$dem_vote/county_metro_sums$total_vote)*100
###setting factors 
county_metro_sums$metro_factor <- factor(county_metro_sums$metro_type, levels=c("Large Metro","Medium Metro","Small Metro",
                                                                                "Micro","Noncore"))

natl_metro_plot <- ggplot(county_metro_sums, aes(y=dem_pct,x=metro_factor,fill=metro_factor)) + 
  geom_bar(position="stack", stat="identity") + theme_minimal() + 
  scale_fill_manual(values = medsl_brands[c(1:4,6)],drop=F)  +  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100))  +
  labs(title="Democratic Vote Share by Metro Designation, GA 2020",x="Metro Type",y="Democratic vote %", fill="Metro Type",
       caption = paste0(caption_date , "\nMetro areas categorized via National Center for Health Statistics coding.")) +
  theme(plot.caption = element_text(hjust=0)) 
natl_metro_plot

###Select file title name 
file_plot_name <- readline(prompt="Enter file name of plot : ")
ggsave(paste0(file_plot_name,sep="", ".png"), plot = natl_metro_plot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600) 



####################### Now scatter plot for demographics ################################################
####Note: Subset as appropriate here: 
decision_to_sub <- readline(prompt="Subset data? Y/N: ")
if(decision_to_sub=="Y"){
  state_choice <- readline(prompt="Select state via postal code : ")
  state_choice <- str_to_upper(state_choice)
  county_metro2 <- subset(county_metro2, state_po==state_choice)
  rows_num <- as.character(nrow(county_metro2))
  msg_state <- paste0("Chose to subset data to", sep=" ",unique(county_metro2$state),sep=". ", "There are a total of ", sep=" ",
                      rows_num, "observations within the subsetted data.")
  print(msg_state)
}else{
  row_nums2 <- as.character(nrow(county_metro2))
  msg_state2 <- paste0("Chose not to subset. Will therefore plot", sep=" ", row_nums2,sep=" ", "counties as observations.")
  print(msg_state2)
}


names(county_metro2)
variable_selection_x <- readline(prompt="Enter variable name to plot as x-axis : ")
variable_selection_y <- readline(prompt="Enter variable name to plot as y-axis : ")
size_selection <- readline(prompt="Enter variable name to size plot (i.e. pop10 for county total pop.) : ")

###titles 
x_title <- readline(prompt="Enter title for plot x-axis : ")
y_title <- readline(prompt="Enter title for plot y-axis : ")
main_title <- readline(prompt="Enter title for plot : ")
file_demo_plot <- readline(prompt="Enter file name for plot : ") 
###assigning vars to new columns to ease reading 
county_metro2$var_x <- county_metro2[,which(colnames(county_metro2)==variable_selection_x)]
county_metro2$var_y <- county_metro2[,which(colnames(county_metro2)==variable_selection_y)]
county_metro2$var_size <- county_metro2[,which(colnames(county_metro2)==size_selection)]

if(max(county_metro2$var_x,na.rm=T) > 100 | min(county_metro2$var_x,na.rm=T) < 0 ){
  print("Warning: The x variable is outside the 0 - 100 range. Please modify, or be sure to manually modify the ggplot code.")
}else{
  print("X  values appear fine.")
}

if(max(county_metro2$var_y,na.rm=T) > 100 | min(county_metro2$var_y,na.rm=T) < 0 ){
  print("Warning: The y variable is outside the 0 - 100 range. Please modify, or be sure to manually modify the ggplot code.")
}else{
  print("y  values appear fine.")
}


###plot here; set to group by metro type 
demo_plot <- ggplot(county_metro2,aes(y=var_y,x=var_x,size=var_size,color=metro_factor)) +
  geom_point(alpha=0.4) + scale_color_manual(values = medsl_brands[c(1:4,6)],drop=F) + theme_minimal() +
  guides(size=FALSE)  + labs(title=main_title,x=x_title,y=y_title, color="Metro Type",
                            caption = paste0(caption_date , "\nMetro areas categorized via National Center for Health Statistics coding."))+
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + theme(plot.caption = element_text(hjust=0)) 

demo_plot
ggsave(paste0(file_demo_plot,sep="", ".png"), plot = demo_plot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600) 
county_metro2$biden2016dif <- county_metro2$dem_pct2020-county_metro2$dem_pct
summary(county_metro2$biden2016dif)
View(county_metro2)

###let's now get vote total diff 
county_metro2$dem_vote_diff <- county_metro2$biden - county_metro2$democrat
summary(county_metro2$dem_vote_diff)
democrat_diff_plot <- ggplot(county_metro2,aes(y=dem_vote_diff,x=var_x,size=var_size,color=metro_factor,label=county.y)) +
  geom_text(alpha=0.6) + scale_color_manual(values = medsl_brands[c(1:4,6)],drop=F) + theme_minimal() +
  guides(size=FALSE)  + labs(title="Michigan county change in Democratic vote share \nfrom 2016, by race (11/04/2020 3:45 PM)",
                             x=x_title,y="Change in Democratic Vote share", color="Metro Type",
                             caption = paste0(caption_date , "\nMetro areas categorized via National Center for Health Statistics coding."))+
  scale_y_continuous(label=comma, limits = c(-50000,100000)) + 
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + theme(plot.caption = element_text(hjust=0)) 
ggsave(paste0("change_in_dem_vote_mi_345pm",sep="", ".png"), plot = democrat_diff_plot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600) 
democrat_diff_plot


demo_results_plot <- ggplot(county_metro2,aes(y=dem_pct2020,x=dem_pct,size=var_size,color=metro_factor,label=county.y)) +
  geom_text(alpha=0.6) + scale_color_manual(values = medsl_brands[c(1:4,6)],drop=F) + theme_minimal() +
  guides(size=FALSE)  + labs(title="Ohio Correlation between Democratic vote share",x="Clinton %",y="Biden %", color="Metro Type",
                             caption = paste0(caption_date , "\nMetro areas categorized via National Center for Health Statistics coding."))+
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + theme(plot.caption = element_text(hjust=0)) +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5) 

demo_results_plot
###let's create point version 
demo_results_plotpt <- ggplot(county_metro2,aes(y=dem_pct2020,x=dem_pct,size=var_size,color=metro_factor)) +
  geom_point(alpha=0.6) + scale_color_manual(values = medsl_brands[c(1:4,6)],drop=F) + theme_minimal() +
  guides(size=FALSE)  + labs(title="Ohio Correlation between Democratic vote share",x="Clinton %",y="Biden %", color="Metro Type",
                             caption = paste0(caption_date , "\nMetro areas categorized via National Center for Health Statistics coding."))+
  scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + 
  scale_x_continuous(labels = function(x) paste0(x, "%"), limits = c(0,100)) + theme(plot.caption = element_text(hjust=0)) +
  geom_abline(intercept = 0, slope = 1, color="red", 
              linetype="dashed", size=1.5) 
demo_results_plotpt





ggsave(paste0("oh_dem_pct_correlation",sep="", ".png"), plot = demo_results_plot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600) 
ggsave(paste0("oh_dem_pct_correlationpt",sep="", ".png"), plot = demo_results_plotpt, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600) 
