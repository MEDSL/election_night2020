###################################################################################
####################### Election Night Results CleanR Georgia #################################
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

######################## read          
state_nyt_gini <- readRDS('results/state_nyt_gini.rds')
nyt_counties2 <- readRDS("results/nyt15minute_county_data.rds")

###now let's plot the data, with other states grayed out for gini, and then the county pcts for GA
## grob values for plot text 
grob_biden <- grobTree(textGrob("Biden", x=0.8,  y=0.7, hjust=0,
                                gp=gpar(col=medsl_brands[1], fontsize=12, fontface="bold")))
grob_trump <- grobTree(textGrob("Trump", x=0.8,  y=0.6, hjust=0,
                                gp=gpar(col=medsl_brands[6], fontsize=12, fontface="bold")))
grob_title <- grobTree(textGrob("Lead in results", x=0.7,  y=0.8, hjust=0,
                                gp=gpar(col="black", fontsize=14, fontface="bold")))



###plot 
gini_state_plot24hrs <- ggplot(data=state_nyt_gini, aes(x=interval15min_num,y=gini_reported, group=state)) +
  geom_line(lwd=1.05,alpha=0.3, col="gray") +
  geom_line(data=subset(state_nyt_gini, state=="GEORGIA"), aes(x=interval15min_num,y=gini_reported), col=medsl_brands[1],alpha=0.8, size=1.2 ) +
  theme_minimal() + 
  theme(title = element_text(size = rel(1.2), family="Styrene B"), 
        plot.caption = element_text(hjust=0),panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45,vjust=0.5)) +
  scale_color_manual(values = medsl_brands[c(1,6)]) + 
  guides(colour = FALSE) +
  scale_x_continuous(breaks = seq(0,48,by=8), 
                     labels=c("0",  "2",  "4","6","8","10","12"), 
                     limits = c(0,48))  + ylim(0.0,1)+
  labs(title="Inequality in county reporting over time, by state",
       x="Hours from polls closing",y="Inequality in reports (gini coef.)",color="Lead",
       caption = paste0("Note: Highlighted line reflects trends for Georgia.", sep=" ", caption_date) ) 

gini_state_plot24hrs 
ggsave("results/plots/state_gini_ts_plot_party24hrs.png", plot = gini_state_plot24hrs, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600)
###Let's get the results for ga alone 
ga_gini <- subset(state_nyt_gini, state=="GEORGIA")
View(ga_gini)


###Let's get the gini summ stats by hour 
gini_sum <- state_nyt_gini %>% group_by(interval15min_num) %>% summarise(p25=quantile(gini_reported, 0.25), p50=quantile(gini_reported, 0.50),
                                                                         p75=quantile(gini_reported, 0.75),ave_rep=mean(gini_reported))
View(gini_sum)


