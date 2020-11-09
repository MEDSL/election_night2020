###3 alcona county analysis #####
library(ggplot2)
setwd("/Users/jessesmac/Downloads/")

caption_date <- paste0("Graph Source: MIT Elections Data and Science Lab\nGraph date:",
                                       sep=" ", format(Sys.Date(),format="%m/%d/%Y"))
medsl_brands <- c("#3791FF","#59CBF5","#C0BA79","#F6573E","#156DD0","#C72654","#FF6878")

#### making graphs fo antrim ##
historical<- read.csv("/Users/jessesmac/Downloads/GitHub-Tutorial-master/election_night2020/context/historical_elections/countypres_2000-2016long.csv")

View(historical)
historical<- historical[historical$party == "republican" & historical$state == "Michigan" & historical$year == 2016,]

historical<- historical[complete.cases(historical),]


historical$rep_precent16<- historical$candidatevotes/ historical$totalvotes

data_2020<- read.csv("/Users/jessesmac/Downloads/2020-11-04 15:58:47cbsdata.csv", stringsAsFactors = F)

data_2020<- data_2020[data_2020$Party == "Republican",]
data_2020$rep_percent20<- data_2020$Votes / data_2020$totalvotes

data_2020$rep_percent16<- historical$rep_precent16

repplot<- ggplot(data_2020, aes(y = rep_percent20, x = rep_percent16, size = Votes, color = Party)) +
  geom_point(alpha=0.4)+  ggtitle("County Level Republican Vote Share, Michigan") +
  xlab("2016 Vote Share") + guides(size=FALSE, color = F)+ylab("2020 Vote Share") + theme_minimal() +
  labs(caption = caption_date)

ggsave(paste0("rep_share",sep="", ".png"), plot = repplot, scale = 1,
       width = 9, height = 6, units = c("in"), dpi = 600) 

