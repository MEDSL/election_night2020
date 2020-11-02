###################################################################################
####################### Election Results CleanR #################################
################################################################################
library(ggplot)
library(maps)
library(sp)
library(dplyr)
library(stringi)
library(stringr)
options(stringsAsFactors = FALSE)
################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
list.files()
county_cents <- readRDS("context/maps/counties_centroids.rds")


#### Step 0: Read in data 
master_data <- read.csv() #left blank; this is something the user needs to fille in 

#example 
#test_data <- readRDS("context/historical_elections/county_prez_wide.rds")
#test_data$dem_pct <- (test_data$democrat/(test_data$democrat+test_data$republican+test_data$other))*100


####This script will be used to create county maps for election night.
state_codes <- read.csv("context/merge_on_statecodes.csv")

##Step 1: Choose state: 
state_choice <- readline(prompt="Enter state abbreviation: ")
state_choice <- str_to_upper(state_choice)
state_choicedf <- subset(state_codes, state_po==state_choice)
if(nrow(state_choicedf)==0){
  print("State choice incorrectly specified. Choose again.")
}else{
  msg1 <- paste0("Congrats, you chose to map",sep=" ", state_choicedf$state)
  print(msg1)
}
###selecting county cents based on state 
county_cents_sub <- subset(county_cents, state_po==state_choice)

###step 2: Choose breaks for Dem vote share of two party vote 
dem_breaks <-  readline(prompt="Enter 5 breaks for dem vote share, not including 0 (space separated): ")
dem_breaks <- as.integer(strsplit(dem_breaks, " ")[[1]])

##Step 3 (automatic), will choose shades of red and blue 
gop_breaks <- dem_breaks[dem_breaks<50]
dem_breaks2 <- dem_breaks[dem_breaks>50]
even_break <- dem_breaks[dem_breaks==50]
##colors for GOP 
gop_colors <- colorRampPalette(c("#EDD0CB", "#8D2115"))(length(gop_breaks))
dem_colors <- colorRampPalette(c("#9FDDF3", "#0B2E4F"))(length(dem_breaks2))
even_color <- '#EBD600'
if(exists("even_break")){
  party_color_spectrum <- c(rev(gop_colors),even_color,dem_colors)
}else{
  party_color_spectrum <- c(rev(gop_colors),dem_colors)
}

###Step 4: name field with election data, the two party vote share 
party_field <- readline(prompt="Enter name of column field with the Democratic 2 party vote share data, as percent : ")

color_df_fxn <- function(dataframe1, party_field, color_vec, dem_breaks_vec){
  col_nump <- which(colnames(dataframe1)==party_field)
  asn_seq <- seq(1,length(color_vec), by=1 )
  colnames(dataframe1)[col_nump] <- "dem_pct"
  dataframe1$color <- ""
  for(i in 1:length(asn_seq)){
    if(i==1){
      dataframe1$color[dataframe1$dem_pct < dem_breaks_vec[i]] <- color_vec[i]
    }else{
      dataframe1$color[dataframe1$dem_pct < dem_breaks_vec[i] & dataframe1$dem_pct >=  dem_breaks_vec[i-1] ] <- color_vec[i]
    }
  }
  dataframe1$color[dataframe1$color==""] <- "gray70"
  return(as.data.frame(dataframe1))
}


###creating legend text 
# add a legend
for(i in 1:length(dem_breaks)){
  if(i==1){
    legend.text <- c(paste0("0",sep=" -< ", dem_breaks[i], sep="%" ))
  }else{
    temp_leg <- paste0(dem_breaks[i-1], sep=" -< ", dem_breaks[i], sep="%")
    legend.text <- c(legend.text, temp_leg)
  }
}
legend.text


##step 5: Specify the county fips field 
county_field <- readline(prompt="Enter name of column field with the county FIPs code, length of 5 : ")
master_data[,which(colnames(master_data)==county_field)] <- 
  str_pad(master_data[,which(colnames(master_data)==county_field)], width=5,side="left",pad="0")
county_cents_sub2 <- merge(county_cents_sub, master_data, by.x="GEOID", by.y=county_field, all.x=T)

#cents_coords <- SpatialPointsDataFrame(coords = spat_coords1, data = spdf,
#                                      proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
max_x <- max(county_cents_sub2$V1,na.rm=TRUE) 
min_x <- min(county_cents_sub2$V1,na.rm=TRUE) 
max_y <- max(county_cents_sub2$V2,na.rm=TRUE) 
min_y <- min(county_cents_sub2$V2,na.rm=TRUE) 

###Step 7: Create title 
#plot_title <- readline(prompt="Enter title of plot : ")
file_plot_name <- readline(prompt="Enter file name of plot : ")
colpos <- readline(prompt="Enter position of plot (left, bottomleft, topleft, right, ...) : ")
##mapping results here

jpeg(paste0(file_plot_name,sep=".","jpeg") ,width=9,height = 6, units = "in", res=600)
#map('county', state_choice , fill = TRUE, col = "white",ylim=c(min_y,max_y),mar=c(0,0,0,0))
maps::map('county', state_choicedf$state , fill = TRUE, col = "white", ylim=c(min_y-2,max_y+2),mar=c(2,2,2,2) )
points(county_cents_sub2$V1,county_cents_sub2$V2,pch=21,bg=county_cents_sub2$color,cex=county_cents_sub2$weight)
#legend("bottom", legend = legend.text, 
#     fill = shades, title = "Per 100k",cex=0.6,bty = "n",ncol=2,xpd=TRUE)
legend(colpos, legend = legend.text, 
       fill = party_color_spectrum, title = "Dem % of vote",cex=0.6,bty = "n",ncol=2,xpd = TRUE)
dev.off()
