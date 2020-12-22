###################################################################################
####################### National Election Results HexmapR #################################
################################################################################
library(ggplot)
library(sp)
library(dplyr)
library(stringi)
library(stringr)
options(stringsAsFactors = FALSE)
################################################################################
medsl_brands <- c("#3791FF","#59CBF5","#C0BA79","#F6573E","#156DD0","#C72654","#FF6878")
medsl_purple <- c("#DDDBFB", "#B1AAFB","#7D76C7","#635E99",  "#4E4A81")
medsl_blues <- c("#9FDDF3","#00BAFF","#3791FF","#04448B","#0B2E4F")
#### function for shadow text 
shadowtext <- function(x, y=NULL, labels, col='white', bg='black',
                       theta= seq(pi/4, 2*pi, length.out=8), r=0.1, ... ) {
  
  xy <- xy.coords(x,y)
  xo <- r*strwidth('x')
  yo <- r*strheight('x')
  
  for (i in theta) {
    text( xy$x + cos(i)*xo, xy$y + sin(i)*yo, labels, col=bg, ... )
  }
  text(xy$x, xy$y, labels, col=col, ... )
}


######
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
list.files()

###loading in the hexmap here
hexmap <- readRDS("context/maps/hexmap.Rdata")

### Step 1: load in national election results, nyt 

nyt_counties_final <- read.csv("results/nyt_counties_final12152020.csv")
nyt_counties_final$fips <- str_pad(nyt_counties_final$fips,width=5,pad="0",side="left")
nyt_counties_final$st_fips <- substr(nyt_counties_final$fips, 1,2)
nyt_state_final <- nyt_counties_final %>% group_by(state,st_fips) %>% 
  summarise(bidenj=sum(bidenj),trumpd=sum(trumpd),total.votes=sum(total.votes),abs_trumpd=sum(abs_trumpd),abs_bidenj=sum(abs_bidenj))
nyt_state_final$abs_pct <- ((nyt_state_final$abs_bidenj+nyt_state_final$abs_trumpd)/(nyt_state_final$total.votes))*100
nyt_state_final$biden_abs_pct <- (nyt_state_final$abs_bidenj/nyt_state_final$bidenj)*100
nyt_state_final$trump_abs_pct <- (nyt_state_final$abs_trumpd/nyt_state_final$trumpd)*100

###Let's look at what the quantiles are
quantile(nyt_state_final$abs_pct, seq(0,1,by=0.05))
quantile(nyt_state_final$biden_abs_pct,seq(0,1,by=0.05))
quantile(nyt_state_final$trump_abs_pct,seq(0,1,by=0.05))

###Read in state codes 
state_codes <- read.csv("context/merge_on_statecodes.csv")
state_codes$state <- str_to_upper(state_codes$state)
nyt_state_final <- merge(nyt_state_final, state_codes, by="state")
nyt_state_final$color_biden <- medsl_purple[1]
nyt_state_final$color_biden[nyt_state_final$biden_abs_pct > 20 & nyt_state_final$biden_abs_pct <= 40] <- medsl_purple[2]
nyt_state_final$color_biden[nyt_state_final$biden_abs_pct > 40 & nyt_state_final$biden_abs_pct <= 60] <- medsl_purple[3]
nyt_state_final$color_biden[nyt_state_final$biden_abs_pct > 60 & nyt_state_final$biden_abs_pct <= 80] <- medsl_purple[4]
nyt_state_final$color_biden[nyt_state_final$biden_abs_pct > 80] <- medsl_purple[5]
###Now let's get the colors for trump
nyt_state_final$color_trump <- medsl_purple[1]
nyt_state_final$color_trump[nyt_state_final$trump_abs_pct > 20 & nyt_state_final$trump_abs_pct <= 40] <- medsl_purple[2]
nyt_state_final$color_trump[nyt_state_final$trump_abs_pct > 40 & nyt_state_final$trump_abs_pct <= 60] <- medsl_purple[3]
nyt_state_final$color_trump[nyt_state_final$trump_abs_pct > 60 & nyt_state_final$trump_abs_pct <= 80] <- medsl_purple[4]
nyt_state_final$color_trump[nyt_state_final$trump_abs_pct > 80] <- medsl_purple[5]
###All 
nyt_state_final$color_all <- medsl_purple[1]
nyt_state_final$color_all[nyt_state_final$abs_pct > 20 & nyt_state_final$abs_pct <= 40] <- medsl_purple[2]
nyt_state_final$color_all[nyt_state_final$abs_pct > 40 & nyt_state_final$abs_pct <= 60] <- medsl_purple[3]
nyt_state_final$color_all[nyt_state_final$abs_pct > 60 & nyt_state_final$abs_pct <= 80] <- medsl_purple[4]
nyt_state_final$color_all[nyt_state_final$abs_pct > 80] <- medsl_purple[5]

###merge onto hexmap 
hexmap2  <- merge(hexmap ,nyt_state_final, by.x="iso3166_2", by.y="state_po")

###Map creation 
###projecting map 
hexmap2<- spTransform(hexmap2, CRS=CRS("+init=epsg:3395")) #ignore warning. 

##stats from mcdonald 
65642049/101453111

##actual map creation 
jpeg(paste0("hexmap_all_abs" ,sep=".","jpeg") ,width=9,height = 6, units = "in", res=600)
plot(hexmap2,col=hexmap2$color_all,main= "Non-election day proportion of all ballots cast" )
shadowtext(coordinates(hexmap2)[,1],coordinates(hexmap2)[,2],  hexmap2$iso3166_2)
legend("top", 
       legend = c("0 to 20%", "20 to 40%", "40 to 60%", "60 to 80%", "80% +"), 
       fill = medsl_purple, title = "Non-elec Day %",ncol=2, cex=0.6,bty = "n")
dev.off()


#natl_results <- read.csv("context/historical_elections/1976-2016-president.csv")
#natl_results <- subset(natl_results, party=="democrat" & year==2016)
#natl_results$dem_pct <- (natl_results$candidatevotes/natl_results$totalvotes)*100
#natl_results <- subset(natl_results, select=c(state_po,candidatevotes,totalvotes,dem_pct))
#natl_results <- natl_results %>% group_by(state_po) %>% slice(which.max(dem_pct))


### ###step 2: Choose breaks for Dem vote share of two party vote 
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


###Step 4: Merge on data 
state_po_field <- readline(prompt="Enter name of column field with the state PO field, : ")
hexmap2 <- merge(hexmap, natl_results, by.x="iso3166_2", by.y=state_po_field, all.x=T)


###Step 5: name field with election data, the two party vote share 
party_field <- readline(prompt="Enter name of column field with the Democratic 2 party vote share data, as percent : ")

color_df_fxn <- function(dataframe1, party_field, color_vec, dem_breaks_vec){
  col_nump <- which(colnames(dataframe1)==party_field)
  asn_seq <- seq(1,length(color_vec), by=1 )
  colnames(dataframe1@data)[col_nump] <- "dem_pct"
  dataframe1$color <- ""
  for(i in 1:length(asn_seq)){
    if(i==1){
      dataframe1$color[dataframe1$dem_pct < dem_breaks_vec[i]] <- color_vec[i]
    }else{
      dataframe1$color[dataframe1$dem_pct < dem_breaks_vec[i] & dataframe1$dem_pct >=  dem_breaks_vec[i-1] ] <- color_vec[i]
    }
  }
  dataframe1$color[dataframe1$color==""] <- "gray70"
  return(dataframe1)
}


hexmap2 <- color_df_fxn(hexmap2, "dem_pct", party_color_spectrum, dem_breaks )


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

###Step 6: Choose Map and file title:
plot_title <- readline(prompt="Enter title of plot : ")
file_plot_name <- readline(prompt="Enter file name of plot : ")

###projecting map 
hexmap2<- spTransform(hexmap2, CRS=CRS("+init=epsg:3395")) #ignore warning. 


##actual map creation 
jpeg(paste0(file_plot_name,sep=".","jpeg") ,width=9,height = 6, units = "in", res=600)
plot(hexmap2,col=hexmap2$color,main=plot_title)
shadowtext(coordinates(hexmap2)[,1],coordinates(hexmap2)[,2],  hexmap2$iso3166_2)
legend("top", 
       legend = legend.text, 
       fill = party_color_spectrum, title = "Dem. vote share %",ncol=2, cex=0.6,bty = "n")
dev.off()

