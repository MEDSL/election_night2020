##############################################################################
############# Raw file CleanR for ACS Data ###################################
#############################################################################
library(dplyr)
library(stringi)
library(stringr)
options(stringsAsFactors = FALSE)
#####################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
list.files()
###reading in the social explorer acs data 
acs_raw <- read.csv("econ_acs_raw.csv")

#####fixing col names 
names(acs_raw)
colnames(acs_raw)[6:ncol(acs_raw)] <- c("pop3over","school_enrolled","school_not_enrolled","households","pop_under25k","pop25k_49k",
                                        "pop50k_74k","pop75k_99k","pop100kover","median_income","pov_denom","pov_under2q",
                                        "pov2q_3q","pov3q_4q","pov4q_6q","pov6q_8q","pov_over8q")
###now for percents 
acs_raw$school_enrolled_pct <- (acs_raw$school_enrolled/acs_raw$pop3over)*100
acs_raw$income_under25kpct <- (acs_raw$pop_under25k/acs_raw$households)*100
acs_raw$income25k_49kpct <- (acs_raw$pop25k_49k/acs_raw$households)*100
acs_raw$pop50k_74kpct <- (acs_raw$pop50k_74k/acs_raw$households)*100
acs_raw$pop75k_99kpct <- (acs_raw$pop75k_99k/acs_raw$households)*100
acs_raw$pop100koverpct <- (acs_raw$pop100kover/acs_raw$households)*100
acs_raw$pov_below_line <- ((acs_raw$pov_under2q+acs_raw$pov2q_3q+acs_raw$pov3q_4q)/acs_raw$pov_denom)*100
acs_raw$pov_1_2overline <- ((acs_raw$pov4q_6q+acs_raw$pov6q_8q)/acs_raw$pov_denom)*100
acs_raw$pov_doubleline <- (acs_raw$pov_over8q/acs_raw$pov_denom)*100
###str pad 
acs_raw$Geo_FIPS <- str_pad(acs_raw$Geo_FIPS,width=5,side="left",pad="0")

######## writing out csv and rds 

saveRDS(acs_raw, "econ_acs_cleaned.rds")
write.csv(acs_raw, "econ_acs_cleaned.csv",row.names = FALSE)


