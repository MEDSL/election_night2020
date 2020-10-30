###################################################################################
################## Election Night Repository Set Up ###############################
##################################################################################
library(dplyr)
library(stringi)
library(stringr)
#####################################################################################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
main_wd <- dirname(rstudioapi::getActiveDocumentContext()$path)

###create loop for states 
state.abb

for(i in 1:length(state.abb)){
  setwd(main_wd)
  path_name <- paste0("results",sep="/",state.abb[i])
  ifelse(!dir.exists(path_name), dir.create(path_name), FALSE)# will create folder if does not exist
  setwd(path_name)
  saveRDS(state.abb,"state_abbs.rds")
  
}


data_dir_path <- paste0(wd_abs_pa,sep="/","data",sep="/","results",sep="",Sys.Date())
ifelse(!dir.exists(data_dir_path), dir.create(data_dir_path), FALSE)# will create folder if does not exist
