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

####This script will be used to create figures for election night. 