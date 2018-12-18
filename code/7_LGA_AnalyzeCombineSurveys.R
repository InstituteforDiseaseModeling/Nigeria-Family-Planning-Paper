#################################################
########
### Analyze 
### DHS: 1990, 2003, 2008, 2013
### at the LGA level
### Updated October 24, 2018

rm(list=ls())

###################
# -- Libraries -- #
###################
library(survey);library(haven);
library(dplyr);library(readr); library(foreign)
library(maptools);library(raster);library(sp)
library(labelled);library(data.table);library(rgdal)

options(survey.lonely.psu="adjust")


###############################
# -- set working directory -- #
###############################
setwd("C:/Users/lmercer/Dropbox (IDM)/SmallAreaEstimationForFP/")
home<-paste0(getwd(),"/")

###################################################
# -- load supporting functions from UN scripts -- #
###################################################

source("NumericalAnalysis/UsefulFunctions/MICS_GenerateUnmet.R")
source("NumericalAnalysis/UsefulFunctions/MICS_Categorization.R")
source("NumericalAnalysis/UsefulFunctions/survey_compute.R")
source("NumericalAnalysis/UsefulFunctions/summarize_pma.R")
source("NumericalAnalysis/UsefulFunctions/summarize_pma_ipums.R")
source("NumericalAnalysis/UsefulFunctions/unmet_recode.R")
source("NumericalAnalysis/UsefulFunctions/add_fp_indicators.R")
source("NumericalAnalysis/UsefulFunctions/survey_compute.R")
source("NumericalAnalysis/UsefulFunctions/Packages.R")
source("NumericalAnalysis/UsefulFunctions/expit_logit.R")

##################
# -- 1990 DHS -- #
##################
system.time({
source("NumericalAnalysis/LainaScripts/NGA_SAE_paper/SurveyCode/Nigeria_DHS1990_LGA.R")
})# 36.7s
##################
# -- 2003 DHS -- #
##################
system.time({
source("NumericalAnalysis/LainaScripts/NGA_SAE_paper/SurveyCode/Nigeria_DHS2003_LGA.R")
}) #79.7s
##################
# -- 2008 DHS -- #
##################
system.time({
source("NumericalAnalysis/LainaScripts/NGA_SAE_paper/SurveyCode/Nigeria_DHS2008_LGA.R")
}) #343.7s
##################
# -- 2013 DHS -- #
##################
system.time({
source("NumericalAnalysis/LainaScripts/NGA_SAE_paper/SurveyCode/Nigeria_DHS2013_LGA.R")
}) #399.6s


#######################################################
### Merging the Nigeria Surveys
### Updated October 24, 2018

### this file will read in the most recent 
### versions of these surveys
### and save with the date.

# DHS #
loc<-"Data/Processed/Nigeria/"

processed<-list.files(loc)[grep("LGA",list.files(loc))]
processed<-processed[-grep("All",processed)]
# DHS LGA level analysis#
dat<-read_csv(paste0(loc,processed[1]))
for(i in 2:length(processed)){
  # i<-2
  tmp<-read_csv(paste0(loc,processed[i]))
  dat<-rbind(dat,tmp[,names(dat)])
  
}

###########################
# -- Save the new data -- #
###########################
write_csv(dat,paste0(loc,"All_Nigeria_LGA_",as.character(today()),".csv"))



