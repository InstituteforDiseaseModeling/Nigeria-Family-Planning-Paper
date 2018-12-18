#################################################
########
### Analyze 
### DHS: 1990, 2003, 2008, 2013
### MICS: 2007, 2011, 2016
### PMA2020: 2014, 2015, 2016, 2017
### NNHS: 2014, 2015

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
home<-paste0(getwd(),"/")

###################################################
# -- load supporting functions from UN scripts -- #
###################################################

source("UsefulFunctions/MICS_GenerateUnmet.R")
source("UsefulFunctions/MICS_Categorization.R")
source("UsefulFunctions/survey_compute.R")
source("UsefulFunctions/summarize_pma.R")
source("UsefulFunctions/summarize_pma_ipums.R")
source("UsefulFunctions/unmet_recode.R")
source("UsefulFunctions/add_fp_indicators.R")
source("UsefulFunctions/survey_compute.R")

##################
# -- 1990 DHS -- #
##################
source("SurveyCode/Nigeria_DHS1990.R")

##################
# -- 2003 DHS -- #
##################
source("SurveyCode/Nigeria_DHS2003.R")

##################
# -- 2008 DHS -- #
##################
source("SurveyCode/Nigeria_DHS2008.R")

##################
# -- 2013 DHS -- #
##################
source("SurveyCode/Nigeria_DHS2013.R")

###################
# -- 2007 MICS -- #
###################
source("SurveyCode/Nigeria_MICS2007.R")

###################
# -- 2011 MICS -- #
###################
source("SurveyCode/Nigeria_MICS2011.R")

###################
# -- 2016 MICS -- #
###################
source("SurveyCode/Nigeria_MICS2016.R")

#########################
# -- 2014-16 PMA2020 -- #
#########################
source("SurveyCode/Nigeria_PMA2014-16.R")

######################
# -- 2017 PMA2020 -- #
######################
source("SurveyCode/Nigeria_PMA2017.R")

##########################
# -- 2014 & 2015 NNHS -- #
##########################
source("SurveyCode/Nigeria_NNHS2014-15.R")
