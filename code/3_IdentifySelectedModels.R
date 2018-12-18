#####################################################
##
##
## Identifying the selected models
## for All women and the 4 ageXparity groups
## Last Modified: October 10, 2018

rm(list=ls())

###############################
# -- Set working directory -- #
setwd("C:/Users/lmercer/Dropbox (IDM)/SmallAreaEstimationForFP/")

#####################################
# -- load packages and functions -- #
source("NumericalAnalysis/UsefulFunctions/Packages.R")
source("NumericalAnalysis/UsefulFunctions/addTrans.R")
source("NumericalAnalysis/UsefulFunctions/expit_logit.R")

###################################
# --- read in the shape files --- #
shape<- shapefile("Data/Shapes/nigeria_sdr_subnational_boundaries/shps/sdr_subnational_boundaries")
shape2<- shapefile("Data/Shapes/nigeria_sdr_subnational_boundaries/shps/sdr_subnational_boundaries2")

# adding a row number to the shape2 object, will be handy for plotting later on
shape2@data$row_num<-1:nrow(shape2@data)

key<-shape2@data[,c("row_num","DHSREGEN")]
key$state<-tolower(key$DHSREGEN)

#################################
# --- looking at the shapes --- #
nb.r <- poly2nb(shape2, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) # mat is the 0/1 adjacency matrix
#
row = 14
indx=nb.r[[row]]  # Determine adjacent districts (by row in shapefile)
indx

# Plot FCTand make sure it highlights the neighbores
plot(shape2)
plot(shape2[row,], col='blue', add=T)
plot(shape2[indx,], col='red', add=T)


##################################
# -- read in the combine data -- #
dat<-read_csv("Data/Processed/Nigeria/All_Nigeria_2018-10-09.csv.csv")


table(dat$survey,dat$recode,useNA = "ifany")



################################################################################
################################################################################
############ Loop through the subsets of data and select a model ###############
################################################################################
################################################################################

indicators<-c("logit_mcpr","logit_unmet","logit_trad","logit_ds")
parity<-c("0","1+")
age<-c("15-24","25+")

groups<-expand.grid(outcome=indicators,parity=parity,age=age)
groups_all<-data.frame(outcome=indicators,parity="ALL",age="ALL")

results<-rbind(groups_all,groups)
results$var<-paste0("var_",results$outcome)

results$model<-results$time<-results$space<-NA

#######################
# -- cycle through -- #

for(j in 1:nrow(results)){
  # j<-1
  
  tmp<-read_csv(paste0("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/ModelSelection/Selection_Indicator",
          results$outcome[j],"_Parity",results$parity[j],"_Age",results$age[j],".csv"))
  tmp<-tmp%>%filter(time=="rw2")
  
  min_row<-which(tmp$WAIC==min(tmp$WAIC))
  # min_row<-which(tmp$DIC==min(tmp$DIC))
  # min_row<-which(tmp$sum_log_cpo==max(tmp$sum_log_cpo))
  
  results[j,"model"]<-tmp$model[min_row]
  results[j,"space"]<-tmp$space[min_row]
  results[j,"time"]<-tmp$time[min_row]

}


##################################
# -- save the selected models -- #
write_csv(results,"NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/ModelSelection/SelectedModels_WAIC.csv")
