#####################################################
##
##
## looking at the decomposition of variance
## Last Modified: October 18, 2018

rm(list=ls())

#####################################
# -- load packages and functions -- #
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/addTrans.R")
source("UsefulFunctions/expit_logit.R")

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
dat<-read_csv(paste0("Data/Processed/Nigeria/All_Nigeria_",as.character(today()),".csv"))


table(dat$survey,dat$recode,useNA = "ifany")



####################################################################################
####################################################################################
############ Loop through the subsets of data and the selected model ###############
####################################################################################
####################################################################################

results<-read_csv("Results/ModelSelection/SelectedModels_WAIC.csv")


variances<-NULL
#######################
# -- cycle through -- #

for(j in 1:nrow(results)){
# j<-3
  print(paste0(j,"/",nrow(results),", ",Sys.time()))


###########################################
# -- plot the predictions and the data -- #
###########################################

 loc<-paste0(substr(results$outcome[j],7,20),
           "_Age",results$age[j],"_Parity",results$parity[j])

variable<-substr(results$outcome[j],7,20)

variable<-c("modern_method","unmet_need","trad_method","demand_satisfied")[variable==c("mcpr","unmet","trad","ds")]


###############################
# -- save the model output -- #
###############################

load(file=paste0("Results/Model_Fits/",loc, "_model.RDATA"))

# need to get the scale number out for BYM2 model
var<-1/(mod$summary.hyperpar$"0.5quant"[grep("Prec",row.names(mod$summary.hyperpar))])^2
var_names<-row.names(mod$summary.hyperpar)[grep("Prec",row.names(mod$summary.hyperpar))]

# need to combine the two variances for dist.id into BYM
dist_var<-sum(var[grep("dist.id",var_names)])
var<-var[-grep("dist.id",var_names)]
var_names<-var_names[-grep("dist.id",var_names)]

# need to sum rows that have "dist. id" in the name, all spatial #
tot_var<-sum(dist_var,var)

# combined the results
if(results$model[j]==4){
variances<-rbind(variances,t(cbind(c("dist.id",var_names,"Precision for survey3"),
      round(c(dist_var,var,NA)/tot_var,3)*100)))
}
if(results$model[j]==6){
  variances<-rbind(variances,t(cbind(c("dist.id",var_names),
      round(c(dist_var,var)/tot_var,3)*100)))
}



}

########################################
# -- remove the rows with the names -- #
########################################

dat<-as.data.frame(variances[seq(2,40,by=2),])
names(dat)<-variances[1,]

##############################
# -- combine with results -- #
##############################
res<-cbind(results,dat)

write_csv(res,"Results/DecompVariance/results.csv")