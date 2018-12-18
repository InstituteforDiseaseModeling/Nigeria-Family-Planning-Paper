#####################################################
##
##
## Figure 3: figures for Lagos unmet need and Kaduna mCPR
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
source("NumericalAnalysis/UsefulFunctions/firstup.R")


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



############################### Lagos ######################################

png(paste0("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/PaperFigs/Figure3/Map_Lagos_Highlighted",
           # format(today(), '%Y%m%d'),
           ".png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

row = 36
indx=nb.r[[row]]  # Determine adjacent districts (by row in shapefile)
indx

# Plot FCTand make sure it highlights the neighbores
plot(shape2,col="grey80")
plot(shape2[row,], col='firebrick', add=T)
# plot(shape2[indx,], col="blue", add=T)
dev.off()



################################### Kaduna #####################################

png(paste0("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/PaperFigs/Figure3/Map_Kaduna_Highlighted",
           # format(today(), '%Y%m%d'),
           ".png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

row = 11
indx=nb.r[[row]]  # Determine adjacent districts (by row in shapefile)
indx
# Plot FCTand make sure it highlights the neighbores
plot(shape2,col="grey80")
plot(shape2[row,], col='firebrick', add=T)
# plot(shape2[indx,], col="blue", add=T)
dev.off()



###########################################
# -- create the figures for the paper -- #

###############################
  # -- mCPR in Kaduna-- #
###############################

###############################
# -- read the model output -- #
###############################

load("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/Model_Fits/mcpr_AgeALL_ParityALL_model.RDATA")



#############################
# --- plot the indicator -- #
#############################

# 11 - Kaduna
# 36 - Lagos
i<-11
state<-unique(All$state)[i]
  
 
#########################
# -- prediction data -- #
#########################
preds<-filter(mod_dat,preds==1)


#######################
# -- observed data -- #
#######################
indices2<-which(All$state==state) 
indices<-which(preds$state==state )



png(paste0("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/PaperFigs/Figure3/Kaduna_mCPR.png"),
    height=4*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(4,4,1,1))


plot(preds$year[indices],preds$mdn[indices],
     ylim=c(0,0.6),lty=1,type="l", #col=addTrans("grey40",90),
     ylab="Prevalence",xlab="",xlim=c(2003,2018),
     lwd=2,cex.lab=1.3,cex=1.3)

polygon(c(1990:2018,2018:1990),
          c(preds$low[indices],
             rev(preds$up[indices])),
        col=addTrans("grey80",200),border=NA)

# -- add in observed data -- #

points(All$year[indices2],All$variable[indices2],
       col=addTrans(c(1,"orange3","royalblue2","olivedrab4")[(All$survey[indices2])],250),
       pch=c(19,19,19,19)[(All$survey[indices2])],cex=1.5)

segments(All$year[indices2],
         (All$variable-1.96*All$SE)[indices2],
         All$year[indices2],
         (All$variable+1.96*All$SE)[indices2],
        col=addTrans(c(1,"orange3","royalblue2","olivedrab4")[(All$survey[indices2])],250),lwd=2)


legend(2003,0.5,c("Observed","DHS","MICS","NNHS","PMA2020"),
       col=c(NA,1,"orange3","olivedrab4","royalblue2"),lty=1,pch=c(NA,19,19,19,19),
       lwd=c(NA,1,1,1,1))
# 
legend(2003,.6,c("Modeled","95% CI"),
       lty=1,lwd=c(2,10),pch=c(NA),col=c(1,addTrans("grey80",250)))

# add the line for PMA round 3 again

segments(All$year[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
         (All$variable-1.96*All$SE)[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
         All$year[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
         (All$variable+1.96*All$SE)[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
        col="royalblue2",lwd=2)
points(All$year[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],All$variable[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
       col="royalblue2",
       pch=c(19,19,19,19)[(All$survey[indices2])][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],cex=1.5)


dev.off()




############################################################################
############################################################################

##################################
  # -- Unmet Need in Lagos -- #
##################################

###############################
# -- read the model output -- #
###############################

load("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/Model_Fits/unmet_AgeALL_ParityALL_model.RDATA")


# 11 - Kaduna
# 36 - Lagos
i<-36
state<-unique(All$state)[i]
  
 
#########################
# -- prediction data -- #
#########################
preds<-filter(mod_dat,preds==1)


#######################
# -- observed data -- #
#######################
indices2<-which(All$state==state) 
indices<-which(preds$state==state )
 


png(paste0("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/PaperFigs/Figure3/Lagos_UnmetNeed.png"),
    height=4*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(4,4,1,1))



plot(preds$year[indices],preds$mdn[indices],
     ylim=c(0,0.6),lty=1,type="l", #col=addTrans("grey40",90),
     ylab="Prevalence",xlab="",xlim=c(2003,2018),
     lwd=2,cex.lab=1.3,cex=1.3)

polygon(c(1990:2018,2018:1990),
          c(preds$low[indices],
             rev(preds$up[indices])),
        col=addTrans("grey80",200),border=NA)

# -- add in observed data -- #

points(All$year[indices2],All$variable[indices2],
       col=addTrans(c(1,"orange3","royalblue2")[(All$survey[indices2])],250),
       pch=c(19,19,19,19)[(All$survey[indices2])],cex=1.5)

segments(All$year[indices2],
         (All$variable-1.96*All$SE)[indices2],
         All$year[indices2],
         (All$variable+1.96*All$SE)[indices2],
        col=addTrans(c(1,"orange3","royalblue2")[(All$survey[indices2])],250),lwd=2)


legend(2003,0.5,c("Observed","DHS","MICS","PMA2020"),
       col=c(NA,1,"orange3","royalblue2"),lty=1,pch=c(NA,19,19,19,19),
       lwd=c(NA,1,1,1,1))
# 
legend(2003,.6,c("Modeled","95% CI"),
       lty=1,lwd=c(2,10),pch=c(NA),col=c(1,addTrans("grey80",250)))

# add the line for PMA round 3 again

segments(All$year[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
         (All$variable-1.96*All$SE)[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
         All$year[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
         (All$variable+1.96*All$SE)[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
        col="royalblue2",lwd=2)
points(All$year[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],All$variable[indices2][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],
       col="royalblue2",
       pch=c(19,19,19,19)[(All$survey[indices2])][All$recode[indices2]%in%c("PMA1","PMA2","PMA3")],cex=1.5)

dev.off()



