#####################################################
##
##
## Creating the supporting figures for Figure 1 in the Manuscript
## Last Modified: October 10, 2018

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


######################## Anambra #################

  # k<-2013
png(paste0("Results/PaperFigs/Figure1/Map_AnambraHighlighted",
           # format(today(), '%Y%m%d'),
           ".png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

row = 26
indx=nb.r[[row]]  # Determine adjacent districts (by row in shapefile)
indx
# Plot FCTand make sure it highlights the neighbores
plot(shape2,col="grey80")
plot(shape2[row,], col='red', add=T)
plot(shape2[indx,], col="blue", add=T)
dev.off()

###############################
# -- read the model output -- #
###############################

load("Results/Model_Fits/mcpr_AgeALL_ParityALL_model.RDATA")

########################################################
# -- Just making a smoothed trend for Anambra state -- #
########################################################

i<-26 #State Number for Anambra
  
  state<-unique(All$state)[i]
  
 

png(paste0("Results/PaperFigs/Figure1/",state,"_mCPR_All.png"),
    height=4*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(4,4,1,1))

#########################
# -- prediction data -- #
#########################
preds<-filter(mod_dat,preds==1)


#######################
# -- observed data -- #
#######################
indices2<-which(All$state==state) 
indices<-which(preds$state==state )


#############################
# --- plot the indicator -- #
#############################

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
# cols<-addTrans(c(1,"orange3",2,"olivedrab3")[(All$survey[indices2])],200)
cols<-addTrans(c(1,"orange3","royalblue2","olivedrab4")[(All$survey[indices2])],250)

segments(All$year[indices2],
         (All$variable-1.96*All$SE)[indices2],
         All$year[indices2],
         (All$variable+1.96*All$SE)[indices2],
        col=cols,lwd=2)

points(All$year[indices2],All$variable[indices2],
       col=cols,
       pch=c(19,19,19,19)[(All$survey[indices2])],cex=1.5)

# add the line for PMA round 3 again

segments(All$year[indices2][All$recode[indices2]=="PMA3"],
         (All$variable-1.96*All$SE)[indices2][All$recode[indices2]=="PMA3"],
         All$year[indices2][All$recode[indices2]=="PMA3"],
         (All$variable+1.96*All$SE)[indices2][All$recode[indices2]=="PMA3"],
        col="royalblue2",lwd=2)
points(All$year[indices2][All$recode[indices2]=="PMA3"],All$variable[indices2][All$recode[indices2]=="PMA3"],
       col="royalblue2",
       pch=c(19,19,19,19)[(All$survey[indices2])][All$recode[indices2]=="PMA3"],cex=1.5)

dev.off()


