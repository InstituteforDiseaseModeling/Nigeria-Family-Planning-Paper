#####################################################
##
##
## Figure 4: Differences in mCPR 2012-2018
## Last Modified: October 10, 2018


rm(list=ls())

#####################################
# -- load packages and functions -- #
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/addTrans.R")
source("UsefulFunctions/expit_logit.R")
source("UsefulFunctions/firstup.R")
source("UsefulFunctions/simpleCap.R")


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


###############################
# -- read the model output -- #
###############################

load("Results/Model_Fits/mcpr_AgeALL_ParityALL_model.RDATA")


##########################################################
###### Comparing change from 2012 to 2017 ################
  
set.seed(1985)
post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=37,ncol=1000)
samps<-matrix(nrow=37,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-(expit(post[[jj]]$latent[1:nrow(mod_dat)][mod_dat$year==2017 & mod_dat$preds==1])-expit(post[[jj]]$latent[1:nrow(mod_dat)][mod_dat$year==2012 & mod_dat$preds==1]))/(2017-2012)
 
  
   samps[,jj]<-rank(-post[[jj]]$latent[1:nrow(mod_dat)][mod_dat$year==2017 & mod_dat$preds==1])
   

}

##################
## plot results ##

png(paste0("Results/PaperFigs/Figure4/AnnualChangeCI_2012-2017_mCPR.png"),
    height=7,width=4,res=400, unit="in")
par(mar=c(4,6.5,1,1))

order_p<-order(apply(sampsDiff,1,median))
StateNames<-sapply(All$state[All$year==2013][order_p],simpleCap)
StateNames[StateNames=="Fct Abuja"]<-"FCT, Abuja"
StateNames[StateNames=="Akwa ibom"]<-"Akwa Ibom"
StateNames[StateNames=="Cross river"]<-"Cross River"

plot(apply(sampsDiff,1,median)[order_p]*100,1:37,xlim=c(-5,5),pch=19,
     yaxt="n",xlab="",ylab="",cex.axis=1.4, cex.lab=1.4)


axis(2,1:37,c(rep("",37)),srt=45)
# axis(1, at=seq(1, 10, by=1), labels = FALSE)
text(x=-8,y = 1:37+1.15, par("usr")[3]-0.015 , labels = StateNames, 
     srt = 0, pos = 1, xpd = TRUE,cex=1.1)

abline(v=0,lty=2,col="grey30")

# abline(v=1.4,lty=2,col="grey80")

segments(apply(sampsDiff,1,function(x)quantile(x,0.975))[order_p]*100,1:37,
         apply(sampsDiff,1,function(x)quantile(x,0.025))[order_p]*100,1:37)
abline(v=0,lty=2)

dev.off()


png(paste0("Results/PaperFigs/Figure4/AnnualChangeMap_2012-2017.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = apply(sampsDiff,1,median)*100
 
brks=seq(-3,3,length=11)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(shape2,border="black",lwd=0.5,col=colcode)

color.legend(12.5,4.5,14,7, rect.col = plotclr,gradient="y",
             legend=paste0(seq(-3,3,length=3),"%"),
             align="rb",cex=1.4)

dev.off()


##########################################################
###### Comparing change from 2012 to 2018 ################
  
set.seed(1985)
post<-inla.posterior.sample(1000,mod)
names(post)

sampsDiff<-matrix(nrow=37,ncol=1000)
samps<-matrix(nrow=37,ncol=1000)
for(jj in 1:1000){
  # jj<-1
  sampsDiff[,jj]<-(expit(post[[jj]]$latent[1:nrow(mod_dat)][mod_dat$year==2018 & mod_dat$preds==1])-expit(post[[jj]]$latent[1:nrow(mod_dat)][mod_dat$year==2012 & mod_dat$preds==1]))/(2018-2012)
 
  
   samps[,jj]<-rank(-post[[jj]]$latent[1:nrow(mod_dat)][mod_dat$year==2018 & mod_dat$preds==1])
   
}


##################
## plot results ##

png(paste0("Results/PaperFigs/Figure4/AnnualChangeCI_2012-2018_mCPR.png"),
    height=7,width=4.75,res=400, unit="in")
par(mar=c(4,6.5,1,1))

order_p<-order(apply(sampsDiff,1,median))
StateNames<-sapply(All$state[All$year==2013][order_p],simpleCap)
StateNames[StateNames=="Fct Abuja"]<-"FCT, Abuja"
StateNames[StateNames=="Akwa ibom"]<-"Akwa Ibom"
StateNames[StateNames=="Cross river"]<-"Cross River"

plot(apply(sampsDiff,1,median)[order_p]*100,1:37,xlim=c(-4.5,6),pch=19,
     yaxt="n",xlab="",ylab="",cex.axis=1.4, cex.lab=1.4)


axis(2,1:37,c(rep("",37)),srt=45)
# axis(1, at=seq(1, 10, by=1), labels = FALSE)
text(x=-7.5,y = 1:37+1.15, par("usr")[3]-0.015 , labels = StateNames, 
     srt = 0, pos = 1, xpd = TRUE,cex=1.1)

abline(v=0,lty=2,col="grey30")

# abline(v=1.4,lty=2,col="grey80")

segments(apply(sampsDiff,1,function(x)quantile(x,0.975))[order_p]*100,1:37,
         apply(sampsDiff,1,function(x)quantile(x,0.025))[order_p]*100,1:37)
abline(v=0,lty=2)

dev.off()


png(paste0("Results/PaperFigs/Figure4/AnnualChangeMap_2012-2018.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = apply(sampsDiff,1,median)*100
 
brks=seq(-3,3,length=11)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(shape2,border="black",lwd=0.5,col=colcode)

color.legend(12.5,4.5,14,7, rect.col = plotclr,gradient="y",
             legend=paste0(seq(-3,3,length=3),"%"),
             align="rb",cex=1.4)

dev.off()

