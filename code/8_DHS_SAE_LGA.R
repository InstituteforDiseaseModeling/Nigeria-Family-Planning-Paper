#####################################################
##
##
## Fitting the model 2 on the DHS data at LGA and state level
## Last Modified: October 24, 2018

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
shape3<- shapefile("Data/Shapes/nigeria_sdr_subnational_boundaries/shps/NIE_Admin2")

# adding a row number to the shape object, will be handy for plotting later on
shape1@data$row_num<-1:nrow(shape1@data)
shape2@data$row_num<-1:nrow(shape2@data)
shape3@data$row_num<-1:nrow(shape3@data)

key2<-shape2@data[,c("row_num","DHSREGEN")]
key2$state<-tolower(key2$DHSREGEN)
key3<-data.frame(lga=shape3@data$LGAName,row_num=shape3@data$row_num)


#################################
# --- looking at the shapes --- #
nb.r <- poly2nb(shape3, queen=F)
mat <- nb2mat(nb.r, style="B",zero.policy=TRUE) # mat is the 0/1 adjacency matrix
#
row = 14
indx=nb.r[[row]]  # Determine adjacent districts (by row in shapefile)
indx

# Plot an LGA in Borno
plot(shape3)
plot(shape3[row,], col='blue', add=T)
plot(shape3[indx,], col='red', add=T)


##################################
# -- read in the combine data -- #
All<-read_csv("Data/Processed/Nigeria/All_Nigeria_LGA_2018-10-24.csv")

#####################################################################################

logit_outcome<-function(indicator,thresh){
  
  logit.indicator<-ifelse(!is.na(indicator),logit(indicator),NA)
  logit.indicator<-ifelse(indicator<thresh | indicator>(1-thresh),NA,logit.indicator)
  logit.indicator
}

var_logit_outcome<-function(indicator,se,thresh){
   var_logit<-(se^2)/(indicator^2*(1-indicator)^2)
   var_logit<-ifelse(se<thresh,NA, var_logit)
   var_logit
}




All<-All%>%mutate(logit_mcpr=ifelse(!is.na(modern_method),logit(modern_method),NA),
                  var_logit_mcpr=(se^2)/(modern_method^2*(1-modern_method)^2))

All$logit_mcpr[All$se<0.001]<-NA
All$var_logit_mcpr[All$se<0.001]<-NA


####################################################################################
#################################
# -- fit model 2 on the LGAs -- #
#################################


#------------------------------------------------------------------#
#------ add in all missing timepoints from 1990 through 2018 ------#
#------------------------------------------------------------------#

# grid<-expand.grid(row_num=key3$row_num,year=c(1990:2018))
# grid<-grid%>%left_join(key3)
# dim(grid)
# dim(All)
# 
# All<-All%>%right_join(grid)
# dim(All)


# creating some variables for the random effects #
All$period.id3<-All$period.id2<-All$period.id<-as.numeric(as.factor(All$year))
All$dist.id3<-All$dist.id2<-All$dist.id<-All$row_num

All<-arrange(All,row_num,year)

All$outcome<-All$logit_mcpr
All$prec<-1/All$var_logit_mcpr

# a dataset for predictions #


##################################
#### -- Set some priors --- ######
##################################

prior.iid = c(0.1,0.1)
prior.besag = c(0.1,0.1)
initial.iid = 4
initial.besag = 3

############################
# -- set up some models -- #

# -- model 1: no interactions -- #
# -- model 2: space-time interactions -- #
# -- model 3: space-time + survey RE -- #
# -- model 4: space-time + survey RE + survey-space -- #
# -- model 5: space-time + survey RE + survey-time -- #
# -- model 6: space-time + survey RE + survey-space + survey-time -- #


# -- model 2: space-time interactions -- #

model <- outcome ~ f(dist.id, model="bym", param=prior.iid, graph=mat) +  # state effect
  f(period.id, model="rw1",  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model="rw1", replicate=dist.id, param=prior.besag)  # state X time



#######################################
# -- fit the model for the outcomes-- #
#######################################
system.time({
mod <- inla(model,
            family = "gaussian", 
            data =All, 
            control.predictor=list(compute=TRUE),
            control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
            control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
            scale=prec)
})#1805.65

###########################################
# --- add predictions to the data set --- #
###########################################

All$mdn<-expit(mod$summary.fitted.values$`0.5quant`)
All$up<-expit(mod$summary.fitted.values$`0.975quant`)
All$low<-expit(mod$summary.fitted.values$`0.025quant`)


###############################
# -- save the model output -- #
###############################

save(mod,All,file=paste0("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/Model_Fits/DHS_only_LGA_mCPR_model2.RDATA"))

##########################
# -- Plot the results -- #
##########################



png(paste0("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/Figures_Supplement",
"/LGA_SAE_mcpr/Map_mCPR_LGA_2013.png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = expit(mod$summary.fitted.values$`0.5quant`)[All$year==2013]
 
brks=seq(0,0.3,length=11)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(shape3,border="black",lwd=0.5,col=colcode)
plot(shape2,add=T,lwd=2)

color.legend(12.5,4.5,14,7, rect.col = plotclr,gradient="y",
             legend=paste0(seq(0,0.3,length=3)*100,"%"),
             align="rb",cex=1.4)

dev.off()

############################################
###### Plot ranked estimates ###############

png(paste0("NumericalAnalysis/LainaScripts/NGA_SAE_paper/Results/Figures_Supplement",
"/LGA_SAE_mcpr/Ranked_mCPR_LGA_2013.png"),
    height=8*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(4,7,1,1))

order_p<-order(plotvar2)

plot(plotvar2[order_p]*100,1:774,xlim=c(0,30),pch=19,
     yaxt="n",xlab="mCPR (%) 2013 Estimate",ylab="")


axis(2,1:774,c(rep("",774)),srt=45)
# axis(1, at=seq(1, 10, by=1), labels = FALSE)
text(x=-5,y = 1:774+0.75, par("usr")[3]-0.015 , labels = StateNames, 
     srt = 0, pos = 1, xpd = TRUE)


segments(All$up[All$year==2013][order_p]*100,1:774,
         All$low[All$year==2013][order_p]*100,1:774,lwd=0.75)
# abline(v=0,lty=2)

dev.off()


