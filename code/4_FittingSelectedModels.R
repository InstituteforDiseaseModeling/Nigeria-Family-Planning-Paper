#####################################################
##
##
## Fitting the selected models for all indicators and groups
## for All women and the 4 ageXparity groups
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
row = 14
indx=nb.r[[row]]  # Determine adjacent districts (by row in shapefile)
indx

# Plot FCTand make sure it highlights the neighbores
plot(shape2)
plot(shape2[row,], col='blue', add=T)
plot(shape2[indx,], col='red', add=T)


##################################
# -- read in the combine data -- #
dat<-read_csv("Data/Processed/Nigeria/All_Nigeria_2018-10-09.csv")


table(dat$survey,dat$recode,useNA = "ifany")



####################################################################################
####################################################################################
############ Loop through the subsets of data and the selected model ###############
####################################################################################
####################################################################################

results<-read_csv("Results/ModelSelection/SelectedModels_WAIC.csv")



#######################
# -- cycle through -- #

for(j in 1:nrow(results)){
# j<-
  print(paste0(j,"/",nrow(results),", ",Sys.time()))
All<-filter(dat,parity==results$parity[j],age==results$age[j],state!="ALL")

All<-All%>%left_join(key)

#------------------------------------------------------------------#
#------ add in all missing timepoints from 1990 through 2018 ------#
#------------------------------------------------------------------#

state.key<-unique(All[,c("row_num","state")])


grid<-expand.grid(row_num=c(1:37),year=c(1990:2018))
grid<-grid%>%left_join(state.key)
dim(grid)
dim(All)

All<-All%>%right_join(grid)
dim(All)

All<-All%>%mutate(survey2=survey,survey3=survey)


# creating some variables for the random effects #
All$period.id3<-All$period.id2<-All$period.id<-as.numeric(as.factor(All$year))
All$dist.id3<-All$dist.id2<-All$dist.id<-All$row_num

All<-arrange(All,row_num,year)

pred.all<-unique(All[,c("period.id","period.id2","period.id3","dist.id","dist.id2","dist.id3","row_num","state","year")])
pred.all<-pred.all%>%mutate(outcome=NA, prec=NA,survey=NA,survey2=NA,preds=1,survey3=NA)


All$outcome<-All[,as.character(results$outcome[j])]
All$prec<-1/All[,as.character(results$var[j])]
All$preds<-0

# a dataset for predictions #
row.names(All)<-NULL
row.names(pred.all)<-NULL

tmp<-All[,names(pred.all)]
tmp$outcome<-as.numeric(unlist(tmp$outcome))
tmp$prec<-as.numeric(unlist(tmp$prec))
mod_dat<-rbind(tmp,pred.all)



##################################
#### -- Set some priors --- ######
##################################
# 
# prior.iid = c(0.5,0.008)

# default prior
prior.iid = c(1, 5e-05)
prior.besag = c(1, 5e-05)


############################
# -- set up some models -- #

# -- model 1: no interactions -- #
# -- model 2: space-time interactions -- #
# -- model 3: space-time + survey RE -- #
# -- model 4: space-time + survey RE + survey-space -- #
# -- model 5: space-time + survey RE + survey-time -- #
# -- model 6: space-time + survey RE + survey-space + survey-time -- #


# -- model 1: no interactions -- #
if(results$model[j]==1){
model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid)   # iid time
}

# -- model 2: space-time interactions -- #
if(results$model[j]==2){
model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag)  # state X time
}

# -- model 3: space-time + survey RE -- #
if(results$model[j]==3){
model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=results$time[j],  param=prior.besag) +  # state X time
  f(survey, model="iid",replicate=dist.id, param=prior.iid) # survey RE
}

# -- model 4: space-time + survey RE + survey-space -- #
if(results$model[j]==4){
model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +  # state X time
  f(survey, model="iid", param=prior.iid) + # survey RE
  f(survey2, model="iid",replicate=dist.id, param=prior.iid) # survey-state
}

# -- model 5: space-time + survey RE + survey-time -- #
if(results$model[j]==5){
model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +  # state X time
  f(survey, model="iid",param=prior.iid) + # survey RE
  f(survey3, model="iid",replicate=period.id,param=prior.iid) # survey-time
}

# -- model 6: space-time + survey RE + survey-space + survey-time -- #
if(results$model[j]==6){
model <- outcome ~ f(dist.id, model=results$space[j], param=prior.iid, graph=mat) +  # state effect
  f(period.id, model=results$time[j],  param=prior.iid) +  # random walk 
  f(period.id2, model="iid",  param=prior.iid) +  # iid time
  f(period.id3, model=results$time[j], replicate=dist.id, param=prior.besag) +  # state X time
  f(survey, model="iid", param=prior.iid) + # survey RE
  f(survey2, model="iid",replicate=dist.id, param=prior.iid) + # survey-state
  f(survey3, model="iid",replicate=period.id, param=prior.iid) # survey-time
}

#######################################
# -- fit the model for the outcomes-- #
#######################################

mod <- inla(model, 
            family = "gaussian", 
            data =mod_dat, 
            control.predictor=list(compute=TRUE),
            control.compute=list(cpo=TRUE,dic=TRUE,waic=T,config=T),
            control.family=list(hyper=list(prec=list(initial=log(1),fixed=TRUE))),
            scale=prec)

###########################################
# --- add predictions to the data set --- #
###########################################

mod_dat$mdn<-expit(mod$summary.fitted.values$`0.5quant`)
mod_dat$up<-expit(mod$summary.fitted.values$`0.975quant`)
mod_dat$low<-expit(mod$summary.fitted.values$`0.025quant`)



###########################################
# -- plot the predictions and the data -- #
###########################################

 loc<-paste0(substr(results$outcome[j],7,20),
           "_Age",results$age[j],"_Parity",results$parity[j])

variable<-substr(results$outcome[j],7,20)

variable<-c("modern_method","unmet_need","trad_method","demand_satisfied")[variable==c("mcpr","unmet","trad","ds")]
SE<-paste0("se.",variable)

All$variable<-as.numeric(unlist(All[,variable]))
All$SE<-as.numeric(unlist(All[,SE]))


###############################
# -- save the model output -- #
###############################

save(mod,mod_dat,All,file=paste0("Results/Model_Fits/",loc,"_model.RDATA"))

##########################
# -- Plot the results -- #
##########################

for(i in 1:length(unique(All$state))){

  # i<-36#11
  # View(state.key)
  
  state<-unique(All$state)[i]
  
 

png(paste0("Results/Figures_Supplement/",loc,"/",state,"_",loc,
           # format(today(), '%Y%m%d'),
           ".png"),
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

plot(preds$year[indices],preds$mdn[indices],
     ylim=c(0,1.0),lty=1,type="l", #col=addTrans("grey40",90),
     ylab="Prevalence",xlab="",xlim=c(2003,2018),
     lwd=2,cex.lab=1.3,cex=1.3)

polygon(c(1990:2018,2018:1990),
          c(preds$low[indices],
             rev(preds$up[indices])),
        col=addTrans("grey80",200),border=NA)

# -- add in observed data -- #

points(All$year[indices2]+0.2,All$variable[indices2],
       col=addTrans(c(1,4,2,"dodgerblue")[(All$survey[indices2])],200),
       pch=c(19,19,19,19)[(All$survey[indices2])],cex=1.3)

segments(All$year[indices2]+0.2,
         (All$variable-1.96*All$SE)[indices2],
         All$year[indices2]+0.2,
         (All$variable+1.96*All$SE)[indices2],
        col=addTrans(c(1,4,2,"dodgerblue")[(All$survey[indices2])],200),lwd=1.3)
# 



legend(2003,0.9,c("Observed","DHS","MICS","NNHS","PMA2020"),
       col=c(NA,1,4,"dodgerblue",2),lty=1,pch=c(NA,19,19,19,19),
       lwd=c(NA,1,1,1,1))
# 
legend(2003,1,c("Modeled","95% CI"),
       lty=1,lwd=c(2,10),pch=c(NA),col=c(1,addTrans("grey80",250)))

dev.off()
# 
}



##############################################################
for(k in 2013:2018){
  # k<-2013
png(paste0("Results/Figures_Supplement/",loc,"/Map_",loc,"_Year",k,
           # format(today(), '%Y%m%d'),
           ".png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = expit(mod$summary.fitted.values$`0.5quant`)[preds$year==k]
 
brks=seq(0,0.3,length=11)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(shape2,border="black",lwd=0.5,col=colcode)

color.legend(12.5,4.5,14,7, rect.col = plotclr,gradient="y",
             legend=paste0(seq(0,0.3,length=3)*100,"%"),
             align="rb",cex=1.4)

dev.off()
}

##################################################
#### UNMET NEED opposite #########################

if(variable=="unmet_need"){
  
  for(k in 2013:2018){
  # k<-2013
png(paste0("Results/Figures_Supplement/",loc,"/Map_",loc,"_Year",k,
           # format(today(), '%Y%m%d'),
           ".png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = -expit(mod$summary.fitted.values$`0.5quant`)[preds$year==k]
 
brks=seq(-0.3,0,length=11)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"RdYlBu")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(shape2,border="black",lwd=0.5,col=colcode)

color.legend(12.5,4.5,14,7, rect.col = plotclr,gradient="y",
             legend=paste0(seq(0.3,0,length=3)*100,"%"),
             align="rb",cex=1.4)

dev.off()
}}
  
##################################################
#### Demand Satisfied Blue Scale ################

if(variable=="demand_satisfied"){
  
  for(k in 2013:2018){
  # k<-2013
png(paste0("Results/Figures_Supplement/",loc,"/Map_",loc,"_Year",k,
           # format(today(), '%Y%m%d'),
           ".png"),
    height=6*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(0,0,1.5,0))

plotvar2 = expit(mod$summary.fitted.values$`0.5quant`)[preds$year==k]
 
brks=seq(0,0.75,length=9)
nclr<-length(brks)-1

plotclr<-brewer.pal(nclr,"Blues")
colornum<-findInterval(plotvar2, brks, all.inside=T)
colcode<-plotclr[colornum]

plot(shape2,border="black",lwd=0.5,col=colcode)

color.legend(12.5,4.5,14,7, rect.col = plotclr,gradient="y",
             legend=paste0(seq(0,0.75,length=4)*100,"%"),
             align="rb",cex=1.4)

dev.off()
}}
    

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
   
   if(variable=="unmet_need"){
     
        samps[,jj]<-rank(post[[jj]]$latent[1:nrow(mod_dat)][mod_dat$year==2018 & mod_dat$preds==1])
   }
}

##################
## plot results ##

png(paste0("Results/Figures_Supplement/",loc,"/AnnualChangeCI_",loc,"_Year",k,
           # format(today(), '%Y%m%d'),
           ".png"),
    height=8*1.15,width=6*1.15,res=400, unit="in")
par(mar=c(4,7,1,1))

order_p<-order(apply(sampsDiff,1,median))

plot(apply(sampsDiff,1,median)[order_p]*100,1:37,xlim=c(-5,5),pch=19,
     yaxt="n",xlab="Annual Change 2012 to 2018",ylab="")


axis(2,1:37,c(rep("",37)),srt=45)
# axis(1, at=seq(1, 10, by=1), labels = FALSE)
text(x=-6,y = 1:37+0.75, par("usr")[3]-0.015 , labels = All$state[All$year==2013][order_p], 
     srt = 0, pos = 1, xpd = TRUE)

abline(v=0,lty=2,col="grey30")

segments(apply(sampsDiff,1,function(x)quantile(x,0.975))[order_p]*100,1:37,
         apply(sampsDiff,1,function(x)quantile(x,0.025))[order_p]*100,1:37)
abline(v=0,lty=2)

dev.off()


#####################################
#### how robust are the rankings ####
order_p<-order(apply(samps,1,median))

png(paste0("Results/Figures_Supplement/",loc,"/RobustnessOfRankings2015_",loc,"_Year",k,
           # format(today(), '%Y%m%d'),
           ".png"),
    height=6*1.15,width=10*1.15,res=400, unit="in")

par(mar=c(4,4,1,1))

boxplot.matrix(t(samps[order_p,]),xaxt="n",col="grey50",ex=1.4)
axis(1,1:37,c(rep("",37)),srt=45,cex=1.4)

text(x = 1:37+0.05, par("usr")[3]-0.5 , labels = All$state[All$year==2015][order_p], 
     srt = 45, pos = 1, xpd = TRUE,cex=1.4)

dev.off()


png(paste0("Results/Figures_Supplement/",loc,"/RobustnessOfRankings2018_",loc,"_Year",k,
           # format(today(), '%Y%m%d'),
           ".png"),
    height=6*1.15,width=10*1.15,res=400, unit="in")

par(mar=c(4,4,1,1))

boxplot.matrix(t(samps[order_p,]),xaxt="n",col="grey50",ex=1.4)
axis(1,1:37,c(rep("",37)),srt=45,cex=1.4)

text(x = 1:37+0.05, par("usr")[3]-0.5 , labels = All$state[All$year==2018][order_p], 
     srt = 45, pos = 1, xpd = TRUE,cex=1.4)

dev.off()



}


