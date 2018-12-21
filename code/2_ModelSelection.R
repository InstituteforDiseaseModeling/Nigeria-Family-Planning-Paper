#####################################################
##
##
## Selecting Models for mCPR, Unmet Need, and Demand Satisfied
## for All women and the 4 ageXparity groups
## Last Modified: October 9, 2018

rm(list=ls())

#####################################
# -- load packages and functions -- #
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/addTrans.R")
source("UsefulFunctions/expit_logit.R")
source("UsefulFunctions/SAE_Models.R")

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

#######################
# -- cycle through -- #

for(j in 1:nrow(results)){
print(paste0(j," of ",nrow(results),
             " outcomes and ageXparity groups. ",Sys.time()))
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
mod_dat<-mod_dat%>%mutate(outcome=ifelse(is.na(prec),NA,outcome),
                          prec=ifelse(is.na(outcome),NA,prec))


##################################
#### -- Set some priors --- ######
##################################

prior.iid = c(0.5,0.008)
prior.besag = c(0.5,0.008)


############################
# -- set up some models -- #

# -- model 1: no interactions -- #
# -- model 2: space-time interactions -- #
# -- model 3: space-time + survey RE -- #
# -- model 4: space-time + survey RE + survey-space -- #
# -- model 5: space-time + survey RE + survey-time -- #
# -- model 6: space-time + survey RE + survey-space + survey-time -- #

# time<-c("rw1","rw2")
time<-c("rw2")
space<-c("bym","bym2")

models<-expand.grid(time=time,space=space)
models$time<-as.character(models$time)
models$space<-as.character(models$space)

model_results<-NULL

for(i in 1:nrow(models)){
print(paste0(i," of ",nrow(models)," of spatial options.",Sys.time()))


#######################################
# -- fit the model for the outcomes-- #
#######################################

source("UsefulFunctions/Fit_SAE_Models_INLA.R")
#####################################
# --- look at model performance --- #
#####################################
# looking for low DIC and WAIC and high sum(log(cpo))
res<-rbind(cbind(mod1$dic$dic,mod1$dic$p.eff,sum(log(mod1$cpo$cpo),na.rm=T),mod1$waic$waic),# model 1 is the winner!
    cbind(mod2$dic$dic,mod2$dic$p.eff,sum(log(mod2$cpo$cpo),na.rm=T),mod2$waic$waic),
    cbind(mod3$dic$dic,mod3$dic$p.eff,sum(log(mod3$cpo$cpo),na.rm=T),mod3$waic$waic), 
    cbind(mod4$dic$dic,mod4$dic$p.eff,sum(log(mod4$cpo$cpo),na.rm=T),mod4$waic$waic), 
    cbind(mod5$dic$dic,mod5$dic$p.eff,sum(log(mod5$cpo$cpo),na.rm=T),mod5$waic$waic), 
    cbind(mod6$dic$dic,mod6$dic$p.eff,sum(log(mod6$cpo$cpo),na.rm=T),mod6$waic$waic))

res<-as.data.frame(res)
names(res)<-c("DIC","p_eff","sum_log_cpo","WAIC")
res$time<-models$time[i]
res$space<-models$space[i]
res$model<-1:6


model_results<-rbind(model_results,res)

}
write_csv(model_results,paste0("Results/ModelSelection/Selection_Indicator",
          results$outcome[j],"_Parity",results$parity[j],"_Age",results$age[j],".csv"))

}


