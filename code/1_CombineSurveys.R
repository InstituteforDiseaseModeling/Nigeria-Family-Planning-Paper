#######################################################
### Merging the Nigeria Surveys
### Updated October 9, 2018

### this file will read in the most recent 
### versions of these surveys
### and save with the date.

rm(list=ls())

#####################################
# -- load packages and functions -- #
#####################################
source("UsefulFunctions/Packages.R")
source("UsefulFunctions/expit_logit.R")

# DHS #
loc<-"Data/Processed/Nigeria/"

processed<-list.files(loc)[grep("Processed",list.files(loc))]
processed_full<-processed
processed<-processed[-c(1,2,11)]# removing NNHS and 2007 MICS, which need help

# combine the MICS, DHS, and PMA2020 #
dat<-read_csv(paste0(loc,processed[1]))
for(i in 2:length(processed)){
  # i<-2
  tmp<-read_csv(paste0(loc,processed[i]))
  dat<-rbind(dat,tmp[,names(dat)])
  
}


###########################
# -- Add the 2007 MICS -- #
###########################
mics2007<-read_csv(paste0(loc,"Processed_Nigeria2007.csv"))
names(dat)[!names(dat)%in%names(mics2007)]
mics2007<-mics2007%>%mutate(demand_satisfied=NA,
                            se.demand_satisfied=NA,
                            unmet_need=NA,
                            unmet_need_limit=NA,
                            se.unmet_need=NA,
                            se.unmet_need_limit=NA,
                            unmet_need_space=NA,
                            se.unmet_need_space=NA)
mics2007<-mics2007[,names(dat)] # change the the order of the names
dat<-rbind(dat,mics2007)
######################
# -- Add the NNHS -- #
######################
smart15<-read_csv(paste0(loc,"Processed_Nigeria_NNHS_2015.csv"))
names(smart15)<-c("state","modern_method","se.modern_method")
smart15$year<-2015
smart15$recode<-"NNHS"
smart15$age<-"ALL"
smart15$parity<-"ALL"
smart15$state<-tolower(smart15$state)

smart14<-read_csv(paste0(loc,"Processed_Nigeria_NNHS_2014.csv"))
names(smart14)<-c("state","modern_method","se.modern_method")
smart14$year<-2014
smart14$recode<-"NNHS"
smart14$age<-"ALL"
smart14$parity<-"ALL"
smart14$state<-tolower(smart14$state)

################################
# -- fixing the state names -- #
################################
unique(dat$state)
unique(smart14$state)[!unique(smart14$state)%in%unique(dat$state)]
unique(smart15$state)[!unique(smart15$state)%in%unique(dat$state)]

unique(dat$state)[!unique(dat$state)%in%unique(smart14$state)]

smart14$state[smart14$state=="fct"]<-"fct-abuja"
smart14$state[smart14$state=="akwa-ibom"]<-"akwa ibom"

smart15$state[smart15$state=="fct"]<-"fct-abuja"
smart15$state[smart15$state=="akwa-ibom"]<-"akwa ibom"


##########################
# -- joining the data -- #

dim(dat)
dat<-dat%>%full_join(smart15)
dim(dat)

dim(dat)
dat<-dat%>%full_join(smart14)
dim(dat)


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


# Add Columns for SAE #
threshold<-0.00001
dat<-dat%>%mutate(modern_method=as.numeric(modern_method),
                  unmet_need=as.numeric(unmet_need),
                  trad_method=as.numeric(trad_method),
                  demand_satisfied=as.numeric(demand_satisfied),
                  se.modern_method=as.numeric(se.modern_method),
                  se.unmet_need=as.numeric(se.unmet_need),
                  se.trad_method=as.numeric(se.trad_method),
                  se.demand_satisfied=as.numeric(se.demand_satisfied),
                  logit_unmet=logit_outcome(unmet_need,thresh=threshold),
                  logit_mcpr=logit_outcome(modern_method,thresh=threshold),
                  logit_trad=logit_outcome(trad_method,thresh=threshold),
                  logit_ds=logit_outcome(demand_satisfied,thresh=threshold),
                  var_logit_unmet= var_logit_outcome(unmet_need,se.unmet_need,thresh=threshold),
                  var_logit_mcpr= var_logit_outcome(modern_method,se.modern_method,thresh=threshold),
                  var_logit_trad= var_logit_outcome(trad_method,se.trad_method,thresh=threshold),
                  var_logit_ds= var_logit_outcome(demand_satisfied,se.demand_satisfied,thresh=threshold),
                  logit_unmet=ifelse(is.na(var_logit_unmet),NA,logit_unmet),
                  logit_mcpr=ifelse(is.na(var_logit_mcpr),NA,logit_mcpr),
                  logit_trad=ifelse(is.na(var_logit_trad),NA,logit_trad),
                  logit_ds=ifelse(is.na(var_logit_ds),NA,logit_ds),
                  DHS=as.numeric(recode%in%c("1","2","3","4","5","6")), # this may need to be updated for other countries
                  MICS=grepl("MI",recode),
                  PMA=grepl("PMA",recode),
                  SMART=grepl("NNHS",recode),
                  survey=NA
                  )

dat$survey<-ifelse(dat$DHS==1,1,dat$survey)
dat$survey<-ifelse(dat$MICS==1,2,dat$survey)
dat$survey<-ifelse(dat$PMA==1,3,dat$survey)
dat$survey<-ifelse(dat$SMART==1,4,dat$survey)


summary(dat$logit_mcpr)
summary(dat$logit_ds)


# save a new version of the combine datasets #
table(is.na(dat$state))# there should be none.
dat<-dat%>%filter(!is.na(state))
dim(dat)


###########################
# -- Save the new data -- #
###########################
write_csv(dat,paste0(loc,"All_Nigeria_",as.character(today()),".csv"))

