summarize_pma<-function(data){
  

# --------------- Prepare survey objects ------------------ #

# recode variables of interest
data<-data%>%mutate(
  unmet_need=as.numeric(unmettot==1),
  
  unmet_need_space=as.numeric(unmet==1),
  unmet_need_space=ifelse(!unmet %in% c(-97,-99), unmet_need_space, NA),
  unmet_need_limit=as.numeric(unmet==2),
  unmet_need_limit=ifelse(!unmet %in% c(-97,-99), unmet_need_limit, NA),
  
  current_methodnum=ifelse(is.na(current_methodnum) & !is.na(cp), 0, current_methodnum),
  pill=as.numeric(current_methodnum==7),
  iud=as.numeric(current_methodnum==4),
  injections=as.numeric(current_methodnum==5),
  diaphragm=as.numeric(current_methodnum==11),
  condom=as.numeric(current_methodnum==9),
  fem_ster=as.numeric(current_methodnum==1),
  implant=as.numeric(current_methodnum==3),
  
  any_method=as.numeric(cp==1),
  modern_method=as.numeric(mcp==1)
  
  # evermarr=(MARSTAT%in%c(20,21,22,30,31,32)),
  # evermarr=ifelse(!MARSTAT %in% c(95,96,98,99), evermarr,NA)
)

# traditional method variable
data<-data%>%mutate(trad_method=as.numeric(any_method!=modern_method))
with(data,table(trad_method,modern_method,useNA = "ifany"))
data$trad_method_pma <- as.numeric(data$tcp == 1)
# encodings are the same (minus NAs)
sum(data$trad_method==data$trad_method_pma, na.rm=T)

# demand satisfied variable
data<-data%>%mutate(demand_satisfied=ifelse(modern_method+unmet_need+trad_method>0,
                                            modern_method,NA))

# a coarse age recode #
data$age <- ifelse(as.numeric(data$age)<25, "15-24", "25+")


# a coarse parity recode #
data<-data%>%mutate(birth_events1=ifelse(is.na(birth_events), 0, birth_events),
                    parity=ifelse(birth_events1>0, "1+", "0")
                    # parity=ifelse(!as.numeric(birth_events) %in% c(-99), parity, NA)
)



# ------------ Setup grids for merging results -------------- #

both_merge<-expand.grid(c("15-24","25+"), c("0","1+"), unique(data$state))
names(both_merge)<-c("age","parity","state")

both_merge$age<-as.character(both_merge$age)
both_merge$state<-as.character(both_merge$state)

parity_merge<-data.frame(age="ALL",parity=c("0","1+"),state="ALL")

age_merge<-data.frame(age=c("15-24","25+"),parity="ALL",state="ALL")
age_merge$age<-as.character(age_merge$age)

state_merge<-data.frame(age="ALL",parity="ALL",state=unique(data$state))




# ----------------------------------------------------------------------------- #
# ---------------- Set up the survey design object ---------------------------- #


data$strata_num <- as.numeric(unclass(data$strata))

my.svydesign <- svydesign(id= ~Cluster_ID,
                          strata=~strata_num,nest=T, 
                          weights= ~wt, data=data)


###############################################################
## Population proportions and marriage status #################
###############################################################

####### creating the formula if unmet_need


if(sum(!is.na(data$unmet_need))>0){
  formula_need <- ~unmet_need+unmet_need_limit+unmet_need_space
  formula_mcpr <- ~any_method+modern_method+trad_method+pill+iud+injections+diaphragm+condom+fem_ster+implant
  formula_demand <- ~demand_satisfied
}else{    formula<-~any_method+modern_method+trad_method+pill+iud+injections+diaphragm+condom+fem_ster+implant
}


# by state #
State_need<-svyby(formula_need,
                  ~state,my.svydesign,svymean,na.rm=T)
State_mcpr<-svyby(formula_mcpr,
                  ~state,my.svydesign,svymean,na.rm=T)
State_demand<-svyby(formula_demand,
                    ~state,my.svydesign,svymean,na.rm=T)

names(State_demand)[which(names(State_demand)=="se")]<-"se.demand_satisfied"
State<-merge(State_need, State_mcpr)
State<-merge(State, State_demand)

# both #
Both_need<-svyby(formula_need,
                 ~age+parity+state,my.svydesign,svymean,na.rm=T)
Both_mcpr<-svyby(formula_mcpr,
                 ~age+parity+state,my.svydesign,svymean,na.rm=T)

Both_demand<-NULL
tmp<-expand.grid(age=c("15-24","25+"),parity=c("0","1+"),state=NA,demand_satisfied=NA,se=NA)

for(j in 1:length(unique(data$state))){
  # print(j)
  x<-try(svyby(formula_demand,
               ~age+parity+state,
               subset(my.svydesign,state==unique(data$state)[j]),
               svymean,na.rm=T),silent=T)
  
  if(class(x)[1]=="try-error"){ # then we need to loop through the age and parity groups
    
    for(i in 1:4){
      # i<-4
      tmp$state<-unique(data$state)[j]
      y<-try(svyby(formula_demand,
                   ~age+parity+state,
                   subset(my.svydesign,state==unique(data$state)[j] & age==tmp$age[i] & parity==tmp$parity[i]),
                   svymean,na.rm=T),silent=T)
      
      if(class(y)[1]!="try-error"){tmp[i,]<-y} # if there is no error, then populate tmp
      
    }# end of i loop
    x<-tmp
  }
  Both_demand<-rbind(Both_demand,x)
} # end of j loop


# rename the standard error to match other conventions #
names(Both_demand)[which(names(Both_demand)=="se")]<-"se.demand_satisfied"


Both<-merge(Both_need, Both_mcpr)
Both<-merge(Both, Both_demand)

# Age<-Age%>%right_join(age_merge)
# Parity<-Parity%>%right_join(parity_merge)
State<-State%>%right_join(state_merge)
Both<-Both%>%right_join(both_merge)

# rearrange columns of Both to match expected output of other DHS surveys
Both<-Both[c(1:3,30:31,4:6,10:18,7:9,19:27)]

# setting up the data.frame and starting with 'All' results
# start with Parity dataset, but then replace with the data from all,
# b/c All was not a matrix.


##########################################################################
##########################################################################

final<-rbind(State[,names(Both)],Both[,names(Both)])
final$country<-"Nigeria"
final$year<-2017
final$recode<-data$recode[1]


final

}
