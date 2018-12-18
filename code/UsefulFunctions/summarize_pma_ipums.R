summarize_pma_ipums<-function(data){
  
  # recode variables of interest
data<-data%>%mutate(
  unmet_need=as.numeric(UNMETYN==1),
  unmet_need=ifelse(!UNMETYN %in% c(99,95,96), unmet_need, NA),
  
  unmet_need_space=as.numeric(UNMETNEED==11),
  unmet_need_limit=as.numeric(UNMETNEED==12),
  
  pill=as.numeric(as.character(FPCURREFFMETH)=="131"),
  iud=as.numeric(as.character(FPCURREFFMETH)=="112"),
  injections=as.numeric(as.character(FPCURREFFMETH) %in% c("120","121","122")),
  diaphragm=as.numeric(as.character(FPCURREFFMETH)=="151"),
  condom=as.numeric(as.character(FPCURREFFMETH) %in% c("140","141")),
  fem_ster=as.numeric(as.character(FPCURREFFMETH)=="101"),
  implant=as.numeric(as.character(FPCURREFFMETH)=="111"),
  
  any_method=as.numeric(FPCURRUSE==1),
  any_method=ifelse(!FPCURRUSE %in% c(95,96,98), any_method, NA),
  modern_method=as.numeric(MCP==1),
  modern_method=ifelse(!MCP %in% c(95,96,98), modern_method, NA),
  
  evermarr=(MARSTAT%in%c(20,21,22,30,31,32)),
  evermarr=ifelse(!MARSTAT %in% c(95,96,98,99), evermarr,NA)
)

# traditional method variable
data<-data%>%mutate(trad_method=as.numeric(any_method!=modern_method))
with(data,table(trad_method,modern_method,useNA = "ifany"))

# demand satisfied variable
data<-data%>%mutate(demand_satisfied=ifelse(modern_method+unmet_need+trad_method>0,
                                          modern_method,NA))

# a coarse age recode #
data$age <- ifelse(as.numeric(data$AGE)<25, "15-24", "25+")


# a coarse parity recode #
data<-data%>%mutate(parity=ifelse(as.numeric(BIRTHEVENT)>0, "1+", "0"),
                    parity=ifelse(!as.numeric(BIRTHEVENT) %in% c(95,96,97,98,99), parity, NA)
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


# renumber strata from 1 to n_strata
strata_lab <- data[,"STRATA"]
strata<-unique(strata_lab)
if (is.na(strata)) {
  data$strata <- 1
} else {
strata$strata<-1:nrow(strata)
data<-data%>%left_join(strata)
}



my.svydesign <- svydesign(id= ~CLUSTERNG,
                          strata=~strata,nest=T, 
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
State<-merge(State_need, State_mcpr)
State<-merge(State, State_demand)

# both #
Both_need<-svyby(formula_need,
                 ~age+parity+state,my.svydesign,svymean,na.rm=T)
Both_mcpr<-svyby(formula_mcpr,
                 ~age+parity+state,my.svydesign,svymean,na.rm=T)
Both_demand<-svyby(formula_demand,
                 ~age+parity+state,my.svydesign,svymean,na.rm=T)
Both<-merge(Both_need, Both_mcpr)
Both<-merge(Both, Both_demand)


State<-State%>%right_join(state_merge)
Both<-Both%>%right_join(both_merge)

# rearrange columns of Both to match expected output of other DHS surveys
Both<-Both[c(1:3,30:31,4:6,10:18,7:9,19:27)]

##########################################################################
##########################################################################

final<-rbind(State[,names(Both)],Both[,names(Both)])
colnames(final)[5] <- 'se.demand_satisfied'
final$country<-"Nigeria"
final$year<-floor(data$YEAR[1])
final$recode<-data$recode[1]

final

}
