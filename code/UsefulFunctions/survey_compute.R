compute_survey_proportions <- function(dhs){

  # -- set up the formula -- #
  
  print("Unmet need and CPR: ")
  if(sum(!is.na(dhs$unmet_need))>0){
    formula<-~unmet_need+unmet_need_limit+unmet_need_space+any_method+modern_method+trad_method+pill+iud+injections+diaphragm+condom+fem_ster+implant
  }else{    formula<-~any_method+modern_method+trad_method+pill+iud+injections+diaphragm+condom+fem_ster+implant
  }
  
  print("# ALL #")
  All<-svymean(formula,
               my.svydesign,na.rm=T)
  
  
  print("# by parity #")
  Parity<-svyby(formula,
                ~parity,my.svydesign,svymean,na.rm=T)
  
  
  print("# by age #")
  Age<-svyby(formula,
             ~age,my.svydesign,svymean,na.rm=T)
  
  print("# by state #")
  State<-svyby(formula,
               ~state,my.svydesign,svymean,na.rm=T)
  
  
  print("# both #")
  Both<-svyby(formula,
              ~age+parity+state,my.svydesign,svymean,na.rm=T)
  
  
  Parity<-Parity%>%right_join(parity_merge)
  Age<-Age%>%right_join(age_merge)
  State<-State%>%right_join(state_merge)
  Both<-Both%>%right_join(both_merge)
  
  
  
  ###########################################
  ## -- now do it with demand satisfied -- ##
  ###########################################
  
  if(sum(!is.na(dhs$unmet_need))>0){
  
  print("Demand satisfied: ")
  formula2<-~demand_satisfied
 
  print("# ALL #")
  All2<-svymean(formula2,
                my.svydesign,na.rm=T)
  
  
  print("# by parity #")
  Parity2<-svyby(formula2,
                 ~parity,my.svydesign,svymean,na.rm=T)
  
  
  print("# by age #")
  Age2<-svyby(formula2,
              ~age,my.svydesign,svymean,na.rm=T)
  
  print("# by state #")
  State2<-svyby(formula2,
                ~state,my.svydesign,svymean,na.rm=T)
  
  
  print("# both #")
  # --- doing this a bit differently because of some empty cells #
  Both2<-NULL
  
  tmp<-expand.grid(age=c("15-24","25+"),parity=c("0","1+"),state=NA,demand_satisfied=NA,se=NA)
  
  for(j in 1:length(unique(dhs$state))){
    # print(j)
    x<-try(svyby(formula2,
                 ~age+parity+state,
                 subset(my.svydesign,state==unique(dhs$state)[j]),
                 svymean,na.rm=T),silent=T)
    
    if(class(x)[1]=="try-error"){ # then we need to loop through the age and parity groups
      
      for(i in 1:4){
        # i<-4
        tmp$state<-unique(dhs$state)[j]
        y<-try(svyby(formula2,
                     ~age+parity+state,
                     subset(my.svydesign,state==unique(dhs$state)[j] & age==tmp$age[i] & parity==tmp$parity[i]),
                     svymean,na.rm=T),silent=T)
        
        if(class(y)[1]!="try-error"){tmp[i,]<-y} # if there is no error, then populate tmp
        
      }# end of i loop
      x<-tmp
    }
    Both2<-rbind(Both2,x)
  } # end of j loop
  
  
  # rename the standard error to match other conventions #
  names(Both2)[which(names(Both2)=="se")]<-"se.demand_satisfied"
  names(Parity2)[which(names(Parity2)=="se")]<-"se.demand_satisfied"
  names(Age2)[which(names(Age2)=="se")]<-"se.demand_satisfied"
  names(State2)[which(names(State2)=="se")]<-"se.demand_satisfied"
  
  
  # merge results with empty objects #
  print("Merging results.")
  Parity<-Parity2%>%right_join(Parity)
  Age<-Age2%>%right_join(Age)
  State<-State2%>%right_join(State)
  Both<-Both2%>%right_join(Both)
  
  } # end of demand satisfied if statement

  # All is returned as list, create matrix using the shape of Parity
  dat<-Parity[1,]
  dat$parity<-"ALL"
  dat$age<-"ALL"
  dat$state<-"ALL"
  dat[,names(All)]<-as.data.frame(All)[,1]
    if(sum(!is.na(dhs$unmet_need))>0){dat[,names(All2)]<-as.data.frame(All2)[,1]}
  dat[,paste0("se.",names(All))]<-as.data.frame(All)[,2]
   if(sum(!is.na(dhs$unmet_need))>0){ dat[,paste0("se.",names(All2))]<-as.data.frame(All2)[,2]}
  
  final<-rbind(dat[,names(Both)],Parity[,names(Both)],Age[,names(Both)],
               State[,names(Both)],Both[,names(Both)])

  final
}