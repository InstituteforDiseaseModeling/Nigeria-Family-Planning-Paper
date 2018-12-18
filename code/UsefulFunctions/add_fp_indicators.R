#################################################
#### add the FP indicators to a DHS dataset #####

add_fp_indicators<-function(dhs,unmet_need_surveyID){
  dhs<-dhs%>%mutate(
  pill=as.numeric(as.character(v312)%in%c("pill","1",1)),
  pill=ifelse(!is.na(v312),pill,NA),
  iud=as.numeric(as.character(v312)%in%c("iud","2",2)),
  iud=ifelse(!is.na(v312),iud,NA),
  injections=as.numeric(as.character(v312)%in%c("injections","3",3)),
  injections=ifelse(!is.na(v312),injections,NA),
  diaphragm=as.numeric(as.character(v312)%in%c("diaphragm","4","diaphragm/foam/jelly",4)),
  diaphragm=ifelse(!is.na(v312), diaphragm,NA),
  condom=as.numeric(as.character(v312)%in%c("condom","5","male condom",5)),
  condom=ifelse(!is.na(v312), condom,NA),
  fem_ster=as.numeric(as.character(v312)%in%c("female sterilization","6",6)),
  fem_ster=ifelse(!is.na(v312), fem_ster,NA),
  implant=as.numeric(as.character(v312)%in%c("norplant","11","implants/norplant","implants",11)),
  implant=ifelse(!is.na(v312), implant,NA),
  parity1=as.numeric(v220),
  age1=as.character(v013),
  evermarr=(v502%in%c(1,2)),
  evermarr=ifelse(!is.na(v502), evermarr,NA),
  any_method=as.numeric(as.character(v313)%in%c("modern method","3",3,"2",2,"traditional method",1,"1","folkloric method")),
  any_method=ifelse(!is.na(v313),any_method,NA))

  # a coarse age recode #
  dhs<-dhs%>%mutate(age=recode(age1,
                               "15-19"="15-24",
                               "20-24"="15-24",
                               "25-29"="25+",
                               "30-34"="25+",
                               "35-39"="25+",
                               "40-44"="25+",
                               "45-49"="25+",
                               "1"="15-24",
                               "2"="15-24",
                               "3"="25+",
                               "4"="25+",
                               "5"="25+",
                               "6"="25+",
                               "7"="25+"
  ))
  
  
  # a coarse parity recode #
  dhs<-dhs%>%mutate(parity=recode(parity1,"0"="0",
                                  "1"="1+",
                                  "2"="1+",
                                  "3"="1+",
                                  "4"="1+",
                                  "5"="1+",
                                  "6"="1+"
  ))
  
  
  ################################################################################################
  # any_method #
  # denominator for SDG - any woman that does not want to become pregnant #
  # this is unmet need, modern method, traditional method #
  # http://sdg.iisd.org/news/demand-satisfied-infographic-explains-sdg-family-planning-indicator/
  ################################################################################################
  
  ################################
  # -- Modern Method Variable -- #
  ################################
  
  if(sum(c("modern method","3",3)%in%unique(dhs$v313))>1){
    dhs<-dhs%>%mutate(modern_method=as.numeric(as.character(v313)%in%c("modern method","3",3)),
                      modern_method=ifelse(!is.na(v313),modern_method,NA))
  }
  
  if(sum(c("modern method","3",3)%in%unique(dhs$v313))==0){ # this variable was recoded at some point
    dhs<-dhs%>%mutate(modern_method=as.numeric(as.character(v313)%in%c("modern method","2",2)),
                      modern_method=ifelse(!is.na(v313),modern_method,NA))
  }
  
  #####################################
  # -- Traditional Method Variable -- #
  #####################################
  
  dhs<-dhs%>%mutate(trad_method=as.numeric(any_method!=modern_method))
  with(dhs,table(trad_method,modern_method,useNA = "ifany"))
  
  #############################
  # -- Unmet Need Variable -- #
  #############################
  
  # Unmet Need, NA if doesn't have it #
  if(sum(c("v624","v626a")%in%names(dhs))>0){ # this variable was recoded at some point
    dhs<-Unmet(dhs, SurveyID=survey.id)
  }else{
    dhs$unmet_need<-dhs$unmet_need_space<-dhs$unmet_need_limit<-NA
  }
  
  #####################################################
  # -- set up the outcome for multinomial analysis -- #
  #####################################################
  
  with(dhs,table(unmet_need,any_method,useNA = "ifany"))
  
  dhs<-dhs%>%mutate(outcome=0,
                    outcome=ifelse(unmet_need==1,1,outcome),
                    outcome=ifelse(trad_method==1,2,outcome),
                    outcome=ifelse(modern_method==1,3,outcome))
  
  ##########################
  # -- demand satisfied -- #
  ##########################
  
  dhs<-dhs%>%mutate(demand_satisfied=ifelse(modern_method+unmet_need+trad_method>0,
                                            modern_method,NA))
}

