########################################################
###
### Analysis of 2016 MICS
### Updated October 9, 2018

####################################################
# -- load harmonized data for Nigeria MICS 2016 -- #
####################################################

load("Data/MICS/ngwm5.RData")
surveyID<-"ngwm5"

#Only calculate women age 15-49
if("[50-54]"%in%df$AGE5YEAR_LAB){
  df[which(df$AGE5YEAR_LAB == "[50-54]"),] <- NA
}


# compute secondary variables of interest
df <- MaritalStatusCategories(df, "Both")
df <- MethodCategories(df)
df <- CalcUnmet(df)


################################
# -- Prepare survey objects -- #
################################

# recode variables of interest
df<-df%>%mutate(
  unmet_need=as.numeric(as.character(UnmetSim)=="Unmet_Need"),
  unmet_need=ifelse(as.character(UnmetSim)!="MissingData", unmet_need, NA),
  
  # should add NAs corresponding to unmet_need NAs
  unmet_need_space=as.numeric(as.character(Specific_unmet)=="UnmetNeed_for_Spacing"),
  unmet_need_limit=as.numeric(as.character(Specific_unmet)=="UnmetNeed_for_Limiting"),
  
  pill=as.numeric(as.character(FPMETHOD_LAB)=="PILL"),
  iud=as.numeric(as.character(FPMETHOD_LAB)=="IUD_IUS"),
  injections=as.numeric(as.character(FPMETHOD_LAB)=="INJ"),
  diaphragm=as.numeric(as.character(FPMETHOD_LAB)=="DIA"),
  condom=as.numeric(as.character(FPMETHOD_LAB)=="CONM"),
  fem_ster=as.numeric(as.character(FPMETHOD_LAB)=="FST"),
  implant=as.numeric(as.character(FPMETHOD_LAB)=="IMP"),
  
  any_method=as.numeric(as.character(FPANY_LAB)=="Using_any"),
  modern_method=as.numeric(as.character(FPTYPE_LAB)=="Using_modern"),
  
  evermarr=(MARSTAT_LAB%in%c("Married or living together", "Formerly in union")),
  evermarr=ifelse(!MARSTAT_LAB=="9", evermarr,NA)
)


# traditional method variable
df<-df%>%mutate(trad_method=as.numeric(any_method!=modern_method))
with(df,table(trad_method,modern_method,useNA = "ifany"))

# demand satisfied variable
df<-df%>%mutate(demand_satisfied=ifelse(modern_method+unmet_need+trad_method>0,
                                          modern_method,NA))

# a coarse age recode #
df<-df%>%mutate(age=recode(AGE5YEAR_LAB,"[15-19]"="15-24",
                           "[20-24]"="15-24",
                           "[25-29]"="25+",
                           "[30-34]"="25+",
                           "[35-39]"="25+",
                           "[40-44]"="25+",
                           "[45-49]"="25+"
))

# a coarse parity recode #
df<-df%>%mutate(parity=ifelse(as.numeric(ceb)>0, "1+", "0")
)

# extract state variable
df$state<-tolower(as_factor(df$hh7))
df$state[df$state == "fct abuja"] <- "fct-abuja"

#################################################
# -- set up empty objects for survey results -- #
#################################################

both_merge<-expand.grid(c("15-24","25+"), c("0","1+"), unique(df$state))
names(both_merge)<-c("age","parity","state")

both_merge$age<-as.character(both_merge$age)
both_merge$state<-as.character(both_merge$state)

parity_merge<-data.frame(age="ALL",parity=c("0","1+"),state="ALL")

age_merge<-data.frame(age=c("15-24","25+"),parity="ALL",state="ALL")
age_merge$age<-as.character(age_merge$age)

state_merge<-data.frame(age="ALL",parity="ALL",state=unique(df$state))



##########################################
# -- Set up the survey design object -- #
##########################################
df_cm <- subset(df, subset=MSTAT_LAB%in%c("Married/In-union"))
df_nm <- subset(df, subset=MSTAT_LAB%in%c("Unmarried/Not-in-union"))


# hh7 is state names (assumed for now to be strata variable)
my.svydesign <- svydesign(id= ~as.numeric(CLUSTERNO),
                          strata=~hh7,nest=T, 
                          weights= ~wmweight, data=df)



final <- compute_survey_proportions(df)

final$year<-2016
final$recode<-"MICS5"
final$country<-"Nigeria"


# ----------------------------------------------------------------------------- #
# ---------------------- save the data ---------------------------------------- #

write_csv(as.data.frame(final),
          "Data/Processed/Nigeria/Processed_Nigeria2016.csv")



