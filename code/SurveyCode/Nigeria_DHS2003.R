########################################################
###
### Analysis of 2003 DHS
### Updated October 9, 2018

# constants
survey.id <- "ng4b"
survey.file <- "NGIR4B"
gps.file <- "NGGE4BFL"
strata.vars <- c("v024","v025")


##############################################
# -- Read in the GPS points and shapefile -- #
##############################################
pts<- readOGR(paste0("Data/DHS/GPS/", gps.file), layer = gps.file)
shape1<- shapefile("Data/Shapes/nigeria_sdr_subnational_boundaries/shps/sdr_subnational_boundaries")
shape2<- shapefile("Data/Shapes/nigeria_sdr_subnational_boundaries/shps/sdr_subnational_boundaries2")

# adding a row number to the shape object, will be handy for plotting later on
shape1@data$row_num<-1:nrow(shape1@data)
shape2@data$row_num<-1:nrow(shape2@data)

# are the GPS points contained in the polygons? #
plot(shape2)
plot(pts,add=T,col=4)

# Assign the cluster locations to most accurate shapefile state names
x<-over(SpatialPoints(coordinates(pts),proj4string = shape2@proj4string), shape2)
key<-data.frame(v001=pts@data[,c("DHSCLUST")],state=x$REGNAME,row_num=x$row_num)
dim(state_key)
state_key<-state_key[!is.na(state_key$row_num),]#remove the row of NAs
dim(state_key)


# some clusters are not located by matching with shapefile
na_states <- is.na(x$row_num)
sum(na_states)
unlocated_clusters <- pts[na_states, ]

plot(shape2)
plot(unlocated_clusters, add=T,col=2)
table(unlocated_clusters$ADM1NAME)



##############################################
# -- read individual recode data from DHS -- #
##############################################
system.time({
  dhs<-read_dta(paste0("Data/DHS/",survey.file,"DT/",survey.file,"FL.DTA"))
}) # 5.3seconds
dim(dhs)# how many rows?
summary(dhs$v001)# the cluster variable
summary(key$v001)
length(unique(dhs$v001))
length(unique(key$v001))

# -- merge the key in with the dhs data -- #
dhs<-dhs%>%left_join(key)
# should still have the same number of rows
dim(dhs)


# -- deal with clusters without GPS points --#
# 2013 DHS states are up-to-date, so use values from sstate var.

# convert sstate from labelled double to char
dhs$recorded_state <- as.character(to_factor(dhs$sstate))
tmp <- as.character(dhs$state)
tmp[is.na(tmp)] <- dhs$recorded_state[is.na(tmp)]
dhs$state <- as.factor(tmp)


#################################################
# -- set up empty objects for survey results -- #
#################################################

both_merge<-expand.grid(c("15-24","25+"),c("0","1+"),state_key$state)
names(both_merge)<-c("age","parity","state")

both_merge$age<-as.character(both_merge$age)
both_merge$state<-as.character(both_merge$state)

parity_merge<-data.frame(age="ALL",parity=c("0","1+"),state="ALL")

age_merge<-data.frame(age=c("15-24","25+"),parity="ALL",state="ALL")
age_merge$age<-as.character(age_merge$age)

state_merge<-data.frame(age="ALL",parity="ALL",state=state_key$state)




###############################
# -- start analyzing survey --#
###############################

# - first checking to see if "awfctt" variable is in the dataset
# - This is the all women factor, if it is not there we add it
# - it will only be in surveys that ONLY survey married women
if(!("awfctt"%in%names(dhs))){dhs$awfactt<-100}

dhs<-dhs%>%mutate(wt=v005/1000000, # have to scale the weight
                  year_cmc=floor((v008-1)/12)+1900, # finding the year based on Century Month Codes
                  month_cmc=v008-(year_cmc-1900)*12, # finding the month
                  year=year_cmc+month_cmc/12, # grabbing year
                  country_code=substr(v000,start=1,stop=2), # finding the country code
                  recode=substr(v000,3,3) # which recode manual to look at
                  
)


##########################################
# -- recode the variables of interest -- #
##########################################
dhs<-add_fp_indicators(dhs,unmet_need_surveyID=survey.id)



##########################################
# -- Set up the survey design object -- #
##########################################

# for design variables look at: 
# ~\Dropbox (IDM)\SmallAreaEstimationForFP\LiteratureReview\U5MR Supplement 2017-11-12.pdf
# Table 2 provides variables

# -- create strata variable -- #
strata<-unique(dhs[,strata.vars])
strata$strata<-1:nrow(strata)
dhs<-dhs%>%left_join(strata)

my.svydesign <- svydesign(id= ~v001,
                          strata=~strata,nest=T, 
                          weights= ~wt, data=dhs)



###############################################################
## Compute population proportions for vars  ###################
###############################################################

final <- compute_survey_proportions(dhs)
final$year<-floor(dhs$year[1])
final$recode<-dhs$recode[1]
final$country<-"Nigeria"

write_csv(as.data.frame(final),
          "Data/Processed/Nigeria/Processed_Nigeria2003.csv")




