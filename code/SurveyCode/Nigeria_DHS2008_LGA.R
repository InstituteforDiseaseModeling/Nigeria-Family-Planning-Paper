########################################################
###
### Analysis of 2008 DHS at the LGA level
### Updated October 24, 2018

# constants
survey.id <- "ng53"
survey.file <- "NGIR53"
gps.file <- "NGGE52FL"
strata.vars <- c("v024","v025")


##############################################
# -- Read in the GPS points and shapefile -- #
##############################################
pts<- readOGR(paste0("Data/DHS/GPS/", gps.file), layer = gps.file)
shape1<- shapefile("Data/Shapes/nigeria_sdr_subnational_boundaries/shps/sdr_subnational_boundaries")
shape2<- shapefile("Data/Shapes/nigeria_sdr_subnational_boundaries/shps/sdr_subnational_boundaries2")
shape3<- shapefile("Data/Shapes/nigeria_sdr_subnational_boundaries/shps/NIE_Admin2")

# adding a row number to the shape object, will be handy for plotting later on
shape1@data$row_num<-1:nrow(shape1@data)
shape2@data$row_num<-1:nrow(shape2@data)
shape3@data$row_num<-1:nrow(shape3@data)

# are the GPS points contained in the polygons? #
plot(shape3)
plot(pts,add=T,col=4)

# Assign the cluster locations to most accurate shapefile state names
x<-over(SpatialPoints(coordinates(pts),proj4string = shape3@proj4string), shape3)
key<-data.frame(v001=pts@data[,c("DHSCLUST")],lga=x$LGAName,row_num=x$row_num)

# use the shape3 #
lga_key<-data.frame(lga=shape3@data$LGAName,row_num=shape3@data$row_num)


# some clusters are not located by matching with shapefile
na_lgas <- is.na(x$row_num)
sum(na_lgas)
unlocated_clusters <- pts[na_lgas, ]

plot(shape3)
plot(unlocated_clusters, add=T,col=2)
table(unlocated_clusters$ADM1NAME)


##############################################
# -- read individual recode data from DHS -- #
##############################################
system.time({
  dhs<-read_dta(paste0("Data/DHS/",survey.file,"DT/",survey.file,"FL.DTA"))
}) # 27.12 seconds
dim(dhs)# how many rows?
summary(dhs$v001)# the cluster variable
summary(key$v001)
length(unique(dhs$v001))
length(unique(key$v001))


# -- merge the key in with the dhs data -- #
dim(dhs)
dhs<-dhs%>%left_join(key)
# should still have the same number of rows
dim(dhs)


#################################################
# -- set up empty objects for survey results -- #
#################################################
lga_merge<-data.frame(age="ALL",parity="ALL",row_num=lga_key$row_num)



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

#just need to analyze mCPR by state for illustration#
formula<-~modern_method

system.time({
LGA<-svyby(formula,
                ~row_num,my.svydesign,svymean,na.rm=T)
})#


dim(LGA)
final<-LGA%>%right_join(lga_merge)
final<-final%>%right_join(lga_key)
dim(final)

final$year<-floor(dhs$year[1])
final$recode<-dhs$recode[1]
final$country<-"Nigeria"


write_csv(as.data.frame(final),
          "Data/Processed/Nigeria/LGA_Nigeria2008.csv")




