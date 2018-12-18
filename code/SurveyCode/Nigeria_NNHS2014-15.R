########################################################
###
### Analysis of 2014 and 2015 NNHS
### Updated October 9, 2018

##################################################################
# ------------------------- NNHS 2014 ---------------------------#
##################################################################
dat<-read_dta("Data/NNHS/NNHS2014_Dataset/140922_NNHS_R1_2014_Women.dta")

####################################
# # -- details of the 2014 NNHS -- #
# http://www.nigerianstat.gov.ng/nada/index.php/catalog/54/datafile/F3

# same 279 women missing contraceptive type #
dim(dat)
dat<-dat%>%filter(!is.na(fp1))
dim(dat)
dat<-dat%>%mutate(modern_method=as.numeric(
  fp1|fp3|fp4|fp5|fp6|fp7|fp8|fp9|fp10|fp11))


# set up the design #
smart_des <- svydesign(data = dat, 
                       id = ~cluster+hhno, 
                       weights = ~wnatwg, 
                       strata = ~state,nest=T)

svymean(~modern_method,smart_des)
#                  mean     SE
# modern_method 0.14978 0.0039

smart_2014_state <- as.data.table(svyby(formula = ~modern_method, by = ~state, 
                                        design = smart_des, FUN = svymean))

write_csv(smart_2014_state,"Data/Processed/Nigeria/Processed_Nigeria_NNHS_2014.csv")

##################################################################
# ------------------------- NNHS 2015 ---------------------------#
##################################################################
dat<-read_dta("Data/NNHS/2015NNHS_FinalDataset/2015NNHS_Women_Final.dta")

####################################
# # -- details of the 2015 NNHS -- #
# http://www.nigerianstat.gov.ng/nada/index.php/catalog/53/datafile/F3

# no missing data #
dim(dat)
dat<-dat%>%filter(!is.na(fp1))
dim(dat)

dat<-dat%>%mutate(modern_method=as.numeric(
  fp1|fp3|fp4|fp5|fp6|fp7|fp8|fp9|fp10|fp11))

#####################
# set up the design #
smart_des <- svydesign(data = dat, 
                       id = ~cluster+hhno, 
                       weights = ~sampwgt, 
                       strata = ~state,nest=T)


smart_2015_state <- as.data.table(svyby(formula = ~modern_method, by = ~state, 
                                        design = smart_des, FUN = svymean))
svymean(~modern_method,smart_des)
#                  mean    SE
# modern_method 0.11865 0.004


write_csv(smart_2015_state,"Data/Processed/Nigeria/Processed_Nigeria_NNHS_2015.csv")
