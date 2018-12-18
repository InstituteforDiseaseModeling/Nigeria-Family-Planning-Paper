########################################################
###
### Analysis of PMA rounds 1-3 (2014-2016)
### Updated October 9, 2018
library(ipumsr)

##################################
# -- read in the PMA2020 data -- #
##################################

ddi <- read_ipums_ddi("Data/PMA/pma_00002.xml")
data2 <- read_ipums_micro(ddi)



#####################################################################
# ------------ select header code for region and round ------------ #

####################
#--- 2014 Lagos ---#
data <- subset(data2, ROUND == 1)
data <- subset(data, !is.na(FQWEIGHT_LAGOS))
data$recode <- 'PMA1'
data$state <- 'lagos'
data$wt <- data$FQWEIGHT_LAGOS
outname <- 'lagos2014'  # distinct name to put at end of output csv

final<-summarize_pma_ipums(data)

write_csv(as.data.frame(final),
          paste0("Data/Processed/Nigeria/Processed_Nigeria_PMA_",
                 outname, ".csv"))

#####################
#--- 2014 Kaduna ---#
data <- subset(data2, ROUND == 1)
data <- subset(data, is.na(FQWEIGHT_LAGOS))
data$recode <- 'PMA1'
data$state <- 'kaduna'
data$wt <- data$FQWEIGHT_KADUNA
outname <- 'kaduna2014'  # distinct name to put at end of output csv

final<-summarize_pma_ipums(data)

write_csv(as.data.frame(final),
          paste0("Data/Processed/Nigeria/Processed_Nigeria_PMA_",
                 outname, ".csv"))

####################
#--- 2015 Lagos ---#
data <- subset(data2, ROUND == 2)
data <- subset(data, !is.na(FQWEIGHT_LAGOS))
data$recode <- 'PMA2'
data$state <- 'lagos'
data$wt <- data$FQWEIGHT_LAGOS
outname <- 'lagos2015'  # distinct name to put at end of output csv


final<-summarize_pma_ipums(data)

write_csv(as.data.frame(final),
          paste0("Data/Processed/Nigeria/Processed_Nigeria_PMA_",
                 outname, ".csv"))

#####################
#--- 2015 Kaduna ---#
data <- subset(data2, ROUND == 2)
data <- subset(data, is.na(FQWEIGHT_LAGOS))
data$recode <- 'PMA2'
data$state <- 'kaduna'
data$wt <- data$FQWEIGHT_KADUNA
outname <- 'kaduna2015'  # distinct name to put at end of output csv

final<-summarize_pma_ipums(data)

write_csv(as.data.frame(final),
          paste0("Data/Processed/Nigeria/Processed_Nigeria_PMA_",
                 outname, ".csv"))


##############
#--- 2016 ---#
data <- subset(data2, ROUND == 3)
outname <- '2016'  # distinct name to put at end of output csv
states <- data.frame(state=c("lagos","kaduna","anambra","kano","nasarawa","taraba","rivers"),
                     STRATA=c(56606,56604,56603,56605,56607,56609,56608)
)
data$recode <- 'PMA3'
data <- left_join(data,states)
data$wt <- data$FQWEIGHT

final<-summarize_pma_ipums(data)

write_csv(as.data.frame(final),
          paste0("Data/Processed/Nigeria/Processed_Nigeria_PMA_",
                 outname, ".csv"))



# ipums_var_desc() is function to get more info on var







