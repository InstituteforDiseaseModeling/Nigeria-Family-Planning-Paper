########################################################
###
### Analysis of PMA rounds 4 (2017)
### Updated October 9, 2018
library(ipumsr)


# load dataset
# labeled_data <- read.dta("Data/PMA/PMA2017_NGR4_National/PMA2017_NGR4.dta")
# labeled_data <- data2[39401:nrow(data2), ]
data <- read.csv("Data/PMA/PMA2017_NGR4_National/PMA2017_NGR4.csv")
data <- data[39401:nrow(data), ]


#--- processing info ---#
outname <- '2017'  # distinct name to put at end of output csv
data$recode <- 'PMA4'


# state names are cleanly encoded in strata var
data$state <- tolower(data$strata)

data$wt <- rowSums(data[,c("FQweight_Kaduna","FQweight_Lagos","FQweight_Anambra","FQweight_Kano",
                           "FQweight_Nasarawa","FQweight_Rivers","FQweight_Taraba")], na.rm=T)
# data$wt <- data$FQweight_National

############################################
# -- read in the summarize pma function -- #
############################################
source("UsefulFunctions/summarize_pma.R")

final<-summarize_pma(data)


# ----------------------------------------------------------------------------- #
# ---------------------- save the data ---------------------------------------- #

write_csv(as.data.frame(final),
          paste0("Data/Processed/Nigeria/Processed_Nigeria_PMA_",
                 outname, ".csv"))








