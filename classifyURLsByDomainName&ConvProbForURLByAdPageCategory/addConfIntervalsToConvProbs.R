#_________________________________________________________________________________________________________________________________________________

# Once I've run 'classifyURLsByDomainName&ConvProbForURLByAdPageCategory.r', run this script to assign binomial CIs to the outputs. Should really have just done this inside of the above script, but it's currently running... 
# So this code reads in the output of the above script and adds binomial CIs to it.

# Repeating all code 5x, once for each route.... Just need to change name of csv that gets read in, everything else updates auto=style

#_________________________________________________________________________________________________________________________________________________

### BOOKMARKS
# 1. TAS
# 2. LH
# 3. DOM
# 4. PI
# 5. allRoutes

#__________________________________________________________________________________________________________________________________

#1. TAS
#__________________________________________________________________________________________________________________________________

#===============
# set parameters
#===============

rm(list=ls())
csvPath <- 'E:/JAY/AirNZ segmentation results/'
csvName <- 'conversionProbVsAdxPageCategoryByUrlDomain&Subdomain_1randomlySelectedPointPerUser_fullRImage_samplesize0.1_TAS.csv'
library(stringr)
suppressMessages(library(dplyr))
suppressMessages(library(Hmisc)) # binom conf intervals
samplesize <- as.numeric(str_match(csvName, '(?:.*samplesize)([^_]*)')[,2]) # pull samplesize out of csv name

# NB any new parameters have to be included in any 'setdiff' statements (ie objects not to remove) in this code and all other scripts that get run by it.

#===============
# Read in data & calc probs
#===============

myCsv <- read.csv(file=paste0(csvPath, csvName))
myCsv$numNonconvs_uncorrected <- myCsv$numNonconvs*samplesize
myCsv$meanConvProb_uncorrected <- with(myCsv, sum(numConvs)/(sum(numConvs)+(sum(numNonconvs_uncorrected)))) # this is reversing the previous correction for downsampling. think I might need raw numbers to calculate binomial conf intervals.

myCsv <- arrange(myCsv, desc(numNonconvs_uncorrected))
myCsv <- (bind_cols(myCsv, 
                    as.data.frame(binconf(x=myCsv$numConvs, n=myCsv$numConvs + myCsv$numNonconvs_uncorrected))))
myCsv$status <- with(myCsv, ifelse(Upper<meanConvProb_uncorrected, 'significantlyLowerConvProbThanAverage',
                                   ifelse(Lower>meanConvProb_uncorrected, 'significantlyHigherConvProbThanAverage', 'convProbNotMeasurablyDifferentFromAverage')))

#===============
# save results
#===============

write.csv(myCsv, file=paste0(csvPath, gsub('.csv', 'plusCI.csv', csvName)), row.names=FALSE)


#__________________________________________________________________________________________________________________________________

#2. LH
#__________________________________________________________________________________________________________________________________

#===============
# set parameters
#===============

rm(list=ls())
csvPath <- 'E:/JAY/AirNZ segmentation results/'
csvName <- 'conversionProbVsAdxPageCategoryByUrlDomain&Subdomain_1randomlySelectedPointPerUser_fullRImage_samplesize0.1_LH.csv'
library(stringr)
suppressMessages(library(dplyr))
suppressMessages(library(Hmisc)) # binom conf intervals
samplesize <- as.numeric(str_match(csvName, '(?:.*samplesize)([^_]*)')[,2]) # pull samplesize out of csv name

# NB any new parameters have to be included in any 'setdiff' statements (ie objects not to remove) in this code and all other scripts that get run by it.

#===============
# Read in data & calc probs
#===============

myCsv <- read.csv(file=paste0(csvPath, csvName))
myCsv$numNonconvs_uncorrected <- myCsv$numNonconvs*samplesize
myCsv$meanConvProb_uncorrected <- with(myCsv, sum(numConvs)/(sum(numConvs)+(sum(numNonconvs_uncorrected)))) # this is reversing the previous correction for downsampling. think I might need raw numbers to calculate binomial conf intervals.

myCsv <- arrange(myCsv, desc(numNonconvs_uncorrected))
myCsv <- (bind_cols(myCsv, 
                    as.data.frame(binconf(x=myCsv$numConvs, n=myCsv$numConvs + myCsv$numNonconvs_uncorrected))))
myCsv$status <- with(myCsv, ifelse(Upper<meanConvProb_uncorrected, 'significantlyLowerConvProbThanAverage',
                                   ifelse(Lower>meanConvProb_uncorrected, 'significantlyHigherConvProbThanAverage', 'convProbNotMeasurablyDifferentFromAverage')))

#===============
# save results
#===============

write.csv(myCsv, file=paste0(csvPath, gsub('.csv', 'plusCI.csv', csvName)), row.names=FALSE)


#__________________________________________________________________________________________________________________________________

#3. DOM
#__________________________________________________________________________________________________________________________________

#===============
# set parameters
#===============

rm(list=ls())
csvPath <- 'E:/JAY/AirNZ segmentation results/'
csvName <- 'conversionProbVsAdxPageCategoryByUrlDomain&Subdomain_1randomlySelectedPointPerUser_fullRImage_samplesize0.1_DOM.csv'
library(stringr)
suppressMessages(library(dplyr))
suppressMessages(library(Hmisc)) # binom conf intervals
samplesize <- as.numeric(str_match(csvName, '(?:.*samplesize)([^_]*)')[,2]) # pull samplesize out of csv name

# NB any new parameters have to be included in any 'setdiff' statements (ie objects not to remove) in this code and all other scripts that get run by it.

#===============
# Read in data & calc probs
#===============

myCsv <- read.csv(file=paste0(csvPath, csvName))
myCsv$numNonconvs_uncorrected <- myCsv$numNonconvs*samplesize
myCsv$meanConvProb_uncorrected <- with(myCsv, sum(numConvs)/(sum(numConvs)+(sum(numNonconvs_uncorrected)))) # this is reversing the previous correction for downsampling. think I might need raw numbers to calculate binomial conf intervals.

myCsv <- arrange(myCsv, desc(numNonconvs_uncorrected))
myCsv <- (bind_cols(myCsv, 
                    as.data.frame(binconf(x=myCsv$numConvs, n=myCsv$numConvs + myCsv$numNonconvs_uncorrected))))
myCsv$status <- with(myCsv, ifelse(Upper<meanConvProb_uncorrected, 'significantlyLowerConvProbThanAverage',
                                   ifelse(Lower>meanConvProb_uncorrected, 'significantlyHigherConvProbThanAverage', 'convProbNotMeasurablyDifferentFromAverage')))

#===============
# save results
#===============

write.csv(myCsv, file=paste0(csvPath, gsub('.csv', 'plusCI.csv', csvName)), row.names=FALSE)


#__________________________________________________________________________________________________________________________________

#4. PI
#__________________________________________________________________________________________________________________________________

#===============
# set parameters
#===============

rm(list=ls())
csvPath <- 'E:/JAY/AirNZ segmentation results/'
csvName <- 'conversionProbVsAdxPageCategoryByUrlDomain&Subdomain_1randomlySelectedPointPerUser_fullRImage_samplesize0.1_PI.csv'
library(stringr)
suppressMessages(library(dplyr))
suppressMessages(library(Hmisc)) # binom conf intervals
samplesize <- as.numeric(str_match(csvName, '(?:.*samplesize)([^_]*)')[,2]) # pull samplesize out of csv name

# NB any new parameters have to be included in any 'setdiff' statements (ie objects not to remove) in this code and all other scripts that get run by it.

#===============
# Read in data & calc probs
#===============

myCsv <- read.csv(file=paste0(csvPath, csvName))
myCsv$numNonconvs_uncorrected <- myCsv$numNonconvs*samplesize
myCsv$meanConvProb_uncorrected <- with(myCsv, sum(numConvs)/(sum(numConvs)+(sum(numNonconvs_uncorrected)))) # this is reversing the previous correction for downsampling. think I might need raw numbers to calculate binomial conf intervals.

myCsv <- arrange(myCsv, desc(numNonconvs_uncorrected))
myCsv <- (bind_cols(myCsv, 
                    as.data.frame(binconf(x=myCsv$numConvs, n=myCsv$numConvs + myCsv$numNonconvs_uncorrected))))
myCsv$status <- with(myCsv, ifelse(Upper<meanConvProb_uncorrected, 'significantlyLowerConvProbThanAverage',
                                   ifelse(Lower>meanConvProb_uncorrected, 'significantlyHigherConvProbThanAverage', 'convProbNotMeasurablyDifferentFromAverage')))

#===============
# save results
#===============

write.csv(myCsv, file=paste0(csvPath, gsub('.csv', 'plusCI.csv', csvName)), row.names=FALSE)


#__________________________________________________________________________________________________________________________________

#5. allRoutes
#__________________________________________________________________________________________________________________________________

#===============
# set parameters
#===============

rm(list=ls())
csvPath <- 'E:/JAY/AirNZ segmentation results/'
csvName <- 'conversionProbVsAdxPageCategoryByUrlDomain&Subdomain_1randomlySelectedPointPerUser_fullRImage_samplesize0.1_allRoutes.csv'
library(stringr)
suppressMessages(library(dplyr))
suppressMessages(library(Hmisc)) # binom conf intervals
samplesize <- as.numeric(str_match(csvName, '(?:.*samplesize)([^_]*)')[,2]) # pull samplesize out of csv name

# NB any new parameters have to be included in any 'setdiff' statements (ie objects not to remove) in this code and all other scripts that get run by it.

#===============
# Read in data & calc probs
#===============

myCsv <- read.csv(file=paste0(csvPath, csvName))
myCsv$numNonconvs_uncorrected <- myCsv$numNonconvs*samplesize
myCsv$meanConvProb_uncorrected <- with(myCsv, sum(numConvs)/(sum(numConvs)+(sum(numNonconvs_uncorrected)))) # this is reversing the previous correction for downsampling. think I might need raw numbers to calculate binomial conf intervals.

myCsv <- arrange(myCsv, desc(numNonconvs_uncorrected))
myCsv <- (bind_cols(myCsv, 
                    as.data.frame(binconf(x=myCsv$numConvs, n=myCsv$numConvs + myCsv$numNonconvs_uncorrected))))
myCsv$status <- with(myCsv, ifelse(Upper<meanConvProb_uncorrected, 'significantlyLowerConvProbThanAverage',
                                   ifelse(Lower>meanConvProb_uncorrected, 'significantlyHigherConvProbThanAverage', 'convProbNotMeasurablyDifferentFromAverage')))

#===============
# save results
#===============

write.csv(myCsv, file=paste0(csvPath, gsub('.csv', 'plusCI.csv', csvName)), row.names=FALSE)

gc()