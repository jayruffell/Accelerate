
#__________________________________________________________________________________________________________________________________

# Add description of what code does here...
#__________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________

# 0. Set parameters and load data ----
#__________________________________________________________________________________________________________________________________

#+++++++++++++++++
# set params
#+++++++++++++++++

rm(list=ls())
set.seed(1)
dataset <- 'full'
basePath <- 'E:/JAY/'
pathData <- paste0(basePath, 'data/')
pathSave <- paste0(basePath, 'AirNZ segmentation results/')
responseVar <- 'lastClickConversion' # 'Conversion' or 'lastClickConversion'. Former is whether a user converted at any point over model period; latter is whether a given ad was attributed a covnersion via last click methodology. lastClickConv only applies to outputDatabase==df_oneRowPerAdAndLastClickConversionResponse

# Define which types of ads we want to look at     ***CURRENTLY ONLY IMPLEMENTED FOR df_oneRowPerAdAndLastClickConversionResponse ***
offerAds <- 'allRoutes' # 'TAS', 'DOM', 'PI', 'LH', 'allRoutes' # e.g. if offerAds==TAS then we would only include ads that have a 'Tasman sale' message in the model
layerAds <- 'all' # 'notRemarketing', 'Remarketing', or 'all'. If 'notRemarketing' then exclude RMK ads from model.
groupAds <- 'all' # 'retail', 'brand', or 'all'. Az reckons we would only ever use the model to buy retail ads (i.e. ads relating to specific sales), but including other options for completeness (and may be required if there aren't enough convs just looking at retail & non-RMK).

# Define which var sets to include in models
varCategories <- c('responseVar', 'adCatVars', 'regionVars', 'timeVars') # responseVar, adCatVars, urlVars, deviceVars, regionVars, countryCodeVars, bidPriceVars, timeVars. # See bottom of BKMK 0 for which vars are included in each of these sets.

# Define cross validation training control params *NB needed values of at least ~15 & ~15 in intial tests, otherwise end up with NAs in predictions.  
horizon=15
initialWindow=15 # see http://stackoverflow.com/questions/30233144/time-series-splitting-data-using-the-timeslice-method for deets.

# Set params which influence how many segments end up in final outputs
numLambdaSEs <- 1 # if using lasso feature selection, pick highest lambda that is within [numLambdaSEs] standard errors of best model. Common advice is that the best model within 1se is the best, but I've found this retains many features and heaps of segments. NB the code will output AUC values for best vs lambdaSE model, so can review how much worse the stronger feature selection makes the model.
liftThreshold <- 1.25 
numRowsThreshold <- 10000 # in full df - after reversing negativeInstanceDownsampling - how many rows (imps) should each ad have?

# Add in params used to read in data & poss name outputs - need to match names in EDA&CreateModelDatabase.r:
firstDate <- '2016-11-16'
maxDate <- '2016-12-31'
negativeInstanceDownsampling <- 0.001
route <- 'allRoutes'    
outputDatabase <- 'df_oneRowPerAdAndLastClickConversionResponse'              # these params have no other use; just used to pull in R data

#+++++++++++++++++
# load data
#+++++++++++++++++
  
cat(paste0('\n---- Loading data - ', dataset, 'DF // ', route, ' // offer ads: ', offerAds, ' // group ads: ', groupAds, ' // layer ads: ', layerAds, ' // date range: ', gsub('-', '', firstDate), '-', gsub('-', '', maxDate),' ----\n\n'))

load(paste0(pathData, outputDatabase, '_negativeInstanceDownsampling', negativeInstanceDownsampling, '_', dataset, 'DF_', route, '_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.rda'))

df <- df_oneRowPerAdAndLastClickConversionResponse # easier to type
rm(df_oneRowPerAdAndLastClickConversionResponse)

suppressMessages(library(dplyr))
suppressMessages(library(ggplot2))
suppressMessages(library(psych))
# suppressMessages(library(mlr)) # not available for MRAN
suppressMessages(library(caret))
suppressMessages(library(glmnet))
# suppressMessages(library(mlbench))
suppressMessages(library(rattle)) # for nice tree plots
library(partykit) # improved trees (vs party, which is used by caret)
library(parallel)
library(doParallel)

#+++++++++++++++++
# Modify vars for modelling
#+++++++++++++++++

if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){
  df$Converter <- df[,which(grepl('lastClickConversion', colnames(df)))]
  df <- select(df, -lastClickConversion)
  cat(' WARNING - renaming "lastClickConversion" var to "Converter", Just so theres a single name for the target var thru rest of code. \n')
}
df$Converter <- as.factor(ifelse(df$Converter==1, 'y', 'n')); cat(' Changing conv/nonconv values from 1/0 to y/n for modelling\n')
df <- select(df, -user_id)
df$DBMRegion <- gsub('-', '', df$DBMRegion) # 'Manawatu-Wanganui' hyphen caused probs in models.

# Convert any character vars to factors - can cause problems for random forest otherwise
df$DBMRegion <- as.factor(df$DBMRegion)
df$day <- as.factor(df$day)
df$daypart <- as.factor(df$daypart)
cat(' NB make sure any new character vars are converted to factors to avoid potential errors with random forest\n')

#+++++++++++++++++
# Define variable groups - makes it easy to quickly include/exclude vars from models
#+++++++++++++++++

responseVar <- 'Converter'

adCatVars <- colnames(df)[grepl('adX_', colnames(df))] 
urlVars <- colnames(df)[grepl('url_', colnames(df))] 
deviceVars <- colnames(df)[grepl('Browser', colnames(df)) |grepl('Device', colnames(df)) | 
                               grepl('Operating', colnames(df)) | grepl('Mobile', colnames(df))]
regionVars <- 'DBMRegion'
countryCodeVars <- 'CountryCode'
bidPriceVars <- 'medianBidPrice'
timeVars <- c('daypart', 'day')
# ==> subsequent code will be like this 'modelDF <- select(df, which(colnames(df) %in% c(responseVar, adCatVars, regionVars)))'

#+++++++++++++++++
# check for and remove NAs
#+++++++++++++++++

naRows <- nrow(filter(df, is.na(DBMRegion)))
if(naRows >0){
  cat(paste0(' WARNING - ', naRows, ' rows have NAs & have been removed\n'))
  df <- na.omit(df)
}

#+++++++++++++++++++
# read in createIrregularTimeSlices fucntion. See here for details https://r-norberg.blogspot.com.au/2016/08/data-splitting-time-slices-with.html
#+++++++++++++++++++

createIrregularTimeSlices <- function(y, initialWindow, horizon = 1, unit = c("sec", "min", "hour", "day", "week", "month", "year", "quarter"), fixedWindow = TRUE, skip = 0) {
  if(inherits(y, 'Date')) y <- as.POSIXct(y)
  stopifnot(inherits(y, 'POSIXt'))
  
  # generate the sequence of date/time values over which to split. These will always be in ascending order, with no missing date/times.
  yvals <- seq(from = lubridate::floor_date(min(y), unit), 
               to = lubridate::ceiling_date(max(y), unit), 
               by = unit)
  
  # determine the start and stop date/times for each time slice
  stops <- seq_along(yvals)[initialWindow:(length(yvals) - horizon)]
  if (fixedWindow) {
    starts <- stops - initialWindow + 1
  }else {
    starts <- rep(1, length(stops))
  }
  
  # function that returns the indices of y that are between the start and stop date/time for a slice 
  ind <- function(start, stop, y, yvals) {
    which(y > yvals[start] & y <= yvals[stop])
  }
  train <- mapply(ind, start = starts, stop = stops, MoreArgs = list(y = y, yvals = yvals), SIMPLIFY = FALSE)
  test <- mapply(ind, start = stops, stop = (stops + horizon), MoreArgs = list(y = y, yvals = yvals), SIMPLIFY = FALSE)
  names(train) <- paste("Training", gsub(" ", "0", format(seq(along = train))), sep = "")
  names(test) <- paste("Testing", gsub(" ", "0", format(seq(along = test))), sep = "")
  
  # reduce the number of slices returned if skip > 0
  if (skip > 0) {
    thin <- function(x, skip = 2) {
      n <- length(x)
      x[seq(1, n, by = skip)]
    }
    train <- thin(train, skip = skip + 1)
    test <- thin(test, skip = skip + 1)
  }
  
  # eliminate any slices that have no observations in either the training set or the validation set
  empty <- c(which(sapply(train, function(x) length(x) == 0)),
             which(sapply(test, function(x) length(x) == 0)))
  if(length(empty) > 0){
    train <- train[-empty]
    test <- test[-empty]
  }
  
  out <- list(train = train, test = test)
  out
}


#_______________________________________________________________________________________________________________________________________________________________

# 1.i. Based on values for ad attributes in parameters, either include or exclude individual ads from analysis ----
#__________________________________________________________________________________________________________________________________

if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){

  cat('\n----Filtering database by specified ad types (e.g. exclude RMK ads). Only doing for outputDatabase==df_oneRowPerAdAndLastClickConversionResponse for now... need to think about whether I should do for the other output databases too----\n\n')

  dfPositiveInstancesPreAdRemoval <- nrow(filter(df, Converter=='y'))
  dfNegativeInstancesPreAdRemoval <- nrow(filter(df, Converter=='n'))
  
  if(offerAds!='allRoutes'){ # if route==allRoutes then don't need this step
    df <- filter(df, OFFER==route)
  } else if(offerAds=='allRoutes'){
    df <- df
  } else (cat(' WARNING - check offerAds value\n'))
  # unique(df$OFFER)

  if(layerAds=='notRemarketing'){
    df <- filter(df, LAYER!='Remarketing')
  } else if(layerAds=='Remarketing'){
    df <- filter(df, LAYER=='Remarketing')
  } else if(layerAds=='all'){
    df <- df
  } else (cat(' WARNING - check layerAds value\n'))
  # unique(df$LAYER)

  if(groupAds=='retail'){
    df <- filter(df, GROUP=='RETAIL')
  } else if(groupAds=='brand'){
    df <- filter(df, GROUP=='BRAND')
  } else if(groupAds=='all'){
    df <- df
  } else (cat(' WARNING - check groupAds value\n'))
  # unique(df$GROUP)

  dfPositiveInstancesPostAdRemoval <- nrow(filter(df, Converter=='y'))
  dfNegativeInstancesPostAdRemoval <- nrow(filter(df, Converter=='n'))

  cat(paste0('  - Number of positive instances before / after ad removal: ', dfPositiveInstancesPreAdRemoval, ' / ', dfPositiveInstancesPostAdRemoval, '\n',
             '  - Number of negative instances before / after ad removal: ', dfNegativeInstancesPreAdRemoval, ' / ', dfNegativeInstancesPostAdRemoval, '\n'))
  }

#__________________________________________________________________________________________________________________________________

# 1.i. create 'modelDF', which contains a subset of predictors as defined in parameters (NB may be subsequent feature selection) ----
#__________________________________________________________________________________________________________________________________

cat('\n----Creating "modelDF", which contains a subset of predictors for input into models (NB may still be subsequent feature selection----\n\n')

# adCatVars, urlVars, deviceVars, regionVars, countryCodeVars, bidPriceVars, timeVars
varsForModelVec <- c(responseVar) # time needed for createTimeSlices cross validation
if('adCatVars' %in% varCategories) varsForModelVec <- c(varsForModelVec, adCatVars)
if('urlVars' %in% varCategories) varsForModelVec <- c(varsForModelVec, urlVars)
if('deviceVars' %in% varCategories) varsForModelVec <- c(varsForModelVec, deviceVars)
if('regionVars' %in% varCategories) varsForModelVec <- c(varsForModelVec, regionVars)
if('countryCodeVars' %in% varCategories) varsForModelVec <- c(varsForModelVec, countryCodeVars)
if('bidPriceVars' %in% varCategories) varsForModelVec <- c(varsForModelVec, bidPriceVars)
if('timeVars' %in% varCategories) varsForModelVec <- c(varsForModelVec, timeVars)

modelDF <- select(df, which(colnames(df) %in% varsForModelVec))
modelDFTimes <- df$time # needed for createTimeSlices

#__________________________________________________________________________________________________________________________________

# # 2. Some quick EDA - plot & model bivariate relationships between predictors and response ----
# #__________________________________________________________________________________________________________________________________
# 
# plotDF <- mutate(df, ConverterNumeric=ifelse(Converter=='y', 1, 0))
# windows()
# ggplot(plotDF, aes(daypart, ConverterNumeric)) + geom_jitter(alpha=0.5) + stat_summary(fun.data="mean_cl_boot", colour = "red", size = 0.75)
# graphics.off()
# x <- glm(Converter~daypart, data=df, family='binomial')
# summary(x)
# rm(x)


#__________________________________________________________________________________________________________________________________

# 3. Preliminary modelling - run major model types on all predictors, using caret's default hyperparam test values and caret's downsampling to balance sample, then compare performance ----
#__________________________________________________________________________________________________________________________________

cat('\n----Running some major models using downsampling & caret hyperparameter defaults, to compare predictive performance----\n\n')

set.seed(0803)

# Set up parallel backend
cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
registerDoParallel(cl)
getDoParWorkers() # check how many cores are getting used

# Set up training control
my_partitions <- createIrregularTimeSlices(modelDFTimes, initialWindow=initialWindow, horizon=horizon, unit = "day", fixedWindow = T) # see here for description of this function https://r-norberg.blogspot.com.au/2016/08/data-splitting-time-slices-with.html
  
control <- trainControl(index=my_partitions$train, 
                        indexOut=my_partitions$test, # part of createIrregularTimeSlices - see above
                        # method="repeatedcv", number=5, repeats=5,
                        summaryFunction=twoClassSummary,	# Use AUC to pick the best model (e.g. lambda in glmnet)
                        classProbs=TRUE,
                        sampling='down') # downsample within each resampling fold, then test on full holdout fold. https://topepo.github.io/caret/subsampling-for-class-imbalances.html for details

#+++++++++++++++++
# run models using caret defaults
#+++++++++++++++++
  
mGlmnet <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC')
mSVMLinear <- train(Converter~., data=modelDF, method="svmLinear", trControl=control, metric='ROC')
mSVMRadial <- train(Converter~., data=modelDF, method="svmRadial", trControl=control, metric='ROC')
mLR <- train(Converter~., data=modelDF, method="glm", family='binomial', trControl=control, metric='ROC')
mCTree <- train(Converter~., data=modelDF, method="ctree", trControl=control, metric='ROC')
# mRTree <- train(Converter~., data=modelDF, method="rpart", trControl=control, metric='ROC')
mRF <- train(Converter~., data=modelDF, method="rf", trControl=control, metric='ROC') # NB recommnedation for mtry param is mtry=floor(sqrt(ncol(x))) or mtry=7, see http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

#+++++++++++++++++++
# Compare predictive performance
#+++++++++++++++++++

AUCs <- resamples(list(Glmnet = mGlmnet, SVMLinear = mSVMLinear, SVMRadial=mSVMRadial, LogReg = mLR, RF=mRF, Ctree=mCTree))
windows()
bwplot(AUCs) # log reg pretty good; glment slightly better
graphics.off()

AUCtable <- select(AUCs$values, which(!grepl('Sens', colnames(AUCs$values)) & 
                                        !grepl('Spec', colnames(AUCs$values)) & colnames(AUCs$values)!='Resample'))

colnames(AUCtable) <- gsub('~ROC', '', colnames(AUCtable))
modelNames <- colnames(AUCtable)
AUCtable <- data.frame(model=modelNames,
                       median=round(apply(AUCtable, 2, quantile, 0.5), 2),
                       lowerQ=round(apply(AUCtable, 2, quantile, 0.25), 2), 
                       upperQ=round(apply(AUCtable, 2, quantile, 0.75), 2))
AUCtable <- arrange(AUCtable, desc(median))
  
cat(' AUC of major models, using caret defaults:\n')
print(AUCtable, row.names=F)

# #__________________________________________________________________________________________________________________________________
# 
# 4.i. Feature selection methods -  try use GLMnet with lasso, modifying lambda to get different degrees of feature selection, then pick model with fewest features that is within 1se of best ----
# #__________________________________________________________________________________________________________________________________

cat(paste0('\n----Using lasso for feature selection - picking lasso model with highest lambda within ', numLambdaSEs, ' standard errors of best lasso to get maximum feature selection while still retaining a good model----\n\n'))
  
### Standard rule is the '1se rule'; see here https://sebastianraschka.com/blog/2016/model-evaluation-selection-part3.html. BUT too many segments result from this, so I'm going to go with 2se instead

# NB I manually find the lambda value that produces a model within 2se (looking at the se's of the different AUC values and finding lambda within 2se)... but if using glmnet c.f. caret I could also just use lambda.1se for predictions, which is provided within the package. BUT running with caret has benefits, i.e. automated downsampling and preprocessing; easy AUCs etc.

set.seed(1203)

# Set up parallel backend
cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
registerDoParallel(cl)
getDoParWorkers() # check how many cores are getting used

#+++++++++++++++++
# run models using caret defaults, to see if setting alpha=1 (which then allows feature selecting by tuning lambda) gives an ok model
#+++++++++++++++++

mGlmnet <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC')

glmnetROCDF <- mGlmnet$results
if(glmnetROCDF$alpha[which.max(glmnetROCDF$ROC)]==1){
  cat(paste0(' Running glmnet with caret defaults to see if lasso is best (can then tune lambda for feature selection)... yes best model has alpha=1, ROC=',
             round(glmnetROCDF$ROC[which.max(glmnetROCDF$ROC)], 2), '\n'))
} else {
  cat(paste0(' Running glmnet with caret defaults to see if lasso is best (can then tune lambda for feature selection)... nope, best model has alpha=',
             glmnetROCDF$alpha[which.max(glmnetROCDF$ROC)], ', ROC=', round(glmnetROCDF$ROC[which.max(glmnetROCDF$ROC)], 2), '. Best lasso model has ROC=',
             round(filter(glmnetROCDF, alpha==1)$ROC[which.max(filter(glmnetROCDF, alpha==1)$ROC)], 2), '\n'))
}

# Look at lambda and alpha values chosen by train function:
mGlmnet$bestTune$lambda; mGlmnet$bestTune$alpha # extract best hyperparam values from cross validation grid

# final model is then fitted on all data, based on best alpha above BUT new lambda sequence then gets automatically generated by glmnet:
mGlmnet$finalModel$lambda # new sequence of lambdas, but can ignore these and just predict based on best from caret grid
mGlmnet$finalModel$tuneValue$alpha
mGlmnet$finalModel$tuneValue$lambda # optimal values from train function
mGlmnet$finalModel$lambdaOpt # can also get best lambda from train() this way

#+++++++++++++++++++
# Retrain lasso model using expanded training grid, to try a range of lambdas outside of caret defaults
#+++++++++++++++++++

# extend tuning grid beyond values selected by caret default - use mix of caret defaults & glmnet defauts
lambda_combinedRange <- c(min(mGlmnet$results$lambda), max(mGlmnet$results$lambda), # lambda range selected by caret train function
                          min(mGlmnet$finalModel$lambda), max(mGlmnet$finalModel$lambda)) # new lambda range selected by glmnet after model is fitted to new data.

tGrid <- expand.grid(alpha=1, # wanting feature selection, so set alpha=1
                     lambda=seq(from=min(lambda_combinedRange), to=max(lambda_combinedRange), length.out=10))

# Set up parallel backend
cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
registerDoParallel(cl)
getDoParWorkers() # check how many cores are getting used

# Rerun model
mGlmnet2 <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC', tuneGrid=tGrid)

#+++++++++++++++++++
# Explore resampling results and look at model that needs fewest predictors while also being within 1se of best performing model
#+++++++++++++++++++

# # Quick EDA to confirm that lambda controls strength of penalty and therefore no. of predictors that get shrunk to zero:
# numCoeffsList <- list()
# for(i in 1:length(tGrid$lambda)){
#   lambda_i <- tGrid$lambda[i]
#   numCoeffsList[[i]] <- data.frame(lambda=lambda_i,
#                                    numPreds=length(coef(mGlmnet2$finalModel, s=lambda_i)[coef(mGlmnet2$finalModel, s=lambda_i)!=0]))
# }
# numCoeffsDF <- bind_rows(numCoeffsList)
# numCoeffsDF
# windows()
# ggplot(numCoeffsDF, aes(lambda, numPreds)) + geom_line(colour='purple')
# graphics.off()

# Use ggplot to find model with fewest predictors that is still <1se from best model
inputModel <- mGlmnet2
seDF <- inputModel$results %>%
  mutate(roc2SE_low = ROC-numLambdaSEs*(ROCSD/sqrt(inputModel$control$number*inputModel$control$repeats)),
         roc2SE_high = ROC+numLambdaSEs*(ROCSD/sqrt(inputModel$control$number*inputModel$control$repeats)), # calculate std errors of resamples; see http://stats.stackexchange.com/questions/206139/is-there-a-way-to-return-the-standard-error-of-cross-validation-predictions-usin
         modelPerformance='other')
seDF$modelPerformance[which(seDF$ROC==max(seDF$ROC))] <- 'best'
seDF$within2seVec <- ifelse(seDF$roc2SE_high>=max(seDF$ROC) & seDF$modelPerformance!='best', 'y', 'n')
seDF$modelPerformance[which(seDF$lambda==max(seDF$lambda[seDF$within2seVec=='y']))] <- paste0('fewestPredsWithin', numLambdaSEs, 'SE')

windows()
ggplot(seDF, aes(lambda, ROC, colour=modelPerformance)) + geom_point(size=2) +
  geom_errorbar(aes(ymin=roc2SE_low, ymax=roc2SE_high), width=0) + ggtitle(paste0('ROC +/- ', numLambdaSEs, 'se'))
graphics.off()

# find biggest lambda (i.e. fewest predictors) within 2se, & report no. of predictors excluded
bestLambda <- seDF$lambda[seDF$modelPerformance=='best']
highestLambda_withinXse <- seDF$lambda[grepl('fewestPredsWithin', seDF$modelPerformance)]
numPreds_bestModel=length(coef(inputModel$finalModel, s=bestLambda)[coef(inputModel$finalModel, s=bestLambda)!=0])
numPreds_within2se=length(coef(inputModel$finalModel, s=highestLambda_withinXse)[coef(inputModel$finalModel, s=highestLambda_withinXse)!=0])

cat(paste0(' Using lasso model whose performance is within ', numLambdaSEs, 'SE of best model for subsequent glmnet inference, to reduce no. of features in model.\n Best model has ',  numPreds_bestModel, ' predictors and AUC=', round(seDF$ROC[seDF$modelPerformance=='best'], 2), '\n Simplest within ', numLambdaSEs, ' SE has ', numPreds_within2se, ' predictors and AUC=', round(seDF$ROC[grepl('fewestPredsWithin', seDF$modelPerformance)], 2), '\n'))

# ==> model predictions from now on should use alpha=1, lambda=highestLambdaWithin2se


# #__________________________________________________________________________________________________________________________________
# 
# # 4.ii. Feature selection methods - test performance of random forest with feature selection [*** HASHING OUT - this is just a placeholder; can do later if I want to explore RF's further ----
#__________________________________________________________________________________________________________________________________

# See answer here for a good starting point. http://stats.stackexchange.com/questions/200823/does-it-makes-sense-to-use-feature-selection-before-random-forest. This answer says that feature selection with RF may be a good idea if lasso outperforms ridge.

# Also keep in mind that, for purporse of keeping no. of features to a minimum, can use 1se rule - ie choose simplest model that produces performance metric within 1se of best-performing model.


#__________________________________________________________________________________________________________________________________

# # 4.iii. Feature selection methods -  try recursive feature elimination on a random forest model & bagged tree model to look at which vars I should include [*** Hashing out - takes ages, and I can return to this later if needs be ***] ----
# #__________________________________________________________________________________________________________________________________

# cat('  Running recursive feature elimination to get an idea of best vars to include - this can take a while, particularly if no downsampling\n')
# 
# #+++++++++++++++++
# # First, remove vars I definitely don't want to predict with, then recreate dummy vars as required
# #+++++++++++++++++
#  
# # # Dummify factors - actually not needed, at least for the methods I'm curretnly using
# # modelDF$Converter <- as.character(modelDF$Converter) # get warning about Converter not being a factor as part of dummification otherwise - easier to just treat as character and change back to factor below
# # modelDFDummified <- dummyVars('Converter ~ .', data=modelDF, fullRank=TRUE) # caret's dummyVars ignores numeric columns automatically. FullRank excludes 1 level of each factor (becoming intercept), same as model.matrix. Should always do this, otherwise can get perfectly correlated vars.
# # modelDFDummified <- as.data.frame(predict(modelDFDummified, newdata=modelDF)) # need to do this to convert back to a dataframe
# # modelDFDummified$Converter <- as.factor(modelDF$Converter) # have to manually add back in
# # modelDF$Converter <- as.factor(modelDF$Converter) # ..and manually chnage back to factor
# 
# # Downsample, to (1) make rfe function run faster, and (2) avoid issues with model fitting to unbalanced sample (rfe function uses accuracy, which can't be changed, and cant downsample function internally) 
# modelDF_ds <- bind_rows(filter(modelDF, Converter=='y'), 
#                         sample_n(filter(modelDF, Converter=='n'), 
#                                  size=1*length(modelDF$Converter[modelDF$Converter=='y']))) 
# 
# #+++++++++++++++++++
# # Recursive feature elimination for a random forest & bagged trees             # see for details https://topepo.github.io/caret/recursive-feature-elimination.html
# #+++++++++++++++++++
# 
# # ==> doing mulitple methods to test robustness of included vars. RF and TB are best available default methods (can also define own, but it's a pain).
#  
# subsets <- c(1:5, seq(10, ncol(modelDF)-1, 5)) # different no. of vars to try
# 
# set.seed(0803)
# cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
# registerDoParallel(cl)
# getDoParWorkers() # check how many cores are getting used
# ctrl <- rfeControl(functions = rfFuncs, method = "repeatedcv", repeats = 10) # apparently different seeds can produce variable var selections, so good to use lots of repeats to reduce this variance
# rfProfile <- rfe(x=as.data.frame(select(modelDF_ds, -Converter)), y=modelDF_ds$Converter, sizes = subsets, rfeControl = ctrl)
# 
# set.seed(0803)
# cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
# registerDoParallel(cl)
# getDoParWorkers() # check how many cores are getting used
# ctrl <- rfeControl(functions = treebagFuncs, method = "repeatedcv", repeats = 10)
# btProfile <- rfe(x=as.data.frame(select(modelDF_ds, -Converter)), y=modelDF_ds$Converter, sizes = subsets, rfeControl = ctrl)
# 
# # visualise how performance changes with no. of predictors
# windows()
# plot(rfProfile, type = c("g", "o"))
# plot(btProfile, type = c("g", "o"))
# graphics.off()
# 
# # Look at relative importance of each var
# varImp(rfProfile)
# varImp(btProfile)
# 
# # Look at consistency of results across the two methods 
# rfPredsDF <- data.frame(preds=predictors(rfProfile), RFmodel='y')
# btPredsDF <- data.frame(preds=predictors(btProfile), BTmodel='y')
# allPredsDF <- data.frame(preds=colnames(select(modelDF, -Converter)), AllData='y')
# tempDF <- merge(rfPredsDF, btPredsDF, by='preds', all=TRUE)
# mergedDF <- merge(allPredsDF, tempDF, by='preds', all=TRUE)
# mergedDF
# 
# bestRFPreds <- predictors(rfProfile) # if I want to use this for subsequent models. 
# cat('  WARNING - if using best predictor set from random forest RFE on other models this will result in overly optimistic performance, cos the model training doesnt know that the features were selected - see http://stats.stackexchange.com/questions/60734/caret-rfe-variable-selection-and-test-prediction\n')
# 
# #+++++++++++++++++++
# # Use elastic net's inbuilt var selection and compare this method
# #+++++++++++++++++++
# 
# # Run a few models using caret defaults
# mGlmnet <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC')
# glmnetCoefs <- coef(mGlmnet$finalModel, s=mGlmnet$bestTune$lambda)
# glmSelectedCoefs <- labels(glmnetCoefs)[[1]][which(glmnetCoefs!=0)]
# glmSelectedCoefs <- glmSelectedCoefs[!grepl('Intercept', glmSelectedCoefs)]
# glmnetPredsDF <- data.frame(preds=glmSelectedCoefs, Glmnetmodel='y')
# 
# # Look at most important glmnet vars
# varImp(mGlmnet)
# 
# # Compare vars that made it into RF vs BT vs GLMNET models
# merge(mergedDF, glmnetPredsDF, by='preds', all=TRUE)
# warning('  glmnet dummifies factors, so there will be unmatched categories here just because e.g. RF will call them "Region" and glmnet will call them "RegionGisborne". Could always dummify factors prior to modelling, then should be more comparable.\n')


#__________________________________________________________________________________________________________________________________

# # 5. For major model classes that appear to work well, try different downsampling regimes [*** HASHING OUT - takes ages, and I can return to this later if needs be. ALSO NEED TO SPECFIY CONTROL WITH CREATETIMESLICES METHOD AS ABOVE ***]----
# #__________________________________________________________________________________________________________________________________
# 
# # Set up parallel backend
# cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
# registerDoParallel(cl)
# getDoParWorkers() # check how many cores are getting used
# # 
# #+++++++++++++++++++
# # Run models
# #+++++++++++++++++++
# 
# mGlmnet_ds <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC')
# mCTree_ds <- train(Converter~., data=modelDF, method="ctree", trControl=control, metric='ROC')
# # mRF_predSubset_ds <- train(Converter~., data=modelDF_predSubset, method="rf", trControl=control, metric='ROC') # using subset for RF, not for others cos they kind of do their own var selection. Subset may not have quite as good performance as full but it's more workable.
# 
# control$sampling <- 'up'
# mGlmnet_us <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC')
# mCTree_us <- train(Converter~., data=modelDF, method="ctree", trControl=control, metric='ROC')
# # mRF_predSubset_us <- train(Converter~., data=modelDF_predSubset, method="rf", trControl=control, metric='ROC') # using subset for RF, not for others cos they kind of do their own var selection. Subset may not have quite as good performance as full but it's more workable.
# 
# control$sampling <- 'smote'
# mGlmnet_smote <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC')
# mCTree_smote <- train(Converter~., data=modelDF, method="ctree", trControl=control, metric='ROC')
# # mRF_predSubset_smote <- train(Converter~., data=modelDF_predSubset, method="rf", trControl=control, metric='ROC') # using subset for RF, not for others cos they kind of do their own var selection. Subset may not have quite as good performance as full but it's more workable.
# 
# control$sampling <- 'rose'
# mGlmnet_rose <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC')
# mCTree_rose <- train(Converter~., data=modelDF, method="ctree", trControl=control, metric='ROC')
# # mRF_predSubset_rose <- train(Converter~., data=modelDF_predSubset, method="rf", trControl=control, metric='ROC') # using subset for RF, not for others cos they kind of do their own var selection. Subset may not have quite as good performance as full but it's more workable.
# 
# control <- trainControl(method="repeatedcv", number=5, repeats=3, summaryFunction=twoClassSummary,	classProbs=TRUE) # respecifiying so that there's no sampling.
# mGlmnet_none <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC')
# mCTree_none <- train(Converter~., data=modelDF, method="ctree", trControl=control, metric='ROC')
# # mRF_predSubset_none <- train(Converter~., data=modelDF_predSubset, method="rf", trControl=control, metric='ROC') # using subset for RF, not for others cos they kind of do their own var selection. Subset may not have quite as good performance as full but it's more workable.
# 
# #+++++++++++++++++++
# # Compare predictive performance
# #+++++++++++++++++++
# 
# AUCs <- resamples(list(Glmnet_ds = mGlmnet_ds, Ctree_ds=mCTree_ds, # RF_predSubset_ds=mRF_predSubset_ds, 
#                        Glmnet_us = mGlmnet_us, Ctree_us=mCTree_us, # RF_predSubset_us=mRF_predSubset_us, 
#                        Glmnet_rose = mGlmnet_rose, Ctree_rose=mCTree_rose, # RF_predSubset_rose=mRF_predSubset_rose, 
#                        Glmnet_smote = mGlmnet_smote, Ctree_smote=mCTree_smote, # RF_predSubset_smote=mRF_predSubset_smote,
#                        Glmnet_none = mGlmnet_none, Ctree_none=mCTree_none)) #, RF_predSubset_none=mRF_predSubset_none))
# windows()
# bwplot(AUCs) # log reg pretty good; glment slightly better
# graphics.off()


#__________________________________________________________________________________________________________________________________

# # 6. For final **tree** model, tabulate segment rules, lift, and CPA, to give to media buyers [HASHING OUT - try using non-tree models instead; see below] ----
# #__________________________________________________________________________________________________________________________________
# 
# #+++++++++++++++++++
# # Rerun final model with partykit, after first potentially messing with mincriterion and maxdepth params to make a smaller tree for planners
# #+++++++++++++++++++
# 
# set.seed(1)
# 
# # Set up tuning grid
# grid <- expand.grid(mincriterion=c(0.1, seq(0.5, 0.99, 0.05), 0.99), maxdepth=c(5, 10))
# 
# mCTree_ds <- train(Converter~., data=modelDF, method="ctree2", trControl=control, tuneGrid=grid, metric='ROC')
# 
# windows()
# plot(mCTree_ds) # look at best tuning grid combinations
# graphics.off()
# 
# # up/downsample as necessary, to match final caret tree, then run in partykit
# convsDF <- filter(modelDF, Converter=='y')
# nonconvsDF <- filter(modelDF, Converter=='n')
# modelDF_manualDownsample <- bind_rows(convsDF, sample_n(nonconvsDF, size=nrow(convsDF)))
# 
# mCTree_PK <- partykit::ctree(Converter~., data=modelDF_manualDownsample, maxdepth=50, mincriterion=0.65)
# 
# cat('  WARNING: for ctree outputs for planners Im running model through partykit cf using carets finalModel; means I can
#    - 1 specify mincriterion/maxdepth parameters manually (cos best performing tree may be too complex for planners) and
#    - 2 plot with partykits nice plotting methods.
#    - NB may mean model isnt quite identical to caret version
#    - NB If the best class-balancing method is SMOTE or ROSE I need to figure out how to do these outside of caret and apply before running model.\n')
# 
# #+++++++++++++++++++
# # plot trees and save to file, if desired (but excel output below is better I think)
# #+++++++++++++++++++
# 
# windows()
# plot(mCTree_PK, gp = gpar(fontsize = 6), simplifiy=TRUE)
# graphics.off()
# # # save plots to file
# # png(paste0(pathSave, 'exampleCTree1.png'), height=10000, width=10000, res=400)
# # plot(mCTree_PK, gp = gpar(fontsize = 6), simplifiy=TRUE)
# # graphics.off()
# 
# #+++++++++++++++++
# # Use predicted values to get summary stats for each node/leaf
# #+++++++++++++++++
# 
# treeModelToSummarise <- mCTree_PK
# 
# nodePredictions <- predict(treeModelToSummarise, type = "prob") # one row per instance, with row labels giving node and cell values giving prob of "y" and prob of "n"
# nodeLabels <- labels(nodePredictions)[[1]]
# nodeProbLabels <- labels(nodePredictions)[[2]]
# nodePredictionsDF <- data.frame(node=nodeLabels, probYes=nodePredictions[,which(nodeProbLabels=='y')])
# 
# # Summarise conversion probabilities per node
# treeSummaryDF <- nodePredictionsDF %>%
#   group_by(node) %>%
#   summarise(numConversions_modelDF=length(node)*unique(probYes), numNonConversions_modelDF=length(node)*(1-unique(probYes)),
#             relativeProbConversion=unique(probYes)) %>%
#   mutate(baselineProbConversion=sum(numConversions_modelDF)/(sum(numConversions_modelDF)+sum(numNonConversions_modelDF)),
#          lift=relativeProbConversion/baselineProbConversion) %>%
#   arrange(desc(lift))#  %>%
# 
# # Round numeric cols to sensible number
# treeSummaryDF[,colnames(treeSummaryDF)[colnames(treeSummaryDF)!='node']] <-
#   round(treeSummaryDF[,colnames(treeSummaryDF)[colnames(treeSummaryDF)!='node']], 4)
# 
# # Add in verbal description of rules for each node
# treeRules <- partykit:::.list.rules.party(treeModelToSummarise)
# treeSummaryDF$segment <- unname(treeRules)[match(treeSummaryDF$node, labels(treeRules))]
# options(scipen=1000)
# # as.data.frame(treeSummaryDF)
# 
# #+++++++++++++++++++
# # Add in info about expected number of rows in the full dataset
# #+++++++++++++++++++
# 
# cat('  WARNING: \n   - numRows and cost variables per segment are calculated based on df  ***AFTER FILTERING AD CATEGORIES*** i.e. may be filtered by Layer, Offer, & Group depending on parameter values.\n   - Example: if route=TAS, offerAds=all, groupAds=Retail, & layerAds=notRemarketing, then CPA = [total adspend across retail, non-remarketing DBM ads advertising any route] / [no. of retail, non-remarketing DBM ads advertising any route that were attributed a TAS conversion]\n   - Parameter values are given in name of tree output - make sure planners read this and know what cols refer to.\n')
# 
# # NB following code is based on df, which has already been filtered to remove ads that don't match group, offer, and layer criteria
# 
# numConvs_fullDF <- nrow(filter(df, Converter=='y'))
# numNonConvs_fullDF <- nrow(filter(df, Converter=='n'))/negativeInstanceDownsampling  # NB while modelDF may have some downsampling, df won't (aside from that specified in negativeInstanceDownsampling parameter)
# nonConvsRatio_fullVsModelDF <- numNonConvs_fullDF/sum(treeSummaryDF$numNonConversions_modelDF)
# 
# treeSummaryDF <- treeSummaryDF %>%
#   mutate(numRows=round(numConversions_modelDF+(numNonConversions_modelDF*nonConvsRatio_fullVsModelDF), 0))
# sum(treeSummaryDF$numRows) # should be roughly 8 million per month for *all* DBM ads, if 1 row=1 ad c.f. 1 row=1 user
# 
# #+++++++++++++++++++
# # Ad info about expected cost per ad
# #+++++++++++++++++++
# 
# costDF <- df
# costDF$node <- as.character(predict(treeModelToSummarise, type = "node", newdata = costDF))
# # as.data.frame(select(treeSummaryDF, node, segment)); unique(filter(costDF, node==9)$adX_Finance) # checking they align. Totes do
# 
# costDF <- costDF %>%
#   group_by(node) %>%
#   summarise(medianDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.5), 1),
#             lowerQuartileDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.25), 1),
#             upperQuartileDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.75), 1),
#             meanDBMRevenueAdvertiserCurrencyCPM=round(mean(DBMRevenueAdvertiserCurrencyCPM), 1)) %>%
#   transmute(node=node, medianRevenueNZD=medianDBMRevenueAdvertiserCurrencyCPM,
#             IQRRevenueNZD=paste0(lowerQuartileDBMRevenueAdvertiserCurrencyCPM, '-', upperQuartileDBMRevenueAdvertiserCurrencyCPM),
#             meanRevenueNZD=meanDBMRevenueAdvertiserCurrencyCPM)
# 
# warning('Could split out cost into weighted average based on converter vs nonconv cost, if worthwhile. Did notice that for some segments converter costs were more than nonconv costs, so could be worth doing. ***ALSO SUGGESTS THERES SOME IMPORTANT VAR IM NOT MODELLING - OR AT LEAST SOME VAR THAT SIGNIFICANTLY INFLUENCES PRICE. E.g. Video vs banner ads***\n')
# 
# treeSummaryDF <- treeSummaryDF %>%
#   left_join(costDF, by='node') %>%
#   select(-node, -numNonConversions_modelDF) %>%
#   rename(numConversions=numConversions_modelDF)  %>%
#   mutate(TotalRevenueNZD=round(numRows*(meanRevenueNZD/1000),0))  %>% # meanRevenue is CPM, so need to divide by 1000 to extrapolate total spend from no. of ads served.
#   mutate(CPA=round(TotalRevenueNZD/numConversions, 0))
# 
# #+++++++++++++++++++
# # Data cleaning
# #+++++++++++++++++++
# 
# treeSummaryDF$numRows <- formatC(treeSummaryDF$numRows, format="d", big.mark=",") # add commas into big numbers for easier readability
# colnames(treeSummaryDF)[colnames(treeSummaryDF)=='numRows'] <- paste0('numDBMAds_', gsub('-', '', firstDate), 'To', gsub('-', '', maxDate)) # incorporating date range over which numRows is measured
# cat(  'WARNING: numDBMAds column may need to be calculated differently if outputDatabase is based on one row per user cf one row per ad. At very least name will need to change name to numDBMUsers or similar\n')
# 
# # Change to nicer order
# tempSegment <- treeSummaryDF$segment
# treeSummaryDF <- select(treeSummaryDF, -segment)
# treeSummaryDF$segment <- tempSegment
# 
# # Make segment descriptions more readable
# treeSummaryDF$segment <- gsub("%in%", "INCLUDES:", treeSummaryDF$segment)
# treeSummaryDF$segment <- gsub("c\\(", "", treeSummaryDF$segment)
# treeSummaryDF$segment <- gsub("&", "AND", treeSummaryDF$segment)
# treeSummaryDF$segment <- gsub("<= 0", "IS EXCLUDED", treeSummaryDF$segment)
# treeSummaryDF$segment <- gsub("> 0", "IS INCLUDED", treeSummaryDF$segment)
# treeSummaryDF$segment <- gsub("\\)", "", treeSummaryDF$segment)
# 
# # Round numeric vars to make more readable
# treeSummaryDF[, colnames(treeSummaryDF) %in% c('lift', 'baselineProbConversion', 'relativeProbConversion')] <-
#   round(treeSummaryDF[, colnames(treeSummaryDF) %in% c('lift', 'baselineProbConversion', 'relativeProbConversion')], 2)
# 
# as.data.frame(treeSummaryDF)
# 
# # # Check it matches with real data
# # nodeXdf <- filter(modelDFDummified, adX_Reference <= 0 & adX_News > 0 & adX_WorldLocalities <= 0)
# # table(nodeXdf$Converter)
# # 152/(152+9031) # yup, all ads up.
# 
# #+++++++++++++++++++
# # Save to file
# #+++++++++++++++++++
# 
# write.csv(treeSummaryDF, file=paste0(pathSave, 'TREERESULTS_', outputDatabase, '_negativeInstanceDownsampling', negativeInstanceDownsampling, '_', dataset, 'DF_', route, '_offer', offerAds, '_group', groupAds, '_layer', layerAds, '_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.csv'), row.names=FALSE)
# 
# 
# #+++++++++++++++++++
# # 6.ii. TEMP graphing distribution of costs per ad across segments based on best CTree model above] ----
# #+++++++++++++++++++
# 
# plotDF <- df
# plotDF$node <- as.character(predict(treeModelToSummarise, type = "node", newdata = plotDF))
#   
# windows()
# ggplot(plotDF, aes(node, DBMRevenueAdvertiserCurrencyCPM)) + geom_jitter(height=0, alpha=0.01) + # horiz jitter only
#   stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour='blue', width = 0.2)
# graphics.off()


#__________________________________________________________________________________________________________________________________
# 
# 7. For non-tree models, try creating segments manaully - iterate thru all possible combinations of final modelled vars, then predict from model ----
#__________________________________________________________________________________________________________________________________

cat('\n----Creating segments manually for non-tree models - create all possible combinations of selected features, then predict conv prob for each----\n\n')

#+++++++++++++++++++
# Create a dataframe with all possible combinations of selected features
#+++++++++++++++++++

### Currently using GLMnet model with feature selection from above bookmark 

# Rerun lasso model, manually specifying alpha & lambda (since these may not be the optimal values)

# Set up parallel backend
cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
registerDoParallel(cl)
getDoParWorkers() # check how many cores are getting used

# Set up training control - no need for split into train/test sets, just rerunning model on prespecified param values.
control <- trainControl(method="none",
                        summaryFunction=twoClassSummary,
                        classProbs=TRUE,
                        sampling='down') # downsample within each resampling fold, then test on full holdout fold. https://topepo.github.io/caret/subsampling-for-class-imbalances.html for details

# Rerun model
mGlmnet3 <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC', tuneGrid=expand.grid(alpha=1, lambda=highestLambda_withinXse))
finalModel <- mGlmnet3

# Get nonzero predictors from lasso model
coeffsDF <- data.frame(feature=row.names(coef(finalModel$finalModel, s=highestLambda_withinXse)),
                                beta=unname(coef(finalModel$finalModel, s=highestLambda_withinXse))[,1]) # NB 'finalModel' at the moment is just caret's glmnet model, but run with alpha=1, definined in # 4.i
nonZeroFeaturesDF <- filter(coeffsDF, beta!=0)
zeroFeaturesDF <- filter(coeffsDF, beta==0)
tempNonZeroFeatures <- nonZeroFeaturesDF$feature
nonZeroFeaturesDF
zeroFeaturesDF
nonZeroFeaturesDF <- filter(nonZeroFeaturesDF, 
                            (!grepl('Region', feature) & !grepl('daypart', feature) & !grepl('day', feature)) |
                            (grepl('Region', feature) & beta>0) |
                              (grepl('daypart', feature) & beta>0) |
                              (grepl('day', feature) & beta>0)) # removing factor levels with negative effect - dont need these in our segments.
nonZeroFeatures <- as.character(nonZeroFeaturesDF$feature)
zeroFeatures <- as.character(zeroFeaturesDF$feature)

cat(' Significant features used in final model (excludes significant factor levels that have a negative effect, cos theres no need to have these in our final segments since we only care about high-lift segments):\n')
print(nonZeroFeatures[!grepl('Intercept', nonZeroFeatures)])
cat(' Nonsignificant features removed from final model:\n')
print(zeroFeatures)
cat(' Significant factor levels with a negative effect that were removed from segments:\n')
print(as.character(tempNonZeroFeatures[!tempNonZeroFeatures %in% nonZeroFeatures]))

### EXTRACT UNIQUE VALUES OF NON-ZERO PREDICTORS - (1) FOR NON-FACTORS

modelDFnonFactorVars <- colnames(modelDF)[!unlist(lapply(modelDF, is.factor))]
nonZeroFeatures_nonFactors <- nonZeroFeatures[nonZeroFeatures %in% modelDFnonFactorVars] 
modelledData_nonFactors <- modelDF[, colnames(modelDF) %in% nonZeroFeatures_nonFactors]

allVarsUniqueLevelsList <- list()
for(i in 1:length(nonZeroFeatures_nonFactors)){
  allVarsUniqueLevelsList[[i]] <- unique(modelledData_nonFactors[,as.character(nonZeroFeatures_nonFactors[i])])
}
names(allVarsUniqueLevelsList) <- nonZeroFeatures_nonFactors

### EXTRACT UNIQUE VALUES OF NON-ZERO PREDICTORS - (2) FOR FACTORS  (glmnet internally dummifies factors, so have to use different method)

modelDFfactorVars <- colnames(modelDF)[unlist(lapply(modelDF, is.factor))]

if('DBMRegion' %in% modelDFfactorVars){
  uniqueFactorLevelsInModel <- nonZeroFeatures[substr(nonZeroFeatures, 1, 9)=='DBMRegion']
  uniqueFactorLevelsInModel <- gsub('DBMRegion', '', uniqueFactorLevelsInModel)
  uniqueFactorLevelsInModel <- c(uniqueFactorLevelsInModel, levels(modelDF$DBMRegion)[1]) # adding in baseline level - may still have an effect in model
  allVarsUniqueLevelsList[[length(allVarsUniqueLevelsList)+1]] <- uniqueFactorLevelsInModel
  names(allVarsUniqueLevelsList)[[length(allVarsUniqueLevelsList)]] <- 'DBMRegion'
}

if('day' %in% modelDFfactorVars){
  uniqueFactorLevelsInModel <- nonZeroFeatures[substr(nonZeroFeatures, 1, 3)=='day' & !grepl('daypart', nonZeroFeatures)]
  uniqueFactorLevelsInModel <- substr(uniqueFactorLevelsInModel, 4, nchar(as.character(uniqueFactorLevelsInModel)))
  uniqueFactorLevelsInModel <- c(uniqueFactorLevelsInModel, levels(modelDF$day)[1]) # adding in baseline level - may still have an effect in model
  allVarsUniqueLevelsList[[length(allVarsUniqueLevelsList)+1]] <- uniqueFactorLevelsInModel
  names(allVarsUniqueLevelsList)[[length(allVarsUniqueLevelsList)]] <- 'day'
}

if('daypart' %in% modelDFfactorVars){
  uniqueFactorLevelsInModel <- nonZeroFeatures[substr(nonZeroFeatures, 1, 7)=='daypart']
  uniqueFactorLevelsInModel <- substr(uniqueFactorLevelsInModel, 8, nchar(as.character(uniqueFactorLevelsInModel)))
  uniqueFactorLevelsInModel <- c(uniqueFactorLevelsInModel, levels(modelDF$daypart)[1]) # adding in baseline level - may still have an effect in model
  allVarsUniqueLevelsList[[length(allVarsUniqueLevelsList)+1]] <- uniqueFactorLevelsInModel
  names(allVarsUniqueLevelsList)[[length(allVarsUniqueLevelsList)]] <- 'daypart'
}

cat(' WARNING - Creating all possible combinations of selected features from model. For non-factors (e.g. adX categories, url categories) this is done automatically, although code may break if there arent any nonfactors in model. For factors, have to manually ad in (to get around glmnets internal dummification) - have done this for region, day, and daypart, but no others\n')

allCombinationsModelledVarsDF <- expand.grid(allVarsUniqueLevelsList)

#+++++++++++++++++++
# Add in features that lasso zeroed out - still required for predict()
#+++++++++++++++++++

# - actual value doesnt matter cos they have a zero coeff; just need the colnames in there. BUT matters if factor or not... so using modal value for each.
featuresInFullModel <- colnames(modelDF)
zeroFeatures2 <- featuresInFullModel[!featuresInFullModel %in% names(allCombinationsModelledVarsDF) & featuresInFullModel!='Converter'] # zeroFeatures2 is names of zeroed vars in modelDF, whereas zeroFeatures is names used by glmnet internally - e.g. 'DBMRegionAuckland' may be in zeroFeatures, whereas 'DBMRegion' would be in zeroFeatures2 [tho only if *no* regions had nonzero coeffs]

# Function to find modal value
modefunc <- function(x){
  var_iTable <- table(x)
  if(is.numeric(x)) modalValue <- as.numeric(labels(which.max(var_iTable)))
  else if(is.factor(x)) modalValue <- as.factor(labels(which.max(var_iTable)))
  else modalValue <- labels(which.max(var_iTable))
  return(modalValue)
}

# combine nonZero features dataframe with zero features dataframe, where latter has modal value in each row
modalDF <- bind_cols(lapply(modelDF, modefunc))
zeroFeaturesDF2 <- modalDF[, colnames(modalDF) %in% zeroFeatures2 & colnames(modalDF)!='Converter']
zeroFeaturesDF2 <- zeroFeaturesDF2[rep(1, times=nrow(allCombinationsModelledVarsDF)),] # make same dimensions as allCombinationsModelledVarsDF

segmentsDF <- bind_cols(allCombinationsModelledVarsDF, zeroFeaturesDF2) # added in vars needed for predict to work, but they have no effect on preds & dont add extra rows to allCombinationsDF
nonZeroFeatures2 <- colnames(allCombinationsModelledVarsDF) # needed for outputs below
rm(allCombinationsModelledVarsDF, zeroFeaturesDF2)

#+++++++++++++++++++
# Predict probabilities for each combination of predictors
#+++++++++++++++++++

segmentsDF$probYes <- predict(finalModel, type='prob', newdata=segmentsDF)[,2]
segmentsDF <- select(segmentsDF, which(colnames(segmentsDF) %in% c('probYes', nonZeroFeatures2))) # now that I've predicted, can get rid of unimportant features.

#__________________________________________________________________________________________________________________________________

# 8. For final **nontree** model, tabulate segment rules, lift, and CPA, to give to media buyers ----
#__________________________________________________________________________________________________________________________________

cat('\n----Tabulating lift, CPA, & segment size per segment, & saving results to file----\n\n')

#+++++++++++++++++++
# Assign each row in df to a segment
#+++++++++++++++++++

# Make sure segments & df have identical schema - both snipped down to only those predictors that were important in the model
matchDF_df <- select(df, which(colnames(df) %in% nonZeroFeatures2))
matchDF_segments <- select(segmentsDF, which(colnames(segmentsDF) %in% nonZeroFeatures2)) # same as segments DF, but just excluding probYes
matchDF_df <- matchDF_df[,order(colnames(matchDF_df))]
matchDF_segments <- matchDF_segments[,order(colnames(matchDF_segments))] # making sure colnames are in same order - important for creating joinID below

# For factor levels that were removed from model, combine with baseline level. 
# ==> Zeroed out factor levels will give identical predictions to the baseline, so they're all the same segment
levelsToMerge <- c(levels(modelDF$DBMRegion)[1], gsub('DBMRegion', '', zeroFeatures[substr(zeroFeatures, 1, 9)=='DBMRegion']))
mergedLevels <- paste0(levelsToMerge, collapse="")
paste0(levelsToMerge, collapse="")
matchDF_df$DBMRegion <- as.character(matchDF_df$DBMRegion) # needed to add new factor level below
matchDF_df$DBMRegion[matchDF_df$DBMRegion %in% levelsToMerge] <- mergedLevels

zeroDays <- zeroFeatures[substr(zeroFeatures, 1, 3)=='day' & !grepl('daypart', zeroFeatures)] # extra step needed for day cos 'day' and 'daypart' both contain day string at start
levelsToMerge <- c(levels(modelDF$day)[1], substr(zeroDays, 4, nchar(zeroDays)))
mergedLevels <- paste0(levelsToMerge, collapse="")
matchDF_df$day <- as.character(matchDF_df$day) # needed to add new factor level below
matchDF_df$day[matchDF_df$day %in% levelsToMerge] <- mergedLevels

levelsToMerge <- c(levels(modelDF$daypart)[1], gsub('daypart', '', zeroFeatures[substr(zeroFeatures, 1, 7)=='daypart']))
mergedLevels <- paste0(levelsToMerge, collapse="")
matchDF_df$daypart <- as.character(matchDF_df$daypart) # needed to add new factor level below
matchDF_df$daypart[matchDF_df$daypart %in% levelsToMerge] <- mergedLevels

# Repeat for segments DF, so that segments match up with original df
levelsToMerge <- c(levels(modelDF$DBMRegion)[1], gsub('DBMRegion', '', zeroFeatures[substr(zeroFeatures, 1, 9)=='DBMRegion']))
mergedLevels <- paste0(levelsToMerge, collapse="")
matchDF_segments$DBMRegion <- as.character(matchDF_segments$DBMRegion) # needed to add new factor level below
matchDF_segments$DBMRegion[matchDF_segments$DBMRegion %in% levelsToMerge] <- mergedLevels

zeroDays <- zeroFeatures[substr(zeroFeatures, 1, 3)=='day' & !grepl('daypart', zeroFeatures)] # extra step needed for day cos 'day' and 'daypart' both contain day string at start
levelsToMerge <- c(levels(modelDF$day)[1], substr(zeroDays, 4, nchar(zeroDays)))
mergedLevels <- paste0(levelsToMerge, collapse="")
matchDF_segments$day <- as.character(matchDF_segments$day) # needed to add new factor level below
matchDF_segments$day[matchDF_segments$day %in% levelsToMerge] <- mergedLevels

levelsToMerge <- c(levels(modelDF$daypart)[1], gsub('daypart', '', zeroFeatures[substr(zeroFeatures, 1, 7)=='daypart']))
mergedLevels <- paste0(levelsToMerge, collapse="")
matchDF_segments$daypart <- as.character(matchDF_segments$daypart) # needed to add new factor level below
matchDF_segments$daypart[matchDF_segments$daypart %in% levelsToMerge] <- mergedLevels

# Also have to replace df & segmentsDF factor levels with merged factor levels
segmentsDF[,colnames(segmentsDF)=='DBMRegion'] <- matchDF_segments$DBMRegion
segmentsDF[,colnames(segmentsDF)=='daypart'] <- matchDF_segments$daypart
segmentsDF[,colnames(segmentsDF)=='day'] <- matchDF_segments$day
df[,colnames(df)=='DBMRegion'] <- matchDF_df$DBMRegion
df[,colnames(df)=='daypart'] <- matchDF_df$daypart
df[,colnames(df)=='day'] <- matchDF_df$day

cat(' WARNING - factor levels that were zeroed out by lasso belong to the same segment as the baseline level (i.e. they give identical predictions). Have manually combined these levels for region, day, and daypart, but will need to add in any new factors that get modelled\n')

# Create an index for joining segmentID to df, based on concatentation of all nonzero predictors:
matchDF_df$joinID <- do.call(paste0, matchDF_df)
matchDF_segments$joinID <- do.call(paste0, matchDF_segments)
matchDF_segments$segmentID <- 1:nrow(matchDF_segments)

# add segmentIDs to segmentsDF & df
matchDF_df <- left_join(matchDF_df, select(matchDF_segments, joinID, segmentID), by='joinID')
df$segmentID <- matchDF_df$segmentID
segmentsDF$segmentID <- matchDF_segments$segmentID
rm(matchDF_df, matchDF_segments)

cat(' WARNING - joining segments to df to calculate cost, numRows etc - this means that combinations of predictors with the highest lift may not make it into the final segments, cos this combination didnt exist in reality.\n')

#+++++++++++++++++++
# Calculate summary info about each segment - no. of convs, no. of nonconvs, no. of rows, and expected cost per ad for each segment
#+++++++++++++++++++

cat(' WARNING: \n   - numRows and cost variables per segment are calculated based on df  ***AFTER FILTERING AD CATEGORIES*** i.e. may be filtered by Layer, Offer, & Group depending on parameter values.\n   - Example: if route=TAS, offerAds=all, groupAds=Retail, & layerAds=notRemarketing, then CPA = [total adspend across retail, non-remarketing DBM ads advertising any route] / [no. of retail, non-remarketing DBM ads advertising any route that were attributed a TAS conversion]\n')

# NB following code is based on df, which has already been filtered to remove ads that don't match group, offer, and layer criteria

summaryDF <- df %>%
  group_by(segmentID) %>%
  summarise(numConversions=length(Converter[Converter=='y']),
            numRows=length(Converter[Converter=='y']) +
              (length(Converter[Converter=='n'])/negativeInstanceDownsampling),
    medianDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.5), 1),
            lowerQuartileDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.25), 1),
            upperQuartileDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.75), 1),
            meanDBMRevenueAdvertiserCurrencyCPM=round(mean(DBMRevenueAdvertiserCurrencyCPM), 1)) %>%
  transmute(segmentID=segmentID, numConversions=numConversions, numRows=numRows,
            medianRevenueNZD=medianDBMRevenueAdvertiserCurrencyCPM,
            IQRRevenueNZD=paste0(lowerQuartileDBMRevenueAdvertiserCurrencyCPM, '-', upperQuartileDBMRevenueAdvertiserCurrencyCPM),
            meanRevenueNZD=meanDBMRevenueAdvertiserCurrencyCPM)

warning('Could split out cost into weighted average based on converter vs nonconv cost, if worthwhile. Did notice that for some segments converter costs were more than nonconv costs, so could be worth doing. ***ALSO SUGGESTS THERES SOME IMPORTANT VAR IM NOT MODELLING - OR AT LEAST SOME VAR THAT SIGNIFICANTLY INFLUENCES PRICE. E.g. Video vs banner ads***\n')

sum(summaryDF$numRows) # sanity check on number of rows - should be around 8million per month of data.

#+++++++++++++++++++
# Join summary info to actual conversion probs, calculate lift, and remove rows below lift threshold
#+++++++++++++++++++

summaryDF <- summaryDF %>%
  left_join(segmentsDF, by="segmentID") %>%
  mutate(baselineProbConversion=0.5, 
         lift=round(probYes/baselineProbConversion, 2)) %>%
  arrange(desc(lift)) %>%
  filter(lift>liftThreshold) # ignoring segments with lift lower than this

cat(' WARNING - manually adding baselineConv prob of 50%, based on fact that caret model has 50% internal downsampling. If this changes then lift value will be wrong\n')

#+++++++++++++++++++
# Data cleaning
#+++++++++++++++++++

summaryDF$numRows <- formatC(summaryDF$numRows, format="d", big.mark=",") # add commas into big numbers for easier readability
colnames(summaryDF)[colnames(summaryDF)=='numRows'] <- paste0('numDBMAds_', gsub('-', '', firstDate), 'To', gsub('-', '', maxDate)) # incorporating date range over which numRows is measured
colnames(summaryDF)[colnames(summaryDF)=='probYes'] <- 'relativeProbConversion'

# Change to nicer order
reorderDF <- data.frame(lift=summaryDF$lift,
                        relativeProbConversion=summaryDF$relativeProbConversion,
                        baselineProbConversion=summaryDF$baselineProbConversion)
summaryDF <- select(summaryDF, -relativeProbConversion, -baselineProbConversion, -lift, -segmentID)
summaryDF <- bind_cols(reorderDF, summaryDF)


# Round numeric vars to make more readable
summaryDF[, colnames(summaryDF) %in% c('lift', 'baselineProbConversion', 'relativeProbConversion')] <-
  round(summaryDF[, colnames(summaryDF) %in% c('lift', 'baselineProbConversion', 'relativeProbConversion')], 2)

# # Check it matches with real data
# nodeXdf <- filter(modelDFDummified, adX_Reference <= 0 & adX_News > 0 & adX_WorldLocalities <= 0)
# table(nodeXdf$Converter)
# 152/(152+9031) # yup, all ads up.

#+++++++++++++++++++
# Save to file
#+++++++++++++++++++

cat(' Final output:\n')
print(head(summaryDF, 2))
print(tail(summaryDF, 2))

write.csv(summaryDF, file=paste0(pathSave, 'LASSORESULTS_', outputDatabase, '_negativeInstanceDownsampling', negativeInstanceDownsampling, '_', dataset, 'DF_', route, '_offer', offerAds, '_group', groupAds, '_layer', layerAds, '_', numLambdaSEs, 'lambdaSEs_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.csv'), row.names=FALSE)

cat(' WARNING - Need to make sure models dont have numeric variables - at least if Im using non-tree models to make segments out of all combinations of variables (there are infinite possible combinations for continuous vars). If I do add in numerics, will need to use tree models I think, and also poss normalise/scale before modelling\n')

gc()
