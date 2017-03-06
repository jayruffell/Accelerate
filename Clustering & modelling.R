#__________________________________________________________________________________________________________________________________

# Add description of what code does here...
#__________________________________________________________________________________________________________________________________

warning('Are there any numeric vars that I need to normalise/scale for any models?')

#__________________________________________________________________________________________________________________________________

# 0. Set parameters and load data ----
#__________________________________________________________________________________________________________________________________

#+++++++++++++++++
# set params & load data
#+++++++++++++++++

rm(list=ls())
set.seed(1)
dataset <- 'full'
basePath <- 'E:/JAY/'
pathData <- paste0(basePath, 'data/')
pathSave <- paste0(basePath, 'AirNZ segmentation results/')
responseVar <- 'lastClickConversion' # 'Conversion' or 'lastClickConversion'. Former is whether a user converted at any point over model period; latter is whether a given ad was attributed a covnersion via last click methodology. lastClickConv only applies to outputDatabase==df_oneRowPerAdAndLastClickConversionResponse

# Define which types of ads we want to look at     ***CURRENTLY ONLY IMPLEMENTED FOR df_oneRowPerAdAndLastClickConversionResponse ***
offerAds <- 'allRoutes' # 'focalRoute', 'allRoutes' # e.g. if routeAds==focalRoute, then for route==TAS we would only include ads that have a 'Tasman sale' message in the model
layerAds <- 'all' # 'notRemarketing', 'Remarketing', or 'all'. If 'notRemarketing' then exclude RMK ads from model.
groupAds <- 'all' # 'retail', 'brand', or 'all'. Az reckons we would only ever use the model to buy retail ads (i.e. ads relating to specific sales), but including other options for completeness.

# Add in params used to read in data & poss name outputs - need to match names in EDA&CreateModelDatabase.r:
firstDate <- '2016-11-16'
maxDate <- '2016-12-31'
negativeInstanceDownsampling <- 0.001
route <- 'LH'    
outputDatabase <- 'df_oneRowPerAdAndLastClickConversionResponse'              # these params have no other use; just used to pull in R data

cat(paste0('LOADING DATA - ', dataset, 'DF\n'))
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
cat('  NB make sure any new character vars are converted to factors to avoid potential errors with random forest\n')

#+++++++++++++++++
# Define variable groups - makes it easy to quickly include/exclude vars from models
#+++++++++++++++++

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
  cat(paste0(' WARNING - ', naRows, ' have NAs & have been removed\n'))
  df <- na.omit(df)
}

#_______________________________________________________________________________________________________________________________________________________________

# 1. Based on values for ad attributes in parameters, either include or exclude individual ads from analysis ----
#__________________________________________________________________________________________________________________________________

if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){

  cat(' Filtering database by specified ad types (e.g. exclude RMK ads). Only doing for outputDatabase==df_oneRowPerAdAndLastClickConversionResponse for now... need to think about whether I should do for the other output databases too\n')

  dfPositiveInstancesPreAdRemoval <- nrow(filter(df, Converter=='y'))
  dfNegativeInstancesPreAdRemoval <- nrow(filter(df, Converter=='n'))
  
  if(offerAds=='focalRoute' & route!='allRoutes'){ # if route==allRoutes then don't need this step
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

# 2. Some quick EDA - plot & model bivariate relationships between predictors and response ----
#__________________________________________________________________________________________________________________________________

plotDF <- mutate(df, ConverterNumeric=ifelse(Converter=='y', 1, 0))
windows()
ggplot(plotDF, aes(daypart, ConverterNumeric)) + geom_jitter(alpha=0.5) + stat_summary(fun.data="mean_cl_boot", colour = "red", size = 0.75)
graphics.off()
x <- glm(Converter~daypart, data=df, family='binomial')
summary(x)



#__________________________________________________________________________________________________________________________________

# 3. Preliminary modelling - run major model types on all predictors, using caret's default hyperparam test values and caret's downsampling to balance sample, then compare performance ----
#__________________________________________________________________________________________________________________________________

modelDF <- select(df, which(colnames(df) %in% c('Converter', adCatVars, regionVars, timeVars))) # , deviceVars, timeVars)))

set.seed(1)

# Set up parallel backend
cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
registerDoParallel(cl)
getDoParWorkers() # check how many cores are getting used

# Set up training control
control <- trainControl(method="repeatedcv", number=5, repeats=3,
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

cat('  WARNING - if using best predictor set from random forest RFE on other models this will result in overly optimistic performance, cos the model training doesnt know that the features were selected - see http://stats.stackexchange.com/questions/60734/caret-rfe-variable-selection-and-test-prediction\n')



#__________________________________________________________________________________________________________________________________

# # 4. Feature selection methods -  try use GLMnet with ridge vs lasso vs blend, modifying alpha/lambda params to get different degrees of feature selection, then pick simplest model within 1se of best ----
# #__________________________________________________________________________________________________________________________________

### For info on the '1se rule' see here https://sebastianraschka.com/blog/2016/model-evaluation-selection-part3.html

modelDF <- select(df, which(colnames(df) %in% c('Converter', adCatVars, regionVars, timeVars))) # , deviceVars, timeVars)))
set.seed(1203)

# Set up parallel backend
cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
registerDoParallel(cl)
getDoParWorkers() # check how many cores are getting used

# Set up training control
control <- trainControl(method="repeatedcv", number=5, repeats=5,
                        summaryFunction=twoClassSummary,	# Use AUC to pick the best model (e.g. lambda in glmnet)
                        classProbs=TRUE,
                        sampling='down') # downsample within each resampling fold, then test on full holdout fold. https://topepo.github.io/caret/subsampling-for-class-imbalances.html for details

#+++++++++++++++++
# run models using caret defaults, then delve into accessing the different lambda/ alpha values
#+++++++++++++++++

mGlmnet <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC')

# Look at lambda and alpha values chosen by train function:
mGlmnet
mGlmnet$bestTune$lambda; mGlmnet$bestTune$alpha # extract best hyperparam values from cross validation grid
windows()
plot(mGlmnet)
graphics.off()

# final model is then fitted on all data, based on best alpha above BUT new lambda sequence then gets automatically generated by glmnet:
mGlmnet$finalModel$lambda # new sequence of lambdas, but can ignore these and just predict based on 
mGlmnet$finalModel$tuneValue$alpha
mGlmnet$finalModel$tuneValue$lambda # optimal values from train function
mGlmnet$finalModel$lambdaOpt # can also get best lambda from train() this way

#+++++++++++++++++++
# Retrain model using expanded training grid
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

# Set up training control
control <- trainControl(method="repeatedcv", number=5, repeats=5,
                        summaryFunction=twoClassSummary,	# Use AUC to pick the best model (e.g. lambda in glmnet)
                        classProbs=TRUE,
                        sampling='down') # downsample within each resampling fold, then test on full holdout fold. https://topepo.github.io/caret/subsampling-for-class-imbalances.html for details

# Rerun model
mGlmnet2 <- train(Converter~., data=modelDF, method="glmnet", trControl=control, metric='ROC', tuneGrid=tGrid)

#+++++++++++++++++++
# Explore resampling results and look at model that needs fewest predictors while also being within 1se of best performing model
#+++++++++++++++++++

# ==> note that when lambda controls strength of penalty and therefore no. of predictors that get shrunk to zero:
numCoeffsList <- list()
for(i in 1:length(tGrid$lambda)){
  lambda_i <- tGrid$lambda[i]
  numCoeffsList[[i]] <- data.frame(lambda=lambda_i, 
                                   numPreds=length(coef(mGlmnet2$finalModel, s=lambda_i)[coef(mGlmnet2$finalModel, s=lambda_i)!=0]))
}
numCoeffsDF <- bind_rows(numCoeffsList)
numCoeffsDF
windows()
ggplot(numCoeffsDF, aes(lambda, numPreds)) + geom_line(colour='purple')
graphics.off()

# explore best model using native caret plots
windows()
plot(mGlmnet2)
graphics.off()

# Use ggplot to add in se's & find model with fewest predictors that is still <1se from best model
inputModel <- mGlmnet2
seDF <- inputModel$results %>%
  mutate(rocSE_low = ROC-ROCSD/sqrt(inputModel$control$number*inputModel$control$repeats),
         rocSE_high = ROC+(ROCSD/sqrt(inputModel$control$number*inputModel$control$repeats)), # calculate std errors of resamples; see http://stats.stackexchange.com/questions/206139/is-there-a-way-to-return-the-standard-error-of-cross-validation-predictions-usin
         modelPerformance='other')
seDF$modelPerformance[which(seDF$ROC==max(seDF$ROC))] <- 'best'
seDF$within1seVec <- ifelse(seDF$rocSE_high>=max(seDF$ROC) & seDF$modelPerformance!='best', 'y', 'n')
seDF$modelPerformance[which(seDF$lambda==max(seDF$lambda[seDF$within1seVec=='y']))] <- 'fewestPredsWithin1se'

windows()
ggplot(seDF, aes(lambda, ROC, colour=modelPerformance)) + geom_point(size=2) + 
  geom_errorbar(aes(ymin=rocSE_low, ymax=rocSE_high), width=0) + ggtitle('ROC +/- 1se')
graphics.off()

===== TEMPORARY BOOKMARK =====
  
# find biggest lambda (i.e. fewest predictors) within 1se

# Calculate no. of predictors in each model
seDF$numPreds <- NULL
seDF[1, 'numPreds'] <- 1

control_finalModel <- trainControl(method="none", classProbs=TRUE, sampling='down')
mGlmnet_i <- train(Converter~., data=modelDF, method="glmnet", trControl=control_finalModel, 
                   tuneGrid=expand.grid(alpha=inputModel$bestTune$alpha, lambda=inputModel$bestTune$lambda))

coef(mGlmnet_i$finalModel, s=mGlmnet_i$bestTune$lambda)
coef(inputModel$finalModel, s=inputModel$bestTune$lambda)
str(mGlmnet2)


  ggplot(aes(x = C)) +
  geom_line(aes(y = Accuracy)) +
  geom_point(aes(y = Accuracy)) +
  scale_x_log10() + #correct spacing of the cost parameter
  ylim(0.65, 0.8) + #set correct y-axis
  geom_errorbar(aes(ymin=accuracySD_low, ymax=accuracySD_high), 
                colour="gray50",
                width=.1) +
  labs(title="Estimates of prediction accuracy\nwith 2 SD errror bars")

str(mGlmnet2)



mGlmnet$finalModel$lambdaOpt %in% mGlmnet$finalModel$lambda
mGlmnet$finalModel$alphaOpt
mGlmnet$bestTune$lamdbaOpt


coef(mGlmnet$finalModel, s=)
mGlmnet$bestTune
mGlmnet
plot(mGlmnet)

#__________________________________________________________________________________________________________________________________

# # 4.Feature selection methods - test performance of random forest with feature selection [*** Hashing out - this is just a placeholder; can do later if I want to explore RF's further ----
#__________________________________________________________________________________________________________________________________

# See answer here for a good starting point. http://stats.stackexchange.com/questions/200823/does-it-makes-sense-to-use-feature-selection-before-random-forest. This answer says that feature selection with RF may be a good idea if lasso outperforms ridge.

# Also keep in mind that, for purporse of keeping no. of features to a minimum, can use 1se rule - ie choose simplest model that produces performance metric within 1se of best-performing model.


#__________________________________________________________________________________________________________________________________

# # 4. Feature selection methods -  try recursive feature elimination on a random forest model & bagged tree model to look at which vars I should include [*** Hashing out - takes ages, and I can return to this later if needs be ***] ----
# #__________________________________________________________________________________________________________________________________

# cat('  Running recursive feature elimination to get an idea of best vars to include - this can take a while, particularly if no downsampling\n')
# 
# #+++++++++++++++++
# # First, remove vars I definitely don't want to predict with, then recreate dummy vars as required
# #+++++++++++++++++
# 
# modelDF <- select(df, which(colnames(df) %in% c('Converter', adCatVars, regionVars, timeVars))) # , deviceVars, timeVars))) # just look at region and ad cat to start with
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
# control <- trainControl(method="repeatedcv", number=5, repeats=5,
#                         summaryFunction=twoClassSummary,	# Use AUC to pick the best model (e.g. lambda in glmnet)
#                         classProbs=TRUE,
#                         sampling='down') # downsample within each resampling fold, then test on full holdout fold. https://topepo.github.io/caret/subsampling-for-class-imbalances.html for details
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

# # 5. For major model classes that appear to work well, try different downsampling regimes [*** Hashing out - takes ages, and I can return to this later if needs be ***]----
# #__________________________________________________________________________________________________________________________________
# 
# # Set up parallel backend
# cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
# registerDoParallel(cl)
# getDoParWorkers() # check how many cores are getting used
# 
# # Set up training control
# control <- trainControl(method="repeatedcv", number=5, repeats=3,
#                         summaryFunction=twoClassSummary,	# Use AUC to pick the best model (e.g. lambda in glmnet)
#                         classProbs=TRUE,
#                         sampling='down') # downsampling initially, but below update this to upsample, SMOTE, and ROSE. As per https://topepo.github.io/caret/subsampling-for-class-imbalances.html for details
# 
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

# 6. For final **tree** model, tabulate segment rules, lift, and CPA, to give to media buyers ----
#__________________________________________________________________________________________________________________________________

#+++++++++++++++++++
# Rerun final model with partykit, after first potentially messing with mincriterion and maxdepth params to make a smaller tree for planners
#+++++++++++++++++++

set.seed(1)

# Set up tuning grid 
grid <- expand.grid(mincriterion=c(0.1, seq(0.5, 0.99, 0.05), 0.99), maxdepth=c(5, 10))

# Set up training control
control <- trainControl(method="repeatedcv", number=5, repeats=3,
                        summaryFunction=twoClassSummary,	# Use AUC to pick the best model (e.g. lambda in glmnet)
                        classProbs=TRUE,
                        sampling='down') # downsampling initially, but below update this to upsample, SMOTE, and ROSE. As per https://topepo.github.io/caret/subsampli
mCTree_ds <- train(Converter~., data=modelDF, method="ctree2", trControl=control, tuneGrid=grid, metric='ROC')

windows()
plot(mCTree_ds) # look at best tuning grid combinations
graphics.off()

# up/downsample as necessary, to match final caret tree, then run in partykit
convsDF <- filter(modelDF, Converter=='y')
nonconvsDF <- filter(modelDF, Converter=='n')
modelDF_manualDownsample <- bind_rows(convsDF, sample_n(nonconvsDF, size=nrow(convsDF)))

mCTree_PK <- partykit::ctree(Converter~., data=modelDF_manualDownsample, maxdepth=50, mincriterion=0.65)

cat('  WARNING: for ctree outputs for planners Im running model through partykit cf using carets finalModel; means I can 
   - 1 specify mincriterion/maxdepth parameters manually (cos best performing tree may be too complex for planners) and 
   - 2 plot with partykits nice plotting methods.
   - NB may mean model isnt quite identical to caret version
   - NB If the best class-balancing method is SMOTE or ROSE I need to figure out how to do these outside of caret and apply before running model.\n')

#+++++++++++++++++++
# plot trees and save to file, if desired (but excel output below is better I think)
#+++++++++++++++++++

windows()
plot(mCTree_PK, gp = gpar(fontsize = 6), simplifiy=TRUE)
graphics.off()
# # save plots to file
# png(paste0(pathSave, 'exampleCTree1.png'), height=10000, width=10000, res=400)
# plot(mCTree_PK, gp = gpar(fontsize = 6), simplifiy=TRUE)
# graphics.off()

#+++++++++++++++++
# Use predicted values to get summary stats for each node/leaf 
#+++++++++++++++++

treeModelToSummarise <- mCTree_PK

nodePredictions <- predict(treeModelToSummarise, type = "prob") # one row per instance, with row labels giving node and cell values giving prob of "y" and prob of "n"
nodeLabels <- labels(nodePredictions)[[1]]
nodeProbLabels <- labels(nodePredictions)[[2]]
nodePredictionsDF <- data.frame(node=nodeLabels, probYes=nodePredictions[,which(nodeProbLabels=='y')])

# Summarise conversion probabilities per node
treeSummaryDF <- nodePredictionsDF %>%
  group_by(node) %>%
  summarise(numConversions_modelDF=length(node)*unique(probYes), numNonConversions_modelDF=length(node)*(1-unique(probYes)), 
            relativeProbConversion=unique(probYes)) %>% 
  mutate(baselineProbConversion=sum(numConversions_modelDF)/(sum(numConversions_modelDF)+sum(numNonConversions_modelDF)),
         lift=relativeProbConversion/baselineProbConversion) %>%
  arrange(desc(lift))#  %>%

# Round numeric cols to sensible number
treeSummaryDF[,colnames(treeSummaryDF)[colnames(treeSummaryDF)!='node']] <- 
  round(treeSummaryDF[,colnames(treeSummaryDF)[colnames(treeSummaryDF)!='node']], 4)

# Add in verbal description of rules for each node
treeRules <- partykit:::.list.rules.party(treeModelToSummarise)
treeSummaryDF$segment <- unname(treeRules)[match(treeSummaryDF$node, labels(treeRules))]
options(scipen=1000)
# as.data.frame(treeSummaryDF)

#+++++++++++++++++++
# Add in info about expected number of rows in the full dataset 
#+++++++++++++++++++

cat('  WARNING: \n   - numRows and cost variables per segment are calculated based on df  ***AFTER FILTERING AD CATEGORIES*** i.e. may be filtered by Layer, Offer, & Group depending on parameter values.\n   - Example: if route=TAS, offerAds=all, groupAds=Retail, & layerAds=notRemarketing, then CPA = [total adspend across retail, non-remarketing DBM ads advertising any route] / [no. of retail, non-remarketing DBM ads advertising any route that were attributed a TAS conversion]\n   - Parameter values are given in name of tree output - make sure planners read this and know what cols refer to.\n')
  
# NB following code is based on df, which has already been filtered to remove ads that don't match group, offer, and layer criteria

numConvs_fullDF <- nrow(filter(df, Converter=='y')) 
numNonConvs_fullDF <- nrow(filter(df, Converter=='n'))/negativeInstanceDownsampling  # NB while modelDF may have some downsampling, df won't (aside from that specified in negativeInstanceDownsampling parameter) 
nonConvsRatio_fullVsModelDF <- numNonConvs_fullDF/sum(treeSummaryDF$numNonConversions_modelDF)

treeSummaryDF <- treeSummaryDF %>%
  mutate(numRows=round(numConversions_modelDF+(numNonConversions_modelDF*nonConvsRatio_fullVsModelDF), 0))
sum(treeSummaryDF$numRows) # should be roughly 8 million per month for *all* DBM ads, if 1 row=1 ad c.f. 1 row=1 user

#+++++++++++++++++++
# Ad info about expected cost per ad
#+++++++++++++++++++

costDF <- df
costDF$node <- as.character(predict(treeModelToSummarise, type = "node", newdata = costDF))
# as.data.frame(select(treeSummaryDF, node, segment)); unique(filter(costDF, node==9)$adX_Finance) # checking they align. Totes do

costDF <- costDF %>%
  group_by(node) %>%
  summarise(medianDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.5), 1),
            lowerQuartileDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.25), 1),
            upperQuartileDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.75), 1),
            meanDBMRevenueAdvertiserCurrencyCPM=round(mean(DBMRevenueAdvertiserCurrencyCPM), 1)) %>%
  transmute(node=node, medianRevenueNZD=medianDBMRevenueAdvertiserCurrencyCPM, 
            IQRRevenueNZD=paste0(lowerQuartileDBMRevenueAdvertiserCurrencyCPM, '-', upperQuartileDBMRevenueAdvertiserCurrencyCPM),
            meanRevenueNZD=meanDBMRevenueAdvertiserCurrencyCPM)
  
warning('Could split out cost into weighted average based on converter vs nonconv cost, if worthwhile. Did notice that for some segments converter costs were more than nonconv costs, so could be worth doing. ***ALSO SUGGESTS THERES SOME IMPORTANT VAR IM NOT MODELLING - OR AT LEAST SOME VAR THAT SIGNIFICANTLY INFLUENCES PRICE. E.g. Video vs banner ads***\n')

treeSummaryDF <- treeSummaryDF %>%
  left_join(costDF, by='node') %>%
  select(-node, -numNonConversions_modelDF) %>%
  rename(numConversions=numConversions_modelDF)  %>%
  mutate(TotalRevenueNZD=round(numRows*(meanRevenueNZD/1000),0))  %>% # meanRevenue is CPM, so need to divide by 1000 to extrapolate total spend from no. of ads served.
  mutate(CPA=round(TotalRevenueNZD/numConversions, 0))
  
#+++++++++++++++++++
# Data cleaning
#+++++++++++++++++++

treeSummaryDF$numRows <- formatC(treeSummaryDF$numRows, format="d", big.mark=",") # add commas into big numbers for easier readability
colnames(treeSummaryDF)[colnames(treeSummaryDF)=='numRows'] <- paste0('numDBMAds_', gsub('-', '', firstDate), 'To', gsub('-', '', maxDate)) # incorporating date range over which numRows is measured
cat(  'WARNING: numDBMAds column may need to be calculated differently if outputDatabase is based on one row per user cf one row per ad. At very least name will need to change name to numDBMUsers or similar\n')

# Change to nicer order
tempSegment <- treeSummaryDF$segment
treeSummaryDF <- select(treeSummaryDF, -segment)
treeSummaryDF$segment <- tempSegment

# Make segment descriptions more readable
treeSummaryDF$segment <- gsub("%in%", "INCLUDES:", treeSummaryDF$segment)
treeSummaryDF$segment <- gsub("c\\(", "", treeSummaryDF$segment)
treeSummaryDF$segment <- gsub("&", "AND", treeSummaryDF$segment)
treeSummaryDF$segment <- gsub("<= 0", "IS EXCLUDED", treeSummaryDF$segment)
treeSummaryDF$segment <- gsub("> 0", "IS INCLUDED", treeSummaryDF$segment)
treeSummaryDF$segment <- gsub("\\)", "", treeSummaryDF$segment)

# Round numeric vars to make more readable
treeSummaryDF[, colnames(treeSummaryDF) %in% c('lift', 'baselineProbConversion', 'relativeProbConversion')] <- 
  round(treeSummaryDF[, colnames(treeSummaryDF) %in% c('lift', 'baselineProbConversion', 'relativeProbConversion')], 2)

as.data.frame(treeSummaryDF)

# # Check it matches with real data
# nodeXdf <- filter(modelDFDummified, adX_Reference <= 0 & adX_News > 0 & adX_WorldLocalities <= 0)
# table(nodeXdf$Converter)
# 152/(152+9031) # yup, all ads up.

#+++++++++++++++++++
# Save to file
#+++++++++++++++++++

write.csv(treeSummaryDF, file=paste0(pathSave, 'TREERESULTS_', outputDatabase, '_negativeInstanceDownsampling', negativeInstanceDownsampling, '_', dataset, 'DF_', route, '_offer', offerAds, '_group', groupAds, '_layer', layerAds, '_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.csv'), row.names=FALSE)

  
#+++++++++++++++++++
# TEMP graphing distribution of costs per ad across segments based on best CTree model above] ----
#+++++++++++++++++++

plotDF <- df
plotDF$node <- as.character(predict(treeModelToSummarise, type = "node", newdata = plotDF))
  
windows()
ggplot(plotDF, aes(node, DBMRevenueAdvertiserCurrencyCPM)) + geom_jitter(height=0, alpha=0.01) + # horiz jitter only
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", colour='blue', width = 0.2)
graphics.off()




#__________________________________________________________________________________________________________________________________

# 7. For non-tree models, try creating segments manaully - iterate thru all possible combinations of final modelled vars, then predict from model ----
#__________________________________________________________________________________________________________________________________

modelledData <- modelDF
modelPreds <- colnames(modelledData)[colnames(modelledData)!='Converter']

allVarsUniqueLevelsList <- list()
for(i in 1:length(modelPreds)){
  allVarsUniqueLevelsList[[i]] <- unique(modelledData[,modelPreds[i]])
}
names(allVarsUniqueLevelsList) <- modelPreds

allCombinationsModelledVarsDF <- expand.grid(allVarsUniqueLevelsList) # ERror - out of memory

