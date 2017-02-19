
# BOOKMARKS
# 1. set parameters & load data
# xxx

#__________________________________________________________________________________________________________________________________

#0. set parameters & load data
#__________________________________________________________________________________________________________________________________

#===============
# set params & load data
#===============

rm(list=ls())
set.seed(1)
dataset <- 'full'
basePath <- 'E:/JAY/'
pathData <- paste0(basePath, 'data/')
pathSave <- paste0(basePath, 'AirNZ segmentation results/')
downsampleFactor <- 100000 # for models, how many more nonconvs than convs should there be at a maximum? [Note that there has already been some downsampling in EDA&CreateModelDatabase.r; see nonconvDownsampling param below]
responseVar <- 'lastClickConversion' # 'Conversion' or 'lastClickConversion'. Former is whether a user converted at any point over model period; latter is whether a given ad was attributed a covnersion via last click methodology. lastClickConv only applies to outputDatabase==df_oneRowPerAdAndLastClickConversionResponse

# Define which types of ads we want to look at     ***CURRENTLY ONLY IMPLEMENTED FOR df_oneRowPerAdAndLastClickConversionResponse ***
offerAds <- 'allRoutes' # 'focalRoute', 'allRoutes' # e.g. if routeAds==focalRoute, then for route==TAS we would only include ads that have a 'Tasman sale' message in the model
layerAds <- 'allRoutes' # 'notRemarketing', 'Remarketing', or 'all'. If 'notRemarketing' then exclude RMK ads from model.
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

#===============
# Modify vars for modelling
#===============

if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){
  df$Converter <- df[,which(grepl('lastClickConversion', colnames(df)))]
  df <- select(df, -lastClickConversion)
  cat(' WARNING - renaming "lastClickConversion" var to "Converter", Just so theres a single name for the target var thru rest of code. \n')
}
df$Converter <- as.factor(ifelse(df$Converter==1, 'y', 'n')); cat(' Changing conv/nonconv values from 1/0 to y/n for modelling\n')
df <- select(df, -user_id)
df$DBMRegion <- gsub('-', '', df$DBMRegion) # 'Manawatu-Wanganui' hyphen caused probs in models.

#===============
# Define variable groups - makes it easy to quickly include/exclude vars from models
#===============

adCatVars <- colnames(df)[grepl('adX_', colnames(df))] 
urlVars <- colnames(df)[grepl('url_', colnames(df))] 
deviceVars <- colnames(df)[grepl('Browser', colnames(df)) |grepl('Device', colnames(df)) | 
                               grepl('Operating', colnames(df)) | grepl('Mobile', colnames(df))]
regionVars <- 'DBMRegion'
countryCodeVars <- 'CountryCode'
bidPriceVars <- 'medianBidPrice'
timeVars <- c('daypart', 'day')

# ==> subsequent code will be like this 'modelDF <- select(df, which(colnames(df) %in% c(responseVar, adCatVars, regionVars)))'

#===============
# check for and remove NAs
#===============

naRows <- nrow(filter(df, is.na(DBMRegion)))
if(naRows >0){
  cat(paste0(' WARNING - ', naRows, ' have NAs & have been removed\n'))
  df <- na.omit(df)
}

#__________________________________________________________________________________________________________________________________

#1. Based on values for ad attributes in parameters, either include or exclude individual ads from analysis, then do any additional downsampling 
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

#===============
# additional downsampling to ensure no. of nonconvs is only x times more than no. of convs
#===============

set.seed(1203)
numNonconvs <- length(df$Converter[df$Converter=='n'])   # NOT DOING FOR NOW - EASIER NOT TO FOR NOW COS EASIER TO BACK-CALCULATE NUM USERS IN DATABASE
numConvs <- length(df$Converter[df$Converter=='y'])
if(numNonconvs/numConvs>downsampleFactor){
  df <- bind_rows(filter(df, Converter=='y'),
                    sample_n(filter(df, Converter=='n'), size=numConvs*downsampleFactor))
} else {df <- df}
cat(paste0('  - Number of positive / negative instances after final downsampling: ', nrow(filter(df, Converter=='y')), ' / ', nrow(filter(df, Converter=='n')), '\n'))

#__________________________________________________________________________________________________________________________________

#2. Some preliminary modelling - use caret's varImp function to find single predictors that are important
#__________________________________________________________________________________________________________________________________

# i.e. looks at each predictor's total effect, c.f. marginal effect, using model-specific metrics

set.seed(1)

#===============
# First, remove vars I definitely don't want to predict with, then recreate dummy vars as required
#===============

modelDF <- select(df, which(colnames(df) %in% c('Converter', adCatVars, regionVars, timeVars))) # , deviceVars, timeVars))) # just look at region and ad cat to start with

# Dummify factors
modelDF$Converter <- as.character(modelDF$Converter) # get warning about Converter not being a factor as part of dummification otherwise - easier to just treat as character and change back to factor below
modelDFDummified <- dummyVars('Converter ~ .', data=modelDF, fullRank=TRUE) # caret's dummyVars ignores numeric columns automatically. FullRank excludes 1 level of each factor (becoming intercept), same as model.matrix. Should always do this, otherwise can get perfectly correlated vars.
modelDFDummified <- as.data.frame(predict(modelDFDummified, newdata=modelDF)) # need to do this to convert back to a dataframe
modelDFDummified$Converter <- as.factor(modelDF$Converter) # have to manually add back in
modelDF$Converter <- as.factor(modelDF$Converter) # ..and manually chnage back to factor

# #===============
# # some quick EDA for individual vars
# #===============
# 
# plotDF <- mutate(modelDF, ConverterNumeric=ifelse(Converter=='y', 1, 0))
# ggplot(plotDF, aes(daypart, ConverterNumeric)) + geom_jitter(alpha=0.5) + stat_summary(fun.data="mean_cl_boot", colour = "red", size = 2)
# x <- glm(Converter~daypart, data=modelDF, family='binomial')
# summary(x)

#===============
# run models
#===============

# Set up parallel backend
cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
registerDoParallel(cl)
getDoParWorkers() # check how many cores are getting used

# Set up training control
control <- trainControl(method="repeatedcv", number=5, repeats=3,
                        summaryFunction=twoClassSummary,	# Use AUC to pick the best model (e.g. lambda in glmnet)
                        classProbs=TRUE)

# Run a few models using caret defaults
mGlmnet <- train(Converter~., data=modelDFDummified, method="glmnet", trControl=control, metric='ROC')
mSVM <- train(Converter~., data=modelDFDummified, method="svmLinear", trControl=control, metric='ROC')
mLR <- train(Converter~., data=modelDFDummified, method="glm", family='binomial', trControl=control, metric='ROC')
mCTree <- train(Converter~., data=modelDFDummified, method="ctree", trControl=control, metric='ROC')
mRTree <- train(Converter~., data=modelDFDummified, method="rpart", trControl=control, metric='ROC')
mRF <- train(Converter~., data=modelDFDummified, method="rf", trControl=control, metric='ROC') # NB recommnedation for mtry param is mtry=floor(sqrt(ncol(x))) or mtry=7, see http://machinelearningmastery.com/tune-machine-learning-algorithms-in-r/

# plot(varImp(mGlmnet), top=10)
# plot(varImp(mLR), top=10)
# plot(varImp(mSVM), top=10) 
# plot(varImp(mCTree), top=10)
# plot(varImp(mRF), top=10) # all give quite differnt results... eek
# graphics.off()

# While I'm here, compare predictive performance of these models
AUCs <- resamples(list(Glmnet = mGlmnet, SVMLinear = mSVM, LogReg = mLR, RF=mRF, Ctree=mCTree, Rtree=mRTree))
windows()
bwplot(AUCs) # log reg pretty good; glment slightly better
graphics.off()


#__________________________________________________________________________________________________________________________________

#3. Compare major models for predicting conv prob using AUC
#__________________________________________________________________________________________________________________________________

# # i.e SVMs for GLMs vs lasso vs trees vs NB etc etc. Just to see how well trees stack up.
# 
# set.seed(1)
# 
# #===============
# # First, remove vars I definitely don't want to predict with, then recreate dummy vars as required
# #===============
# 
# modelDF <- select(df, which(colnames(df) %in% c('Converter', adCatVars, regionVars, deviceVars, timeVars))) # just look at region and ad cat to start with
# 
# # plotDF <- mutate(modelDF, ConverterNumeric=ifelse(Converter=='y', 1, 0))
# # library('Hmisc')
# # ggplot(plotDF, aes(daypart, ConverterNumeric)) + geom_jitter(alpha=0.5) + stat_summary(fun.data="mean_cl_boot", colour = "red", size = 2)
# # x <- glm(Converter~daypart, data=modelDF, family='binomial')
# # summary(x)
# 
# # Dummify factors, as per code above
# modelDFDummified <- dummyVars('Converter ~ .', data=modelDF, fullRank=TRUE) # caret's dummyVars ignores numeric columns automatically. FullRank excludes 1 level of each factor (becoming intercept), same as model.matrix. Should always do this, otherwise can get perfectly correlated vars.
# modelDFDummified <- as.data.frame(predict(modelDFDummified, newdata=modelDF)) # need to do this to convert back to a dataframe
# modelDFDummified$Converter <- modelDF$Converter # have to manually add back in
# 
#===============
# Run some models
#===============

# Set up parallel backend
cl <- makeCluster(detectCores()-1) # leave one core spare      * NB CARET AUTOMATICALLY DOES PARALLEL, IF BACKEND REGISTERED
registerDoParallel(cl)
getDoParWorkers() # check how many cores are getting used

# Set up training control
control <- trainControl(method="repeatedcv", number=3, repeats=3, 
                        summaryFunction=twoClassSummary,	# Use AUC to pick the best model (e.g. lambda in glmnet)
                        classProbs=TRUE)  

# # Run a few models to compare
# mGlmnet <- train(Converter~., data=modelDFDummified, method="glmnet", trControl=control, metric='ROC')
# mSVM_linear <- train(Converter~., data=modelDFDummified, method="svmLinear", trControl=control, metric='ROC')
# mSVM_Poly <- train(Converter~., data=modelDFDummified, method="svmPoly", trControl=control, metric='ROC')
# mSVM_Radial <- train(Converter~., data=modelDFDummified, method="svmRadial", trControl=control, metric='ROC')
# mLR <- train(Converter~., data=modelDFDummified, method="glm", family='binomial', trControl=control, metric='ROC')
# mNB <- train(Converter~., data=modelDFDummified, method="nb", trControl=control, metric='ROC') # looks like caret doesnt automatically tune NB hyperparams (type 'mNB' into console for details) - so good to use tuneGrid here. 
# detach("package:klaR", unload=TRUE)
# detach("package:MASS", unload=TRUE) # these packages mask dplyr's select() 
mCTree <- train(Converter~., data=modelDFDummified, method="ctree", trControl=control, metric='ROC')
mCTree2 <- train(Converter~., data=modelDFDummified, method="ctree2", trControl=control, metric='ROC',
                 tuneGrid=expand.grid(maxdepth = seq(50, 50,5),  # setting maxdepth at a single value for now
                                      mincriterion=c(0.5, 0.55, 0.6, 0.65, 0.7, 0.75, 0.8, 0.85, 0.9, 0.95, 0.99))) # try manually tuning hyperparams

# # For rpart, annoyingly won't accept 'Converter ~.' formula. Need to specify all vars manually:     # ctree always performs better anyway...
# manForm <- formula(paste('Converter ~', paste(colnames(select(modelDFDummified, -Converter)), collapse=' + ')))
# mRTree <- train(manForm, data=modelDFDummified, method="rpart", trControl=control, metric='ROC')
# mRTree2 <- train(manForm, data=modelDFDummified, method="rpart2", trControl=control, metric='ROC')
# manForm_noDummies <- formula(paste('Converter ~', paste(colnames(select(modelDF, -Converter)), collapse=' + ')))
# mRTree_noDummies <- train(manForm_noDummies, data=modelDF, method="rpart", trControl=control, metric='ROC')
# mRTree2_noDummies <- train(manForm_noDummies, data=modelDF, method="rpart2", trControl=control, metric='ROC') # seeing if inputting factors (ie single DBMregion var) rather than dummy vars (AucklandYN, WellingtonYN etc) improves performance or changes interpretation

# # Compare predictive performance of these models
# AUCs <- resamples(list(Glmnet=mGlmnet, SVMLinear=mSVM_linear, SVMPoly=mSVM_Poly, SVMRadial=mSVM_Radial, LogReg=mLR, NB=mNB, CTree=mCTree,CTree2=mCTree2, CTree_noDummies=mCTree_noDummies,CTree2_noDummies=mCTree2_noDummies, RTree=mRTree, RTree2=mRTree2, RTree_noDummies=mRTree_noDummies, RTree2_noDummies=mRTree2_noDummies))
# windows()
# bwplot(AUCs)
# graphics.off()

#__________________________________________________________________________________________________________________________________

#4. Delve deeper into classification trees
#__________________________________________________________________________________________________________________________________

# Use code above to check that classification trees perform ok, compared with other methods. Below, try tuning hyperparams manually to get better performance, then find the best tree and plot it.

# NEED TO READ IN CODE ABOVE TO CREATE MODELLING DATABASES

#===============
# Tune hyperparameters better
#===============

#EDIT - NOT DOING THIS YET.... looking at caret's plots of hyperparam values vs AUC, diff values make almost no difference to AUC. So not a priority to optimise just yet.

# See hyperparams that can easily be tuned (NB according to this post, there are other params but caret only tunes the most important one http://stats.stackexchange.com/questions/168269/which-parameters-to-tune-in-cart)
# mRTree # cp, with caret trying 0.004, 0.01, and 0.02. Plot shows values <0.004 could have higher AUC
# mRTree2 # maxdepth, with caret trying 4, 5, & 9. Plot shows values <9 could have higher AUC
mCTree # mincriterion, with caret trying 0.01, 0.5, & 0.99. Plot shows values <0.01 could have higher AUC
mCTree2 # maxdepth & mincriterion, with caret trying combinations of 15, 20, 25...50 & 0.5, 0.95 & 0.99. Plot shows values <0.05 could have higher AUC, and no's <15 OR >50 could too.
mCTree2$finalModel
# ==> look at plots and see at which end of distribution the values could be expanded.

graphics.off()

cat('NB I think trees can also have a parameter set that defines minimum no. of observations in a terminal node. Could be useful for planners to e.g. say "only define segments with >100 users" or similar\n')

#===============
# Create segments based on best model
#===============

# TRY partykit c.f. party, and mob() for lmtree & glmtree stuff
# And good explanation of rpart vs ctree here http://stats.stackexchange.com/questions/22035/party-vs-rpart-vs-for-partitioning-trees-in-r

fancyRpartPlot(mRTree$finalModel)
fancyRpartPlot(mRTree2$finalModel)
# fancyRpartPlot(mCTree$finalModel) # doesn't work

# Redo ctree with partykit package - newer, better implementation, with better plotting options
mCTree_PK <- partykit::ctree(Converter~., data=modelDF, mincriterion=0.5)# , maxdepth=50)

mCTree_PK
mCTree_PK2 <- partykit::ctree(Converter~., data=modelDFDummified, maxdepth=50, mincriterion=0.75)
mCTree_PK2

mCTree2
mCTree
plot(mCTree_PK, gp = gpar(fontsize = 6), simplifiy=TRUE)
plot(mCTree_PK2, gp = gpar(fontsize = 6), simplifiy=TRUE)

plot(mCTree$finalModel)
summary(mCTree_PK)

head(data_party(mCTree_PK, id=4))


# # save some of these plots to file
# png(paste0(pathSave, 'exampleCTree1.png'), height=10000, width=10000, res=400)
# plot(mCTree_PK, gp = gpar(fontsize = 6), simplifiy=TRUE)
# graphics.off()
# 
# png(paste0(pathSave, 'exampleCTree2.png'), height=10000, width=10000, res=400)
# plot(mCTree_PK2, gp = gpar(fontsize = 20), simplifiy=TRUE)
# graphics.off()
# 
# png(paste0(pathSave, 'exampleRTree.png'), height=3000, width=3000, res=400)
# fancyRpartPlot(mRTree$finalModel)
# graphics.off()
# 
# png(paste0(pathSave, 'exampleRTree2.png'), height=3000, width=3000, res=400)
# fancyRpartPlot(mRTree2$finalModel)
# graphics.off()
# 

#__________________________________________________________________________________________________________________________________

#5. Print out characteristics of trees, to give to media planners
#__________________________________________________________________________________________________________________________________

#===============
# Use predicted values to get summary stats for each node/leaf 
#===============

treeModelToSummarise <- mCTree_PK2

nodePredictions <- predict(treeModelToSummarise, type = "prob") # one row per instance, with row labels giving node and cell values giving prob of "y" and prob of "n"
head(nodePredictions)
nodeLabels <- labels(nodePredictions)[[1]]
nodeProbLabels <- labels(nodePredictions)[[2]]
nodePredictionsDF <- data.frame(node=nodeLabels, probYes=nodePredictions[,which(nodeProbLabels=='y')])
nodePredictionsDF

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
as.data.frame(treeSummaryDF)

# Add in info about expected number of rows in the full dataset
names(df)
treeSummaryDF <- treeSummaryDF %>%
  mutate(numRows=numConversions_modelDF+((numNonConversions_modelDF*downsampleFactor)/negativeInstanceDownsampling))
colnames(treeSummaryDF)[colnames(treeSummaryDF)=='numRows'] <- paste0('numRows_', gsub('-', '', firstDate), 'To', gsub('-', '', maxDate)) # incorporating date range over which numRows is measured

# Ad info about expected cost per ad
costDF <- df
costDF$node <- nodePredictionsDF$node
# filter(costDF, node==9)$day # checking they align. Totes do

costDF <- costDF %>%
  group_by(node) %>%
  summarise(medianDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.5), 1),
            lowerQuartileDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.25), 1),
            upperQuartileDBMRevenueAdvertiserCurrencyCPM=round(quantile(DBMRevenueAdvertiserCurrencyCPM, 0.75), 1)) %>%
  transmute(node=node, medianRevenueNZD=medianDBMRevenueAdvertiserCurrencyCPM, 
            IQRRevenueNZD=paste0(lowerQuartileDBMRevenueAdvertiserCurrencyCPM, '-', upperQuartileDBMRevenueAdvertiserCurrencyCPM))
  
warning('Could split out cost into weighted average based on converter vs nonconv cost, if worthwhile. Did notice that for some segments converter costs were more than nonconv costs, so could be worth doing. ***ALSO SUGGESTS THERES SOME IMPORTANT VAR IM NOT MODELLING - OR AT LEAST SOME VAR THAT SIGNIFICANTLY INFLUENCES PRICE***\n')

treeSummaryDF <- treeSummaryDF %>%
  left_join(costDF, by='node') %>%
  select(-node, -numConversions_modelDF, -numNonConversions_modelDF)

# Change to nicer order
tempSegment <- treeSummaryDF$segment
treeSummaryDF <- select(treeSummaryDF, -segment)
treeSummaryDF$segment <- tempSegment

as.data.frame(treeSummaryDF)

# # Check it matches with real data
# nodeXdf <- filter(modelDFDummified, adX_Reference <= 0 & adX_News > 0 & adX_WorldLocalities <= 0)
# table(nodeXdf$Converter)
# 152/(152+9031) # yup, all ads up.

#===============
# Apply classifications segments to original database, to calc no. of users, no. of imps, no. of conversions etc 
#===============

# EDIT - will need to make df dummified - same vars as modelDF_dummified - in order to apply segments.


# Save to file
write.csv(treeSummaryDF, file=paste0(pathSave, 'TREERESULTS_', outputDatabase, '_negativeInstanceDownsampling', negativeInstanceDownsampling, '_', dataset, 'DF_', route, '_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.csv'), row.names=FALSE)


# #__________________________________________________________________________________________________________________________________
# 
# #5. Automatically decide best features to include in the model
# #__________________________________________________________________________________________________________________________________
# 
# set.seed(7)
# 
# # define the control using a random forest selection function
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# # run the RFE algorithm
# results <- rfe(PimaIndiansDiabetes[,1:8], PimaIndiansDiabetes[,9], sizes=c(1:8), rfeControl=control)
# # summarize the results
# print(results)
# # list the chosen features
# predictors(results)
# # plot the results
# plot(results, type=c("g", "o"))
# 
# 
# 
# 
# 
# 
# 
# 
# #===============
# # model - all predictors
# #===============
# 
# startTime2 <- proc.time()[3]
# set.seed(1) # randomness in cross-validation.
# lassoResults <- cv.glmnet(y=y, x=x, family='binomial', alpha=1)  # alpha=0 is ridge (params dont shrink to zero, so isn't a variable selection method. But better when high collinearity among vars or when p>n). alpha=1 is lasso. 0 > alpha < 1 is a blend. Can poss use parallel to do this - see glmnet pdf, cv.glm example section, for an example.
# cat(' ...Lasso regression run in', (proc.time()[3]-startTime2)/60, 'minutes\n')
# 
# betas_minLambda <- coef(lassoResults, s = 'lambda.min') # this is the run that cross-validated lass regression says is best (i.e. has best value of lambda, the param that controls degree of penalisation).
# 
# # save a hard copy of params
# options(scipen = 1000)
# betasTable <- data.frame(X=row.names(betas_minLambda),
#                          Estimate=unname(betas_minLambda[,1])) # need to put betas in print-friendly format
# betasTable
# 
# sum(predict(lassoResults, newx=x, type='response', s='lambda.min'))
# sum(ifelse(df$Converter=='y', 1, 0))
# sum(ifelse(df$Converter=='y', 1, 0))/nrow(df)
# 
# #===============
# # model - ad categories & DBM region only   * cos url categories confounded with ad x categories, & median bid price may not be useful for creating segments
# #===============
# 
# x <- x[, !grepl('url_', colnames(x))]
# x <- x[, !grepl('medianBidPrice', colnames(x))]
# x <- x[, !grepl('CountryCode', colnames(x))]
# 
# startTime2 <- proc.time()[3]
# set.seed(1) # randomness in cross-validation.
# lassoResults <- cv.glmnet(y=y, x=x, family='binomial', alpha=1)  # alpha=0 is ridge (params dont shrink to zero, so isn't a variable selection method. But better when high collinearity among vars or when p>n). alpha=1 is lasso. 0 > alpha < 1 is a blend. Can poss use parallel to do this - see glmnet pdf, cv.glm example section, for an example.
# cat(' ...Lasso regression run in', (proc.time()[3]-startTime2)/60, 'minutes\n')
# 
# betas_minLambda <- coef(lassoResults, s = 'lambda.min') # this is the run that cross-validated lass regression says is best (i.e. has best value of lambda, the param that controls degree of penalisation).
# 
# # save a hard copy of params
# options(scipen = 1000)
# betasTable <- data.frame(X=row.names(betas_minLambda),
#                          Estimate=unname(betas_minLambda[,1])) # need to put betas in print-friendly format
# betasTable
# 
# sum(predict(lassoResults, newx=x, type='response', s='lambda.min'))
# sum(ifelse(df$Converter=='y', 1, 0)) 
# 
# #===============
# # create conversion prob per segment graphs
# #===============
# 
# newx <- matrix(nrow=1, ncol=ncol(x), data=0)
# colnames(newx) <- colnames(x) # where is DBMREgionAuckland?
# # haven't finished... decided to do with logistic regression instead to get std errors
# 
# #__________________________________________________________________________________________________________________________________
# 
# #2b. Preliminary logistic regression model to look at which vars are significant, & try creating 'conversion probability per segment' graphs based on model outputs. Doing instead of above cos I can get s.e's around estimates  
# #__________________________________________________________________________________________________________________________________
# 
# #===============
# # model
# #===============
# 
# logRegDF <- select(df, which(!grepl('url', colnames(df)))) # too correlated with ad x categories
# logRegDF <- select(logRegDF, -medianBidPrice, -CountryCode)
# 
# mLR <- glm(Converter~., data=logRegDF, family='binomial')
# summary(mLR)
# 
# #===============
# # predict 'n' plot
# #===============
# 
# ### Ad category
# 
# newData <- logRegDF[1,] # setting up schema
# newData$DBMRegion <- 'Auckland' # setting region to most common value
# 
# adCategoriesVec <- colnames(newData)[grepl('adCategory', colnames(newData))]
# resultsList <- list()
# 
# for(i in 1:length(adCategoriesVec)){
#   adCat_i <- adCategoriesVec[i]  
#   newData[,grepl('adCategory', colnames(newData))] <- 0 #  startign off with no ad categories being seen
#   newData[, which(colnames(newData)==adCat_i)] <- 1
#   adCatPrediction <- predict(mLR, newdata=newData, type='response', se=TRUE)
#   resultsList[[i]] <- data.frame(adCategory=adCat_i,
#                                  convProb=adCatPrediction$fit, 
#                                  loCI=adCatPrediction$fit-1.96*adCatPrediction$se.fit,
#                                  upCI=adCatPrediction$fit+1.96*adCatPrediction$se.fit)
# }
# 
# plotDF <- bind_rows(resultsList)
# 
# ggplot(plotDF, aes(adCategory, convProb, colour=adCategory)) + geom_point()  + geom_errorbar(ymin=plotDF$loCI, ymax=plotDF$upCI) + 
#   ylim(min(plotDF$loCI), max(plotDF$upCI)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position="none")
# 
# coef(mLR)
# 
# #__________________________________________________________________________________________________________________________________
# 
# #25. messing around with testing different machine learning algorithms
# #__________________________________________________________________________________________________________________________________
# 
# ggplot(df, aes(medianBidPrice, Converter)) + geom_jitter(alpha=0.1) + geom_smooth()
# suppressMessages(library(randomForest))
# mRF <- randomForest(as.factor(Converter) ~ medianBidPrice, data=df) 
# mRF
# plot(x=df$medianBidPrice, y=predict(mRF, type='prob')[,2], col='red')
# names(df)
# newdata <- data.frame(medianBidPrice=seq(from=0, to=max(df$medianBidPrice), by=0.001))
# newdata
# 
# myPoints <- unname(predict(mRF, newdata, type='prob')[,2])
# plot(x=newdata$medianBidPrice, y=myPoints, type='l')
# myPoints
# as.numeric(newdata)
# graphics.off()
# 
# mPoly2 <- glm(as.factor(Converter)~medianBidPrice + scale(medianBidPrice^2, center=TRUE), data=df, family='binomial')
# mPoly3 <- glm(as.factor(Converter)~medianBidPrice + scale(medianBidPrice^2, center=TRUE) + scale(medianBidPrice^3, center=TRUE), data=df, family='binomial')
# 
# AIC(mPoly2, mPoly3)
# 
# predict(mPoly, type='response')
# 
# plot(df$medianBidPrice, predict(mPoly, type='response'))
# 
# #===============
# # using caret to get AUC
# #===============
# 
# caretDF <- transmute(df, y=as.factor(Converter), medianBidPrice=medianBidPrice, medianBidPrice2=scale(medianBidPrice, scale=FALSE)^2, 
#                      medianBidPrice3=scale(medianBidPrice, scale=FALSE)^3)
# caretDF$y <- ifelse(caretDF$y=='1', 'Converter', 'Nonconverter')
# str(caretDF$y)
# library(caret)
# library(mlbench)
# set.seed(1203)
# 
# # change from default bootstrap to CV
# ctrl <- trainControl(method='repeatedcv', repeats=3, # 3 repeats of 10-fold CV
#                      classProbs=TRUE,
#                      summaryFunction=twoClassSummary) # need classProbs and summaryFunction set thusly to do ROC analysis
# 
# # Train & test models
# mLogReg <- train(y~medianBidPrice, data=caretDF, 
#                  method='glm', family='binomial',
#                  metric='ROC', 
#                  trControl=ctrl)
# mLogReg2 <- train(y~medianBidPrice + medianBidPrice2, data=caretDF, 
#                   method='glm', family='binomial',
#                   metric='ROC', 
#                   trControl=ctrl)
# mLogReg3 <- train(y~medianBidPrice + medianBidPrice2 + medianBidPrice3, data=caretDF, 
#                   method='glm', family='binomial',
#                   metric='ROC', 
#                   trControl=ctrl)
# mGlmNet <- train(y~medianBidPrice + medianBidPrice2 + medianBidPrice3, data=caretDF, 
#                  method='glmnet', family='binomial',
#                  metric='ROC', 
#                  trControl=ctrl)
# mRF <- train(y~medianBidPrice, data=caretDF, 
#              method='rf',
#              metric='ROC', 
#              trControl=ctrl)
# 
# # compare models' average ROC across folds  # yus...
# data.frame(mLogRegROC=mLogReg$results$ROC, mLogRegROC2=mLogReg2$results$ROC, mLogRegROC3=mLogReg3$results$ROC, mRF=mRF$results$ROC,
#            mGlmNet=mGlmNet$results$ROC) # glmnet has multiple ROC values for some reason.
# 
