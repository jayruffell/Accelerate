
# Jane/Az asked for URL domain & subdomain (i.e. URL snipped to second forward slash - 'msn.com/business/' for example). Then break down these domain categories by AdX page category - e.g. 'Finance_msn.com/business/'. Then calc conv probs for each adx * URL category combo.

# This is just a copy of the EDA script, with extra bits to (1) calculate URL Domain category, and (2) save csv of conversion prob per URL & adPage category. Have deleted non-relevant sections of code.

# Have also added in a source all scripts thingo to loop thru the different routes.

#__________________________________________________________________________________________________________________________________

#1. set parameters & load data, also ID converters & then downsample if specified in params
#__________________________________________________________________________________________________________________________________

#===============
# set params
#===============

# Set paramters that may be overwritten by SourceAllScripts script  #   *** WILL BE OVERWRITTEN IF sourceScript PARAMETER CREATED IN SourceAllScripts EXISTS *** 
if(!exists('sourceScript')){
  rm(list=ls())
  route <- route <- 'allRoutes' # 'TAS', 'LH', 'PI', 'DOM', 'allRoutes'
  samplesize <- 0.001 # randomly sample this proportion of nonconvs, keep all convs
  rImageName <- 'database_usefulColumnsOnly_samplesize1.rda'}

# Set parameters that are always specified internally
set.seed(1)
basePath <- 'E:/JAY/' # path to day-level folders (i.e. one folder per day)
pathData <- paste0(basePath, 'data/')
pathSave <- paste0(basePath, 'AirNZ segmentation results/')

# Specificy conversion tags for different routes              
conversionTagIDs_TAS <- c(129429, 129424, 1498012, 1497697) 
conversionTagIDs_DOM <- c(129426, 129423, 1498011, 1497695)
conversionTagIDs_LH <- c(196731, 196730, 1498015, 1497701)
conversionTagIDs_PI <- c(129429, 129424, 1498012, 1497697)
conversionTagIDs_allRoutes <- c(2682414, # 2015 booking tag. Rest are ISIS tags (includes mobile convs) 
                                129423, 129424, 129425, 129426, 129429, 129430, 196730, 196731, 224825, 306899, 327181, 327182, 1497695, 1497697, 1497698, 1497701, 1498011, 1498012, 1498013, 1498015, 556003, 556004, 556005, 556006, 556007, 556008, 556009, 556010, 556011, 556012, 556013, 556014) # look at latest 'match_table_activity_cats' match table and find the ActivityIDs that represent conversions (ask Az).

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
suppressMessages(library(gridExtra))
suppressMessages(library(psych))
# suppressMessages(library(mlr)) # not available for MRAN
suppressMessages(library(caret))
suppressMessages(library(glmnet))
suppressMessages(library(stringr))
suppressMessages(library(tidyr))

#===============
# Load and clean data
#===============

startTime <- proc.time()[3]

if(exists('sourceScript')){
  cat('\nParameters provided by SourceAllScripts.r \n')
} else {
  cat('\nParameters NOT provided by SourceAllScripts.r \n')
}

if(grepl('samplesize1', rImageName)){
  cat(paste0(' \n                    ----- Running EDA for route=', route, ', full R image loaded, and downsampling of R image=', samplesize, ' -------\n'))
} else { 
  cat(paste0(' \n                    ----- Running EDA for route=', route, ', downsampled R image loaded, and downsampling of R image=', samplesize, ' ------\n'))
}

cat(' Loading data\n')
load(paste0(pathData, rImageName))

# rename some vars
df <- df %>% 
  rename(time=EventTime, user_id=UserID, other_data=OtherData) %>%
  filter(user_id!=0)

# ID variables are numeric, but should be character (otherwise models will think they're continuous) - change
IDvars <- colnames(df)[grepl('ID', colnames(df)) | grepl('DBMDeviceType', colnames(df)) | grepl('BrowserSlashPlatformVersion', colnames(df))] # find ID cols
dfIDvars <- (df[, colnames(df) %in% IDvars])
dfNonIDvars <- (df[, !colnames(df) %in% IDvars])
dfIDvars <- bind_rows(lapply(dfIDvars, as.character))
df <- bind_cols(dfNonIDvars, dfIDvars)

# ID converters & downsample if required:
if(route=='TAS') {
  convertersVec <- unique(filter(df, ActivityID %in% conversionTagIDs_TAS)$user_id)
} else if(route=='PI') {
  convertersVec <- unique(filter(df, ActivityID %in% conversionTagIDs_PI)$user_id)
} else if(route=='DOM') {
  convertersVec <- unique(filter(df, ActivityID %in% conversionTagIDs_DOM)$user_id)
} else if(route=='LH') {
  convertersVec <- unique(filter(df, ActivityID %in% conversionTagIDs_LH)$user_id)
} else if(route=='allRoutes') {
  convertersVec <- unique(filter(df, ActivityID %in% conversionTagIDs_allRoutes)$user_id)
} else {
  warning('Check route name')}  

df$Converter <- 0
df$Converter[df$user_id %in% convertersVec] <- 1

if(samplesize<1){
  df <- bind_rows(filter(df, Converter==1),
                  sample_frac(filter(df, Converter==0), samplesize))}
length(unique(df$user_id[df$Converter==1])); length(unique(df$user_id[df$Converter==0]))

df <- arrange(df, time)

# set timezone
Sys.setenv(TZ='Pacific/Auckland')
df$time <- as.POSIXct(df$time, tz=getOption('tz'))

#===============
# delete rows & cols based on BMK2 below
#===============

# delete a bunch of columns
df <- select(df, -AdvertiserID, -FloodlightConfiguration,  -ActivityID) # not needed now that we have converters sorted
df <- select(df, -StateSlashRegion, -EventType,  -`EventSub-Type`, -DBMCityID, -DBMMatchingTargetedSegments) # delete as a result of initial EDA in BK2 below

# delete rows we don't need - activities (now that we've defined converters), clicks, and non-DBM imps (cos DBM vars only apply to DBM imps)
df <- filter(df, action_type=='I')
df <- filter(df, !is.na(DBMBidPriceAdvertiserCurrency)) # summary() above shows that pretty much all DBM fields have exactly same no. of NAs - and this is one of them. So removing NAs from this field removes NAs from other fields as well. These are presumably the rows from touchpoints that weren't bought via DBM, so have DCm data but nothing for DBM.
warning('Now that converters have been identified, only looking at rows with DBM info - this is impressions bought via DBM\n')

# #__________________________________________________________________________________________________________________________________
# 
# #2. Look at variable types, tabulate categories, how many NAs etc, to see which vars we can remove
# #__________________________________________________________________________________________________________________________________

cat(' Creating URL and Ad Page categories\n')

#__________________________________________________________________________________________________________________________________

#2.b. Data cleaning - for vars with lots of levels, combine levels together into something meaningful. Plus some other cleaning stuff
#__________________________________________________________________________________________________________________________________

# # DBMADXPAGECATEGORIES - EDIT - doing this below, using Az's match tables.
# # ggplot(df, aes(DBMAdxPageCategories)) + stat_count() # only a couple of levels have >~20 users
# # graphics.off()
# numTouchpointsDF <- df %>%
#   group_by(DBMAdxPageCategories) %>%
#   summarise(numTouchpoints=length(time))
# numTouchpointsDF$category <- 'other'
# numTouchpointsDF$category[numTouchpointsDF$numTouchpoints>30] <- 
#   numTouchpointsDF$DBMAdxPageCategories[numTouchpointsDF$numTouchpoints>30]
# df$DBMAdxPageCategories_grouped <- numTouchpointsDF$category[match(df$DBMAdxPageCategories, numTouchpointsDF$DBMAdxPageCategories)]
# # ggplot(df, aes(DBMAdxPageCategories_grouped)) + stat_count() # only a couple of levels have >~20 users
# # graphics.off()

# DBMMOBILEDEVICEID
# ggplot(df, aes(DBMMobileMakeID)) + stat_count() # only a couple of levels have >~20 users
# graphics.off()
numTouchpointsDF <- df %>%
  group_by(DBMMobileMakeID) %>%
  summarise(numTouchpoints=length(time))
numTouchpointsDF$category <- 'other'
numTouchpointsDF$category[numTouchpointsDF$numTouchpoints>30] <- 
  numTouchpointsDF$DBMMobileMakeID[numTouchpointsDF$numTouchpoints>30]
df$DBMMobileMakeID_grouped <- numTouchpointsDF$category[match(df$DBMMobileMakeID, numTouchpointsDF$DBMMobileMakeID)]
# ggplot(df, aes(DBMMobileMakeID_grouped)) + stat_count() # only a couple of levels have >~20 users
# graphics.off()

# DBMMOBILEMODELID
# ggplot(df, aes(DBMMobileModelID)) + stat_count() # only a couple of levels have >~20 users
# graphics.off()
numTouchpointsDF <- df %>%
  group_by(DBMMobileModelID) %>%
  summarise(numTouchpoints=length(time))
numTouchpointsDF$category <- 'other'
numTouchpointsDF$category[numTouchpointsDF$numTouchpoints>30] <- 
  numTouchpointsDF$DBMMobileModelID[numTouchpointsDF$numTouchpoints>30]
df$DBMMobileModelID_grouped <- numTouchpointsDF$category[match(df$DBMMobileModelID, numTouchpointsDF$DBMMobileModelID)]
# ggplot(df, aes(DBMMobileModelID_grouped)) + stat_count() # only a couple of levels have >~20 users
# graphics.off()

# DBMURL - EDIT - classifying URL via dbmadpagecategory now, c.f. actual url

# diff approach to above - look thru values and see if I can bin them into something meaningful. *** NB JUST DID THIS QUICKLY AS PROOF OF CONCEPT - MAY BE BETTER CATEGORISATIONS OUT THERE
df$DBMURLCategory <- with(df, ifelse(grepl('weather', DBMURL) |
                                       grepl('metvuw', DBMURL) |
                                       grepl('met-?service', DBMURL), 'weather',
                                     ifelse(grepl('trademe', DBMURL) |
                                              grepl('e-?bay', DBMURL) |
                                              grepl('gumtree', DBMURL), 'auctionSite',
                                            ifelse(grepl('jamieoliver', DBMURL) |
                                                     grepl('cooking', DBMURL) |
                                                     grepl('roast', DBMURL) |
                                                     grepl('kitchen', DBMURL) |
                                                     grepl('pork', DBMURL) |
                                                     grepl('beef', DBMURL) |
                                                     grepl('chicken', DBMURL) |
                                                     grepl('lamb', DBMURL) |
                                                     grepl('marthastewart', DBMURL) |
                                                     grepl('nigella', DBMURL) |
                                                     grepl('food.com', DBMURL) |
                                                     grepl('recipe', DBMURL), 'cooking',
                                                   ifelse(grepl('[Ff]orbes', DBMURL) |
                                                            grepl('[Ff]inance', DBMURL) |
                                                            grepl('fortune', DBMURL) |
                                                            grepl('nasdaq', DBMURL) |
                                                            grepl('business', DBMURL) |
                                                            grepl('invest', DBMURL), 'finance',
                                                          ifelse(grepl('sport', DBMURL) |
                                                                   grepl('cricket', DBMURL) |
                                                                   grepl('espn', DBMURL) |
                                                                   grepl('soccer', DBMURL) |
                                                                   grepl('rugby', DBMURL) |
                                                                   grepl('football', DBMURL), 'sportsNews',
                                                                 ifelse(grepl('pgatour', DBMURL) |
                                                                          grepl('golf', DBMURL), 'golf',
                                                                        ifelse(grepl('tech', DBMURL) |
                                                                                 grepl('pcmag', DBMURL) |
                                                                                 grepl('digitaltrends', DBMURL), 'technology',
                                                                               ifelse(grepl('car', DBMURL) |
                                                                                        grepl('motor-?sport', DBMURL) |
                                                                                        grepl('topgear', DBMURL) |
                                                                                        grepl('automobile', DBMURL), 'cars',
                                                                                      ifelse(grepl('answers.co', DBMURL), 'answersDotCom',
                                                                                             ifelse(grepl('msn.co', DBMURL) |
                                                                                                      grepl('mirror.co', DBMURL) |
                                                                                                      grepl('guardian.co', DBMURL) |
                                                                                                      grepl('independent.co', DBMURL) |
                                                                                                      grepl('fox', DBMURL) |
                                                                                                      grepl('huffington', DBMURL) |
                                                                                                      grepl('nzherald', DBMURL) |
                                                                                                      grepl('news', DBMURL), 'news_other',
                                                                                                    'other')))))))))))

# table(df$DBMURLCategory, useNA='always')
# unique(df$DBMURL[df$DBMURLCategory=='other'])

# for Jane, calculate proportion of URLs in each category
urlCategoryProportionsDF <- data.frame(category=labels(table(df$DBMURLCategory, useNA='always'))[[1]],
                                       numberOfImpressions=as.numeric(unname(table(df$DBMURLCategory, useNA='always'))))
urlCategoryProportionsDF <- urlCategoryProportionsDF %>%
  mutate(propnOfImpressions=round(numberOfImpressions/sum(numberOfImpressions), 3)) %>%
  arrange(desc(propnOfImpressions))
# write.csv(urlCategoryProportionsDF, file=paste0(basePath, 'AirNZ segmentation results/urlCategoryProportions_', route, '.csv'), row.names=FALSE)

# DBM AD PAGE CATEGORY - replacing URL category

# read in match table
adCategoryMatchTable <- read.csv(paste0(basePath, '/data/DBMAdxPageCategoryMatchTableFromAz.csv')) # Az emailed this to me 091216
adCategoryMatchTable$DBMAdXPageCategoryID <- as.character(adCategoryMatchTable$DBMAdXPageCategoryID) # IDs in main df are character
colnames(adCategoryMatchTable)[colnames(adCategoryMatchTable)=='DBMAdXPageCategoryID'] <- 'DBMAdxPageCategories' # this is name of ID var in df

# pull out first ID from each impression (each touchpoint can have mutliple category IDs)
suppressMessages(library(stringr))
df$DBMAdxPageCategories <- gsub(' ', '_', df$DBMAdxPageCategories)
df$DBMAdxPageCategories <- str_match(df$DBMAdxPageCategories, '([^_]*)')[,2] # pulling everything up to first underscore - i.e. first ID category.
# df$DBMAdxPageCategories
# table(df$DBMAdxPageCategories, useNA='always')

# match to category names
df <- df %>%
  left_join(adCategoryMatchTable, by='DBMAdxPageCategories') %>% # 'DBMAdxpageCategories' is name of ID, 'DBMAdxPageCategory' is name of actual category.
  select(-DBMAdxPageCategories)
df$DBMAdXPageCategory <- as.character(df$DBMAdXPageCategory)
df$DBMAdXPageCategory[is.na(df$DBMAdXPageCategory)] <- 'unknown'

# for Jane, calculate proportion of impressions in each adpage category
adPageCategoryProportionsDF <- data.frame(category=labels(table(df$DBMAdXPageCategory, useNA='always'))[[1]],
                                       numberOfImpressions=as.numeric(unname(table(df$DBMAdXPageCategory, useNA='always'))))
adPageCategoryProportionsDF <- adPageCategoryProportionsDF %>%
  mutate(propnOfImpressions=round(numberOfImpressions/sum(numberOfImpressions), 3)) %>%
  arrange(desc(propnOfImpressions))
# write.csv(adPageCategoryProportionsDF, file=paste0(basePath, 'AirNZ segmentation results/adPageCategoryProportions_', route, '.csv'), row.names=FALSE)

warning('Currently classifying ad page category by main category only (see original excel file emailed from Az 091216 for subcategories). Also currently only calssifying by a single category - e.g. if category is 123_43_82 this is 3 categories, but Im only taking the first one. In future look at including more \n')


# URL DOMAIN & SUBDOMAIN

# Define a function to snip urls down to sub-domain:
library(stringr)
snipUrlFun <- function(url, numSlashes){
  
  # Function uses formula modified from template here: https://regex101.com/r/wN6cZ7/63. Just adds an extra bit at end ('([^:\\/\n]*))\\/?') to search for one extra capturing group that matches one extra forward slash (or matches to end if there's no slash at end ('\\/?'))
  
  if(numSlashes==1){
    return(str_match(url, '^(?:https?:\\/\\/)?(?:[^@\\/\n]+@)?(?:www\\.)?(([^:\\/\n]+))\\/?')[,2])
  } else if(numSlashes==2){
    return(str_match(url, '^(?:https?:\\/\\/)?(?:[^@\\/\n]+@)?(?:www\\.)?(([^:\\/\n]+)\\/?([^:\\/\n]*))\\/?')[,2])
  } else if(numSlashes==3){
    return(str_match(url, '^(?:https?:\\/\\/)?(?:[^@\\/\n]+@)?(?:www\\.)?(([^:\\/\n]+)\\/?([^:\\/\n]*)\\/?([^:\\/\n]*))\\/?')[,2])
  } else if(numSlashes==4){
    return(str_match(url, '^(?:https?:\\/\\/)?(?:[^@\\/\n]+@)?(?:www\\.)?(([^:\\/\n]+)\\/?([^:\\/\n]*)\\/?([^:\\/\n]*)\\/?([^:\\/\n]*))\\/?')[,2])
  } else {stop('snipUrlFun currently only runs if numSlashes parameter is between 1 & 4\n')}
} 

df$URL_DomainAndSubdomain <- snipUrlFun(url=df$DBMURL, numSlashes=2)
# cbind(table(df$URL_DomainAndSubdomain, useNA='always'))

#===============
# Some other data cleaning - give state IDs actual names, and divide bid price by 1billion to give CPM
#===============

# Give state IDs actual names
regionMatchTable <- read.csv(paste0(basePath, '/data/StateSlashRegionMatchTableFromAz.csv')) # Az emailed this to me 071216
regionMatchTable$RegionID <- as.character(regionMatchTable$RegionID) # IDs in main df are character
colnames(regionMatchTable)[colnames(regionMatchTable)=='RegionID'] <- 'DBMStateSlashRegionID' # rename for join

df <- left_join(df, regionMatchTable, by='DBMStateSlashRegionID') # we now have var 'DBMRegion'  thanks to the match table
df <- select(df, -DBMStateSlashRegionID)

# Give device IDs actual names
df$DBMDeviceType <- with(df, ifelse(DBMDeviceType=='0', 'Computer',
                                    ifelse(DBMDeviceType=='1', 'Other',
                                           ifelse(DBMDeviceType=='2', 'Smartphone',
                                                  ifelse(DBMDeviceType=='3', 'Tablet',
                                                         ifelse(DBMDeviceType=='4', 'SmartTV', NA))))))

# divide bid price by 1bill
df$DBMBidPriceAdvertiserCurrency <- df$DBMBidPriceAdvertiserCurrency/1000000000

# #__________________________________________________________________________________________________________________________________
# 
# #3. Graph/model bivariate relationships - Which vars are related to conv prob?   *** NB THIS IS ONE ROW PER IMPRESSION - SEE 3B FOR ONE ROW PER USER ***
# #__________________________________________________________________________________________________________________________________
# 

#__________________________________________________________________________________________________________________________________

# - 3.b Repeat above, one row per ***USER*** c.f. one row per ***IMPRESSION***    *NB see also Bookmark 4.b
#__________________________________________________________________________________________________________________________________

# # Code above is one row per impression (and one point per impression in plot), which tells a slightly different story from one row (point) per user:
# # - one row per impression plot gives the prob that a given impression belongs to a converter.
# # - one row per user plot gives the prob that the underlying user that the impression is served to is a converter.   *** THIS IS THE ONE WE'RE INTERESTED IN ***
# # - The one row per impression plots will give us incorrect answers in two circumstances. First, overall conv prob will be wrong if nonconvs tend to receive less imps per user than convs. Second, differences in conv prob per segment (i.e. to answer question of 'which segments have highest conv prob?') will be wrong if the extent to which convs are served more imps per user than nonconvs differ across segments. Both of these are true, at least for region:
# 
# impsPerConvAndNonConvDF <- df %>%
#   group_by(DBMRegion) %>%
#   summarise(impsPerConv=length(user_id[Converter==1])/length(unique(user_id[Converter==1])),
#             impsPerNonConv=length(user_id[Converter==0])/length(unique(user_id[Converter==0]))) %>%
#   mutate(ratio=impsPerConv/impsPerNonConv) %>%
#   arrange(ratio)
# impsPerConvAndNonConvDF  # *** convs get more imps per user than nonconvs, and the extent of this effect varies across segments ***
# 
# # SOLUTION: model (and plot) one row per user. Either by randomly removing rows so that only one per user is modelled/plotted, or by aggregating across rows per user (taking modal factor level for each user, for instance). Below I redo the plots after randomly removing rows, and after that I make a df that has values aggregated per user

#===============
# randomly select one row per user
#===============

cat(' Randomly selecting one row per user\n')

library(pbapply)
user_i_DFsList <- split(x=df, f=df$user_id)
resultsList <- pblapply(user_i_DFsList, function(x) {sample_n(x, size=1)})
df_oneRandomlySelectedRowPerUser <- rbindlist(resultsList)
rm(resultsList)
rm(user_i_DFsList)

# Original method - slower than split-lapply-combine method tho (but has benefit of progress bar)
# uniqueUsers <- unique(df$user_id)
# resultsList <- list()
# pb <- txtProgressBar(style=3)
# 
# for(i in 1:length(uniqueUsers)){
#   user_i <- uniqueUsers[i]
#   user_i_DF <- filter(df, user_id==user_i & !is.na(user_id))
#   user_i_DF <- sample_n(user_i_DF, size=1)
#   resultsList[[i]] <- user_i_DF
#   setTxtProgressBar(pb, value=i/length(uniqueUsers))
# }
# close(pb)
# 
# df_oneRandomlySelectedRowPerUser <- rbindlist(resultsList)

#===============
# make plots
#===============

meanConvProb_oneRandomlySelectedRowPerUser <- mean(df_oneRandomlySelectedRowPerUser$Converter) # for abline on graph


#__________________________________________________________________________________________________________________________________

#4. Convert DF from one row per impression served to one row per user - need to convert non-mutually exclusive categorical vars to dummy vars
#__________________________________________________________________________________________________________________________________

# EDIT - STARTED OFF DOING THINGS THIS WAY, BUT FUNCTION TOOK FOREVER FOR URL DOMAIN&SUBDOMAIN (COS THERE ARE THOUSANDS OF UNIQUES, EACH OF WHICH BECOMES A BINARY VARIABLE... SO JUST USING ONE RANDOMLY SEKLECTED ROW PER USER INSTEAD)

# # Basic strategy:
# # - for variables that are likely to be mutually exclusive, assign one category per user based on the most common factor level
# # - for non mutually exclusive vars( just url & adpagecategory), need to loop thru and record each category that each user falls into as it's own binary varaible
# # - for bid price (continuous var that will turn up multiple times per user), choose median per user
# 
# #===============
# # for variables that are likely to be mutually exclusive, return most common factor level
# #===============
# 
# # write function that does the above for each user
# mostCommonLevelPerUserFun <- function(myDF, myVar){
#   
#   # # testing - HASH OUT
#   # myDF <- df
#   # myDF$user_id[2:100] <- myDF$user_id[1]; myDF$CountryCode[1:100] <- 'BurkinoFaso' # repeating so that there will be multiple levels per user
#   # myVar <- 'CountryCode'; warning('using test values for function - hash these out when running for real!\n')
#   
#   resultsList <- list()
#   pb <- txtProgressBar(style=3)
#   
#   userIDs <- unique(myDF[,'user_id'])
#   
#   for(i in 1:length(userIDs)){
#     user_i_DF <- filter(myDF, user_id==userIDs[i])
#     myVarTable <- table(user_i_DF[, myVar])
#     if(length(myVarTable)>0){
#       mostCommonLevel <- labels(myVarTable[myVarTable==max(myVarTable)])[[1]][1] # the [[1]][1] is in case there are ties; just take first value
#     } else { mostCommonLevel <- NA } # if only value for a user is NA then have to specify separately, otherwise causes an error in the function.
#     resultsList[[i]] <- data.frame(user_id=userIDs[i], 
#                                    myLevel=mostCommonLevel)
#     setTxtProgressBar(pb, value=i/length(userIDs))
#   }
#   resultsDF <- rbindlist(resultsList)
#   colnames(resultsDF)[2] <- myVar
#   return(resultsDF)
# }
# 
# # testDF <- data.frame(user_id=c('a', 'b', 'c', 'a', 'a', 'b'),
# #                      testVar=c('NZ', 'UK', 'AU', 'NZ', 'UZ', 'US'))
# # mostCommonLevelPerUserFun(testDF, 'testVar')
# 
# commonLevelDF_countryCode <- mostCommonLevelPerUserFun(df, 'CountryCode')
# commonLevelDF_BrowserSlashPlatformVersion <- mostCommonLevelPerUserFun(df, 'BrowserSlashPlatformVersion')
# commonLevelDF_DBMDeviceType <- mostCommonLevelPerUserFun(df, 'DBMDeviceType')
# commonLevelDF_BrowserSlashPlatformID <- mostCommonLevelPerUserFun(df, 'BrowserSlashPlatformID')
# commonLevelDF_DBMBrowserSlashPlatformID <- mostCommonLevelPerUserFun(df, 'DBMBrowserSlashPlatformID')
# commonLevelDF_OperatingSystemID <- mostCommonLevelPerUserFun(df, 'OperatingSystemID')
# commonLevelDF_DBMRegion <- mostCommonLevelPerUserFun(df, 'DBMRegion')
# commonLevelDF_DBMOperatingSystemID <- mostCommonLevelPerUserFun(df, 'DBMOperatingSystemID')
# commonLevelDF_DBMMobileMakeID_grouped <- mostCommonLevelPerUserFun(df, 'DBMMobileMakeID_grouped')
# commonLevelDF_DBMMobileModelID_grouped <- mostCommonLevelPerUserFun(df, 'DBMMobileModelID_grouped')
# 
# #===============
# # for continuous vars, return median value
# #===============
# 
# bidPriceDF <- df %>%
#   filter(!is.na(DBMBidPriceAdvertiserCurrency)) %>%
#   group_by(user_id) %>%
#   summarise(medianBidPrice=median(DBMBidPriceAdvertiserCurrency))
# # head(bidPriceDF)
# 
# #===============
# # For vars that are not mutuallly exclusive, Write function that takes the var and returns a dataframe with one row per user, one col per variable level, and 1 or 0 in cells depending on whether user saw that var
# #===============
# 
# convertFactorToBinaryVarsFun <- function(myDF, myVar){ # my var should be a string
#   
#   # # testing - HASH OUT
#   # myDF <- df
#   # myVar <- 'DBMURLCategory'; warning('using test values for function - hash these out when running for real!\n')
#   
#   userIDs <- unique(myDF$user_id)
#   varLevels <- unique(myDF[, myVar], useNA=F)
#   varLevels <- varLevels[!is.na(varLevels)]
#   
#   resultsDF <- as.data.frame(matrix(ncol=length(varLevels), nrow=length(userIDs))); colnames(resultsDF) <- varLevels
#   pb <- txtProgressBar(style=3)
#   
#   for(i in 1:length(userIDs)){
#     user_i_DF <- filter(myDF, user_id==userIDs[i])
#     varLevelsSeenByUser_i <- unique(user_i_DF[,myVar])
#     resultsDF[i, ] <- as.numeric(varLevels %in% varLevelsSeenByUser_i)
#     setTxtProgressBar(pb, value=i/length(userIDs))
#   }
#   resultsDF$user_id <- userIDs
#   resultsDF
#   return(resultsDF)
# }
# 
# urlCategoryDF <- convertFactorToBinaryVarsFun(df, 'DBMURLCategory')
# colnames(urlCategoryDF) <- paste0('url_', colnames(urlCategoryDF))
# colnames(urlCategoryDF)[grepl('user_id', colnames(urlCategoryDF))] <- 'user_id' # stripping suffix out of user_id var
# # head(urlCategoryDF)
# # colSums(select(urlCategoryDF, -user_id)) # looks good
# adPageCategoryDF <- convertFactorToBinaryVarsFun(df, 'DBMAdXPageCategory')
# colnames(adPageCategoryDF) <- paste0('adCategory_', gsub(' ', '_', colnames(adPageCategoryDF)))
# colnames(adPageCategoryDF)[grepl('user_id', colnames(adPageCategoryDF))] <- 'user_id'
# # head(adPageCategoryDF)
# urlDomainAndSubdomainDF <- convertFactorToBinaryVarsFun(df, 'URL_DomainAndSubdomain')
# colnames(urlDomainAndSubdomainDF) <- paste0('url_', colnames(urlDomainAndSubdomainDF))
# colnames(urlDomainAndSubdomainDF)[grepl('user_id', colnames(urlDomainAndSubdomainDF))] <- 'user_id' # stripping suffix out of user_id var
# # head(urlDomainAndSubdomainDF)
#   
# #===============
# # join all together into a final DF ready for modelling
# #===============
# 
# df_rowsGroupedAndSummarisedByUser <- merge(commonLevelDF_countryCode, commonLevelDF_BrowserSlashPlatformVersion, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMDeviceType, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_BrowserSlashPlatformID,by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMBrowserSlashPlatformID,by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_OperatingSystemID, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMRegion, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMOperatingSystemID, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMMobileMakeID_grouped, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMMobileModelID_grouped, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, bidPriceDF, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, urlCategoryDF, by='user_id', all=TRUE)       # using merge(x, y, all=TRUE) so I can check there aren't any NAs
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, adPageCategoryDF, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, urlDomainAndSubdomainDF, by='user_id', all=TRUE)
# # str(df_rowsGroupedAndSummarisedByUser)
# 
# #===============
# # add in response variable
# #===============
#   
# df_rowsGroupedAndSummarisedByUser$Converter <- df$Converter[match(df_rowsGroupedAndSummarisedByUser$user_id, df$user_id)]
# df_rowsGroupedAndSummarisedByUser <- as.data.frame(df_rowsGroupedAndSummarisedByUser) # can be weird indexing things otherwise - think this comes from it being a data.table
# 
# 
# #__________________________________________________________________________________________________________________________________
# 
# # 4.b. Repeat 3.b (plots with one row per user), but use summarised user info rather than a randomly selected row
# #__________________________________________________________________________________________________________________________________
# 
# # this is a better way to do it - for users with one row only, will give identical results, but for users with multiple rows it will give an expected value per user that is lower variance than the randomly selected row solution (cos based on a larger sample size). Also, because I've already gone to the trouble of creating binary variables for URLs and ad page category, can assign users to multiple categories for these vars.
# 
# #===============
# # before starting, need to turn binary variables back into a form ggplot can plot - stack binary vars on top of each other, turning colnames into factor levels.
# #===============
# 
# ### adCategory
# adCategoryVars <- colnames(df_rowsGroupedAndSummarisedByUser)[grepl('adCategory', colnames(df_rowsGroupedAndSummarisedByUser))]
# resultsList <- list()
# 
# for(i in 1:length(adCategoryVars)){
# adCategory_i <- adCategoryVars[i]
# adCategory_i_DF <- select(df_rowsGroupedAndSummarisedByUser, which(colnames(df_rowsGroupedAndSummarisedByUser)==adCategory_i | colnames(df_rowsGroupedAndSummarisedByUser)=='Converter'))
# adCategory_i_DF <- adCategory_i_DF[adCategory_i_DF[,1]==1,] # only want users who actually saw that ad category ('==1')
# adCategory_i_DF$adCategory <- adCategory_i
# adCategory_i_DF <- select(adCategory_i_DF, Converter, adCategory)
# resultsList[[i]] <- adCategory_i_DF
# }
# plotDF_adCategory <- bind_rows(resultsList)
# plotDF_adCategory$adCategory <- gsub('adCategory_', '', plotDF_adCategory$adCategory)
# 
# ### URLCategory
# urlCategoryVars <- colnames(df_rowsGroupedAndSummarisedByUser)[grepl('url_', colnames(df_rowsGroupedAndSummarisedByUser))]
# resultsList <- list()
# 
# for(i in 1:length(urlCategoryVars)){
# urlCategory_i <- urlCategoryVars[i]
# urlCategory_i_DF <- select(df_rowsGroupedAndSummarisedByUser, which(colnames(df_rowsGroupedAndSummarisedByUser)==urlCategory_i | colnames(df_rowsGroupedAndSummarisedByUser)=='Converter'))
# urlCategory_i_DF <- urlCategory_i_DF[urlCategory_i_DF[,1]==1,] # only want users who actually saw that ad category ('==1')
# urlCategory_i_DF$urlCategory <- urlCategory_i
# urlCategory_i_DF <- select(urlCategory_i_DF, Converter, urlCategory)
# resultsList[[i]] <- urlCategory_i_DF
# }
# plotDF_urlCategory <- bind_rows(resultsList)
# plotDF_urlCategory$urlCategory <- gsub('url_', '', plotDF_urlCategory$urlCategory)
# 
# #===============
# # make plots
# #===============
#   
# meanConvProb_rowsGroupedAndSummarisedByUser <- mean(df_rowsGroupedAndSummarisedByUser$Converter) # for abline on graph

#__________________________________________________________________________________________________________________________________

# 4.c. Calculate conv probabilities for each combination of URL & AdPage category and write to file        *** predict from model in future ***
#__________________________________________________________________________________________________________________________________

cat(' Calculating conversion probabilities for each combination of URL*AdXPage category & writing to file\n')

# AKA find top performing URLs and adPage categories, as requested by Jane

URLByPageCategoryProbs <- df_oneRandomlySelectedRowPerUser %>% # easier to summarise across factor levels rather than calc prob separately for every binary ad caetory predictor (which I would have had to do based on df_rowsGroupedAndSummarisedByUser)
  group_by(DBMAdXPageCategory, URL_DomainAndSubdomain) %>%
  summarise(numConvs=length(user_id[Converter==1]),
            numNonconvs=length(user_id[Converter==0])/samplesize) %>% # correct for downsampling
  mutate(convProb=numConvs/(numConvs+numNonconvs)) %>%
  arrange(desc(convProb))
URLByPageCategoryProbs

#===============
# save
#===============

if(grepl('samplesize1', rImageName)){
  csvName <- paste0(pathSave, 'conversionProbVsAdxPageCategoryByUrlDomain&Subdomain_1randomlySelectedPointPerUser_fullRImage_samplesize', samplesize, '_', route, '.csv')
} else {
  csvName <- paste0(pathSave, 'conversionProbVsAdxPageCategoryByUrlDomain&Subdomain_1randomlySelectedPointPerUser_downsampledRImage_samplesize', samplesize, '_', route, '.csv')
}
write.csv(URLByPageCategoryProbs, file=csvName, row.names=FALSE)
csvName

#__________________________________________________________________________________________________________________________________

#5. Save data for reading into modelling scripts
#__________________________________________________________________________________________________________________________________

cat(' Code run in', (proc.time()[3]-startTime)/60, 'minutes\n')

gc()
