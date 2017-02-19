
# -----------------------------------------------------------------------------------------------------------
# In future will be good to source this script from ReadInDT2LogFiles, so I can pass parameters like R image name, pathdata etc from there. At least for when I'm just interested in creating the model database, rather than exploring the data.
# -----------------------------------------------------------------------------------------------------------

#__________________________________________________________________________________________________________________________________

# Read in previously-saved R image of database, then explore data for segmentation
# - find useful variables for creating segments out of
# - transform into one row per user (create binary predictors for non-mutually exclusive segments, like url seen, and for others calculate median/modal value per user for numeric/factor vars)
# - plot conversion prob per segment
# - save one row per user df so it can be read straight in for modelling.

# NB only looking at data for which there is DBM info, which means (1) only impressions, and (2) only the ~1/3rd of impressions that have been bought via DBM. Still have all converters though (didn't delete activities til I'd found out who converted).
# NB not looking at model windows at all at this stage - it's just whether or not someone converted, and what DBM info we have about them.
#__________________________________________________________________________________________________________________________________

# BOOKMARKS
# 1. set parameters & load data, also ID converters & then downsample if specified in params
# 2. Look at variable types, tabulate categories, how many NAs etc, to see which vars we can remove
# - 2.b. Data cleaning - for vars with lots of levels, combine levels together into something meaningful. Plus some other cleaning stuff
# 3. Graph/model bivariate relationships - Which vars are related to conv prob?   *** NB THIS IS ONE ROW PER IMPRESSION - SEE 3B FOR ONE ROW PER USER ***
# - 3.b Repeat above, one row per ***USER*** c.f. one row per ***IMPRESSION***        *NB see also Bookmark 4.b
# 4. Convert DF from one row per impression served to one row per user - need to convert non-mutually exclusive categorical vars to dummy vars
# - 4.b As per 3b, but use summarised values per user (median/modal values) rather than a randomly-selected value.
# - 4.c. TEMP calculate per segment probabilities (red dots in above graphs) and write to file        *** predict from model in future ***
# 5. Save data for reading into modelling scripts

#__________________________________________________________________________________________________________________________________

#1. set parameters & load data, also ID converters & then downsample if specified in params
#__________________________________________________________________________________________________________________________________

#===============
# set params
#===============

rm(list=ls())
set.seed(1)
dataset <- 'full' # 'full' or 'subset'
basePath <- 'E:/JAY/' # path to day-level folders (i.e. one folder per day)
pathData <- paste0(basePath, 'data/')
firstDate <- '2017-01-01'
maxDate <- '2017-01-22' # THESE DATES NEED TO MATCH THOSE IN ReadInDT2LogFiles.r (just used to recreate name of rda file to read in)
pathSave <- paste0(basePath, 'AirNZ segmentation results/')
nonconvDownsampling <- 0.01 # randomly sample this proportion of nonconvs, keep all convs
route <- 'UK' # allRoutes' # 'TAS', 'LH', 'PI', 'LH', 'allRoutes' # NB could use a SourceAllScripts.R script to loop through all routes - see classifyURLsByDomainName&ConvProbForURLByAdPageCategory.r for an e.g.

# Specificy conversion tags for different routes              
conversionTag_Universal <- 2682414 # this is the universal booking tag. Use this one when we want to define converters based on airport visited, based on custom variable in other data, as described above.
warning('If using only universal conversion tag ("booking tag 2015", used for defining converters based on specific airports), we wont be able to capture ppl who convert via airpoints - this is what isis tags are for.')

conversionTagIDs_TAS <- c(129429, 129424, 1498012, 1497697) 
conversionTagIDs_DOM <- c(129426, 129423, 1498011, 1497695)
conversionTagIDs_LH <- c(196731, 196730, 1498015, 1497701)
conversionTagIDs_PI <- c(129429, 129424, 1498012, 1497697)
conversionTagIDs_allRoutes <- c(2682414, # 2015 booking tag. Rest are ISIS tags (includes mobile convs) 
                                129423, 129424, 129425, 129426, 129429, 129430, 196730, 196731, 224825, 306899, 327181, 327182, 1497695, 1497697, 1497698, 1497701, 1498011, 1498012, 1498013, 1498015, 556003, 556004, 556005, 556006, 556007, 556008, 556009, 556010, 556011, 556012, 556013, 556014) # look at latest 'match_table_activity_cats' match table and find the ActivityIDs that represent conversions (ask Az).

# # match table stuff *haven't implemented yet *
# latestMatchTableFolder <- '20161129' # match table info gets appended to previous day's folder, so just read in match tables from latest match table folder.... 
# matchTable_operatingSystems <- 'match_table_operating_systems'
# matchTable_XXX <- 'match_tableXXX'
if(route=='UK'){
  warning(' - NB Using different method for defining converters for route==UK; rather than identifying converters using route-specific floodlight tags, we are using the universal "2015 booking tag", then pulling destination airport from the customvariable in other_data, then using a match table to identify users whose airport is in the uk. Also, just for this route, we are looking through the world locality subcategories to identify "UK locations" and "Other Western Europe locations" and "otherWorldLocalities", and making each of these their own binary predictors, rather than just using the World Localities main category.\n - Doing this for UK because the upcoming sale is Longhaul UK only. Obviously should do this for other destination-specific sales in future.')}

#===============
# Load and clean data
#===============

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

if(dataset=='full'){
  load(paste0(pathData, 'database_usefulColumnsOnly_fullDF_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.rda'))  
} else {
  load(paste0(pathData, 'database_usefulColumnsOnly_subsetDF_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.rda'))  
}

# read in match tables *haven't implemented yet*
# - read in using path varaibles above, then join to main df

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

#===============
# ID converters & downsample if required:
#===============

# First, routes based on airports: (UK only at the moment)

if(route=='UK'){
  df$airportCode <- NA
  df$airportCode[df$ActivityID %in% conversionTag_Universal] <- # assign airport code to rows where universal conversion tag exists.
    str_match(filter(df, ActivityID %in% conversionTag_Universal)$other_data, 'u6=([^;]*)')[,2] # pulls out everything from u6= up until semicolon.
  airportMatch <- read.csv(paste0(basePath, '/data/MatchTablesFromAzriel/AirportToCountryMatchTable.csv'))
  df$destination <- airportMatch$Country[match(df$airportCode, airportMatch$Code)]
  table(df$destination, df$airportCode)
  nrow(airportMatch)
  convertersVec <- unique(filter(df, destination=='United Kingdom')$user_id)
  df <- select(df, -airportCode, -destination)
}

# Now routes based on route-specific conversion tags:
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

set.seed(1)
if(nonconvDownsampling<1){
  df <- bind_rows(filter(df, Converter==1),
                  sample_frac(filter(df, Converter==0), nonconvDownsampling))}
length(unique(df$user_id[df$Converter==1])); length(unique(df$user_id[df$Converter==0]))

df <- arrange(df, time)

# set timezone
Sys.setenv(TZ='Pacific/Auckland')
df$time <- as.POSIXct(df$time, tz=getOption('tz'))

#===============
# delete rows & cols based on BMK2 below
#===============

# # delete a bunch of columns                                 *** EDIT - HAVE NOW DELETED AS PART OF CREATING DATABASE ***
# df <- select(df, -AdvertiserID, -FloodlightConfiguration,  -ActivityID) # not needed now that we have converters sorted
# df <- select(df, -StateSlashRegion, -EventType,  -`EventSub-Type`, -DBMCityID, -DBMMatchingTargetedSegments) # delete as a result of initial EDA in BK2 below

# delete rows we don't need - activities (now that we've defined converters), clicks, and non-DBM imps (cos DBM vars only apply to DBM imps)
df <- filter(df, action_type=='I')
df <- filter(df, !is.na(DBMBidPriceAdvertiserCurrency)) # summary() above shows that pretty much all DBM fields have exactly same no. of NAs - and this is one of them. So removing NAs from this field removes NAs from other fields as well. These are presumably the rows from touchpoints that weren't bought via DBM, so have DCm data but nothing for DBM.
warning('Now that converters have been identified, only looking at rows with DBM info - this is impressions bought via DBM\n')

# #__________________________________________________________________________________________________________________________________
# 
# #2. Look at variable types, tabulate categories, how many NAs etc, to see which vars we can remove
# #__________________________________________________________________________________________________________________________________
# 
# # Summary: for DBM data, only impressions contain any info, and for each variable only ~1/3 of these are not NA. This is likely to be useful info tho, so will be worth subsetting the data to only look DBM users & impressions (use DCM data to find converters, but only use DBM imps as predictors).
# # For some variables, there is a DBM and a DCM version (e.g. 'DBMOperatingSystemID' and 'OperatingSystemID'). They don't quite match up, so keeping them both in for now. DCM may better, cos match tables are unavailable for DBM currently. But for now will put them both into models to see which has the best infomration gain (if any)
# summary(df)
# 
# # First lot of DBM vars
# table(df$DBMCityID, df$action_type, useNA='always') # too many levels - DELETE (but could include later, perhaps grouping cities together?)
# table(df$DBMOperatingSystemID, df$action_type, useNA='always') # almost all NAs or zeros (assume 0='unknown'??). But keep in for now
# table(df$DBMStateSlashRegionID, df$action_type, useNA='always') # ~1/3 imps have a value, hardly any clicks or acts tho. Could be worth looking at if only interested in impressions?
# table(df$DBMBrowserSlashPlatformID, df$action_type, useNA='always') # as above - ~1/3 imps and no clicks/acts have a value
# table(df$DBMMatchingTargetedSegments, df$action_type, useNA='always') # almost all blanks - DELETE    *** BUT this might be an important one to look at in future - targeted segments might hold important info for targeting.
# table(df$DBMDeviceType, df$action_type, useNA='always') # as above - ~1/3 imps have a value, and only 3 categories, '0', '2', or '3'. Is zero 'unknown'?
# table(df$DBMMobileMakeID, df$action_type, useNA='always') # as above - ~1/3 imps have a value
# table(df$DBMMobileModelID, df$action_type, useNA='always') # could manually categorise these into 'new models' or 'old models' or 'phone age'??
# 
# # non-DBM vars that have a DBM counterpart
# table(df$DBMOperatingSystemID, df$OperatingSystemID, useNA='always') # they dont match up
# table(df$DBMStateSlashRegionID, df$StateSlashRegion, useNA='always') # they don't match up
# table(df$DBMBrowserSlashPlatformID, df$BrowserSlashPlatformID, useNA='always') # they match up somewhat, not hugely tho
# table(df$BrowserSlashPlatformVersion, df$BrowserSlashPlatformID, useNA='always') # they don't match up
# names(df)
# 
# # non-DBM vars
# table(df$CountryCode, df$action_type, useNA='always') # heaps of countries, only 'NZ', 'AU', 'UK', and 'US' have near-decent numbers though. No NAs, hardly any blanks
# table(df$StateSlashRegion, df$action_type, useNA='always') # almost all blanks - DELETE
# table(df$BrowserSlashPlatformID, df$action_type, useNA='always') # all rows populated, by IDs only. Maybe some IDs are uninformative, e.g. 'unknown'? Can always use numbers now to see if there's a signal, and if yes, join in actual categories?
# table(df$BrowserSlashPlatformID, df$DBMBrowserSlashPlatformID, useNA='always')
# table(df$BrowserSlashPlatformVersion, df$action_type, useNA='always') # as above
# table(df$OperatingSystemID, df$action_type, useNA='always') # as above
# table(df$OperatingSystemID, df$DBMOperatingSystemID, useNA='always') # as above
# # table(df$other_data, df$action_type, useNA='always') # table not needed, we know we need this one
# table(df$EventType, df$action_type, useNA='always') # no useful info - exactly the same as action_type variable, except they get different names (CONVERSION, CLICK, and VIEW for A, C & I). - DELETE
# table(df$`EventSub-Type`, df$action_type, useNA='always') # not useful - DELETE
# 
# # For remainder, to many uniques to put in a table - so write function to summarise
# tabulateFun <- function(inputDF, focalVar){
#   clicksVar <- inputDF[inputDF$action_type=='C', focalVar]
#   clicksSummary <- data.frame(numNAs=length(clicksVar[clicksVar=='NA' |  clicksVar=='<NA>' | is.na(clicksVar)]),
#                               numBlanks=length(clicksVar[clicksVar=="" & !is.na(clicksVar)]),
#                               numNormal=length(clicksVar[clicksVar!="" & clicksVar!='NA' & clicksVar!='<NA>' & !is.na(clicksVar)]))
#   impsVar <- inputDF[inputDF$action_type=='I', focalVar]
#   impsSummary <- data.frame(numNAs=length(impsVar[impsVar=='NA' |  impsVar=='<NA>' | is.na(impsVar)]),
#                               numBlanks=length(impsVar[impsVar=="" & !is.na(impsVar)]),
#                               numNormal=length(impsVar[impsVar!="" & impsVar!='NA' & impsVar!='<NA>' & !is.na(impsVar)]))
#   actsVar <- inputDF[inputDF$action_type=='A', focalVar]
#   actsSummary <- data.frame(numNAs=length(actsVar[actsVar=='NA' |  actsVar=='<NA>' | is.na(actsVar)]),
#                               numBlanks=length(actsVar[actsVar=="" & !is.na(actsVar)]),
#                               numNormal=length(actsVar[actsVar!="" & actsVar!='NA' & actsVar!='<NA>' & !is.na(actsVar)]))
#   return(bind_rows('C'=clicksSummary, 'I'=impsSummary, 'A'=actsSummary, .id='action_type'))}
# 
# tabulateFun(df, 'DBMBidPriceAdvertiserCurrency') # ~ ~1/3 of imps have a bid price, so could use these? For acts and clicks it's a tiny fraction. May be enough to assign users to a bid price - just give NAs median value?
# tabulateFun(df, 'DBMURL') # identical to above - ~1/3 imps have a value, hardly any clicks or acts tho
# tabulateFun(df, 'DBMAdxPageCategories') # as above
# tabulateFun(df, 'DBMZIPSlashPostalCode') # pretty much all blank - DELETE
# 
# #===============
# # based on tabulations above, delete rows & cols we don't need
# #===============
# 
# # delete cols we don't need
# df <- select(df, -StateSlashRegion, -EventType,  -`EventSub-Type`, -DBMCityID, -DBMMatchingTargetedSegments)
# 
# # delete rows we don't need - activities (now that we've defined converters), clicks, and non-DBM imps (cos DBM vars only apply to DBM imps)
# df <- filter(df, action_type=='I')
# # summary(df)
# df <- filter(df, !is.na(DBMBidPriceAdvertiserCurrency)) # summary() above shows that pretty much all DBM fields have exactly same no. of NAs - and this is one of them. So removing NAs from this field removes NAs from other fields as well. These are presumably the rows from touchpoints that weren't bought via DBM, so have DCm data but nothing for DBM.
# 
# # Some quick EDA - how many touchpoints per conv & nonconv, on average?
# meanTouchpointsPerUser <- df %>%
#   group_by(user_id, Converter) %>%
#   summarise(numTouchpoints=length(user_id)) %>%
#   group_by(Converter) %>%
#   summarise(meanTouchpointsPerConv=mean(numTouchpoints))
# meanTouchpointsPerUser

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
unique(df$DBMURL)
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
write.csv(urlCategoryProportionsDF, file=paste0(basePath, 'AirNZ segmentation results/urlCategoryProportions_', route, '.csv'), row.names=FALSE)

# DBM AD PAGE CATEGORY - replacing URL category

# read in match table
adCategoryMatchTable <- read.csv(paste0(basePath, '/data/MatchTablesFromAzriel/DBMAdxPageCategories_mainCategories.csv')) # Created based on data in email from Az 10/01/17. Could update to include subcategories if I want to; that would mean *lots* of categories tho.
adCategoryMatchTable$DBMAdXPageCategoryID <- as.character(adCategoryMatchTable$DBMAdXPageCategoryID) # IDs in main df are character
colnames(adCategoryMatchTable)[colnames(adCategoryMatchTable)=='DBMAdXPageCategoryID'] <- 'DBMAdxPageCategories' # this is name of ID var in df
# adCategoryMatchTable

# and subcategory match table, so I can find more specific world localities
adCategoryMatchTable_subcats <- read.csv(paste0(basePath, '/data/MatchTablesFromAzriel/DBMAdxPageCategories_subCategories.csv')) # Created based on data in email from Az 10/01/17. Could update to include subcategories if I want to; that would mean *lots* of categories tho.
adCategoryMatchTable_subcats$ID <- as.character(adCategoryMatchTable_subcats$ID) # IDs in main df are character
colnames(adCategoryMatchTable_subcats)[colnames(adCategoryMatchTable_subcats)=='Category'] <- 'DBMAdxPageSubCategories' # this is name of ID

df$DBMAdxPageCategories <- gsub(' ', '_', df$DBMAdxPageCategories)

#===============
# before proceeding with 'one randomly selected ad page category' version, save dataframe that records every category seen by each user as a separate row (gets used below)
#===============

cat('Saving separate DBM Adx page category DF with one category per row - will convert into binary predictors using function below\n')

#... this dataframe can then be input into the function below to create binary predictors for every category seen 
adPageCategoriesDF_1RowPerCategory <- select(df, user_id, DBMAdxPageCategories)
stringSplitList <- strsplit(adPageCategoriesDF_1RowPerCategory$DBMAdxPageCategories, '_')

blankIndices <- which(lengths(stringSplitList)==0)
if(length(blankIndices)>0){
  for(i in 1:length(blankIndices)){
    stringSplitList[[blankIndices[i]]]  <- 'blankID'  
  }} # occasionally users have no ad page categories, i.e. zero elements in the list.. throws an error unless it's filled in. 

resultsList <- list()
pb <- txtProgressBar(style=3)
for(i in 1:length(stringSplitList)){
  user_i_DF <- data.frame(user_id=adPageCategoriesDF_1RowPerCategory$user_id[i], DBMAdxPageCategories=stringSplitList[[i]])
  user_i_DF$DBMAdxPageCategories <- as.character(user_i_DF$DBMAdxPageCategories)
  resultsList[[i]] <- user_i_DF
  setTxtProgressBar(pb, value=i/length(stringSplitList))
}
cat('\n') #new line after prog bar 

adPageCategoriesDF_1RowPerCategory <- resultsList %>%
  bind_rows() %>%
  arrange(user_id) %>%
  left_join(adCategoryMatchTable, by='DBMAdxPageCategories') # %>% # 'DBMAdxpageCategories' is name of ID, 'DBMAdxPageCategory' is name of actual category.

adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory <- as.character(adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory)
adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory[is.na(adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory)] <- 'unknownSubcategory' # any numbers not in the match table are subcategories - see match table in email from Az 10/01/17 for details of these.

if(route=='UK'){ # replacing generic 'world localities' with UK specific localities 
  UKWorldLocalityIDs <- 
    filter(adCategoryMatchTable_subcats, 
           grepl('[Uu]nited ?[Kk]ingdom', adCategoryMatchTable_subcats$DBMAdxPageSubCategories))$ID
  EuropeWorldLocalityIDs <- 
    filter(adCategoryMatchTable_subcats, 
           grepl('[Ee]urope', adCategoryMatchTable_subcats$DBMAdxPageSubCategories))$ID
  AllWorldLocalityIDs <- 
    filter(adCategoryMatchTable_subcats, 
           grepl('[Ww]orld ?[Ll]ocalities', adCategoryMatchTable_subcats$DBMAdxPageSubCategories))$ID
  adPageCategoriesDF_1RowPerCategory$
    DBMAdXPageCategory[adPageCategoriesDF_1RowPerCategory$DBMAdxPageCategories %in% AllWorldLocalityIDs] <- 'AllWorldLocalities'
  adPageCategoriesDF_1RowPerCategory$
    DBMAdXPageCategory[adPageCategoriesDF_1RowPerCategory$DBMAdxPageCategories %in% EuropeWorldLocalityIDs] <- 'EuropeWorldLocalities'
  adPageCategoriesDF_1RowPerCategory$
    DBMAdXPageCategory[adPageCategoriesDF_1RowPerCategory$DBMAdxPageCategories %in% UKWorldLocalityIDs] <- 'UKWorldLocalities'  # *** NOTE ORDER OF CATEGORIES MATTERS - they aren't mutually exclusive, so World Localities will replace UK localities if it comes after
}
# table(adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory)

adPageCategoriesDF_1RowPerCategory <- select(adPageCategoriesDF_1RowPerCategory, -DBMAdxPageCategories)
rm(resultsList)

#===============
# Now proceed with simplified method that just pulls out the first ID from each impression (each touchpoint can have mutliple category IDs)
#===============

suppressMessages(library(stringr))
df$DBMAdxPageCategories <- str_match(df$DBMAdxPageCategories, '([^_]*)')[,2] # pulling everything up to first underscore - i.e. first ID category.
# df$DBMAdxPageCategories
# table(df$DBMAdxPageCategories, useNA='always')

# match to category names
df <- df %>%
  left_join(adCategoryMatchTable, by='DBMAdxPageCategories') # %>% # 'DBMAdxpageCategories' is name of ID, 'DBMAdxPageCategory' is name of actual category.
df$DBMAdXPageCategory <- as.character(df$DBMAdXPageCategory)
df$DBMAdXPageCategory[is.na(df$DBMAdXPageCategory)] <- 'unknownSubcategory' # any numbers not in the match table are subcategories - see match table in email from Az 10/01/17 for details of these.

if(route=='UK'){ # adding in UK specific world locality subcategories
  df$DBMAdXPageCategory[df$DBMAdxPageCategories %in% AllWorldLocalityIDs] <- 'AllWorldLocalities' 
  df$DBMAdXPageCategory[df$DBMAdxPageCategories %in% EuropeWorldLocalityIDs] <- 'EuropeWorldLocalities'
  df$DBMAdXPageCategory[df$DBMAdxPageCategories %in% UKWorldLocalityIDs] <- 'UKWorldLocalities'    # *** NOTE ORDER OF CATEGORIES MATTERS - they aren't mutually exclusive, so World Localities will replace UK localities if it comes after
}
# table(df$DBMAdXPageCategory)
df <- select(df, -DBMAdxPageCategories)

# # for Jane, calculate proportion of impressions in each adpage category
# adPageCategoryProportionsDF <- data.frame(category=labels(table(df$DBMAdXPageCategory, useNA='always'))[[1]],
#                                        numberOfImpressions=as.numeric(unname(table(df$DBMAdXPageCategory, useNA='always'))))
# adPageCategoryProportionsDF <- adPageCategoryProportionsDF %>%
#   mutate(propnOfImpressions=round(numberOfImpressions/sum(numberOfImpressions), 3)) %>%
#   arrange(desc(propnOfImpressions))
# write.csv(adPageCategoryProportionsDF, file=paste0(basePath, 'AirNZ segmentation results/adPageCategoryProportions_', route, '.csv'), row.names=FALSE)

#===============
# Some other data cleaning - give state IDs actual names, and divide bid price by 1billion to give CPM
#===============

# Give state IDs actual names
regionMatchTable <- read.csv(paste0(basePath, '/data/MatchTablesFromAzriel/StateSlashRegionCategories.csv')) # Az emailed this to me 071216
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
# # NB this code is one row per impression (and one point per impression in plot), which tells a slightly different story from one row (point) per user:
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
# # SOLUTION: model (and plot) one row per user. Either by randomly removing rows so that only one per user is modelled/plotted, or by aggregating across rows per user (taking modal factor level for each user, for instance). 
# 
# meanConvProb <- mean(df$Converter) # for abline on graph
# names(df)
# p1 <- ggplot(df, aes(CountryCode, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p1 # definitely higher conv prob for NZ than US, poss AU in between the two (but hardly any AU DBM data)
# graphics.off()
# p2 <- ggplot(df, aes(BrowserSlashPlatformVersion, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p2 # probable differences here too
# graphics.off()
# p3 <- ggplot(df, aes(DBMBidPriceAdvertiserCurrency, y=Converter)) + geom_jitter(alpha=0.05) + geom_smooth() +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p3 # no relationship? Weird bands in the data though - like fixed bid prices - as well as lots of ads with zero price
# graphics.off()
# 
# p4 <- ggplot(df, aes(DBMZIPSlashPostalCode, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p4 # prob no signal - 99.999% of users are from a single code
# graphics.off()
# p5 <- ggplot(df, aes(DBMAdXPageCategory, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p5 # possibly a signal here - may not be mutually exclusive groups though
# graphics.off()
# p6 <- ggplot(df, aes(DBMDeviceType, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p6  # really strong signal here! But note mobile 'nonconverters' may then go on their desktop to buy their ticket!
# graphics.off()
# p7 <- ggplot(df, aes(BrowserSlashPlatformID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p7  # strong signal here too, but could be related to device type - cos mobile OSs and desktop OSs are different.
# graphics.off()
# p8 <- ggplot(df, aes(OperatingSystemID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p8
# graphics.off() # as above
# 
# p9 <- ggplot(df, aes(DBMRegion, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p9  # no signal really
# graphics.off()
# p10 <- ggplot(df, aes(DBMOperatingSystemID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p10 # same as for other OS plot - some signal, but prob not indpedent of device type
# graphics.off()
# p11 <- ggplot(df, aes(DBMBrowserSlashPlatformID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p11 # as above
# graphics.off()
# p12 <- ggplot(df, aes(DBMMobileMakeID_grouped, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p12 # reasonable signal
# graphics.off()
# p13 <- ggplot(df, aes(DBMMobileModelID_grouped, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p13 # reasonable signal
# graphics.off()
# p14 <- ggplot(df, aes(DBMURLCategory, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red')
# p14 # reasonable signal
# graphics.off()
# 
# # redo bid price graph with bid price conditioned by ad page category
# 
# # first, need to group small categories - cos otherwise smooth won't work ('insufficient unique values to support 10 knots' error)
# plotDF <- filter(df, DBMAdXPageCategory!='PetsAndAnimals' &
#                    DBMAdXPageCategory!='BooksAndLiterature' &  # just did prelim facet plot & these were the ones with few data points & no smooth added
#                    DBMAdXPageCategory!='OnlineCommunities' &
#                    DBMAdXPageCategory!='Reference')
# 
# p15 <- ggplot(plotDF, aes(DBMBidPriceAdvertiserCurrency, y=Converter, colour=DBMAdXPageCategory)) + geom_jitter(alpha=0.01) + geom_smooth() +
#   ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb, colour='dark red') + xlim(0, 0.10)
# p15_facetted <-  p15 + facet_wrap(~DBMAdXPageCategory) + theme(legend.position='none')# no relationship? Weird bands in the data though - like fixed bid prices - as well as lots of ads with zero price
# windows()
# p15_facetted
# graphics.off()
# 
# 
# #===============
# # save plots
# #===============
# 
# # Combined plots
# png(file=paste0(pathSave, 'conversionProbVsTouchpointCategoryPlots_1Of2_1pointPerImp_OBSOLETE_', route, '.png'), height=1000, width=1500)
# grid.arrange(p8, p10, p7, p11, p2, p12, p13, p6, ncol=3) # all device type/mobile make/ OS/ browser plots
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsTouchpointCategoryPlots_2Of2_1pointPerImp_OBSOLETE_', route, '.png'), height=1000, width=1500)
# grid.arrange(p1, p9, p14, p5, p3, ncol=3) # all other plots of interest
# graphics.off()
# 
# # Each var separately - only required for a few plots being used in the pitch
# png(file=paste0(pathSave, 'conversionProbVsURLCategory_1pointPerImp_OBSOLETE_', route, '.png'), height=4000, width=4000, res=400)
# p14
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsAdxPageCategory_1pointPerImp_OBSOLETE_', route, '.png'), height=4000, width=4000, res=400)
# p5
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsDBMBidPrice_1pointPerImp_OBSOLETE_', route, '.png'), height=4000, width=4000, res=400)
# p3
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsDBMRegion_1pointPerImp_OBSOLETE_', route, '.png'), height=4000, width=4000, res=400)
# p9
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsDBMBidPrice&AdxPageCategory_1pointPerImp_OBSOLETE_', route, '.png'), height=4000, width=4000, res=400)
# p15_facetted
# graphics.off()
# 
# #===============
# # save some rows of df, to put into lead scoring table
# #===============
# 
# write.csv(df[1:1000, ], paste0(pathSave, 'df_firstThousandRows_1pointPerImp_', route, '.csv'))


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

cat('Randomly select one row per user and save as separate df \n')

uniqueUsers <- unique(df$user_id)
resultsList <- list()
pb <- txtProgressBar(style=3)

for(i in 1:length(uniqueUsers)){
  user_i <- uniqueUsers[i]
  user_i_DF <- filter(df, user_id==user_i & !is.na(user_id))
  user_i_DF <- sample_n(user_i_DF, size=1)
  resultsList[[i]] <- user_i_DF
  setTxtProgressBar(pb, value=i/length(uniqueUsers))
}
cat('\n')

df_oneRandomlySelectedRowPerUser <- rbindlist(resultsList)
rm(resultsList)

#===============
# make plots
#===============

meanConvProb_oneRandomlySelectedRowPerUser <- mean(df_oneRandomlySelectedRowPerUser$Converter) # for abline on graph
# names(df_oneRandomlySelectedRowPerUser)
# p1 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(CountryCode, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p1 # definitely higher conv prob for NZ than US, poss AU in between the two (but hardly any AU DBM data)
# graphics.off()
# p2 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(BrowserSlashPlatformVersion, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p2 # probable differences here too
# graphics.off()
# p3 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMBidPriceAdvertiserCurrency, y=Converter)) + geom_jitter(alpha=0.05) + geom_smooth() +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p3 # no relationship? Weird bands in the data though - like fixed bid prices - as well as lots of ads with zero price
# graphics.off()
# 
# p4 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMZIPSlashPostalCode, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p4 # prob no signal - 99.999% of users are from a single code
# graphics.off()
p5 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMAdXPageCategory, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
p5 # possibly a signal here - may not be mutually exclusive groups though
graphics.off()
# p6 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMDeviceType, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p6  # really strong signal here! But note mobile 'nonconverters' may then go on their desktop to buy their ticket!
# graphics.off()
# p7 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(BrowserSlashPlatformID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p7  # strong signal here too, but could be related to device type - cos mobile OSs and desktop OSs are different.
# graphics.off()
# p8 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(OperatingSystemID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p8
# graphics.off() # as above
# 
p9 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMRegion, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
p9  # no signal really
graphics.off()
# p10 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMOperatingSystemID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p10 # same as for other OS plot - some signal, but prob not indpedent of device type
# graphics.off()
# p11 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMBrowserSlashPlatformID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p11 # as above
# graphics.off()
# p12 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMMobileMakeID_grouped, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p12 # reasonable signal
# graphics.off()
# p13 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMMobileModelID_grouped, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p13 # reasonable signal
# graphics.off()
# p14 <- ggplot(df_oneRandomlySelectedRowPerUser, aes(DBMURLCategory, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red')
# p14 # reasonable signal
# graphics.off()
# 
# # redo bid price graph with bid price conditioned by ad page category
# 
# # first, need to group small categories - cos otherwise smooth won't work ('insufficient unique values to support 10 knots' error)
# plotDF <- filter(df_oneRandomlySelectedRowPerUser, DBMAdXPageCategory!='PetsAndAnimals' &
#                    DBMAdXPageCategory!='BooksAndLiterature' &  # just did prelim facet plot & these were the ones with few data points & no smooth added
#                    DBMAdXPageCategory!='OnlineCommunities' &
#                    DBMAdXPageCategory!='Reference')
# 
# p15 <- ggplot(plotDF, aes(DBMBidPriceAdvertiserCurrency, y=Converter, colour=DBMAdXPageCategory)) + geom_jitter(alpha=0.01) + geom_smooth() +
#   ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red') + xlim(0, 0.10)
# p15_facetted <-  p15 + facet_wrap(~DBMAdXPageCategory) + theme(legend.position='none')# no relationship? Weird bands in the data though - like fixed bid prices - as well as lots of ads with zero price
# windows()
# p15_facetted
# graphics.off()
# 
# #===============
# # save plots
# #===============
# 
# # Combined plots
# png(file=paste0(pathSave, 'conversionProbVsTouchpointCategoryPlots_1Of2_1randomlySelectedPointPerUser_', route, '.png'), height=1000, width=1500)
# grid.arrange(p8, p10, p7, p11, p2, p12, p13, p6, ncol=3) # all device type/mobile make/ OS/ browser plots
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsTouchpointCategoryPlots_2Of2_1randomlySelectedPointPerUser_', route, '.png'), height=1000, width=1500)
# grid.arrange(p1, p9, p14, p5, p3, ncol=3) # all other plots of interest
# graphics.off()
# 
# # Each var separately - only required for a few plots being used in the pitch
# png(file=paste0(pathSave, 'conversionProbVsURLCategory_1randomlySelectedPointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p14
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsAdxPageCategory_1randomlySelectedPointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p5
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsDBMBidPrice_1randomlySelectedPointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p3
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsDBMRegion_1randomlySelectedPointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p9
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsDBMBidPrice&AdxPageCategory_1randomlySelectedPointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p15_facetted
# graphics.off()
# 
# #===============
# # save some rows of df_oneRandomlySelectedRowPerUser, to put into lead scoring table
# #===============
# 
# write.csv(df_oneRandomlySelectedRowPerUser[1:1000, ], paste0(pathSave, 'df_firstThousandRows_oneRandomlySelectedRowPerUser_', route, '.csv'))
# 
# 
#__________________________________________________________________________________________________________________________________

#4. Convert DF from one row per impression served to one row per user - need to convert non-mutually exclusive categorical vars to dummy vars
#__________________________________________________________________________________________________________________________________

# Basic strategy:
# - for variables that are likely to be mutually exclusive, assign one category per user based on the most common factor level
# - for non mutually exclusive vars( just url & adpagecategory), need to loop thru and record each category that each user falls into as it's own binary varaible
# - for bid price (continuous var that will turn up multiple times per user), choose median per user

cat('Summarise variables to create one row per user - NB can potentially speed up code by reducing the number of vars we look at here\n')

#===============
# for variables that are likely to be mutually exclusive, return most common factor level
#===============

cat('...Return most common factor level for vars that are likely to have mutually exlusive levels (e.g. region)\n')

# write function that does the above for each user

mostCommonLevelPerUserFun <- function(myDF, myVar){
  
  # # testing - HASH OUT
  # myDF <- df
  # myDF$user_id[2:100] <- myDF$user_id[1]; myDF$CountryCode[1:100] <- 'BurkinoFaso' # repeating so that there will be multiple levels per user
  # myVar <- 'CountryCode'; warning('using test values for function - hash these out when running for real!\n')
  
  resultsList <- list()
  pb <- txtProgressBar(style=3)
  
  userIDs <- unique(myDF[,'user_id'])
  
  for(i in 1:length(userIDs)){
    user_i_DF <- filter(myDF, user_id==userIDs[i])
    myVarTable <- table(user_i_DF[, myVar])
    if(length(myVarTable)>0){
      mostCommonLevel <- labels(myVarTable[myVarTable==max(myVarTable)])[[1]][1] # the [[1]][1] is in case there are ties; just take first value
    } else { mostCommonLevel <- NA } # if only value for a user is NA then have to specify separately, otherwise causes an error in the function.
    resultsList[[i]] <- data.frame(user_id=userIDs[i], 
                                   myLevel=mostCommonLevel)
    setTxtProgressBar(pb, value=i/length(userIDs))
  }
  cat('\n') # new line after prog bar
  resultsDF <- rbindlist(resultsList)
  colnames(resultsDF)[2] <- myVar
  return(resultsDF)
}

# testDF <- data.frame(user_id=c('a', 'b', 'c', 'a', 'a', 'b'),
#                      testVar=c('NZ', 'UK', 'AU', 'NZ', 'UZ', 'US'))
# mostCommonLevelPerUserFun(testDF, 'testVar')
# commonLevelDF_countryCode <- mostCommonLevelPerUserFun(df, 'CountryCode')
# commonLevelDF_BrowserSlashPlatformVersion <- mostCommonLevelPerUserFun(df, 'BrowserSlashPlatformVersion')
# commonLevelDF_DBMDeviceType <- mostCommonLevelPerUserFun(df, 'DBMDeviceType')
# commonLevelDF_BrowserSlashPlatformID <- mostCommonLevelPerUserFun(df, 'BrowserSlashPlatformID')
# commonLevelDF_DBMBrowserSlashPlatformID <- mostCommonLevelPerUserFun(df, 'DBMBrowserSlashPlatformID')
# commonLevelDF_OperatingSystemID <- mostCommonLevelPerUserFun(df, 'OperatingSystemID')
commonLevelDF_DBMRegion <- mostCommonLevelPerUserFun(df, 'DBMRegion')
# commonLevelDF_DBMOperatingSystemID <- mostCommonLevelPerUserFun(df, 'DBMOperatingSystemID')
# commonLevelDF_DBMMobileMakeID_grouped <- mostCommonLevelPerUserFun(df, 'DBMMobileMakeID_grouped')
# commonLevelDF_DBMMobileModelID_grouped <- mostCommonLevelPerUserFun(df, 'DBMMobileModelID_grouped')

#===============
# for continuous vars, return median value
#===============

cat('...Return median value for continuous vars\n')

bidPriceDF <- df %>%
  filter(!is.na(DBMBidPriceAdvertiserCurrency)) %>%
  group_by(user_id) %>%
  summarise(medianBidPrice=median(DBMBidPriceAdvertiserCurrency))
# head(bidPriceDF)

#===============
# For vars that are not mutually exclusive, Write function that takes the var and returns a dataframe with one row per user, one col per variable level, and 1 or 0 in cells depending on whether user saw that var
#===============

cat('...Create binary predictors for factors with levels that arent mutually exclusive (e.g. Ad Page Categories)\n')

convertFactorToBinaryVarsFun <- function(myDF, myVar){ # my var should be a string
  
  # # testing - HASH OUT
  # myDF <- df
  # myVar <- 'DBMURLCategory'; warning('using test values for function - hash these out when running for real!\n')
  
  userIDs <- unique(myDF$user_id)
  varLevels <- unique(myDF[, myVar], useNA=F)
  varLevels <- varLevels[!is.na(varLevels)]
  
  resultsDF <- as.data.frame(matrix(ncol=length(varLevels), nrow=length(userIDs))); colnames(resultsDF) <- varLevels
  pb <- txtProgressBar(style=3)
  
  for(i in 1:length(userIDs)){
    user_i_DF <- filter(myDF, user_id==userIDs[i])
    varLevelsSeenByUser_i <- unique(user_i_DF[,myVar])
    resultsDF[i, ] <- as.numeric(varLevels %in% varLevelsSeenByUser_i)
    setTxtProgressBar(pb, value=i/length(userIDs))
  }
  cat('\n') # new line after prog bar
  resultsDF$user_id <- userIDs
  resultsDF
  return(resultsDF)
}

# urlCategoryDF <- convertFactorToBinaryVarsFun(df, 'DBMURLCategory')
# colnames(urlCategoryDF) <- paste0('url_', colnames(urlCategoryDF))
# colnames(urlCategoryDF)[grepl('user_id', colnames(urlCategoryDF))] <- 'user_id' # stripping suffix out of user_id var

# head(urlCategoryDF)
# colSums(select(urlCategoryDF, -user_id)) # looks good
adPageCategoryDF <- convertFactorToBinaryVarsFun(adPageCategoriesDF_1RowPerCategory, 'DBMAdXPageCategory')

if(length(colnames(adPageCategoryDF)[grepl('unknownSubcategory', colnames(adPageCategoryDF))])>0){
  adPageCategoryDF <- select(adPageCategoryDF, -unknownSubcategory) # uninformative feature
}

colnames(adPageCategoryDF) <- paste0('adCategory_', gsub(' ', '_', colnames(adPageCategoryDF)))
colnames(adPageCategoryDF)[grepl('user_id', colnames(adPageCategoryDF))] <- 'user_id'
# head(adPageCategoryDF)

#===============
# join all together into a final DF ready for modelling
#===============

df_rowsGroupedAndSummarisedByUser <- merge(commonLevelDF_DBMRegion, adPageCategoryDF, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMDeviceType, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_BrowserSlashPlatformID,by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMBrowserSlashPlatformID,by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_OperatingSystemID, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_countryCode, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMOperatingSystemID, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMMobileMakeID_grouped, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_DBMMobileModelID_grouped, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, bidPriceDF, by='user_id', all=TRUE)
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, urlCategoryDF, by='user_id', all=TRUE)       # using merge(x, y, all=TRUE) so I can check there aren't any NAs
# df_rowsGroupedAndSummarisedByUser <- merge(df_rowsGroupedAndSummarisedByUser, commonLevelDF_BrowserSlashPlatformVersion, by='user_id', all=TRUE)
# str(df_rowsGroupedAndSummarisedByUser)

#===============
# add in response variable
#===============

df_rowsGroupedAndSummarisedByUser$Converter <- df$Converter[match(df_rowsGroupedAndSummarisedByUser$user_id, df$user_id)]
df_rowsGroupedAndSummarisedByUser <- as.data.frame(df_rowsGroupedAndSummarisedByUser) # can be weird indexing things otherwise - think this comes from it being a data.table


#__________________________________________________________________________________________________________________________________

# 4.b. Repeat 3.b (plots with one row per user), but use summarised user info rather than a randomly selected row
#__________________________________________________________________________________________________________________________________

# this is a better way to do it - for users with one row only, will give identical results, but for users with multiple rows it will give an expected value per user that is lower variance than the randomly selected row solution (cos based on a larger sample size). Also, because I've already gone to the trouble of creating binary variables for URLs and ad page category, can assign users to multiple categories for these vars.

#===============
# before starting, need to turn binary variables back into a form ggplot can plot - stack binary vars on top of each other, turning colnames into factor levels.
#===============

### adCategory
adCategoryVars <- colnames(df_rowsGroupedAndSummarisedByUser)[grepl('adCategory', colnames(df_rowsGroupedAndSummarisedByUser))]
resultsList <- list()

for(i in 1:length(adCategoryVars)){
  adCategory_i <- adCategoryVars[i]
  adCategory_i_DF <- select(df_rowsGroupedAndSummarisedByUser, which(colnames(df_rowsGroupedAndSummarisedByUser)==adCategory_i | colnames(df_rowsGroupedAndSummarisedByUser)=='Converter'))
  adCategory_i_DF <- adCategory_i_DF[adCategory_i_DF[,1]==1,] # only want users who actually saw that ad category ('==1')
  adCategory_i_DF$adCategory <- adCategory_i
  adCategory_i_DF <- select(adCategory_i_DF, Converter, adCategory)
  resultsList[[i]] <- adCategory_i_DF
}
plotDF_adCategory <- bind_rows(resultsList)
plotDF_adCategory$adCategory <- gsub('adCategory_', '', plotDF_adCategory$adCategory)

# ### URLCategory
# urlCategoryVars <- colnames(df_rowsGroupedAndSummarisedByUser)[grepl('url_', colnames(df_rowsGroupedAndSummarisedByUser))]
# resultsList <- list()
# 
# for(i in 1:length(urlCategoryVars)){
#   urlCategory_i <- urlCategoryVars[i]
#   urlCategory_i_DF <- select(df_rowsGroupedAndSummarisedByUser, which(colnames(df_rowsGroupedAndSummarisedByUser)==urlCategory_i | colnames(df_rowsGroupedAndSummarisedByUser)=='Converter'))
#   urlCategory_i_DF <- urlCategory_i_DF[urlCategory_i_DF[,1]==1,] # only want users who actually saw that ad category ('==1')
#   urlCategory_i_DF$urlCategory <- urlCategory_i
#   urlCategory_i_DF <- select(urlCategory_i_DF, Converter, urlCategory)
#   resultsList[[i]] <- urlCategory_i_DF
# }
# plotDF_urlCategory <- bind_rows(resultsList)
# plotDF_urlCategory$urlCategory <- gsub('url_', '', plotDF_urlCategory$urlCategory)

#===============
# make plots
#===============

meanConvProb_rowsGroupedAndSummarisedByUser <- mean(df_rowsGroupedAndSummarisedByUser$Converter) # for abline on graph

# p1 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(CountryCode, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p1 # definitely higher conv prob for NZ than US, poss AU in between the two (but hardly any AU DBM data)
# graphics.off()
# p2 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(BrowserSlashPlatformVersion, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p2 # probable differences here too
# graphics.off()
# p3 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(medianBidPrice, y=Converter)) + geom_jitter(alpha=0.05) + geom_smooth() +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p3 # no relationship? Weird bands in the data though - like fixed bid prices - as well as lots of ads with zero price
# graphics.off()
# # p4 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(DBMZIPSlashPostalCode, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
# #  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# # p4 # prob no signal - 99.999% of users are from a single code
# # graphics.off()
p5 <- ggplot(plotDF_adCategory, aes(adCategory, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red') +
  ggtitle('Warning: categories arent mutually exclusive. Users may appear \nacross multiple segments, so meanProb \nline may not reflect an average-performing segment')
windows()
p5
graphics.off()
# p6 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(DBMDeviceType, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p6  # really strong signal here! But note mobile 'nonconverters' may then go on their desktop to buy their ticket!
# graphics.off()
# p7 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(BrowserSlashPlatformID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p7  # strong signal here too, but could be related to device type - cos mobile OSs and desktop OSs are different.
# graphics.off()
# p8 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(OperatingSystemID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p8
# graphics.off() # as above
# 
p9 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(DBMRegion, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
windows()
p9  
graphics.off()
# p10 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(DBMOperatingSystemID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p10 # same as for other OS plot - some signal, but prob not indpedent of device type
# graphics.off()
# p11 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(DBMBrowserSlashPlatformID, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p11 # as above
# graphics.off()
# p12 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(DBMMobileMakeID_grouped, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p12 # reasonable signal
# graphics.off()
# p13 <- ggplot(df_rowsGroupedAndSummarisedByUser, aes(DBMMobileModelID_grouped, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p13 # reasonable signal
# graphics.off()
# p14 <- ggplot(plotDF_urlCategory, aes(urlCategory, y=Converter)) + geom_jitter(alpha=0.05) + stat_summary(fun.y=mean, geom='point', size=4, colour='dark red') +
#  ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_rowsGroupedAndSummarisedByUser, colour='dark red')
# p14 # reasonable signal
# graphics.off()
# 
# ### redo bid price graph with bid price conditioned by ad page category
# # EDIT - not easy to do based on summarised data, so just doing based on one randomly-selected row per user.
# plotDF <- filter(df_oneRandomlySelectedRowPerUser, DBMAdXPageCategory!='PetsAndAnimals' &
#                    DBMAdXPageCategory!='BooksAndLiterature' &  # just did prelim facet plot & these were the ones with few data points & no smooth added
#                    DBMAdXPageCategory!='OnlineCommunities' &
#                    DBMAdXPageCategory!='Reference')
# 
# p15 <- ggplot(plotDF, aes(DBMBidPriceAdvertiserCurrency, y=Converter, colour=DBMAdXPageCategory)) + geom_jitter(alpha=0.01) + geom_smooth() +
#   ylab('Conversion probability') + theme(axis.text.x = element_text(angle = 90), axis.text.y=element_blank(), axis.ticks.y=element_blank()) + geom_abline(slope=0, intercept=meanConvProb_oneRandomlySelectedRowPerUser, colour='dark red') + xlim(0, 0.10)
# p15_facetted <-  p15 + facet_wrap(~DBMAdXPageCategory) + theme(legend.position='none')# no relationship? Weird bands in the data though - like fixed bid prices - as well as lots of ads with zero price
# windows()
# p15_facetted
# graphics.off()
# 

# #===============
# # save plots
# #===============
# 
# # Combined plots
# png(file=paste0(pathSave, 'conversionProbVsTouchpointCategoryPlots_1Of2_1summarisedPointPerUser_', route, '.png'), height=1000, width=1500)
# grid.arrange(p8, p10, p7, p11, p2, p12, p13, p6, ncol=3) # all device type/mobile make/ OS/ browser plots
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsTouchpointCategoryPlots_2Of2_1summarisedPointPerUser_', route, '.png'), height=1000, width=1500)
# grid.arrange(p1, p9, p14, p5, p3, ncol=3) # all other plots of interest
# graphics.off()
# 
# # Each var separately - only required for a few plots being used in the pitch
# png(file=paste0(pathSave, 'conversionProbVsURLCategory_1summarisedPointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p14
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsAdxPageCategory_1summarisedPointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p5
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsDBMBidPrice_1summarisedPointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p3
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsDBMRegion_1summarisedPointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p9
# graphics.off()
# 
# png(file=paste0(pathSave, 'conversionProbVsDBMBidPrice&AdxPageCategory_1pointPerUser_', route, '.png'), height=4000, width=4000, res=400)
# p15_facetted
# graphics.off()
# 
# 
# #__________________________________________________________________________________________________________________________________
# 
# # 4.c. TEMP calculate per segment probabilities (red dots in above graphs) and write to file        *** predict from model in future ***
# #__________________________________________________________________________________________________________________________________
# 
# # Only doing for region and Adx page category for now
# 
# #===============
# # Ad page category
# #===============
# 
# pageCategoryProbs <- df_oneRandomlySelectedRowPerUser %>% # easier to summarise across factor levels rather than calc prob separately for every binary ad caetory predictor (which I would have had to do based on df_rowsGroupedAndSummarisedByUser)
#   group_by(DBMAdXPageCategory) %>%
#   summarise(numConvs=length(user_id[Converter==1]),
#             numNonconvs=length(user_id[Converter==0])/samplesize) %>% # correct for downsampling
#   mutate(convProb=numConvs/(numConvs+numNonconvs)) %>%
#   arrange(desc(convProb))
# pageCategoryProbs
# 
# #===============
# # Region
# #===============
# 
# regionProbs <- df_rowsGroupedAndSummarisedByUser %>% # easier to summarise across factor levels rather than calc prob separately for every binary ad caetory predictor (which I would have had to do based on df_rowsGroupedAndSummarisedByUser)
#   group_by(DBMRegion) %>%
#   summarise(numConvs=length(user_id[Converter==1]),
#             numNonconvs=length(user_id[Converter==0])/nonconvDownsampling) %>% # correct for downsampling
#   mutate(convProb=numConvs/(numConvs+numNonconvs)) %>%
#   arrange(desc(convProb))
# regionProbs
# 
# #===============
# # save
# #===============
# 
# write.csv(pageCategoryProbs, file=paste0(pathSave, 'conversionProbVsAdxPageCategory_1randomlySelectedPointPerUser_', route, '.csv'), row.names=FALSE)
# write.csv(regionProbs, file=paste0(pathSave, 'conversionProbVsDBMRegion_1pointPerUser_', route, '.csv'), row.names=FALSE)


#__________________________________________________________________________________________________________________________________

#5. Save data for reading into modelling scripts
#__________________________________________________________________________________________________________________________________

#===============
# save model database as .rda file to read in for future modelling
#===============

save(df_rowsGroupedAndSummarisedByUser, file=paste0(pathData, 'df_rowsGroupedAndSummarisedByUser_nonconvDownsampling', nonconvDownsampling, '_', dataset, 'DF_', route, '_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.rda'))

gc()
