#__________________________________________________________________________________________________________________________________

# - Read in previously-saved R image of database, then explore data for segmentation 
# - Identify converters and also last click conversions (ie which ads get attributed a conversion bast on last click rules), and downsample negative instances
# - Split out adX page categories from a single variable like this '101_3_14' to binary predictors, one per category ('101 y/n', '3 y/n' etc)
# - Create & save different versions of database; either 
#   (1) transform into one row per user for when we want to predict which *users* will convert. Do this either by randomly selecting one row per user, or by creating summarised variables per user (e.g. modal factor values), as specified in the parameters; or 
#   (2) keep as one row per ad seen, for when we want to predict which *ads* will get attributed a last click conversion 
#__________________________________________________________________________________________________________________________________

#_______________

# Bookmarks
#_______________

# 0. set parameters & load data, also ID converters & then downsample if specified in params, 
# 1. Read in Placements match table, add in info about ad type (RMK vs BROAD; and route-specific ads), and for lastClick database filter out ads we don't want 
# 2. Downsample negative instances (nonConverters or nonLastClickConv ads)
# 2 OBSOLETE. Look at variable types, tabulate categories, how many NAs etc, to see which vars we can remove
# - 2.b. Data cleaning - for vars with lots of levels, combine levels together into something meaningful. Plus some other cleaning stuff
# 3. Graph/model bivariate relationships - Which vars are related to conv prob?   *** ONE ROW PER IMPRESSION CF ONE ROW PER USER ***
# 4. Repeat above, but one row per ***USER*** c.f. one row per ***IMPRESSION***. *** Each row is randomly selected per user ***
# 5. Repeat above - one row per ***USER*** c.f. one row per ***IMPRESSION***. *** Each row is summarised data per user ***
# - 5.a. First need to summarise data per variable - modal value for mutually exclusive categorical vars, median value for continuous vars, and convert non-mutually exclusive categorical vars to dummy vars
# - 5.b. Create plots
# 6. Repeat above - one row per ***IMPRESSION***, and response variable is whether that imp was attributed a last click conversion. Creating database but not doing plots at this stage
# 7. For one row per ad database, add time of day as predictor variable
# 8. For one row per ad database, read in Placements match table and add in info about ad type (RMK vs BROAD; and route-specific ads)
# 9. Save dataframe for subsequent modelling

#__________________________________________________________________________________________________________________________________

#0. set parameters & load data, also ID converters/last click conversions
#__________________________________________________________________________________________________________________________________

#===============
# set params
#===============

rm(list=ls())
Sys.setenv(TZ="Pacific/Auckland")
set.seed(1)
dataset <- 'subset' # 'full' or 'subset'
basePath <- 'E:/JAY/' 
pathData <- paste0(basePath, 'data/') # path to saved R images
firstDate <- '2017-01-09'
maxDate <- '2017-01-31' # THESE DATES NEED TO MATCH THOSE IN ReadInDT2LogFiles.r (just used to recreate name of rda file to read in)
pathSave <- paste0(basePath, 'AirNZ segmentation results/')
route <- 'DOM' # allRoutes' # 'TAS', 'DOM', 'PI', 'LH', 'allRoutes' # NB could use a SourceAllScripts.R script to loop through all routes - see classifyURLsByDomainName&ConvProbForURLByAdPageCategory.r for an e.g.
outputDatabase <- 'df_oneRowPerAdAndLastClickConversionResponse' # 'df_rowsGroupedAndSummarisedByUser', 'df_oneRandomlySelectedRowPerUser', 'df_oneRowPerAdAndLastClickConversionResponse'. How does the final database deal with the fact that there are multiple rows per user? Also is the response whether or not the user converts at some point in data period, or whether or not the ad leads to a last click conversion being attributed?
lastClickWindow <- 10  # no. of days to look back for attributing conversions via last click, if using userDefined method.
lastImpWindow <- 10 # as above, but for impressions. NOTE (1) imps will only be attributed a conversion if there aren't any clicks within the click window; (2) if ignoring impressions altogether then set lastImpWindow to 0.
negativeInstanceDownsampling <- 0.01 # randomly downsample this proportion of nonconvs / non-lastclick conversions

# Define which types of ads we want to look at     ***CURRENTLY ONLY IMPLEMENTED FOR df_oneRowPerAdAndLastClickConversionResponse ***
offerAds <- 'focalRoute' # 'focalRoute', 'allRoutes' # e.g. if routeAds==focalRoute, then for route==TAS we would only include ads that have a 'Tasman sale' message in the model
layerAds <- 'notRemarketing' # 'notRemarketing', 'Remarketing', or 'all'. If 'notRemarketing' then exclude RMK ads from model.
groupAds <- 'retail' # 'retail', 'brand', or 'all'. Az reckons we would only ever use the model to buy retail ads (i.e. ads relating to specific sales), but including other options for completeness. 

cat(' WARNING - havent tested code for outputDatasbase==df_rowsGroupedAndSummarisedByUser or df_oneRandomlySelectedRowPerUser. (Code worked when I just saved both databases by default, but now have nested relevant code within "if(outputDataBase==XXX)")\n')

# Specificy conversion tags for different routes              
conversionTagIDs_TAS <- c(129429, 129424, 1498012, 1497697) 
conversionTagIDs_DOM <- c(129426, 129423, 1498011, 1497695)
conversionTagIDs_LH <- c(196731, 196730, 1498015, 1497701)
conversionTagIDs_PI <- c(129430, 129425, 1498013, 1497698)
conversionTagIDs_allRoutes <- c(2682414, # 2015 booking tag. Rest are ISIS tags (includes mobile convs) 
                                129423, 129424, 129425, 129426, 129429, 129430, 196730, 196731, 224825, 306899, 327181, 327182, 1497695, 1497697, 1497698, 1497701, 1498011, 1498012, 1498013, 1498015, 556003, 556004, 556005, 556006, 556007, 556008, 556009, 556010, 556011, 556012, 556013, 556014) # look at latest 'match_table_activity_cats' match table and find the ActivityIDs that represent conversions (ask Az).

# Specify conversion tags & destination names when defining conversions based on airport visited, c.f. route-specific floodlight tags
# - this allows us to look at segments for sale-specific destinations, e.g. UK or NAM SAM, rather than the more general 'LH' route. 
conversionTag_Universal <- 2682414 # this tag tells us who converted; then we can look in the custom variable to pull out destination airport
destinationName_UK <- 'United Kingdom' # *** has to exactly match name of destination in airport match table***. Only done for UK so far.

warning('If using only universal conversion tag ("booking tag 2015", used for defining converters based on specific airports), we wont be able to capture ppl who convert via airpoints - this is what isis tags are for.')

# # match table stuff *haven't implemented yet *
# latestMatchTableFolder <- '20161129' # match table info gets appended to previous day's folder, so just read in match tables from latest match table folder.... 
# matchTable_operatingSystems <- 'match_table_operating_systems'
# matchTable_XXX <- 'match_tableXXX'

if(route=='UK'){
  warning(' - NB Using different method for defining converters for route==UK; rather than identifying converters using route-specific floodlight tags, we are using the universal "2015 booking tag", then pulling destination airport from the customvariable in other_data, then using a match table to identify users whose airport is in the uk. Also, just for this route, we are looking through the world locality subcategories to identify "UK locations" and "Other Western Europe locations" and "otherWorldLocalities", and making each of these their own binary predictors, rather than just using the World Localities main category.\n - Doing this for UK because the upcoming sale is Longhaul UK only. Obviously should do this for other destination-specific sales in future.')}

#===============
# Load and clean data
#===============

cat(paste0('\nCREATING MODEL DATABASE: \n dataset=', dataset, ' // route=', route, ' // output database=', outputDatabase, ' // C/I lookback windows=', lastClickWindow, '/', lastImpWindow, ' // ad filtering=', offerAds, '/', layerAds, '/', groupAds, ' // date range=', firstDate, '-', maxDate,'\n'))

cat(' Loading data\n')

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

#===============
# remove users who's first ad exposure is less than lastClickWindow days from maxDate
#===============

# For last click reattribution we should ideally be reattributing for full pathways. E.g. usrs who enter database 1day before maxDate may convert after ~2+ days, but will get recoreded as nonconverters because we haven't been able to view the conversion in the data. Best approach is to remove any users who we can't track through a full potential pathway, where a full pathway is date of first ad interaction + no. of days in lookback window.

numUsers_dfWithAllUsers <- length(unique(df$UserID)) # for reporting how many users we lose
firstAdTimePerUserDF <- df %>%
  filter(action_type!='A') %>%   # this will automatically remove any users who don't see
  group_by(UserID) %>%
  summarise(firstAdTime=min(EventTime))

usersWithFullPathways <- unique(filter(firstAdTimePerUserDF, firstAdTime<=(as.POSIXct(maxDate)-lastClickWindow*24*3600))$UserID)
df <- filter(df, UserID %in% usersWithFullPathways)

# # checking it worked
# someRemovedUsers <- unique(df$UserID[!df$UserID %in% usersWithFullPathways])[100000:100005]
# as.data.frame(df %>% filter(UserID %in% someRemovedUsers) %>% select(UserID, EventTime, action_type) %>% arrange(UserID, EventTime)) 

cat(paste0('  Removed users who dont have a full pathway (first ad interaction time is < lastClickWindow days before maxDate). \n   Num users before removal: ', numUsers_dfWithAllUsers, '; \n    num users after removal: ', length(usersWithFullPathways), '\n'))

#===============
# Modify some vars
#===============
  
df <- df %>% 
  rename(time=EventTime, user_id=UserID, other_data=OtherData) %>%
  filter(user_id!=0)

# ID variables are numeric, but should be character (otherwise models will think they're continuous) - change
IDvars <- colnames(df)[grepl('ID', colnames(df)) | grepl('DBMDeviceType', colnames(df)) | grepl('BrowserSlashPlatformVersion', colnames(df))] # find ID cols
dfIDvars <- (df[, colnames(df) %in% IDvars])
dfNonIDvars <- (df[, !colnames(df) %in% IDvars])
dfIDvars <- bind_rows(lapply(dfIDvars, as.character))   # this code takes ages, not sure why. Perhaps speed up in future
df <- bind_cols(dfNonIDvars, dfIDvars)

#===============
# ID conversions and converters
#===============

cat(' Identifying converters\n')

# First, based on airport codes in custom variables (for more specific routes than just 'LH', 'TAS' etc. Only implemented for UK at the mo)
airportMatch <- read.csv(paste0(basePath, '/data/MatchTablesFromAzriel/AirportToCountryMatchTable.csv'))
df$Conversion <- 0 # fill in 1s below

if(route=='UK' | route=='addInOtherRouteNamesHere'){
  destinationName <- destinationName_UK
  df$airportCode <- NA
  df$airportCode[df$ActivityID %in% conversionTag_Universal] <- # assign airport code to rows where universal conversion tag exists.
    str_match(filter(df, ActivityID %in% conversionTag_Universal)$other_data, 'u6=([^;]*)')[,2] # pulls out everything from u6= up until semicolon.
  df$destination <- airportMatch$Country[match(df$airportCode, airportMatch$Code)]
  df$destination <- as.character(df$destination) # needed to add new level below
  df$destination[is.na(df$destination)] <- 'noAirportMatch'
  # table(df$destination, df$airportCode)
  df$Conversion[df$destination==destinationName] <- 1
  df <- select(df, -airportCode, -destination)
}

# Now ID conversions for standard routes (TAS, LH etc), based on route-specific conversion tags:
if(route=='TAS') {
  conversionTagIDs <- conversionTagIDs_TAS
} else if(route=='PI') {
  conversionTagIDs <- conversionTagIDs_PI
} else if(route=='DOM') {
  conversionTagIDs <- conversionTagIDs_DOM
} else if(route=='LH') {
  conversionTagIDs <- conversionTagIDs_LH
} else if(route=='allRoutes') {
  conversionTagIDs <- conversionTagIDs_allRoutes}

if(route=='TAS' | route=='PI' | route=='DOM' | route=='LH' | route=='allRoutes'){
  df$Conversion[df$ActivityID %in% conversionTagIDs] <- 1   
}

# Define converters
convertersVec <- unique(filter(df, Conversion==1)$user_id)
df$Converter <- 0
df$Converter[df$user_id %in% convertersVec] <- 1
min(df$time)

#===============
# For converters, identify which ads resulted in 'last click conversions'; i.e. which ads would get attributed the conversion based on last click attribution rules
#===============

convertersDF <- filter(df, Converter==1)
convertersDF$rowIndices <- 1:nrow(convertersDF) # needed to ID ads that get a last click attribution  

convertersDF$conversionIndices <- 0
convertersDF$conversionIndices[convertersDF$Conversion==1] <- 1:length(convertersDF$conversionIndices[convertersDF$Conversion==1]) # for conversion touchpoints, assign a unique index. Needed for loop below, to ensure we assign each conversion to an ad, rather than just one ad per user

cat('\n Last click attribution\n')

# # FOR TESTING, modify converters DF so that different lastClick windows give different no. of click/impression conversions:
# lastClickWindow <- 10
# lastImpWindow <- 10
# convertersDF <- convertersDF[1:9,]; cat('WARNING - running last click attribution on test data; need to hash out when running for real, and make sure lastClickWindows  & convertersDF are set back to values specified in parameters\n')
# convertersDF <- select(convertersDF, user_id, time, action_type, Conversion, Converter, rowIndices, conversionIndices)
# convertersDF$user_id <- 'JohnNamesly'
# convertersDF$user_id[1:3] <- 'SteveNamesly'
# convertersDF$action_type <- c('C', 'C', 'A', 'C', 'I', 'A', 'A', 'A', 'C')
# convertersDF$Conversion <- c(0, 0, 1, 0, 0, 0, 1, 1, 0)
# convertersDF$conversionIndices[convertersDF$Conversion==1] <- 1:length(convertersDF$conversionIndices[convertersDF$Conversion==1]) 
# convertersDF$time <- as.POSIXct(c('2016-12-21 15:00:00', '2016-12-22 15:00:00', '2017-01-01 15:00:00', 
#                                   '2016-12-24 15:00:00', '2017-01-01 15:00:00', 
#                                   '2017-01-02 15:00:00', '2017-01-03 15:00:00', 
#                                   '2017-01-04 15:00:00', '2017-01-05 15:00:00')) # ==> for Steven 10d from last click to conversion. For John, 10 d from last click & 2d from last imp to conversion #1; and 11d from last click & 3d from last imp to conversion #2
# convertersDF

lastClickRowIndicesVec <- vector() # store results - row index of ad that caused conversion
pb <- txtProgressBar(style=3)

for(i in 1:max(convertersDF$conversionIndices)){
  # i <- 5
  conversion_i_user <- filter(convertersDF, conversionIndices==i)$user_id
  conversion_i_userDF <- filter(convertersDF, user_id==conversion_i_user)
  convTime <- filter(conversion_i_userDF, conversionIndices==i)$time # for users with two conversions, this ensures we get an attribution for both
  clicksBeforeConversionDF <- filter(conversion_i_userDF, time<convTime & time>=(convTime-lastClickWindow*3600*24) & action_type=='C')
  impsBeforeConversionDF <- filter(conversion_i_userDF, time<convTime & time>=(convTime-lastImpWindow*3600*24) & action_type=='I')
  
  if(nrow(clicksBeforeConversionDF)>0){ # use clicks if there are any that meet last-click criteria, otherwise try imps
    lastClickRowIndicesVec[i] <- filter(clicksBeforeConversionDF, time==max(time))$rowIndices[1] # [1] is in case two ads have exactly same time
    lastClickRowIndicesVec
  } else if(nrow(impsBeforeConversionDF)>0){
    lastClickRowIndicesVec[i] <- filter(impsBeforeConversionDF, time==max(time))$rowIndices[1]
  } else { lastClickRowIndicesVec[i] <- NA } # if no click or imps that meet criteria, the conversion just won't get attributed to any ads.
  setTxtProgressBar(pb, value=i/max(convertersDF$conversionIndices))
}
close(pb)
convertersDF$lastClickConversion <- 0
convertersDF$lastClickConversion[convertersDF$rowIndices %in% lastClickRowIndicesVec] <- 1

# convertersDF %>% select(lastClickConversion, Conversion, user_id, action_type, time) %>% arrange(user_id, time) # check it worked
convertersDF <- select(convertersDF, -rowIndices, -conversionIndices)

nonconvertersDF <- df %>%
  filter(Converter==0) %>%
  mutate(lastClickConversion=0) 

df <- bind_rows(convertersDF, nonconvertersDF) # there will be some ads that aren't last click conversions in converters, so need to bind back together before downsampling

#===============
# filter data to only include DBM-related data, and to remove rows that are both a click and an imp for the same ad
#===============

cat(' Removing non-DBM touchpoints, & for click touchpoints removing the associated impression to avoid double-counting\n')

df <- filter(df, !is.na(DBMBidPriceAdvertiserCurrency)) # now DBM only

# whenever an ad has both a click and an imp, remove the imp:
clickAuctionIDsVec <- unique(filter(df, action_type=='C')$DBMAuctionID) # auction IDs link imps and clicks for a given ad served
clickAuctionsDF <- filter(df, DBMAuctionID %in% clickAuctionIDsVec)
noclickAuctionsDF <- filter(df, !DBMAuctionID %in% clickAuctionIDsVec)

# # TEMP - Before filtering, check that there is typically 1 impression associated with each click
# numTouchpointsPerClickAuctionID <- clickAuctionsDF %>%
#   group_by(DBMAuctionID, action_type) %>%
#   summarise(numTouchpoints=length(DBMAuctionID))
# head(as.data.frame(numTouchpointsPerClickAuctionID), 50) # should be one click and one imp per auction ID
# table(numTouchpointsPerClickAuctionID$action_type, numTouchpointsPerClickAuctionID$numTouchpoints) # most touchpoints should have one click and one imp

impsRemovedList <- list()
pb <- txtProgressBar(style=3)
for(i in 1:length(clickAuctionIDsVec)){
  clickAuction_i_DF <- filter(clickAuctionsDF, DBMAuctionID==clickAuctionIDsVec[i])
  revenue_i <- max(clickAuction_i_DF$DBMRevenueAdvertiserCurrency) # whereas all other DBM fields get repeated across all rows within each auctionID, this is only nonzero for one of them. So taking max gives the actual (nonzero) revenue.
  if(nrow(filter(clickAuction_i_DF, lastClickConversion==1))){
    clickAuction_i_DF <- filter(clickAuction_i_DF, lastClickConversion==1) # if any of the clicks/imps for a given ad served got a last click conversion, then choose this as the single touchpoint for that ad. (This should always be a click c.f. impression cos clicks always get the credit if there are both, but occassionally there are multiple clicks per auction ID and in this case we want the one that got the conversion).
    } else {
      clickAuction_i_DF <- filter(clickAuction_i_DF, action_type=='C')[1,] # if no last click conversions for a given ID just use the first (and prob only) click
    }
  clickAuction_i_DF$DBMRevenueAdvertiserCurrency <- revenue_i
  impsRemovedList[[i]] <- clickAuction_i_DF
setTxtProgressBar(pb, value=i/length(clickAuctionIDsVec))
  }
cat('\n')
clickAuctionsDF <- bind_rows(impsRemovedList)

df <- bind_rows(clickAuctionsDF, noclickAuctionsDF)

#__________________________________________________________________________________________________________________________________

#1. Read in Placements match table, add in info about ad type (RMK vs BROAD; and route-specific ads), and for lastClick database filter out ads we don't want 
#__________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________

# 1.a. Define function to read in match tables files within a given folder, then use this function in a loop to read in all files within date range 
#__________________________________________________________________________________________________________________________________

cat(' Adding placement attributes to database\n')

#===============
# define function
#===============

# 'folderPath' is a path to a folder containing only csv files, all of which have the same schema. Each folder actually may only contain only one file, in which case this function isn't really needed... can't hurt though.
rbindAllFilesInAFolderFun <- function(folderPath){
  
  # # for testing *** HASH OUT FOR REAL RUN ***
  # folderPath <- paste0(basePath, 'data/match_table_campaigns/20161128/'); testingFunction <- 'Y'
  # folderPath
  # if(exists('testingFunction')){cat(' *** WARNING: Using testFolder for testing loop - need to hash out when running real code ***\n')}
  
  filesList <- list()
  for(i in 1:length(list.files(folderPath))){
    file_i <- paste0(folderPath, list.files(folderPath))[i]
    filesList[[i]] <- 
      fread(file_i, showProgress=TRUE)
  }
  finalDF <- bind_rows(filesList, .id='fileNumber') # .id used to report on how many files were read in.. in case only 1
  finalDF$fileNumber <- as.numeric(finalDF$fileNumber)
  
  # rename cols to keep ID names consistent with renaming done in main df
  colnames(finalDF) <- gsub(" ", "", colnames(finalDF))
  colnames(finalDF) <- gsub("/", "Slash", colnames(finalDF))
  colnames(finalDF) <- gsub("\\(", "", colnames(finalDF))
  colnames(finalDF) <- gsub(")", "", colnames(finalDF))
  colnames(finalDF) <- gsub("\\.", "", colnames(finalDF))
  colnames(finalDF) <- gsub("\\.\\.", "", colnames(finalDF))
  
  # cat(max(finalDF$fileNumber), 'files read in for path ', folderPath, '\n')
  return(finalDF) 
}

#===============
# read in campaigns match table data
#===============

campaignsPath <- paste0(basePath, 'data/match_table_campaigns/')

# SUBSET FOLDERS BY FIRSTDATE AND MAXDATE
allCampaignsFoldersAsDates <- as.Date(list.files(campaignsPath), format='%Y%m%d')
campaignsFolders_subsettedByDate <- allCampaignsFoldersAsDates[allCampaignsFoldersAsDates>=firstDate &
                                                                 allCampaignsFoldersAsDates<=maxDate]
campaignsFolders_subsettedByDate <- gsub("-", "", as.character(campaignsFolders_subsettedByDate)) # returning to orignal format
campaignsFoldersVec <- paste0(campaignsPath, campaignsFolders_subsettedByDate, '/') 

campaignsList <- list()

for(i in 1:length(campaignsFoldersVec)){
  campaignsFolder_i <- campaignsFoldersVec[i]
  campaignsList[[i]] <- rbindAllFilesInAFolderFun(campaignsFolder_i)
}
campaignsDF <- rbindlist(campaignsList)
cat(paste('  ', length(campaignsFolders_subsettedByDate), 'files read in for match_tables_campaigns\n'))

#===============
# read in placements match table data
#===============

placementsPath <- paste0(basePath, 'data/match_table_placements/')

# SUBSET FOLDERS BY FIRSTDATE AND MAXDATE
allplacementsFoldersAsDates <- as.Date(list.files(placementsPath), format='%Y%m%d')
placementsFolders_subsettedByDate <- allplacementsFoldersAsDates[allplacementsFoldersAsDates>=firstDate &
                                                                   allplacementsFoldersAsDates<=maxDate]
placementsFolders_subsettedByDate <- gsub("-", "", as.character(placementsFolders_subsettedByDate)) # returning to orignal format
placementsFoldersVec <- paste0(placementsPath, placementsFolders_subsettedByDate, '/') 

placementsList <- list()

for(i in 1:length(placementsFoldersVec)){
  placementsFolder_i <- placementsFoldersVec[i]
  placementsList[[i]] <- rbindAllFilesInAFolderFun(placementsFolder_i)
}
placementsDF <- rbindlist(placementsList)
rm(placementsList)
cat(paste('  ', length(placementsFolders_subsettedByDate), 'files read in for match_tables_placements\n'))

#===============
# read in sites match table data
#===============

sitesPath <- paste0(basePath, 'data/match_table_sites/')

# SUBSET FOLDERS BY FIRSTDATE AND MAXDATE
allsitesFoldersAsDates <- as.Date(list.files(sitesPath), format='%Y%m%d')
sitesFolders_subsettedByDate <- allsitesFoldersAsDates[allsitesFoldersAsDates>=firstDate &
                                                         allsitesFoldersAsDates<=maxDate]
sitesFolders_subsettedByDate <- gsub("-", "", as.character(sitesFolders_subsettedByDate)) # returning to orignal format
sitesFoldersVec <- paste0(sitesPath, sitesFolders_subsettedByDate, '/') 

sitesList <- list()

for(i in 1:length(sitesFoldersVec)){
  sitesFolder_i <- sitesFoldersVec[i]
  sitesList[[i]] <- rbindAllFilesInAFolderFun(sitesFolder_i)
}
sitesDF <- rbindlist(sitesList)
rm(sitesList)
cat(paste('  ', length(sitesFolders_subsettedByDate), 'files read in for match_tables_sites\n'))

#===============
# read in ads match table data
#===============

adsPath <- paste0(basePath, 'data/match_table_ads/')

# SUBSET FOLDERS BY FIRSTDATE AND MAXDATE
alladsFoldersAsDates <- as.Date(list.files(adsPath), format='%Y%m%d')
adsFolders_subsettedByDate <- alladsFoldersAsDates[alladsFoldersAsDates>=firstDate &
                                                     alladsFoldersAsDates<=maxDate]
adsFolders_subsettedByDate <- gsub("-", "", as.character(adsFolders_subsettedByDate)) # returning to orignal format
adsFoldersVec <- paste0(adsPath, adsFolders_subsettedByDate, '/') 

adsList <- list()

for(i in 1:length(adsFoldersVec)){
  adsFolder_i <- adsFoldersVec[i]
  adsList[[i]] <- rbindAllFilesInAFolderFun(adsFolder_i)
}
adsDF <- rbindlist(adsList)
rm(adsList)
cat(paste('  ', length(adsFolders_subsettedByDate), 'files read in for match_tables_ads\n'))
#__________________________________________________________________________________________________________________________________

# 1.b. join relevant fields to main df
#__________________________________________________________________________________________________________________________________

df$Campaign <- campaignsDF$Campaign[match(df$CampaignID, campaignsDF$CampaignID)]
df$Placement <- placementsDF$Placement[match(df$PlacementID, placementsDF$PlacementID)]
df$Ad <- adsDF$Ad[match(df$AdID, adsDF$AdID)]
df$Site <- sitesDF$SiteDCM[match(df$SiteIDDCM, sitesDF$SiteIDDCM)]

# # Check it worked
# length(df$Campaign[is.na(df$Campaign)])
# length(df$Placement[is.na(df$Placement)])
# length(df$Ad[is.na(df$Ad)])
# length(df$Site[is.na(df$Site)])
head(unique(df$Campaign),100)
head(unique(df$Placement),100)
head(unique(df$Ad),100)
head(unique(df$Site),100)
head(select(df, Campaign, Placement, Ad, Site), 15)
tail(select(df, Campaign, Placement, Ad, Site), 15)
unique(df$Campaign)

#__________________________________________________________________________________________________________________________________

# 1.c. use code copied from FillInMatchTables.r to assign ads to relevant type - RMK vs other, Retail vs Brand, and route
#__________________________________________________________________________________________________________________________________

# could also ad in things like mob vs display vs vid, but not worrying for now. Az says that our models will be used to buy non-brand, non-remarketing ads (and will be for specific routes), so these are the main vars we need for this.

#===============
# Group variable
#===============

# Its BRAND if there's no date, or if theres a date and Awareness or Alliance. Otherwise RETAIL.
df$GROUP <- with(df, 
                 ifelse(grepl('Jan', Campaign) |
                          grepl('Feb', Campaign) |
                          (grepl('Mar', Campaign) & !grepl('Mark', Campaign)) | # 'market' comes up sometimes in campaign
                          grepl('Apr', Campaign) |
                          grepl('May', Campaign) |
                          grepl('Jun', Campaign) |
                          grepl('Jul', Campaign) |
                          grepl('Aug', Campaign) |
                          grepl('Oct', Campaign) |
                          grepl('Nov', Campaign) |
                          grepl('Dec', Campaign) |
                          grepl('Sep', Campaign), 'RETAIL', 'BRAND')) # All sales have a date associated with them, & are retail. No date = Brand.
df$GROUP <- with(df,
                 ifelse(grepl('Awareness', Campaign) |
                          grepl('Alliance', Campaign), 'BRAND', GROUP)) # some brand Campaigns also mention a date... but these always alsos seem to say awareness or alliance.
table(df$GROUP, useNA='always')
df$Campaign[df$GROUP=='BRAND']

#===============
# Offer variable
#===============

cat(' WARNING - "route" based on df$Campaign is only classified into LH vs DOM vs PAC vs TAS - in future could break down further into e.g. UK for UK sales, NAM SAM for NAMSAM sales etc\n')

df$OFFER <- 
  with(df, ifelse(GROUP=='BRAND', 'BRAND', # dont care about BRAND offers
                  ifelse(grepl('[Ll]ong? ?[Hh]aul', Campaign) |
                           grepl('LH', Campaign) |
                           grepl('[Hh]ouston', Campaign) |
                           grepl('HOUSTON', Campaign) |
                           grepl('[Vv]ietnam', Campaign) |
                           grepl('[Aa]sia', Campaign) |
                           grepl('Singapore', Campaign) |
                           grepl('UK', Campaign) |          # add in other longhaul / Tasman / Domestic / Pacific names as necessary
                           grepl('merica', Campaign) |
                           grepl('[Ee]urope', Campaign) |
                           grepl('hina', Campaign) |
                           grepl('Buenos Aires', Campaign) |
                           grepl('LA', Campaign) |
                           grepl('NAM', Campaign) & grepl('SAM', Campaign), 'LH',
                         ifelse(grepl('asman', Campaign) |
                                  grepl('New South Wales', Campaign) |
                                  grepl('Aus', Campaign) |
                                  grepl('Queensland', Campaign) |
                                  grepl('Brisbane', Campaign) |
                                  grepl('Sydney', Campaign) |
                                  grepl('Western Australia', Campaign) |
                                  grepl('Perth', Campaign) |         
                                  grepl('Melbourne', Campaign) |
                                  grepl('Victoria', Campaign) |
                                  grepl('VA', Campaign) |
                                  grepl('Adelaide', Campaign) |
                                  grepl('Sunshine Coast', Campaign) |
                                  grepl('Gold Coast', Campaign), 'TAS',
                                ifelse(grepl('omestic', Campaign) |
                                         grepl('[Qq]ueenstown', Campaign) |
                                         grepl('NIGHT RIDER', Campaign) |
                                         grepl('Night rider', Campaign), 'DOM',
                                       ifelse(grepl('PI', Campaign) |
                                                grepl('acific', Campaign) |
                                                grepl('Fiji', Campaign) |
                                                grepl('Samoa', Campaign) |
                                                grepl('Tahiti', Campaign) |
                                                grepl('onolulu', Campaign), 'PAC',
                                              ifelse(grepl('artnership', Campaign), 'PARTNERSHIP', # partnership is Always On activity
                                                     'Unknown')))))))
# table(df$OFFER, useNA='always')

#===============
# Layer variable
#===============

df$LAYER <- with(df, ifelse(grepl('[Rr]emarketing', Campaign) |
                              grepl('[Rr]emarketing', Placement) |
                              grepl('[Rr]emarketing', Ad), 'Remarketing', 'notRemarketing')) 
# table(df$LAYER, useNA='always')

#__________________________________________________________________________________________________________________________________

# 1.d. Based on above values for Ad attributes, either include or exclude individual ads from analysis
#__________________________________________________________________________________________________________________________________

if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){
  
  cat(' Filtering database by specified ad types (e.g. exclude RMK ads). Only doing for outputDatabase==df_oneRowPerAdAndLastClickConversionResponse for now... need to think about whether I should do for the other output databases too\n')
  
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
}

#__________________________________________________________________________________________________________________________________

#2. Downsample negative instances (nonConverters or nonLastClickConv ads)
#__________________________________________________________________________________________________________________________________

set.seed(1)

if(outputDatabase=='df_rowsGroupedAndSummarisedByUser' | outputDatabase=='df_oneRandomlySelectedRowPerUser'){
  df <- bind_rows(filter(df, Converter==1),
                  sample_frac(filter(df, Converter==0), negativeInstanceDownsampling))
  cat(' Numconverters in downsampled database: ', nrow(filter(df, Converter==1)), '\n Numnonconverters in downsampled database: ', nrow(filter(df, Converter==0)), 
      '\n NB some of these users may get removed when filtering down to users seeing DBM ads\n')
  
} else if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){
  df <- bind_rows(filter(df, lastClickConversion==1),
                  sample_frac(filter(df, lastClickConversion==0), negativeInstanceDownsampling))
  cat(' Number of DBM-only last click conversion ads in final database: ', nrow(filter(df, lastClickConversion==1 & !is.na(DBMBidPriceAdvertiserCurrency))), '\n Number of DBM-only non last click conversion ads in final database: ', nrow(filter(df, lastClickConversion==0  & !is.na(DBMBidPriceAdvertiserCurrency))), '\n')
  numUnknownRows <- nrow(filter(df, OFFER=='Unknown'))
if(numUnknownRows>0){
  cat(paste0(' WARNING - ', numUnknownRows, ' out of ', nrow(df), ' rows of Retail ads have an unknown route. Corresponding Campaigns:\n'))
  cat(unique(df$Campaign[df$OFFER=='Unknown']))
} # putting here instead of with FillInMatchTables code cos I'm interested in the number in the final database, after downsampling.

  
} else { 
  cat('WARNING: check name of outputDatabase\n')
}

df <- arrange(df, time)

df$time <- as.POSIXct(df$time, tz=getOption('tz'))

#__________________________________________________________________________________________________________________________________

#2 OBSOLETE. Look at variable types, tabulate categories, how many NAs etc, to see which vars we can remove  *DELETED - see old version of this script if required again
#__________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________

#2.b. Data cleaning - for vars with lots of levels, combine levels together into something meaningful. Plus some other cleaning stuff
#__________________________________________________________________________________________________________________________________

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
UKWorldLocalityIDs <- 
  filter(adCategoryMatchTable_subcats, 
         grepl('[Uu]nited ?[Kk]ingdom', adCategoryMatchTable_subcats$DBMAdxPageSubCategories))$ID
EuropeWorldLocalityIDs <- 
  filter(adCategoryMatchTable_subcats, 
         grepl('[Ee]urope', adCategoryMatchTable_subcats$DBMAdxPageSubCategories))$ID
AllWorldLocalityIDs <- 
  filter(adCategoryMatchTable_subcats, 
         grepl('[Ww]orld ?[Ll]ocalities', adCategoryMatchTable_subcats$DBMAdxPageSubCategories))$ID

#===============
# save dataframe that records every category seen by each user/ad as a separate row, instead of multiple categories per row (e.g. '12_101_6')
#===============

# NB HAVEN'T IMPLEMENTED THIS FOR df_oneRandomlySelectedRowPerUser YET
  
if(outputDatabase=='df_rowsGroupedAndSummarisedByUser'){
  
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
    adPageCategoriesDF_1RowPerCategory$
      DBMAdXPageCategory[adPageCategoriesDF_1RowPerCategory$DBMAdxPageCategories %in% AllWorldLocalityIDs] <- 'AllWorldLocalities'
    adPageCategoriesDF_1RowPerCategory$
      DBMAdXPageCategory[adPageCategoriesDF_1RowPerCategory$DBMAdxPageCategories %in% EuropeWorldLocalityIDs] <- 'EuropeWorldLocalities'
    adPageCategoriesDF_1RowPerCategory$
      DBMAdXPageCategory[adPageCategoriesDF_1RowPerCategory$DBMAdxPageCategories %in% UKWorldLocalityIDs] <- 'UKWorldLocalities'  # *** NOTE ORDER OF CATEGORIES MATTERS - they aren't mutually exclusive, so World Localities will replace UK localities if it comes after
  }
  # table(adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory)
  # adPageCategoriesDF_1RowPerCategory <- select(adPageCategoriesDF_1RowPerCategory, -DBMAdxPageCategories)
  rm(resultsList)
}

if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){
  
  cat(' Saving separate DBM Adx page category DF with one category per row - will convert into binary predictors using function below\n')
  
  df$rowIndex <- 1:nrow(df)
  
  #... this dataframe can then be input into the function below to create binary predictors for every category seen 
  adPageCategoriesDF_1RowPerCategory <- select(df, rowIndex, DBMAdxPageCategories)
  stringSplitList <- strsplit(adPageCategoriesDF_1RowPerCategory$DBMAdxPageCategories, '_')
  
  blankIndices <- which(lengths(stringSplitList)==0)
  if(length(blankIndices)>0){
    for(i in 1:length(blankIndices)){
      stringSplitList[[blankIndices[i]]]  <- 'blankID'  
    }} # occasionally users have no ad page categories, i.e. zero elements in the list.. throws an error unless it's filled in. 
  
  resultsList <- list()
  pb <- txtProgressBar(style=3)
  for(i in 1:length(stringSplitList)){
    rowIndex_DF <- data.frame(rowIndex=adPageCategoriesDF_1RowPerCategory$rowIndex[i], DBMAdxPageCategories=stringSplitList[[i]])
    rowIndex_DF$DBMAdxPageCategories <- as.character(rowIndex_DF$DBMAdxPageCategories)
    resultsList[[i]] <- rowIndex_DF
    setTxtProgressBar(pb, value=i/length(stringSplitList))
  }
  cat('\n') #new line after prog bar 
  
  adPageCategoriesDF_1RowPerCategory <- resultsList %>%
    bind_rows() %>%
    arrange(rowIndex) %>%
    left_join(adCategoryMatchTable, by='DBMAdxPageCategories') # %>% # 'DBMAdxpageCategories' is name of ID, 'DBMAdxPageCategory' is name of actual category.
  
  adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory <- as.character(adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory)
  adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory[is.na(adPageCategoriesDF_1RowPerCategory$DBMAdXPageCategory)] <- 'unknownSubcategory' # any numbers not in the match table are subcategories - see match table in email from Az 10/01/17 for details of these.
  
  if(route=='UK'){ # replacing generic 'world localities' with UK specific localities 
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
}

#===============
# Now proceed with simplified method that just pulls out the first ID from each impression
#===============

suppressMessages(library(stringr))
df$DBMAdxPageCategories_firstCat <- str_match(df$DBMAdxPageCategories, '([^_]*)')[,2] # pulling everything up to first underscore - i.e. first ID category.
# df$DBMAdxPageCategories_firstCat
# table(df$DBMAdxPageCategories_firstCat, useNA='always')

# match to category names
df <- df %>%
  left_join(adCategoryMatchTable, by=c('DBMAdxPageCategories_firstCat'='DBMAdxPageCategories')) # %>% # 'DBMAdxPageCategories_firstCat' is name of ID, 'DBMAdxPageCategory' is name of actual category.
df$DBMAdXPageCategory_firstCat <- as.character(df$DBMAdXPageCategory)
df$DBMAdXPageCategory_firstCat[is.na(df$DBMAdXPageCategory_firstCat)] <- 'unknownSubcategory' # any numbers not in the match table are subcategories - see match table in email from Az 10/01/17 for details of these.

if(route=='UK'){ # adding in UK specific world locality subcategories
  df$DBMAdXPageCategory_firstCat[df$DBMAdxPageCategories_firstCat %in% AllWorldLocalityIDs] <- 'AllWorldLocalities' 
  df$DBMAdXPageCategory_firstCat[df$DBMAdxPageCategories_firstCat %in% EuropeWorldLocalityIDs] <- 'EuropeWorldLocalities'
  df$DBMAdXPageCategory_firstCat[df$DBMAdxPageCategories_firstCat %in% UKWorldLocalityIDs] <- 'UKWorldLocalities'    # *** NOTE ORDER OF CATEGORIES MATTERS - they aren't mutually exclusive, so World Localities will replace UK localities if it comes after
}
# table(df$DBMAdXPageCategory)
df <- select(df, -DBMAdXPageCategory, -DBMAdxPageCategories_firstCat) # DBMAdXPageCategory has been superceded by xxxCategory_firstcat. Use DBMAdxPageCategories to create binary predictors below.

# # for Jane, calculate proportion of impressions in each adpage category
# adPageCategoryProportionsDF <- data.frame(category=labels(table(df$DBMAdXPageCategory, useNA='always'))[[1]],
#                                        numberOfImpressions=as.numeric(unname(table(df$DBMAdXPageCategory, useNA='always'))))
# adPageCategoryProportionsDF <- adPageCategoryProportionsDF %>%
#   mutate(propnOfImpressions=round(numberOfImpressions/sum(numberOfImpressions), 3)) %>%
#   arrange(desc(propnOfImpressions))
# write.csv(adPageCategoryProportionsDF, file=paste0(basePath, 'AirNZ segmentation results/adPageCategoryProportions_', route, '.csv'), row.names=FALSE)

#===============
# Some other data cleaning - give state IDs actual names, and divide bid price by 1million to give CPM
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

# divide $ value variables by 1mill to give in 'CPM' units
df$DBMBidPriceAdvertiserCurrencyCPM <- df$DBMBidPriceAdvertiserCurrency/1000000
df$DBMRevenueAdvertiserCurrencyCPM <- df$DBMRevenueAdvertiserCurrency/1000000
df <- select(df, -DBMBidPriceAdvertiserCurrency, -DBMRevenueAdvertiserCurrency)
# ==> raw values in log files are in 'NZD nanos' - i.e. 1/1billionth of an NZD. This is price per impression, so to get CPM in NZD then divide by 1 million

#__________________________________________________________________________________________________________________________________

#3. Graph/model bivariate relationships - Which vars are related to conv prob?   *** ONE ROW PER IMPRESSION CF ONE ROW PER USER ***
#__________________________________________________________________________________________________________________________________

# DELETED - look at old version of script if needed in future

#__________________________________________________________________________________________________________________________________

#4. Repeat above, but one row per ***USER*** c.f. one row per ***IMPRESSION***. *** Each row is randomly selected per user ***
#__________________________________________________________________________________________________________________________________

if(outputDatabase=='df_oneRandomlySelectedRowPerUser'){
  
  #===============
  # randomly select one row per user
  #===============
  
  cat('Randomly select one row per user and save as separate df. NB for this method for ad x page category Ive only used the first category of the row (eg. 16 for 16_3_5). Unlike below, where I split these out and convert each to a dummy variable. Something to modify in future if required\n')
  
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

  # DELETED - look at old version of script if needed in future
} 

#__________________________________________________________________________________________________________________________________

#5. Repeat above - one row per ***USER*** c.f. one row per ***IMPRESSION***. *** Each row is summarised data per user ***
#__________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________

# 5.a. First need to summarise data per variable - modal value for mutually exclusive categorical vars, median value for continuous vars, and convert non-mutually exclusive categorical vars to dummy vars
#__________________________________________________________________________________________________________________________________

if(outputDatabase=='df_rowsGroupedAndSummarisedByUser'){
  
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
    filter(!is.na(DBMBidPriceAdvertiserCurrencyCPM)) %>%
    group_by(user_id) %>%
    summarise(medianBidPrice=median(DBMBidPriceAdvertiserCurrencyCPM))
  # head(bidPriceDF)
  
  #===============
  # For vars that are not mutually exclusive, Write function that takes the var and returns a dataframe with one row per user, one col per variable level, and 1 or 0 in cells depending on whether user saw that var
  #===============
  
  cat(' Creating binary predictors for factors with levels that arent mutually exclusive (e.g. Ad Page Categories)\n')
  
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
  
  colnames(adPageCategoryDF) <- paste0('adX_', gsub(' ', '_', colnames(adPageCategoryDF)))
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
}

#__________________________________________________________________________________________________________________________________

# 5.b. Create plots                                                *** DELETED - look at old version of script if needed in future
#__________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________

# 6. Repeat above - one row per ***IMPRESSION***, and response variable is whether that imp was attributed a last click conversion. Creating database (binary predictors out of ad X page category) but not doing plots at this stage
#__________________________________________________________________________________________________________________________________

if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){
  
  # i.e. now I'm trying to predict which individual impressions will lead to a last click conversion, rather than which users have the highest overall conv prob
  
  #===============
  # For vars that are not mutually exclusive, Write function that takes the var and returns a dataframe with one row per user, one col per variable level, and 1 or 0 in cells depending on whether user saw that var
  #===============
  
  cat('...Create binary predictors for factors with levels that arent mutually exclusive (e.g. Ad Page Categories)\n')
  
  convertFactorToBinaryVarsFun <- function(myDF, myVar){ # my var should be a string
    
    # # testing - HASH OUT
    # myDF <- df
    # myVar <- 'DBMURLCategory'; warning('using test values for function - hash these out when running for real!\n')
    
    rowIDs <- unique(myDF$rowIndex)
    varLevels <- unique(myDF[, myVar], useNA=F)
    varLevels <- varLevels[!is.na(varLevels)]
    
    resultsDF <- as.data.frame(matrix(ncol=length(varLevels), nrow=length(rowIDs))); colnames(resultsDF) <- varLevels
    pb <- txtProgressBar(style=3)
    
    for(i in 1:length(rowIDs)){
      row_i_DF <- filter(myDF, rowIndex==rowIDs[i])
      varLevelsSeenByRow_i <- unique(row_i_DF[,myVar])
      resultsDF[i, ] <- as.numeric(varLevels %in% varLevelsSeenByRow_i)
      setTxtProgressBar(pb, value=i/length(rowIDs))
    }
    cat('\n') # new line after prog bar
    resultsDF$rowIndex <- rowIDs
    resultsDF
    return(resultsDF)
  }
  
  adPageCategoryDF <- convertFactorToBinaryVarsFun(adPageCategoriesDF_1RowPerCategory, 'DBMAdXPageCategory')
  
  if(length(colnames(adPageCategoryDF)[grepl('unknownSubcategory', colnames(adPageCategoryDF))])>0){
    adPageCategoryDF <- select(adPageCategoryDF, -unknownSubcategory) # uninformative feature
  }
  
  colnames(adPageCategoryDF) <- paste0('adX_', gsub(' ', '_', colnames(adPageCategoryDF)))
  colnames(adPageCategoryDF)[grepl('rowIndex', colnames(adPageCategoryDF))] <- 'rowIndex'
  # head(adPageCategoryDF)
  
  #===============
  # join all together into a final DF ready for modelling
  #===============
  
  dfmodelledVars <- select(df, time, user_id, lastClickConversion, Converter, DBMAdxPageCategories, DBMRegion, DBMOperatingSystemID, DBMBrowserSlashPlatformID, 
                           DBMDeviceType, DBMMobileMakeID, DBMMobileModelID, DBMRevenueAdvertiserCurrencyCPM, DBMBidPriceAdvertiserCurrencyCPM, Campaign, Ad, Placement, Site, rowIndex) # NB I do add in a few other vars below

  df_oneRowPerAdAndLastClickConversionResponse <- merge(dfmodelledVars, adPageCategoryDF, by='rowIndex', all=TRUE)
  df_oneRowPerAdAndLastClickConversionResponse <- select(df_oneRowPerAdAndLastClickConversionResponse, -rowIndex)
  # df_oneRowPerAdAndLastClickConversionResponse[3,] # check adxpage categories are reflected in binary predictors
  
  df_oneRowPerAdAndLastClickConversionResponse <- as.data.frame(df_oneRowPerAdAndLastClickConversionResponse) # can be weird indexing things otherwise - think this comes from it being a data.table
}

#__________________________________________________________________________________________________________________________________

#7. For one row per ad database, add time of day & day of week as predictor variables
#__________________________________________________________________________________________________________________________________

if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){

  cat(' Adding hour and daypart to database. Only doing for outputDatabase==df_oneRowPerAdAndLastClickConversionResponse for now... need to think about whether I should do for the other output databases too\n')
    
  df_oneRowPerAdAndLastClickConversionResponse$day <- weekdays(df_oneRowPerAdAndLastClickConversionResponse$time)
  df_oneRowPerAdAndLastClickConversionResponse$hour <- substr(df_oneRowPerAdAndLastClickConversionResponse$time, 12, 13)
  df_oneRowPerAdAndLastClickConversionResponse$daypart <- 
    with(df_oneRowPerAdAndLastClickConversionResponse, 
         ifelse(hour %in% c('05','06','07'), 'BeforeWork_5To8',  # NB 05-07 is 5 to 8, cos 07 encompasses 7:00-7:59
                ifelse(hour %in% c('08','09','10','11'), 'WorkMorning_8To12', 
                       ifelse(hour %in% as.character(12:13), 'Lunch_12To2', 
                              ifelse(hour %in% as.character(14:17), 'WorkAfternoon_2To6', 
                                     ifelse(hour %in% as.character(18:22), 'AfterWork_6To23', 
                                     ifelse(hour %in% c('23','00','01','02','03','04'), 'Overnight_12To5', # NB hours '23-04' means 11pm to 5am, cos '4' goes right up to 4:59.
                                            NA)))))))
  # table(df_oneRowPerAdAndLastClickConversionResponse$daypart, df_oneRowPerAdAndLastClickConversionResponse$hour, useNA='always')

  # # Some EDA to look at day of week effects
  # plotDF <- df_oneRowPerAdAndLastClickConversionResponse %>%
  #   group_by(day) %>%
  #   summarise(numAds=length(day), 
  #             numConvs=length(day[lastClickConversion==1]),
  #             convProb=(numConvs/length(day)))
  # plotDF$day <- factor(plotDF$day, levels=c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'))
  # 
  # ggplot(plotDF, aes(day, convProb, fill=numAds)) + geom_bar(stat='identity') + geom_text(aes(label=numConvs, vjust=-0.25))
  # ggplot(plotDF, aes(day, numAds, fill=convProb)) + geom_bar(stat='identity') # ==> for this test, M T W have most ads served, M > T & S have highest conv prob. BUT different for different routes I think? Sat low for domestic, but high for allRoutes...
  # 
  # # And some EDA to check that DBM time of day corresponds to actual time of day, and to look into how to split up times (hourly, 'worktime' vs 'hometime' vs 'lunchtime' etc)
  # plotDF <- df_oneRowPerAdAndLastClickConversionResponse %>%
  #   group_by(hour) %>%
  #   summarise(numAds=length(hour),
  #             numConvs=length(hour[lastClickConversion==1]),
  #             convProb=(numConvs/length(hour)))
  # 
  # ggplot(plotDF, aes(hour, convProb, fill=numAds)) + geom_bar(stat='identity') + geom_text(aes(label=numConvs, vjust=-0.25))
  # ggplot(plotDF, aes(hour, numAds, fill=convProb)) + geom_bar(stat='identity') # ==> Day time is correctly aligned... too few clicks to see true peak in conversion rate by hour though.
  # 
  # # Daypart effects:
  # plotDF <- df_oneRowPerAdAndLastClickConversionResponse %>%
  #   group_by(daypart) %>%
  #   summarise(numAds=length(daypart),
  #             numConvs=length(daypart[lastClickConversion==1]),
  #             convProb=(numConvs/length(daypart)))
  # 
  # ggplot(plotDF, aes(daypart, convProb, fill=numAds)) + geom_bar(stat='identity') + geom_text(aes(label=numConvs, vjust=-0.25))
  # ggplot(plotDF, aes(daypart, numAds, fill=convProb)) + geom_bar(stat='identity') # ==> Day time is correctly aligned... too few clicks to see true peak in conversion rate by hour though.
}

#__________________________________________________________________________________________________________________________________

#8. Save dataframe for subsequent modelling
#__________________________________________________________________________________________________________________________________

cat(' Save dataframe for subsequent modelling\n')

if(outputDatabase=='df_rowsGroupedAndSummarisedByUser'){
  save(df_rowsGroupedAndSummarisedByUser, file=paste0(pathData, 'df_rowsGroupedAndSummarisedByUser_negativeInstanceDownsampling', negativeInstanceDownsampling, '_', dataset, 'DF_', route, '_', offerAds, layerAds, groupAds, 'Ads_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.rda'))  
}
if(outputDatabase=='df_oneRandomlySelectedRowPerUser'){
  save(df_oneRandomlySelectedRowPerUser, file=paste0(pathData, 'df_oneRandomlySelectedRowPerUser_negativeInstanceDownsampling', negativeInstanceDownsampling, '_', dataset, 'DF_', route, '_', offerAds, layerAds, groupAds, 'Ads_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.rda'))  
}
if(outputDatabase=='df_oneRowPerAdAndLastClickConversionResponse'){
  save(df_oneRowPerAdAndLastClickConversionResponse, file=paste0(pathData, 'df_oneRowPerAdAndLastClickConversionResponse_negativeInstanceDownsampling', negativeInstanceDownsampling, '_', dataset, 'DF_', route, '_', offerAds, layerAds, groupAds, 'Ads_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.rda'))  
}

gc()
