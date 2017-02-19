#__________________________________________________________________________________________________________________________________

# Loop thru log files, reading in each and appending to create full database
#__________________________________________________________________________________________________________________________________

#__________________________________________________________________________________________________________________________________

#1. set parameters, define cols we want to keep in final dataframe, & load packages
#__________________________________________________________________________________________________________________________________

#===============
# parameters
#=============== 

rm(list=ls())
Sys.setenv(TZ="Pacific/Auckland")
samplesize_subsetDF <- 0.001 # proportion of users that will be retained - rest will be randomly removed - when saving subsetDF
basePath <- 'E:/JAY/data/' # path to day-level folders (i.e. one folder per day)
advertiserID <- '620645'
advertiserID2 <- '620645' # If only one advertiserID just use same no. of both.
# DOES AIR NZ HAVE A SECOND ADVERTISER ID???
clicksDirectoryName <- 'click/' 
impsDirectoryName <- 'impression/'  # what is the name of the folder that all clicks/imps/acts subfolders are stored in?
actsDirectoryName <- 'activity/' 
firstDate <- '2016-12-01'
maxDate <- '2016-12-21'
colsToKeep <- c('action_type', 'EventTime', 'UserID', 'AdvertiserID', 
                'CampaignID', 'AdID', 'SiteIDDCM', 'PlacementID', 'RenderingID', # RenderingID is same as CreativeID in DCM - used to pull in creative info.
                'ActivityID', 'FloodlightConfiguration', 
                'CountryCode', 'BrowserSlashPlatformID', 'BrowserSlashPlatformVersion', 'OperatingSystemID', 'OtherData', 
                'DBMBidPriceAdvertiserCurrency', 'DBMRevenueAdvertiserCurrency', 'DBMURL', 'DBMAdxPageCategories', 'DBMStateSlashRegionID', # used to create DBMRegionID
                'DBMOperatingSystemID', 'DBMBrowserSlashPlatformID', 'DBMDeviceType', 'DBMMobileMakeID', 'DBMMobileModelID',
                # Vars that I originally included in EDA, but decided weren't useful:
                'EventType', 'EventSub-Type', 'StateSlashRegion',  'DBMMatchingTargetedSegments', 'DBMCityID', 'DBMZIPSlashPostalCode',
                'DBMAdPosition', 'DBMAuctionID') # this col allows us to match click and imp for an individual ad

# # code used to find column names above:
# impFileFolder1 <- paste0(basePath, impsDirectoryName, list.files(paste0(basePath, impsDirectoryName))[1], '/')
# impFile1 <- list.files(impFileFolder1)[1]
# clickFileFolder1 <- paste0(basePath, clicksDirectoryName, list.files(paste0(basePath, clicksDirectoryName))[1], '/')
# clickFile1 <- list.files(clickFileFolder1)[1]
# actFileFolder1 <- paste0(basePath, actsDirectoryName, list.files(paste0(basePath, actsDirectoryName))[1], '/')
# actFile1 <- list.files(actFileFolder1)[1]
# 
# impFile_firstTenRows <- fread(paste0(impFileFolder1, impFile1))[1:10,]
# clickFile_firstTenRows <- fread(paste0(clickFileFolder1, clickFile1))[1:10,]
# actFile_firstTenRows <- fread(paste0(actFileFolder1, actFile1))[1:10,]
# names(impFile_firstTenRows)
# names(clickFile_firstTenRows)
# names(actFile_firstTenRows)
# str(impFile_firstTenRows)
# str(clickFile_firstTenRows)
# str(actFile_firstTenRows)
# # write.csv(impFile_firstTenRows, paste0(basePath, 'impFile_firstTenRows.csv'), row.names=FALSE)
# # write.csv(clickFile_firstTenRows, paste0(basePath, 'clickFile_firstTenRows.csv'), row.names=FALSE)
# # write.csv(actFile_firstTenRows, paste0(basePath, 'actFile_firstTenRows.csv'), row.names=FALSE)

suppressMessages(library(dplyr))
suppressMessages(library(data.table))
suppressMessages(library(ggplot2))
require('bit64') # prevents fread warning messages

cat('\n      ------ Loading files', firstDate, 'to', maxDate, '// Saving fullDF with all users, & subsetDF with', 100*(1-samplesize_subsetDF), '% of users removed -----\n\n')

#__________________________________________________________________________________________________________________________________

#2-a. Before starting, count the number of files in each folder and throw an error if incorrect  (noticed a problem with an extra file in impressions)
#__________________________________________________________________________________________________________________________________

daysInDateRange <- as.numeric((as.Date(maxDate)-as.Date(firstDate))+1) # +1 COS WE CURRENTLY SPECIFY firstDate-maxDate INCLUSIVE WHEN FILTERING BELOW

# CLICKS

# Subset folders by firstdate and maxdate
clicksPath <- paste0(basePath, clicksDirectoryName)
allclicksFoldersAsDates <- as.Date(list.files(clicksPath), format='%Y%m%d')
clicksFolders_subsettedByDate <- allclicksFoldersAsDates[allclicksFoldersAsDates>=firstDate &
                                                           allclicksFoldersAsDates<=maxDate]
clicksFolders_subsettedByDate <- gsub("-", "", as.character(clicksFolders_subsettedByDate)) # returning to orignal format
clicksFoldersVec <- paste0(clicksPath, clicksFolders_subsettedByDate, '/')

# First, check theres one folder per day in each day's folder
numFilesList <- list()
for(i in 1:length(clicksFoldersVec)){
  clicksFolder_i <- clicksFoldersVec[i]
  numFilesList[[i]] <- data.frame(folder=as.character(clicksFolder_i), numFiles=length(list.files(clicksFolder_i)))
}
numFilesDF <- bind_rows(numFilesList)
incorrectFoldersDF <- filter(numFilesDF, numFiles!=24)

if(nrow(incorrectFoldersDF)>0){
  print(incorrectFoldersDF)
  stop('Number of files in at least one clicksFolder is incorrect. If too many, delete oldest duplicated file (can tell date from filename). If too few, ask Brett... In future can do away with manual deletion by modifying code to only read in newest duplicated file, but for now just delete. \n SEE PRINTOUT ABOVE FOR OFFENDING FOLDERS\n')
} else {
  cat(' Read in following number of files from each clicksFolder: ')
  cat(numFilesDF$numFiles, '\n')
}

# Now check there's one folder per day
if(length(clicksFoldersVec)!=daysInDateRange){
  stop('Number of files in clicksFolder does not equal number of days in date range (assuming we want firstDate-maxDate inclusive)\n')
} else { cat(paste(' Read in', daysInDateRange, 'days worth of clicks data\n\n'))}

# IMPS

# Subset folders by firstdate and maxdate
impsPath <- paste0(basePath, impsDirectoryName)
allImpsFoldersAsDates <- as.Date(list.files(impsPath), format='%Y%m%d')
impsFolders_subsettedByDate <- allImpsFoldersAsDates[allImpsFoldersAsDates>=firstDate &
                                                       allImpsFoldersAsDates<=maxDate]
impsFolders_subsettedByDate <- gsub("-", "", as.character(impsFolders_subsettedByDate)) # returning to orignal format
impsFoldersVec <- paste0(impsPath, impsFolders_subsettedByDate, '/')

# First, check theres one folder per hour in each day's folder
numFilesList <- list()
for(i in 1:length(impsFoldersVec)){
  impsFolder_i <- impsFoldersVec[i]
  numFilesList[[i]] <- data.frame(folder=as.character(impsFolder_i), numFiles=length(list.files(impsFolder_i)))
}
numFilesDF <- bind_rows(numFilesList)
incorrectFoldersDF <- filter(numFilesDF, numFiles!=24)

if(nrow(incorrectFoldersDF)>0){
  print(incorrectFoldersDF)
  stop('Number of files in at least one impsFolder is incorrect. If too many, delete oldest duplicated file (can tell date from filename). If too few, ask Brett... In future can do away with manual deletion by modifying code to only read in newest duplicated file, but for now just delete. \n SEE PRINTOUT ABOVE FOR OFFENDING FOLDERS\n')
} else {
  cat(' Read in following number of files from each impsFolder: ')
  cat(numFilesDF$numFiles, '\n')
}

# Now check there's one folder per day
if(length(impsFoldersVec)!=daysInDateRange){
  stop(' Number of files in impsFolder does not equal number of days in date range (assuming we want firstDate-maxDate inclusive)\n')
} else { cat(paste(' Read in', daysInDateRange, 'days worth of imps data\n\n'))}

# ACTS

# Subset folders by firstdate and maxdate
actsPath <- paste0(basePath, actsDirectoryName)
allactsFoldersAsDates <- as.Date(list.files(actsPath), format='%Y%m%d')
actsFolders_subsettedByDate <- allactsFoldersAsDates[allactsFoldersAsDates>=firstDate &
                                                       allactsFoldersAsDates<=maxDate]
actsFolders_subsettedByDate <- gsub("-", "", as.character(actsFolders_subsettedByDate)) # returning to orignal format
actsFoldersVec <- paste0(actsPath, actsFolders_subsettedByDate, '/')

# First, check theres one folder per day in each day's folder
numFilesList <- list()
for(i in 1:length(actsFoldersVec)){
  actsFolder_i <- actsFoldersVec[i]
  numFilesList[[i]] <- data.frame(folder=as.character(actsFolder_i), numFiles=length(list.files(actsFolder_i)))
}
numFilesDF <- bind_rows(numFilesList)
incorrectFoldersDF <- filter(numFilesDF, numFiles!=1)

if(nrow(incorrectFoldersDF)>0){
  print(incorrectFoldersDF)
  stop('Number of files in at least one actsFolder is incorrect. If too many, delete oldest duplicated file (can tell date from filename). If too few, ask Brett... In future can do away with manual deletion by modifying code to only read in newest duplicated file, but for now just delete. \n SEE PRINTOUT ABOVE FOR OFFENDING FOLDERS\n')
} else {
  cat(' Read in following number of files from each actsFolder: ')
  cat(numFilesDF$numFiles, '\n')
}  

# Now check there's one folder per day
if(length(actsFoldersVec)!=daysInDateRange){
  stop('Number of files in actsFolder does not equal number of days in date range (assuming we want firstDate-maxDate inclusive)\n')
} else { cat(paste(' Read in', daysInDateRange, 'days worth of acts data\n\n'))}

#__________________________________________________________________________________________________________________________________

#2-b. write function that reads in all the files within a given folder, filters by advertiserID, cleans colNames, and then binds together
#__________________________________________________________________________________________________________________________________

# Note that current data structure has clicks, imps, and acts in different folders - so no need to worry about different schemas at this point

# 'folderPath' is a path to a folder containing only csv files, all of which have the same schema.
rbindAllFilesInAFolderFun <- function(folderPath, interactionType){ # 'clicks', 'imps', or 'acts'. Needed because advertiser ID comes from different fields depending on acts vs imps/clicks
  
  # # for testing *** HASH OUT FOR REAL RUN ***
  # folderPath <- paste0(basePath, 'click/20161128/'); interactionType <- 'clicks'; testingFunction <- 'Y'
  # folderPath
  # if(exists('testingFunction')){cat(' *** WARNING: Using testFolder for testing loop - need to hash out when running real code ***\n')}
  
  filesList <- list()
  for(i in 1:length(list.files(folderPath))){
    file_i <- paste0(folderPath, list.files(folderPath))[i]
    filesList[[i]] <- 
      fread(file_i, showProgress=TRUE,
            colClasses=c('Event Time'='numeric',
                         'DBM Matching Targeted Segments'='character',
                         'DBM ZIP/Postal Code'='character',
                         'DBM Attributed Inventory Source External ID'='character')) # these are the variables that will be one colClass in one file and a different one in another file. e.g. may be all '0' in one, just cos no records, which would then get interpreted as integer even if non-blanks are character.
  }
  finalDF <- bind_rows(filesList, .id='fileNumber') # .id used to report on how many files were read in.. in case only 1
  finalDF$fileNumber <- as.numeric(finalDF$fileNumber)
  
  # data cleaning - filter by advertiser, downsample, and rename cols to match R names better
  colnames(finalDF) <- gsub(" ", "", colnames(finalDF))
  colnames(finalDF) <- gsub("/", "Slash", colnames(finalDF))
  colnames(finalDF) <- gsub("\\(", "", colnames(finalDF))
  colnames(finalDF) <- gsub(")", "", colnames(finalDF))
  
  if(interactionType=='clicks' | interactionType=='imps'){
    finalDF <- finalDF %>%
      filter((AdvertiserID==advertiserID | AdvertiserID==advertiserID2) & !is.na(AdvertiserID) & UserID!="0")
  } else if(interactionType=='acts'){
    finalDF <- finalDF %>%
      filter((FloodlightConfiguration==advertiserID | FloodlightConfiguration==advertiserID2) & !is.na(FloodlightConfiguration)  & UserID!="0")
  } else { stop('ERROR - interaction type should be clicks, ints, or acts\n')}
  
  cat(max(finalDF$fileNumber), 'files read in for path ', folderPath, '\n')
  return(finalDF) 
}

#__________________________________________________________________________________________________________________________________

#3. Now define paths to folders containing all clicks, imps, and acts csvs, then use above to read them in and rbind together. Do separately for clicks, imps, and acts, since their schemas are different
#__________________________________________________________________________________________________________________________________

#===============
# clicks
#===============

clicksPath <- paste0(basePath, clicksDirectoryName)

# SUBSET FOLDERS BY FIRSTDATE AND MAXDATE

allClicksFoldersAsDates <- as.Date(list.files(clicksPath), format='%Y%m%d')

clicksFolders_subsettedByDate <- allClicksFoldersAsDates[allClicksFoldersAsDates>=firstDate &
                                                           allClicksFoldersAsDates<=maxDate]
clicksFolders_subsettedByDate <- gsub("-", "", as.character(clicksFolders_subsettedByDate)) # returning to orignal format
clicksFoldersVec <- paste0(clicksPath, clicksFolders_subsettedByDate, '/') 
clicksFoldersVec
clicksList <- list()

for(i in 1:length(clicksFoldersVec)){
  clicksFolder_i <- clicksFoldersVec[i]
  clicksList[[i]] <- rbindAllFilesInAFolderFun(clicksFolder_i, interactionType='clicks')
}
clicksDF <- rbindlist(clicksList)
rm(clicksList)

# # temp - checking all hours of data were read in
# tempDF <- clicksDF
# tempDF$EventTime <- as.POSIXct(tempDF$EventTime/1000000, origin='1970-01-01') # event time is microseconds since 1970-01-01, but R works in seconds
# tempDF <- sample_n(tempDF, size=round(nrow(tempDF)/1)) # will be way too many datapoints otherwise
# tempDF$dateHour <- as.POSIXct(paste0(substr(tempDF$EventTime, start=0, stop=13), ':00:00')) # strip out minutes and seconds, so I can group by date & hour for plot
# plotDF <- tempDF %>%
#   group_by(dateHour) %>%
#   summarise(numTouchpoints=length(dateHour))
# ggplot(plotDF, aes(dateHour, numTouchpoints)) + geom_line() # should just be some value for every hour of every day
# graphics.off()

#===============
# imps
#===============

impsPath <- paste0(basePath, impsDirectoryName)

# SUBSET FOLDERS BY FIRSTDATE AND MAXDATE

allImpsFoldersAsDates <- as.Date(list.files(impsPath), format='%Y%m%d')
impsFolders_subsettedByDate <- allImpsFoldersAsDates[allImpsFoldersAsDates>=firstDate &
                                                           allImpsFoldersAsDates<=maxDate]
impsFolders_subsettedByDate <- gsub("-", "", as.character(impsFolders_subsettedByDate)) # returning to orignal format
impsFoldersVec <- paste0(impsPath, impsFolders_subsettedByDate, '/')
impsFoldersVec
impsList <- list()

for(i in 1:length(impsFoldersVec)){
  impsFolder_i <- impsFoldersVec[i]
  impsList[[i]] <- rbindAllFilesInAFolderFun(impsFolder_i, interactionType='imps')
}
impsDF <- rbindlist(impsList)
rm(impsList)

#===============
# acts
#===============

actsPath <- paste0(basePath, actsDirectoryName)

# SUBSET FOLDERS BY FIRSTDATE AND MAXDATE

allActsFoldersAsDates <- as.Date(list.files(actsPath), format='%Y%m%d')
actsFolders_subsettedByDate <- allActsFoldersAsDates[allActsFoldersAsDates>=firstDate &
                                                       allActsFoldersAsDates<=maxDate]
actsFolders_subsettedByDate <- gsub("-", "", as.character(actsFolders_subsettedByDate)) # returning to orignal format
actsFoldersVec <- paste0(actsPath, actsFolders_subsettedByDate, '/')
actsFoldersVec
actsList <- list()

for(i in 1:length(actsFoldersVec)){
  actsFolder_i <- actsFoldersVec[i]
  actsList[[i]] <- rbindAllFilesInAFolderFun(actsFolder_i, interactionType='acts')
}
actsDF <- rbindlist(actsList)
rm(actsList)

#__________________________________________________________________________________________________________________________________

#4. Bind acts, clicks, and imps together, then clean dataframe (keeping relevant cols, setting time properly etc) & save images
#__________________________________________________________________________________________________________________________________

cat('\nSaving data to file\n')

df <- bind_rows('C'=clicksDF, 'I'=impsDF, 'A'=actsDF, .id='action_type') # this adds an action_type col with C, A, or I values. NB Using bind_rows means not all cols need to match up. Unmatched cols will just be filled with NAs
df <- as.data.frame(df)[, colsToKeep]
rm(clicksDF, actsDF, impsDF)

# Put date into correct time
df$EventTime <- as.POSIXct(df$EventTime/1000000, origin='1970-01-01') # event time is microseconds since 1970-01-01, but R works in seconds

#===============
# Write to file - all data
#===============

save(df, file=paste0(basePath, 'database_usefulColumnsOnly_fullDF_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.rda'))

#===============
# write to file - subsetDF. Randomly remove users according to samplesize parameter
#===============

# downsample according to samplesize parameter
if(samplesize_subsetDF<1){
  usersToKeep <- sample(unique(df$UserID), size=ceiling(length(df$UserID)*samplesize_subsetDF))
  df <- filter(df, UserID %in% usersToKeep)
  gc()
}

save(df, file=paste0(basePath, 'database_usefulColumnsOnly_subsetDF_', gsub('-', '', firstDate), '-', gsub('-', '', maxDate), '.rda'))
gc()
