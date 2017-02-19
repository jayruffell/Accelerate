
rm(list=ls())

# myFloodlightConfiguration <- 5550406
# myEventTime <- 1485760332663137
# myUserID <- 'CAESEBvKfuAIEpsio6lgBGg_L80'

myFloodlightConfiguration <- 5522061
myEventTime <- 1484227054248679
myUserID <- 'CAESEEgRA-YPsQDxdjWI5Y2DteU'

library(data.table)
require('bit64') # prevents fread warning messages
options(scipen = 1000)

# Check date from Brett's row, so I know which folder to read in from
as.POSIXct(myEventTime/1000000, origin='1970-01-01')

# Read in correct folder based on above date:
filePath <- 'E:/JAY/data/activity/20170113/'
myCsv <- fread(paste0(filePath, list.files(filePath)))
colnames(myCsv) <- gsub(' ', '', colnames(myCsv))
myCsv$EventTime[1]

data.frame(maxEventTime=as.POSIXct(max(myCsv$EventTime)/1000000, origin='1970-01-01'),
           minEventTime=as.POSIXct(min(myCsv$EventTime)/1000000, origin='1970-01-01'))

myRows <- filter(myCsv, FloodlightConfiguration==myFloodlightConfiguration & UserID==myUserID & EventTime==myEventTime)
select(myRows, EventTime, UserID, OtherData)

