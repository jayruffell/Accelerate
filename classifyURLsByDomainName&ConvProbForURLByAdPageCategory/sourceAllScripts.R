#_________________________________________________________________________________________________________________________________________________

# Once I've tested the Run Model scripts I need to run the code multiple times; once for each combination of product and funnel stage. This code is easiest way of doing this.
#_________________________________________________________________________________________________________________________________________________

#===============
# set parameters
#===============

rm(list=ls())
pathcodeForSourcing <- 'E:/JAY/AirNZ segmentation code/classifyURLsByDomainName&ConvProbForURLByAdPageCategory/' # where the other scripts are    
sourceFile <- 'classifyURLsByDomainName&ConvProbForURLByAdPageCategory.r'
routeVec <- c('TAS', 'LH', 'PI', 'DOM', 'allRoutes')
samplesize <- 1 # randomly sample this proportion of nonconvs, keep all convs
rImageName <- 'database_usefulColumnsOnly_samplesize1.rda'

# NB any new parameters have to be included in any 'setdiff' statements (ie objects not to remove) in this code and all other scripts that get run by it.

#===============
# Run models
#===============

sourceScript <- 'Y' # if this parameter exists then scripts getting sourced will use above parameters, instead of internally-specified ones.

for(v in 1:length(routeVec)){
    route <- routeVec[v] # need to use subscripts that don't get used elsewhere in sourced scripts
    source(paste0(pathcodeForSourcing, sourceFile))
    rm(list=setdiff(ls(), c('pathcodeForSourcing', 'sourceFile', 'routeVec', 'sourceScript', 'samplesize', 'rImageName', 'v')))
  }

gc()
