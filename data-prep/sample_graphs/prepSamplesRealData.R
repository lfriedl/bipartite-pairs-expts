
source("dataReader.R")

# Sample positive pairs, then sample singletons, making sure they don't accidentally form pairs.
#createRealDataForNewsgroup = function(exptDirFullPath, numPositivePairs, numSingletons, numTrials, 
createRealDataForNewsgroup = function(exptDirFullPath, numPositivePairs, numSingletons, minTrialNum, maxTrialNum,
                                         pathToNGData, whichGroup, truePositivesFile) {
    
    # make sure the output dir exists or create it. Also truePositivesFile, before we read everything in.
    if (checkNGDataPaths(exptDirFullPath, truePositivesFile, pathToNGData, whichGroup)) {
        return()
    }
    
    allItems = readOneNewsGroup(pathToNGData, whichGroup, binary=T)  
    
#    for (i in 1:numTrials) {
    for (i in minTrialNum:maxTrialNum) {
        # create data file for trial i
        newDataFileName = paste(exptDirFullPath, "/data", i, ".Rdata", sep="")
        
        dataItems = sampleNewsgroupData(allItems, truePositivesFile, numPairsWanted = numPositivePairs,
                                             numSingletonsWanted = numSingletons)
        
        save(dataItems, numPositivePairs, file=newDataFileName)    
    }    
}

# Returns 0 if ok, 1 if error
checkNGDataPaths = function(exptDirFullPath, truePositivesFile, pathToNGData, whichGroup) {
    
    dirExists = file.info(exptDirFullPath)$isdir
    if (is.na(dirExists) || !dirExists) {
        dirExists = dir.create(exptDirFullPath)
        if (!dirExists) {
            warning("checkDataPaths: Couldn't find/create expt dir")
            return(1)
        }
    }
    fileExists = file.exists(truePositivesFile)
    if (!fileExists) {
        warning("checkDataPaths: couldn't find true positives file")
        return(1)
    }
    dataExists = file.info(paste(pathToNGData, "/", whichGroup, sep=""))$isdir
    if (is.na(dataExists) || !dataExists) {
        warning("checkDataPaths: couldn't find newsgroup data")
        return(1)
    }
    return(0)
}
