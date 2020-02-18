
source("dataReader.R")
source("../synthetic_data/datagen/reloadClassDefns.R", chdir=T)		# for multivariateBernoulli
source("../converting-from-R/data-conversion.R")        # for convertMatrix(), so we can save Reality's big bipartite while we have it here

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

checkRealityDataPaths = function(exptDirFullPath, realityDataDir) {
    dirExists = file.info(exptDirFullPath)$isdir
    if (is.na(dirExists) || !dirExists) {
        dirExists = dir.create(exptDirFullPath)
        if (!dirExists) {
            warning("checkDataPaths: Couldn't find/create expt dir")
            return(1)
        }
    }
    dataExists = file.info(realityDataDir)$isdir
    if (is.na(dataExists) || !dataExists) {
        warning("checkDataPaths: couldn't find reality mining data")
        return(1)
    }
    return(0)
    
}


createRealDataReality = function(exptDirFullPath, numPositivePairs, numSingletons, minTrialNum, maxTrialNum,
                                      realityDataDir, saveBipartiteOutfileBase=NULL) {
    
    # make sure the output dir exists or create it. 
    if (checkRealityDataPaths(exptDirFullPath, realityDataDir)) {
        return()
    }
    
    allItemsAndIds = readAllRealityData(realityDataDir)
	if (!is.null(saveBipartiteOutfileBase)) {
		adjMatrix = allItemsAndIds$dataMatrix
		outfileForMatrix = paste0(saveBipartiteOutfileBase, ".Rdata")
		save(adjMatrix, file=outfileForMatrix)
		#writeMM(allItemsAndIds$dataMatrix, file=outfileForMatrix)
		convertMatrix(outfileForMatrix, saveBipartiteOutfileBase, quarterAffils=F)
	}

	# this phi matches the way we're sampling rows from the matrix
    phi = learnPhiWithAvgingFromRealityData(allItemsAndIds$dataMatrix, allItemsAndIds$personIds)
    
    for (i in minTrialNum:maxTrialNum) {
        # create data file for trial i
        newDataFileName = paste(exptDirFullPath, "/data", i, ".Rdata", sep="")
        
        dataItems = sampleRealityMiningDataFromMatrix(allItemsAndIds$dataMatrix, allItemsAndIds$personIds,
                                          numPositivePairs, numSingletons)
        
        save(dataItems, numPositivePairs, phi, file=newDataFileName)    
        
    }    
}

# Take avg of each person -> 1 row
# Learn phi from that new matrix
learnPhiWithAvgingFromRealityData = function(dataMatrix, personIds) {
     
    sumByPerson = rowsum(as.matrix(dataMatrix), personIds)   # slow! takes several minutes
    
    # rows are now in numerical order by personId. table(personIds) will also order them this way,
    # but to be sure, let's explicitly grab the correct row names
    tab = table(personIds)  # tab[personId] = num occurrences (within personIds)
    avgByPerson = sumByPerson / as.vector(tab[rownames(sumByPerson)]) # divides each row of sumByPerson by num rows for that person
    
    return(learnPhiFromBinaryDataMatrix(avgByPerson))  # it's okay that input matrix's entries aren't binary
    
}


# For learning a phi (overall distribution) from a data set represented as a matrix
learnPhiFromBinaryDataMatrix = function(dataMatrix) {
    # compute the hat(q_i)'s from the Matrix: sum(column i) / nrow
    # intuition: probability that column gets a 1, across all docs
    q_is = apply(dataMatrix, 2, sum, na.rm=T) / nrow(dataMatrix)
    bernou = new("multivariateBernoulli", componentProbs=q_is)
    
    return(bernou)
}

