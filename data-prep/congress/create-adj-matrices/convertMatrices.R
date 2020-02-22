
source("../../converting-from-R/data-conversion.R")		# for convertMatrix()
source("../true-pairs/createTruePairsFiles.R")			# for constructListOfTruePairs()
library(Matrix)

# convert existing adjacency matrices to mtx
# Clean up item and affil names in the Rdata before converting it: store mtx with itemnames sorted. in affil names, keep only last few components of path.
convertCongressVotesAndSpons = function() {
	dataDir = '/home/lfriedl/ASOUND-bipartite/data-prep/congress/adjacency-matrices'
	# or locally at
	#dataDir = '/Users/lfriedl/Documents/dissertation/binary-ndim/congress/adjacency-matrices'
	# yet a better place to do this
	dataDir = '../adjacency-matrices'
	parties = c("dem", "rep")
	sessions = 110:113
	for (party in parties) {
		for (session in sessions) {
			print(paste("Converting Rdata files for", party, session))
			votesAdjFile = file.path(dataDir, paste0(party, "Votes", session, ".Rdata"))
			matrixVar = load(votesAdjFile)		# (have to load it to find out the varname)
			# cleaning!
			voteMat = get(matrixVar)
			if (is.unsorted(rownames(voteMat))) {
				voteMat = voteMat[order(rownames(voteMat)),]
			}
			colnames(voteMat) = sub(paste0(".*/", session), session, colnames(voteMat))
			# re-save before converting
			assign(matrixVar, voteMat)
			save(list=as.character(matrixVar), file=votesAdjFile)

			convertMatrix(votesAdjFile,  outfileBase = substr(votesAdjFile, 1, nchar(votesAdjFile) - 6), varName=matrixVar)

			sponsAdjFile = file.path(dataDir, paste0(party, "Cospons", session, ".Rdata"))
			matrixVar = load(sponsAdjFile)		
			# cleaning!
			sponsMat = get(matrixVar)
			if (is.unsorted(rownames(sponsMat))) {
				sponsMat = sponsMat[order(rownames(sponsMat)),]
				sponsMat = sponsMat[, order(colnames(sponsMat))]
			}
			colnames(sponsMat) = sub(paste0(".*/", session), session, colnames(sponsMat))
			# re-save before converting
			assign(matrixVar, sponsMat)
			save(list=as.character(matrixVar), file=sponsAdjFile)
			convertMatrix(sponsAdjFile,  outfileBase = substr(sponsAdjFile, 1, nchar(sponsAdjFile) - 6), varName=matrixVar)
		}
	}
}

# get cutoff for true pairs, save thresholded matrix as Rdata, then convert to mtx
# Note that input Cospons file is legislator x bills, whereas output is legislators x legislators (smaller).
# Makes sure rownames are sorted (they will already be if convertCongressVotesAndSpons() was run first).
congressTruePairsSaveConvert = function() {
	dataDir = '/home/lfriedl/ASOUND-bipartite/data-prep/congress/adjacency-matrices'
	# or locally at
	#dataDir = '/Users/lfriedl/Documents/dissertation/binary-ndim/congress/adjacency-matrices'
	dataDir = '../adjacency-matrices'
	parties = c("dem", "rep")
	sessions = 110:113
	for (party in parties) {
		for (session in sessions) {
			print(paste("Writing true pairs matrices for", party, session))

			sponsAdjFile = file.path(dataDir, paste0(party, "Cospons", session, ".Rdata"))
			# cutoff: true pairs have at least this many cosponsorships
			cutoffVal = constructListOfTruePairs(sponsAdjFile, outFile=NULL, fractionOfPeopleWithALink = 1.5)
			
			matrixVar = load(sponsAdjFile)		# expect a Matrix called demSpons or repSpons
			dataItems = get(matrixVar)	
			if (is.unsorted(rownames(dataItems))) {
				dataItems = dataItems[order(rownames(dataItems)),]
				dataItems = dataItems[, order(colnames(dataItems))]
			}
			cosponsMat = tcrossprod(dataItems)
		
			# threshold it (after zeroing diagonal)
			diag(cosponsMat) = 0
			cosponsMat[cosponsMat < cutoffVal] = 0
			cosponsMat@x = rep(1, sum(cosponsMat >= cutoffVal))

			# save as Rdata
			save(cosponsMat, file=file.path(dataDir, paste0(party, "Cospons", session, "GE", cutoffVal, ".Rdata")))

			# save as mtx (no need to re-save rownames using convertMatrix)
			writeMM(cosponsMat, file=file.path(dataDir, paste0(party, "Cospons", session, "GE", cutoffVal, ".mtx")))
		}
	}
}
