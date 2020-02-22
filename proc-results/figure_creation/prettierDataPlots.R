
# Starting with comboPlots/congressPlot.R
# Modified from congress/plotVoteCosponsResults.R
# See below for the main function: plotCongress(flipVotes=F, flipSpons=F) (outdated)
# and plotCongressManyTrials() (newer)


# Specialized for this Congress plot: don't connect the Dems and the Reps
addLine = function(paramsList, xdata, ydata, name, col, lty=1, pch=16, errorBarData=NULL) {
        lines(1:4, ydata[1:4], col=col, type='o', lty=lty, pch=pch, cex=1.2)
        lines(5:8, ydata[5:8], col=col, type='o', lty=lty, pch=pch, cex=1.2)
        paramsList$cols = c(paramsList$cols, col)
        paramsList$lty = c(paramsList$lty, lty)
        paramsList$names = c(paramsList$names, name)
		paramsList$pch = c(paramsList$pch, pch)
        if (length(errorBarData)) {
			error.bar(xdata, ydata, upper=errorBarData, length=.05)
        }
        return(paramsList)
}

# originally in R-sims-2d/utils.R
error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
        if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
        stop("vectors must be same length")
        arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
} 

# I believe: this one is an improvement on the getDataFromDir() in feb2015Expts/allPairsReal-realityMining/affilSubsets.
# It's deficient only in that it can't take a vector of desired tHats as input.
# Returned matrix includes rows describing best tHats per file x method.
getUsualDataFromDir = function(dataDir, methodsWithParams, methodsWithoutParams, fileFilter = NULL, tHats_avail = NULL) {
	methodsWanted = c(methodsWithParams, methodsWithoutParams)

	dataFiles = list.files(dataDir, pattern="txt")
	if (! is.null(fileFilter)) {
		dataFiles = dataFiles[grep(fileFilter, dataFiles)]
	}
	allData = matrix(0, nrow=length(methodsWithoutParams) + 2 * length(methodsWithParams), ncol=length(dataFiles),
					dimnames=list(c(methodsWanted, paste("tHat_", methodsWithParams, sep="")), dataFiles))

	for (i in 1:length(dataFiles)) {
		file = paste(dataDir, dataFiles[i], sep="/")
		tempEnv = new.env()
		source(file, local=tempEnv)
		if (is.null(tHats_avail)) {
			tHats_avail = get("tHat", envir=tempEnv)
		}
		
		# go get stuff we want
		# (code modified from getDataFromAllFiles.R and getCongressDataFromDir below)
		for (j in 1:length(methodsWanted)) {
			methodName = methodsWanted[j]

			if (j > length(methodsWithParams)) {
				if (exists(methodName, envir=tempEnv, inherits=F)) {
					allData[methodsWanted[j], i] = get(methodName, envir=tempEnv)[1]	# methodName is a vector with all items equal
					#rm(methodName)		# silly: that removes the variable "methodName", not its contents
					rm(list=methodName, envir=tempEnv)	# that should do it
				} else {
					allData[methodsWanted[j], i] = NA
				}
			} else {
				if (exists(methodName, envir=tempEnv, inherits=F)) {
					methodVals = get(methodName, envir=tempEnv)
					rm(list=methodName, envir=tempEnv)
				}

				# it has a parameter, so we have to search through all possibilities
				bestTInd = "c" 		# better init: [a string] will give reasonable behavior when var not found, than -1
				bestVal = -1

				for (tInd in 1:length(tHats_avail)) {
					currVal = methodVals[tInd]

					if (currVal > bestVal) {
						bestTInd = tInd
						bestVal = currVal
					}
				}

				# store the best val in the matrix. Also store its tHat.
				allData[methodsWanted[j], i] = bestVal
				allData[paste("tHat_", methodsWanted[j], sep=""), i] = tHats_avail[bestTInd]
			}
		}
	}
	return(allData)
}

# Used for setup with only 1 trial per setting. In these, results files need to be converted to become R code.
getCongressDataFromDir = function(dataDir, methodsWithParams, methodsWithoutParams, tHats_avail, doCorrs = F) {
	methodsWanted = c(methodsWithParams, methodsWithoutParams)

	dataFiles = setdiff(list.files(dataDir, pattern="txt"), list.files(dataDir, pattern="corr"))	# AUC ones are those w/o "corr" in the name
	if (doCorrs) {
		dataFiles = list.files(dataDir, pattern="corr")
	}
	allData = matrix(0, nrow=length(methodsWithoutParams) + 2 * length(methodsWithParams), ncol=length(dataFiles),
					dimnames=list(c(methodsWanted, paste("tHat_", methodsWithParams, sep="")), dataFiles))

	tempFile = paste(dataDir, "tmp.txt", sep="/")
	for (i in 1:length(dataFiles)) {
		file = dataFiles[i]
		fullPath = paste(dataDir, file, sep="/")
		tmpData = read.table(fullPath)
		if (doCorrs) {
			tmpData = tmpData[tmpData[[1]]=="kendallCorr",2:3]
		}
		write.table(tmpData, file=tempFile, row.names=F, col.names=F, sep=" = ", quote=F)
		# now it's valid / sourceable R code
		tempEnv = new.env()
		source(tempFile, local=tempEnv)
		file.remove(tempFile)
		
		# go get stuff we want
		# (code modified from getDataFromAllFiles.R)
		for (j in 1:length(methodsWanted)) {
			methodName = paste("auc_", methodsWanted[j], sep="")
			# correlations, and non-auc variables in results file: pred*, diff*, num*, etc.
			if (doCorrs || (substr(methodsWanted[j], 1, 3) %in% c("pre", "dif", "num", "ent"))) { 
				methodName = methodsWanted[j]
			}

			if (j > length(methodsWithParams)) {
				if (exists(methodName, envir=tempEnv, inherits=F)) {
					allData[methodsWanted[j], i] = get(methodName, envir=tempEnv)
					#rm(methodName)		# silly: that removes the variable "methodName", not its contents
					rm(list=methodName, envir=tempEnv)	# that should do it
				} else {
					allData[methodsWanted[j], i] = NA
				}
			} else {
				# it has a parameter, so we have to search through all possibilities
				bestTInd = "c" 		# better init: [a string] will give reasonable behavior when var not found, than -1
				bestVal = -1

				for (tInd in 1:length(tHats_avail)) {
					methodName = paste("auc_", methodsWanted[j], "_t", tHats_avail[tInd], sep="")
					if (doCorrs || (substr(methodsWanted[j], 1, 3) %in% c("pre", "dif", "num", "ent"))) { 
						methodName = paste(methodsWanted[j], "_t", tHats_avail[tInd], sep="")
					}
					if (exists(methodName, envir=tempEnv, inherits=F)) {
						currVal = get(methodName, envir=tempEnv)
						#rm(methodName)
						rm(list=methodName, envir=tempEnv)

						if (currVal > bestVal) {
							bestTInd = tInd
							bestVal = currVal
						}
					}
				}

				# store the best val in the matrix. Also store its tHat.
				allData[methodsWanted[j], i] = bestVal
				allData[paste("tHat_", methodsWanted[j], sep=""), i] = tHats_avail[bestTInd]
			}
		}
	}
	return(allData)
}

# flips a whole method iff it is mostly helped.
checkAndFlip = function(allData, flippedData) {

	useFlipped = rep(F, nrow(allData))
	epsilon = .000001
	biggerWinFlipped = rep(F, nrow(allData))
	epsilonBigger = .005
	bigLossFlipped = rep(F, nrow(allData))

	names(useFlipped) = rownames(allData)
	names(biggerWinFlipped) = rownames(allData)
	for (rowN in 1:nrow(allData)) {
		# compare the method in rowN to flipped version.
		# We'll report whichever is better for the whole row

		flippedWins = sum(flippedData[rowN,] > allData[rowN,] + epsilon)
		flippedWinsBigger = sum(flippedData[rowN,] > allData[rowN,] + epsilonBigger)
		flippedLosesBig = sum(allData[rowN,] > flippedData[rowN,] + epsilonBigger)
		print(paste("method", rownames(allData)[rowN], ": flipped wins", flippedWins, "times (of", ncol(allData), ");",
				"wins bigger", flippedWinsBigger, "times; loses big", flippedLosesBig, "times"))
		if (flippedWins / ncol(allData) > .5) {
			useFlipped[rowN] = T
		}
		if (flippedWinsBigger) {
			biggerWinFlipped[rowN] = T
		}
	}

	# Swap in flipped methods where they won (not for ci_'s, of course)
	methodsFlipped = c()
	for (rowN in 1:nrow(allData)) {
		if (useFlipped[rowN] & substr(rownames(allData)[rowN], 1, 3) != "ci_") {
			allData[rowN,] = flippedData[rowN,]
			methodsFlipped = c(methodsFlipped, rownames(allData)[rowN])
		}
	}
	print("All methods flipped:")
	print(methodsFlipped)

	return(allData)
}

plotAffilSubsets = function(multiTrials=T, multiTrialsFlip = F) {

	if (multiTrials) {
		# inDir for affilSubset data
		pathToDataFiles = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/affilSubsets400trials"

		# inDir for orig data
		if (multiTrialsFlip) {
			pathToOrigData = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/400trials/resultsFlip"	# flip for votes, noflip for sponsors
		} else {
			pathToOrigData = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/400trials/resultsNoFlip"	# noflip for votes, noflip for sponsors
		}

		# outDir
		if (multiTrialsFlip) {
			outDir = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/affilSubsets400trials/plotsForFlip"
		} else {
			outDir = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/affilSubsets400trials/plots"
		}

		# for getDataFromDir() with specific tHats
		source("/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/affilSubsets/getDataFromAllFiles.R")	
	} else {
		# inDir for affilSubset data
		pathToDataFiles = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/affilSubsetResults"
		# inDir for orig data
		pathToOrigData = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/voteCospons-results-noflipflip"	# noflip for votes, flip for sponsors

		# outDir
		outDir = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/affilSubsetResults/plots"
	}

	methodsWithParams = c("model5LogLR")	# note: no ci_'s for (non multiTrial) Congress runs
	methodsWithoutParams = c("idfZero", "m", "d", "jaccardSimZero", "adamicFixed", "unweighted_cosineZero", 
								"one_over_log_p_m1100", "one_over_log_p_m11", "m11_minus_d", 
								"pearsonWeighted", "pearsonCorrZero", "newmanCollab")

	if (multiTrials) {
		tHats_avail = c(.001, .01, .05, .1, .4, .5)
		data_orig = getUsualDataFromDir(pathToOrigData, methodsWithParams, methodsWithoutParams, tHats_avail=tHats_avail)
		tHats_used = data_orig["tHat_model5LogLR",]
		if (multiTrialsFlip) {
			data_rand = getDataFromDir(paste(pathToDataFiles, "affilSubsetsFlip_rand", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats=tHats_used)
			data_min = getDataFromDir(paste(pathToDataFiles, "affilSubsetsFlip_min", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats=tHats_used)
			data_max = getDataFromDir(paste(pathToDataFiles, "affilSubsetsFlip_max", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats=tHats_used)
		} else {
			data_rand = getDataFromDir(paste(pathToDataFiles, "affilSubsets_rand", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats=tHats_used)
			data_min = getDataFromDir(paste(pathToDataFiles, "affilSubsets_min", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats=tHats_used)
			data_max = getDataFromDir(paste(pathToDataFiles, "affilSubsets_max", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats=tHats_used)
		}

	} else {
		bestTHats = .001
		data_rand = getCongressDataFromDir(paste(pathToDataFiles, "results_0.25affilSubset_rand", sep="/"), methodsWithParams, methodsWithoutParams, bestTHats)
		data_min = getCongressDataFromDir(paste(pathToDataFiles, "results_0.25affilSubset_min", sep="/"), methodsWithParams, methodsWithoutParams, bestTHats)
		data_max = getCongressDataFromDir(paste(pathToDataFiles, "results_0.25affilSubset_max", sep="/"), methodsWithParams, methodsWithoutParams, bestTHats)

		data_orig = getCongressDataFromDir(pathToOrigData, methodsWithParams, methodsWithoutParams, bestTHats)
	}


    # All right! We've got the data. Time to plot it.
	# Plot: do the conditions stack/order as expected?
	for (method in c("model5LogLR", methodsWithoutParams)) {        

		pdf(paste(outDir, "/", method, ".pdf", sep=""), width=7, height=4.5)
		par(mgp=c(1.5, .5, 0), mar=c(3, 2.5, 3, .5), cex=.85)

		# orig data
		plot(1:4, data_orig[method,][1:4], type='o', pch=20,
			#ylim=c(.6,.85),
			#ylim=range(allData["d",], .5, .92, allData[1:(length(methodsWithParams) + length(methodsWithoutParams)),]),
			ylim=range(.5, data_min[method,], data_orig[method,], data_rand[method,], 1),
			ylab="AUCs",
			xaxt="n",
			xlim=c(1,8),
			main=paste("Affil subsets for", method),
			#main="U.S. Representatives,\nby party and Congress number",
			xlab="")
		axis(1, at=1:8, labels=F)
		axis(1, at=c(2.5, 6.5), labels=c("Democrats", "Republicans"), tick=F, padj=1.3)
		axis(1, at=1:8, labels=rep(110:113, 2), tick=F, padj=-.4)
		axis(1, at=4.5, labels="U.S. Representatives, by party and Congress number", tick=F, padj=3.5)

		lines(5:8, data_orig[method,][5:8], type='o', pch=20)

		abline(h=.5, lty=2)

		# min
		lines(1:4, data_min[method,][1:4], type='o', pch=2)
		lines(5:8, data_min[method,][5:8], type='o', pch=2)

		# max
		lines(1:4, data_max[method,][1:4], type='o', pch=4)
		lines(5:8, data_max[method,][5:8], type='o', pch=4)

		# rand
		lines(1:4, data_rand[method,][1:4], type='o', pch=15)
		lines(5:8, data_rand[method,][5:8], type='o', pch=15)

		legend("topright",  c("original", "1/4 affils max", "1/4 affils random", "1/4 affils min"), lty=1, pch=c(20, 4, 15, 2))

		dev.off()

	}
}

# flipSpons: you tell us
# flipVotes: F means don't, T means flip the methods that benefit, and "A" means flip them all
plotCongress = function(flipVotes=F, flipSpons=F) {

	
	votesFlipTxt = "noflip"		# used if flipVotes = F or T
	if (flipVotes == "A") {
		votesFlipTxt = "flip"
	}
	
	sponsorsFlipTxt = "noflip"
	if (flipSpons) {
		sponsorsFlipTxt = "flip"
	}

	dataDir = paste("/Users/lfriedl/Documents/dissertation/binary-ndim/congress/voteCospons-results-", votesFlipTxt, sponsorsFlipTxt, sep="")

	methodsWithParams = c("model5LogLR", "model7LogLR", "mystery")
	methodsWithoutParams = c("m", "d", "m11_minus_d", "idfZero", "jaccardSimZero", "unweighted_cosineZero", 
									"one_over_log_p_m11", "one_over_log_p_m1100", 
									"pearsonWeighted", "pearsonCorrZero", "adamicFixed", "newmanCollab", "cosineStand")
	# (I manually concatenated the newer Ts with the older results, into a single file.) 
	tHats_avail = c(.001, .01, .05, .1, .2, .3, .4, .5)

	allData = getCongressDataFromDir(dataDir, methodsWithParams, methodsWithoutParams, tHats_avail)
	print("best tHats for model5:")
	print(allData["tHat_model5LogLR",])
	if (flipVotes == T) {
		flippedDir = paste("/Users/lfriedl/Documents/dissertation/binary-ndim/congress/voteCospons-results-", "flip", sponsorsFlipTxt, sep="")
		flippedData = getCongressDataFromDir(flippedDir, methodsWithParams, methodsWithoutParams, tHats_avail)

		# Check which cases we prefer flipped data
		allData = checkAndFlip(allData, flippedData)
	}

	# All right! We've got the data. Good start.
	# Time to plot it.

	cl = rainbow(10)

	xAxis = 1:ncol(allData)

	#pdf(paste(dataDir, "../voteCospons-plots", plotFileName, sep="/"), width=7, height=4.5)
	
	par(mgp=c(1.5, .5, 0), mar=c(3, 2.5, 3, .5), cex=.85)

	ylimToUse = range(allData["d",], .5, .92, allData[1:(length(methodsWithParams) + length(methodsWithoutParams)),])

	plot(xAxis, allData["model5LogLR",], type='n', pch=20, 
		#ylim=c(.6,.85), 
		ylim=ylimToUse,
		ylab="AUCs", 
		xaxt="n", 
		#xlim=c(1, 11), 
		main="U.S. Representatives,\nby party and Congress number",
		xlab="")
    axis(1, at=1:8, labels=F)
    axis(1, at=c(2.5, 6.5), labels=c("Democrats", "Republicans"), tick=F, padj=1.3)
    axis(1, at=1:8, labels=rep(110:113, 2), tick=F, padj=-.4)
    #axis(1, at=4.5, labels="U.S. Representatives, by party and Congress number", tick=F, padj=3.5)


	paramsList = list(cols=c("black"), lty=c(1), names=c("MixedPairs"), pch=c(20))
	abline(h=.5, lty=2)

	# (order now readjusted so that the legend makes sense with the presentation I want)

	paramsList = addLine(paramsList, xAxis, allData["pearsonWeighted",], "WeightedCorrelation", "darkgreen", lty=1)
	paramsList = addLine(paramsList, xAxis, allData["pearsonCorrZero",], "Pearson", "darkgreen", lty=4, pch=10)

	# dummy add cosineIDF so it's in the right order in the legend
	paramsList = addLine(paramsList, xAxis, rep(-10, length(xAxis)), "CosineIDF", cl[9])

	paramsList = addLine(paramsList, xAxis, allData["unweighted_cosineZero",], "Cosine", cl[9], lty=4, pch=10)
	paramsList = addLine(paramsList, xAxis, allData["jaccardSimZero",], "Jaccard", "turquoise", pch=10)

	paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m1100",], "SharedWeight1100", "orange", lty=1)
	#paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m11",], "SharedWeight11", "orange", lty=2, pch=15)

	paramsList = addLine(paramsList, xAxis, allData["newmanCollab",], "Newman", "coral4", pch=15)
	paramsList = addLine(paramsList, xAxis, allData["adamicFixed",], "Adamic / Adar", "green", pch=15)

	paramsList = addLine(paramsList, xAxis, allData["m",], "SharedSize", "blue", pch=22)
	paramsList = addLine(paramsList, xAxis, allData["d",], "Hamming", "red", pch=22)
	paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m11",], "SharedWeight11", "orange", lty=2, pch=15)

	# do idf and model5 last so they're visible (have already been added to paramsList)
	addLine(paramsList, xAxis, allData["idfZero",], "CosineIDF", cl[9])
	addLine(paramsList, xAxis, allData["model5LogLR",], "", "black", lty=1)

	return(paramsList)
}

# (not using)
#paramsList = addLine(paramsList, xAxis, allData["model7LogLR",], "model7", "black", lty=3)
#paramsList = addLine(paramsList, xAxis, allData["model6LogLR",], "model6", cl[9], lty=4)
#paramsList = addLine(paramsList, xAxis, allData["mystery",], "mystery", "yellow", lty=1)
#paramsList = addLine(paramsList, xAxis, allData["cosineStand",], "cosineStand", cl[10], lty=4)
#paramsList = addLine(paramsList, xAxis, allData["m11_minus_d",], "SharedMinusDiff", "blue", lty=2)


#legend("topright", legend=paramsList$names, lty=paramsList$lty, col=paramsList$cols, pch=paramsList$pch, cex=.85)
#dev.off()

# flipVotes: F means don't, T means flip the methods that benefit, and "A" means flip them all
# Caution when using this! For original data, use the flipVotes syntax above. But for affilSubsets, just use T vs. F (it's all or nothing).
plotCongressManyTrials = function(flipVotes=F, plotFileName = NULL, affilSubsetType=NULL, forStandAlonePlot=F, forTallNarrowCombo=F) {

        if (is.null(affilSubsetType)) {
                allData = getCongressDataManyTrials(flipVotes)
        } else {
                allData = getCongressDataAffilSubsets(flipVotes, affilSubsetType)
        }

        if (!is.null(affilSubsetType)) {
                ylim = range(allData["m",], allData["idfZero",], allData["pearsonCorrZero",], allData["model5LogLR",])
        }
		if (forTallNarrowCombo) {
			plotAllData(allData, ylim, plotFileName, forStandAlonePlot=forStandAlonePlot, shorterTitle=T, matchingYLim=T)
		} else {
			plotAllData(allData, ylim, plotFileName, forStandAlonePlot=forStandAlonePlot)
		}
}

getCongressDataAffilSubsets = function(flipVotes, affilSubsetType) {
	pathToDataFiles = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/affilSubsets400trials"
	methodsWithParams = c("model5LogLR")	
	methodsWithoutParams = c("idfZero", "m", "d", "jaccardSimZero", "adamicFixed", "unweighted_cosineZero", 
								"one_over_log_p_m1100", "one_over_log_p_m11", "m11_minus_d", 
								"pearsonWeighted", "pearsonCorrZero", "newmanCollab", "ci_idfZero",
									"ci_pearsonCorrZero")

	tHats_avail = c(.001, .01, .05, .1, .4, .5)
	data_orig = getUsualDataFromDir(pathToOrigData, methodsWithParams, methodsWithoutParams, tHats_avail=tHats_avail)
	tHats_used = data_orig["tHat_model5LogLR",]

	subsetStr = c("rand", "max", "min")[affilSubsetType]
	flipStr = c("_", "Flip_")[as.numeric(flipVotes) + 1]
	subdirName = paste0("affilSubsets", flipStr, subsetStr)

	# for getDataFromDir() with specific tHats
	source("/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/affilSubsets/getDataFromAllFiles.R")	

	data = getDataFromDir(paste(pathToDataFiles, subdirName, sep="/"), c(methodsWithParams, "ci_model5LogLR"), methodsWithoutParams, whichTHats=tHats_used)

	return(data)
}


# flipVotes: F means don't, T means flip the methods that benefit, and "A" means flip them all
getCongressDataManyTrials = function(flipVotes=F) {

	# for getDataFromDir() with specific tHats
	#source("/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/affilSubsets/getDataFromAllFiles.R")	
	source("/Users/lfriedl/Documents/dissertation/binary-ndim/comboPlots/getCleanDataFromAllFiles.R")

	votesFlipTxt = "NoFlip"		# used if flipVotes = F or T
	if (flipVotes == "A") {
		votesFlipTxt = "Flip"
	}
	
	dataDir = paste("/Users/lfriedl/Documents/dissertation/binary-ndim/congress/400trials/results", votesFlipTxt, sep="")

	methodsWithParams = c("model5LogLR", "model7LogLR", "mystery")
	methodsWithoutParams = c("m", "d", "m11_minus_d", "idfZero", "jaccardSimZero", "unweighted_cosineZero", 
									"one_over_log_p_m11", "one_over_log_p_m1100", 
									"pearsonWeighted", "pearsonCorrZero", "adamicFixed", "newmanCollab", "cosineStand",
									"ci_pearsonCorrZero")
	# (I manually concatenated the newer Ts with the older results, into a single file.) 
	tHats_avail = c(.001, .01, .05, .1, .4, .5)

	allData = getUsualDataFromDir(dataDir, methodsWithParams, methodsWithoutParams, tHats_avail=tHats_avail)
	methodCIs = c("ci_model5LogLR")
	dataCIs = getCleanDataFromDir(dataDir, methodCIs, c(), whichTHats = allData["tHat_model5LogLR",])

	print("best tHats for model5:")
	print(allData["tHat_model5LogLR",])
	if (flipVotes == T) {
		flippedDir = paste("/Users/lfriedl/Documents/dissertation/binary-ndim/congress/400trials/results", "Flip", sep="")
		flippedData = getUsualDataFromDir(flippedDir, methodsWithParams, methodsWithoutParams, tHats_avail=tHats_avail)

		# Check which cases we prefer flipped data
		allData = checkAndFlip(allData, flippedData)
	}
	allData = rbind(allData, dataCIs)

	return(allData)
}

# shorterTitle & matchingYLim: flags added for thesis version
plotAllData = function(allData, ylim, plotFileName, forStandAlonePlot=F, shorterTitle=F, matchingYLim=F) {

	# All right! We've got the data. Good start.
	# Time to plot it.

	cl = rainbow(10)

	xAxis = 1:ncol(allData)

	if (!is.null(plotFileName)) {
		if (forStandAlonePlot) {
			pdf(plotFileName, width=5.5, height=3.9)
		} else {
			pdf(plotFileName, width=7, height=4.5)
		}
	}
	if (forStandAlonePlot) {
		par(mgp=c(1.5, .5, 0), mar=c(3, 2.5, 2, .5), cex=1)
		titleTxt = "U.S. Representatives, by party and Congress number"
	} else if (shorterTitle) {
		# shorter in height, too
		par(mgp=c(1.5, .5, 0), mar=c(3, 2.5, 2, .5), cex=.85)
		titleTxt = "Congress"	# that's what I keep calling it in the text, anyhow 
	} else {
		par(mgp=c(1.5, .5, 0), mar=c(3, 2.5, 3, .5), cex=.85)
		titleTxt = "U.S. Representatives,\nby party and Congress number"
	}

	if (matchingYLim) {
		ylimToUse = c(.43, 1)
	} else {
		ylimToUse = range(.5, 1, allData["d",], allData["m",])
	}

	plot(xAxis, allData["model5LogLR",], type='n', pch=20, 
		#ylim=c(.6,.85), 
		#ylim=range(allData["d",], .5, .92, allData[1:(length(methodsWithParams) + length(methodsWithoutParams)),]),
		ylim=ylimToUse,
		ylab="AUCs", 
		xaxt="n", 
		#xlim=c(1, 11), 
		main=titleTxt,
		xlab="")
    axis(1, at=1:8, labels=F)
	if (forStandAlonePlot) {
		axis(1, at=c(2.5, 6.5), labels=c("Democrats", "Republicans"), tick=F, padj=1.5)
		axis(1, at=1:8, labels=rep(110:113, 2), tick=F, padj=-.2)
	} else {
		axis(1, at=c(2.5, 6.5), labels=c("Democrats", "Republicans"), tick=F, padj=1.3)
		axis(1, at=1:8, labels=rep(110:113, 2), tick=F, padj=-.4)
	}
    #axis(1, at=4.5, labels="U.S. Representatives, by party and Congress number", tick=F, padj=3.5)


	paramsList = list(cols=c("black"), lty=c(1), names=c("MixedPairs"), pch=c(20))
	abline(h=.5, lty=2)

	# (order now readjusted so that the legend makes sense with the presentation I want)

	paramsList = addLine(paramsList, xAxis, allData["pearsonWeighted",], "WeightedCorrelation", "darkgreen", lty=1)
	paramsList = addLine(paramsList, xAxis, allData["pearsonCorrZero",], "Pearson", "darkgreen", lty=4, pch=10)

	# dummy add cosineIDF so it's in the right order in the legend
	paramsList = addLine(paramsList, xAxis, rep(-10, length(xAxis)), "CosineIDF", cl[9])

	paramsList = addLine(paramsList, xAxis, allData["unweighted_cosineZero",], "Cosine", cl[9], lty=4, pch=10)
	paramsList = addLine(paramsList, xAxis, allData["jaccardSimZero",], "Jaccard", "turquoise", pch=10)

	paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m1100",], "SharedWeight1100", "orange", lty=1)
	#paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m11",], "SharedWeight11", "orange", lty=2, pch=15)

	paramsList = addLine(paramsList, xAxis, allData["newmanCollab",], "Newman", "coral4", pch=15)
	paramsList = addLine(paramsList, xAxis, allData["adamicFixed",], "Adamic / Adar", "green", pch=15)

	paramsList = addLine(paramsList, xAxis, allData["m",], "SharedSize", "blue", pch=22)
	paramsList = addLine(paramsList, xAxis, allData["d",], "Hamming", "red", pch=22)
	paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m11",], "SharedWeight11", "orange", lty=2, pch=15)

	# do idf and model5 last so they're visible (have already been added to paramsList)
	addLine(paramsList, xAxis, allData["idfZero",], "CosineIDF", cl[9])
	addLine(paramsList, xAxis, allData["model5LogLR",], "", "black", lty=1)

	# error bars
	error.bar(xAxis, allData["model5LogLR",], upper=allData["ci_model5LogLR",], length=.05)
	error.bar(xAxis, allData["pearsonCorrZero",], upper=allData["ci_pearsonCorrZero",], length=.05)

	if (!is.null(plotFileName)) {
		dev.off()
	}

	return(paramsList)
}

