
# Modified from the one in the comboPlots directory just a wee bit (titles, margins, axes). 

# Modified from feb2015Expts/allPairsReal-newsgroups/400trials/onePlotAllGroups.v4.R
source("/Users/lfriedl/Documents/dissertation/binary-ndim/comboPlots/congressPlot.R")	# for checkAndFlip()

# v4 = some new ideas for plot symbols / semantics
#source("/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/affilSubsets/getDataFromAllFiles.R")	# for getDataFromDir()
source("/Users/lfriedl/Documents/dissertation/binary-ndim/comboPlots/getCleanDataFromAllFiles.R")   # for getCleanDataFromDir(), which I'm hoping can cleanly replace getDataFromDir().

# default pch: 20 or 16 (bigger)? Use 16 to match the squares.
addLine = function(paramsList, xdata, ydata, name, col, lty=1, pch=16, errorBarData=NULL) {	
        lines(xdata, ydata, col=col, type='o', lty=lty, pch=pch, cex=1.2)
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

# does NG by default, when realityMining=F
# flipped: F means don't, T means flip the methods that benefit, and "A" means flip them all
plotNGReality = function(realityMining=F, flipped=F, affilSubsetType=NULL) {

	if (is.null(affilSubsetType)) {
		allData = getData(realityMining, flipped)
	} else {
		allData = getAffilSubsetData(realityMining, affilSubsetType)
	}

	if (realityMining) {
		sortOrder = 1:6
		ylim = c(.55,.82)
	} else {
		# careful: want this to be the sort order of original data
		#sortOrder = order(allData["model5LogLR",])
		sortOrder = c(1, 19, 15, 16, 3, 11, 17, 18, 20, 4, 6, 2, 12, 14, 9, 10, 8, 5, 13, 7)
		ylim = c(.85, 1)
	}

	if (!is.null(affilSubsetType)) {
		ylim = range(allData["m",], allData["idfZero",], allData["pearsonCorrZero",], allData["model5LogLR",])
	}
	plotAllData(allData, sortOrder, realityMining, ylim)
}

getAffilSubsetData = function(realityMining, affilSubsetType) {

	bestTHats = .001	# always, for NG and Reality
	methodsWithParams = c("model5LogLR", "model7LogLR", "ci_model5LogLR", "mystery")
	methodsWithoutParams = c("idfZero", "m", "d", "jaccardSimZero", "adamicFixed", "unweighted_cosineZero", 
					"one_over_log_p_m1100", "one_over_log_p_m11", "m11_minus_d", "ci_idfZero", 
					"pearsonWeighted", "pearsonCorrZero", "newmanCollab")

	if (realityMining) {
		pathToDataFiles = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/affilSubsets/400trials6datasets"   
	} else {
		pathToDataFiles = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-newsgroups/affilSubsets/400trials"       
	}

	if (affilSubsetType == 1) {

		if (realityMining) {
			data_rand = getCleanDataFromDir(paste(pathToDataFiles, "results_400_0.25affilSubset_rand", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats=bestTHats)
		} else {
			data_rand = getCleanDataFromDir(paste(pathToDataFiles, "results_400_0.25affilSubset_rand2", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats=bestTHats)
		}

		return(data_rand)

	} else if (affilSubsetType == 2) {

		data_max = getCleanDataFromDir(paste(pathToDataFiles, "results_400_0.25affilSubset3_max", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats= bestTHats)
		return(data_max)

	} else if (affilSubsetType == 3) {

		data_min = getCleanDataFromDir(paste(pathToDataFiles, "results_400_0.25affilSubset3_min", sep="/"), methodsWithParams, methodsWithoutParams, whichTHats= bestTHats)
		return(data_min)

	}

}

getData = function(realityMining=F, flipped=F) {

	if (realityMining) {
		
		numFiles = 6	# for reality mining

		pathToDataFiles = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/400trials/results_allto6"
		pathToFlippedDataFiles = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/400trials/resultsFlip_allto6"
		pathToMoreTsModel5 = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/400trials/results_moreTs_0"

	} else {
	
		numFiles = 20 	# for newsgroups

		pathToDataFiles = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-newsgroups/400trials/results_quarterAffils400trials_merged1to6"
		pathToFlippedDataFiles = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-newsgroups/400trials/resultsFlip_quarterAffils400trials_allto6"
		pathToMoreTsModel5 = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-newsgroups/400trials/results_400trials_moreTs_0"

	}

	bestTHats = rep(.01, numFiles)	# we'll splice in t=.001, which are the best, farther down
	methodsWithParams = c("model5LogLR", "model7LogLR", "ci_model5LogLR", "mystery")
	methodsWithoutParams = c("idfZero", "m", "d", "jaccardSimZero", "adamicFixed", "unweighted_cosineZero", 
					"one_over_log_p_m1100", "one_over_log_p_m11", "m11_minus_d", "ci_idfZero", 
					"pearsonWeighted", "pearsonCorrZero", "newmanCollab")

	allData = getCleanDataFromDir(pathToDataFiles, methodsWithParams, methodsWithoutParams, fileFilter=NULL, whichTHats = bestTHats)	
		# (explicitly sending in bestTHats is just a note-to-self that yes, I manually checked them to all be .01)
	if (flipped != F) {
		flippedData = getCleanDataFromDir(pathToFlippedDataFiles, methodsWithParams, methodsWithoutParams, fileFilter=NULL, whichTHats = bestTHats)
			# for model5, flipping doesn't affect tHats. (model7 isn't going in paper, so doesn't matter)
	}

	# Actually t=.001 is even better. Splice that in, even though the difference is minuscule.
	moreTsData = getCleanDataFromDir(pathToMoreTsModel5, methodsWithParams = c("model5LogLR", "ci_model5LogLR"), methodsWithoutParams=c(),
					fileFilter=NULL, whichTHats = rep(.001, numFiles))

	if (flipped == "A") {
		allData = flippedData
	} else if (flipped) {
		allData = checkAndFlip(allData, flippedData)
	}


	# Splice in the moreTs (model5 scores are indep of flipping)
	for (rowN in 1:nrow(moreTsData)) {
		methodName = rownames(moreTsData)[rowN]
		print(paste("Splicing in lower-t results for method", methodName))
		allData[methodName,] = moreTsData[methodName,]
	}

	return(allData)
}


plotAllData = function(allData, sortOrder, realityMining, ylimToUse) {

	# plotting time!

	cl = rainbow(10)
	xAxis = 1:ncol(allData)

	if (realityMining) {
		par(mgp=c(1.5, .5, 0), mar=c(2.5, 2.5, 2, .5), cex=.85)

		plot(xAxis, allData["model5LogLR",][sortOrder], type='n', pch=16, # no longer pch=20,
			#ylim=c(.5,.85),
			#ylim=c(.55,.82),
			#ylim=range(allData["d",], allData["model7LogLR",], .5, .92),
			ylim = ylimToUse,
			ylab="AUCs",
			xaxt="n",
			main="Reality Mining",
			xlab="")
		axis(1, at=1:6, labels=F)
		axis(1, at=c(1.5, 3.5, 5.5), labels=c("Apps", "Bluetooth", "Cell towers"), tick=F, padj=1.3)
		#axis(1, at=1:6, labels=rep(c("day", "week"), 3), tick=F, padj=-.8) # with former (default?) margins, this worked
		axis(1, at=1:6, labels=rep(c("day", "week"), 3), tick=F, padj=-.4)
		#axis(1, at=3.5, labels="Reality Mining", tick=F, padj=3)

		error.bar(xAxis, allData["model5LogLR",][sortOrder], upper=allData["ci_model5LogLR",][sortOrder], length=.05)

	} else {

		par(mgp=c(1.5, .5, 0), mar=c(2.5, 2.5, 2, .5), cex=.85)

		# Should I label the x-axis somehow?
		labelsOrigOrder = c("atheism", "graphics", "ms-windows", "ibm", "mac", "windows.x", "forsale", "autos", "motorcycles",
							"baseball", "hockey", "crypt", "electronics", "med", "space", "christian", "guns", "mideast", "politics.misc",
							"religion.misc")
	
		plot(xAxis, allData["model5LogLR",][sortOrder], ylim=ylimToUse, 	# ylim=c(.85,1), 
			type='n', pch=16, # no longer pch=20, 
			#ylab="AUCs", xlab="Newsgroups (ordered by AUC of MixedPairs)")
			ylab="AUCs", main="Newsgroups", xlab="(ordered by AUC of MixedPairs)")

		#error.bar(xAxis, allData["model5LogLR",][sortOrder], upper=allData["ci_model5LogLR",][sortOrder], length=.05)
	}

	paramsList = list(cols=c("black"), lty=c(1), names=c("MixedPairs"), pch=c(16))	# no longer 20, in legend too
	abline(h=.5, lty=2)

	# (order now readjusted so that the legend makes sense with the presentation I want)
	if (F) {
		paramsList = addLine(paramsList, xAxis, allData["pearsonWeighted",][sortOrder], "WeightedCorrelation", "darkgreen", lty=1)
		paramsList = addLine(paramsList, xAxis, allData["pearsonCorrZero",][sortOrder], "Pearson", "darkgreen", lty=4, pch=10)

		paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m1100",][sortOrder], "SharedWeight1100", "orange", lty=1)
		paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m11",][sortOrder], "SharedWeight11", "orange", lty=2, pch=15)

		if (!realityMining) {
			# only use this in the zoom
			error.bar(xAxis, allData["idfZero",][sortOrder], upper=allData["ci_idfZero",][sortOrder], length=.05)
		}

		# dummy add cosineIDF so it's in the right order in the legend
		paramsList = addLine(paramsList, xAxis, rep(-1, length(xAxis)), "CosineIDF", cl[9])

		paramsList = addLine(paramsList, xAxis, allData["unweighted_cosineZero",][sortOrder], "Cosine", cl[9], lty=4, pch=10)	
		paramsList = addLine(paramsList, xAxis, allData["jaccardSimZero",][sortOrder], "Jaccard", "turquoise", pch=10)
		paramsList = addLine(paramsList, xAxis, allData["newmanCollab",][sortOrder], "Newman", "coral4", pch=15)
		paramsList = addLine(paramsList, xAxis, allData["adamicFixed",][sortOrder], "Adamic / Adar", "green", pch=15)

		paramsList = addLine(paramsList, xAxis, allData["m",][sortOrder], "SharedSize", "blue", pch=22)

		#if (realityMining) {
		# in this function, always do it; it won't show up in newsgroups, but it'll still come out in its legend, as we'd like
		paramsList = addLine(paramsList, xAxis, allData["d",][sortOrder], "Hamming", "red", pch=22)
		#}

	} else {
		# alternative ordering: dark circles, open circles, dark squares, open squares
		paramsList = addLine(paramsList, xAxis, allData["pearsonWeighted",][sortOrder], "WeightedCorrelation", "darkgreen", lty=1)
		paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m1100",][sortOrder], "SharedWeight1100", "orange", lty=1)
		# dummy add cosineIDF so it's in the right order in the legend
		paramsList = addLine(paramsList, xAxis, rep(-1, length(xAxis)), "CosineIDF", cl[9])
		if (!realityMining) {
			error.bar(xAxis, allData["idfZero",][sortOrder], upper=allData["ci_idfZero",][sortOrder], length=.05)
		}


		paramsList = addLine(paramsList, xAxis, allData["pearsonCorrZero",][sortOrder], "Pearson", "darkgreen", lty=4, pch=10)
		paramsList = addLine(paramsList, xAxis, allData["unweighted_cosineZero",][sortOrder], "Cosine", cl[9], lty=4, pch=10)	
		paramsList = addLine(paramsList, xAxis, allData["jaccardSimZero",][sortOrder], "Jaccard", "turquoise", pch=10)

		paramsList = addLine(paramsList, xAxis, allData["one_over_log_p_m11",][sortOrder], "SharedWeight11", "orange", lty=2, pch=15)
		paramsList = addLine(paramsList, xAxis, allData["newmanCollab",][sortOrder], "Newman", "coral4", pch=15)
		paramsList = addLine(paramsList, xAxis, allData["adamicFixed",][sortOrder], "Adamic / Adar", "green", pch=15)

		paramsList = addLine(paramsList, xAxis, allData["m",][sortOrder], "SharedSize", "blue", pch=22)

		# Hamming won't show up in newsgroups, but it'll still come out in its legend, as we'd like
		paramsList = addLine(paramsList, xAxis, allData["d",][sortOrder], "Hamming", "red", pch=22)

	}

	# do idf and model5 last so they're visible (have already been added to paramsList)
	addLine(paramsList, xAxis, allData["idfZero",][sortOrder], "CosineIDF", cl[9])
	addLine(paramsList, xAxis, allData["model5LogLR",][sortOrder], "", "black", lty=1)

	return(paramsList)
}

