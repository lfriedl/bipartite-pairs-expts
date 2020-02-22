library(data.table)

if (T) {	# if using old data (temporarily)
	# getCleanDataFromDir: just like getAUCsFromFiles, except (1) the files it reads contain R vectors, (2) it takes tHat as an arg, and (3) the two have rows/cols transposed.
	source("/Users/lfriedl/Documents/dissertation/binary-ndim/comboPlots/getCleanDataFromAllFiles.R")       # for getCleanDataFromDir(), i.e., cleanly
	source("/Users/lfriedl/Documents/dissertation/binary-ndim/congress/prettierDataPlots.R")        # for getUsualDataFromDir(), for congress, since tHats need to be picked dynamically.
	methodsWithParams = c("model5LogLR", "ci_model5LogLR")
	methodsWithoutParams = c("idfZero", "m", "d", "jaccardSimZero", "adamicFixed", "unweighted_cosineZero", 
							"one_over_log_p_m1100", "one_over_log_p_m11", "ci_idfZero", 
							"pearsonWeighted", "pearsonCorrZero", "newmanCollab")
}


# inspired by /Users/lfriedl/Documents/dissertation/text/documents/dissertationDoc/chapterAffilData/figures/plotGrandAvgs2.R


# save before playing around
origParams = par()

playAround = function() {
	#pdf("test_grandAvgs.pdf", height=6, width=9)	# v0, uses par(cex=.85), cex.axis=.8, text(cex=1.5), legend(pt.cex=1.5)
													# Reality Mining label at y=.75 and segment to .74
	#pdf("test_grandAvgs_v1.pdf", height=5, width=9)	# v1, uses par(cex=.85), cex.axis=.8, text(cex=1.2), legend(cex=.85, pt.cex=1)
													# Reality Mining label at y=.73 and segment to .72
	#pdf("test_grandAvgs_v2.pdf", height=5, width=9)	# v2, uses Gowalla data instead of Brightkite
	#pdf("test_grandAvgs_v3.pdf", height=5, width=9)	# v3, updates to data2020's newsgroup and reality data, moving Reality Mining label and segment
	pdf("test_grandAvgs_v4.pdf", height=5, width=9)		# v4, updates to data2020's congress data, moving Congress label and segment
							# state of the data here: Gowalla & Reality Mining = done. Congress all-pairs is done except need to refresh 113 data. Newsgroups is done for sampling;
							# still could redo using all-pairs.
	plotAllOneStrip()
	dev.off()

}

# hard-code things for the first cut
plotAllOneStrip = function() {

	# set up axes
	# (not sure if they'll play nicely using same y-axis, or if I'll end up with separate calls to "plot" after all.)
	xAxisPerPlot = 1:5
	xAxisLabelsPerPlot = c("Flipped", "Original", "Max\nSubset", "Random\nSubset", "Min\nSubset")

	# let's say we'll have: Reality, Congress, Brightkite, Gowalla
	numDataSets = 4
	xAxisStarts = ((.25 + length(xAxisPerPlot)) * 0:(numDataSets-1)) + 1	# here, the initial scalar*2 gives the spacing between plots
	origAt = xAxisStarts + 1
	xAxisLabels = rep(xAxisLabelsPerPlot, numDataSets)
	xAxisLabelPositions = rep(1:length(xAxisPerPlot), times=numDataSets) + rep((xAxisStarts - 1), each=length(xAxisPerPlot)) 

	# specialized for: Reality, Congress, Gowalla w/o flipping, Newsgroups
	indexToRm = 11	
	xAxisLabels = xAxisLabels[-indexToRm]
	xAxisLabelPositions = xAxisLabelPositions[-indexToRm]
	xAxisLabelPositions[indexToRm:length(xAxisLabelPositions)] = xAxisLabelPositions[indexToRm:length(xAxisLabelPositions)] - 1 # and shift the remaining positions over
	origAt[3:4] = origAt[3:4] - 1
	xAxisStarts[4] = xAxisStarts[4] - 1

	par(mgp=c(1.5, .5, 0), mar=c(2.5, 2.5, 2, 1), cex=.85)
	plot(xAxisLabelPositions, rep(0, length(xAxisLabelPositions)), ylim=c(.5, 1), ylab="AUCs", xlab="", type='n', xaxt="n")	# set up plot w/y-axis
	#pAdjToUse = c(0,0, .5,.5,.5)	# ignore, b/c a bit weird: moves 2-line labels so their centers match the 1-line ones

	# axis: since R is too cautious to print all the labels next to each other, divide the list into alternating pieces
	#axis(1, at=xAxisLabelPositions, labels=xAxisLabels, padj=1)
	axis(side=1, at=xAxisLabelPositions[seq(1,length(xAxisLabelPositions),2)], labels=xAxisLabels[seq(1,length(xAxisLabelPositions),2)], 
		padj=1, cex.axis=0.8, mgp=c(1.5, .25, 0))	# align top of labels; shrink text beyond rest of plot; push labels closer (.25) than on y-axis
	axis(side=1, at=xAxisLabelPositions[seq(2,length(xAxisLabelPositions),2)], labels=xAxisLabels[seq(2,length(xAxisLabelPositions),2)], 
		padj=1, cex.axis=0.8, mgp=c(1.5, .25, 0))
	abline(h=.5, lty=2)		# for "random" AUC

	# for each data set, get its data and put it in the plot
	data = getRealityData()		# note: it's in the order "orig, flipped", so change that in next line
	addDataToPlot(data[c(2,1,3:5),], startAtX=xAxisStarts[1])

	data = getCongressData()
	addDataToPlot(data[c(2,1,3:5),], startAtX=xAxisStarts[2])

	#data = getBrightkiteData()
	#ddDataToPlot(data, startAtX=xAxisStarts[3])
	data = getGowallaData()
	paramsForLegend = addDataToPlot(data, startAtX=xAxisStarts[3])

	data = getNewsgroupData()
	paramsForLegend = addDataToPlot(data[c(2,1,3:5),], startAtX=xAxisStarts[4])

	# add dataset labels
	#text(x=origAt, y=c(.73, .85, .8, .8), labels=c("Reality Mining", "Congress", "Brightkite", "Newsgroups"), cex=1.2)
	text(x=origAt, y=c(.76, .81, .895, .8), labels=c("Reality Mining", "Congress", "Gowalla", "Newsgroups"), cex=1.2)
	# want to highlight the Original settings. An abline is too strong, but try segments().
	segments(x0=origAt, y0=c(.73, .735, .82, .81), y1=c(.75, .795, .88, .875), lty=2)
	# todo later: add matching segments coming up from the x axis

	# add legend
	legend("topleft", legend=paramsForLegend$names, lty=paramsForLegend$lty, col=paramsForLegend$cols,
                                        pch=paramsForLegend$pch, cex=.85,
                                        pt.cex=1.2)             # don't shrink the points as much as the text
}

# data is in rows. Each column will be 1 line.
addDataToPlot = function(data, startAtX, oldMethNames=F) {
	cl = rainbow(10)
	xAxis = startAtX:(startAtX + nrow(data) - 1)

	paramsList = list(cols=c("black"), lty=c(1), names=c("MixedPairs"), pch=c(16))

	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"pearsonWeighted"], data[, "auc_weighted_corr"]), "WeightedCorr", "darkgreen", lty=1)
	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"one_over_log_p_m1100"], data[, "auc_shared_weight1100"]), "SharedWeight10", "orange", lty=1)

	# dummy add cosineIDF so it's in the right order in the legend
	paramsList = addLine(paramsList, xAxis, rep(-10, length(xAxis)), "CosineIDF", cl[9])

	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"pearsonCorrZero"], data[, "auc_pearson"]), "Pearson", "darkgreen", lty=4, pch=10)
	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"unweighted_cosineZero"], data[, "auc_cosine"]), "Cosine", cl[9], lty=4, pch=10)
	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"jaccardSimZero"], data[, "auc_jaccard"]), "Jaccard", "turquoise", pch=10)

	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"one_over_log_p_m11"], data[, "auc_shared_weight11"]), "SharedWeight1", "orange", lty=2, pch=15)
	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"newmanCollab"], data[, "auc_newman"]), "Newman", "coral4", pch=15)
	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"adamicFixed"], data[, "auc_adamic_adar"]), "Adamic / Adar", "green", pch=15)

	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"m"], data[, "auc_shared_size"]), "SharedSize", "blue", pch=22)

	paramsList = addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"d"], data[, "auc_hamming"]), "Hamming", "red", pch=22)

	# do idf and model5 last so they're visible (have already been added to paramsList)
	addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"idfZero"], data[, "auc_cosineIDF"]), "CosineIDF", cl[9])
	addLine(paramsList, xAxis, ifelse(rep(oldMethNames, nrow(data)), data[,"model5LogLR"], data[, "auc_mixed_pairs_0.001"]), "", "black", lty=1)
	return(paramsList)
}


# adds a line at the given coords, plus stores its data to use for legend
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

getNewsgroupData = function(old=F) {
	if (old) {
		grandAvgDir = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-newsgroups/400trials/grand-avgs"
		best_tHats = .01

		# Here, each call to getCleanDataFromDir will return a matrix with just 1 column. <--> 1 results file 
		flippedData = getCleanDataFromDir(paste0(grandAvgDir, "/inferenceFlip_allto6"), methodsWithParams, methodsWithoutParams, fileFilter = "inference", whichTHats = best_tHats)

		data1 = getCleanDataFromDir(paste0(grandAvgDir, "/inference_part1_2"), methodsWithParams, methodsWithoutParams, fileFilter = "inference", whichTHats = best_tHats)
		data4 = getCleanDataFromDir(paste0(grandAvgDir, "/inference_part4"), methodsWithParams, methodsWithoutParams, fileFilter = "inference", whichTHats = best_tHats)
		data6 = getCleanDataFromDir(paste0(grandAvgDir, "/inference_part6"), methodsWithParams, methodsWithoutParams, fileFilter = "inference", whichTHats = best_tHats)

		dataSmallT = getCleanDataFromDir(paste0(grandAvgDir, "/inference_moreTs"), methodsWithParams, c(), fileFilter = "inference", whichTHats = .001)

		origData = data1
		origData[is.na(origData),] = data4[is.na(origData),]	# data4 and data6 fill in the holes in data1 (they're holes b/c we sent in a list of methods)
		origData[is.na(origData),] = data6[is.na(origData),]
		origData[1:2,] = dataSmallT             # counting on model5 and its ci_ to stay in rows 1 & 2
		flippedData[1:2,] = dataSmallT
		best_tHats = .001       # for later use

		subsetMinData = getCleanDataFromDir(paste0(grandAvgDir, "/affilSubsets/inference_0.25affils_min"), methodsWithParams, methodsWithoutParams,
																				fileFilter = "inference", whichTHats = best_tHats)
		subsetMaxData = getCleanDataFromDir(paste0(grandAvgDir, "/affilSubsets/inference_0.25affils_max"), methodsWithParams, methodsWithoutParams,
																				fileFilter = "inference", whichTHats = best_tHats)
		subsetRandData = getCleanDataFromDir(paste0(grandAvgDir, "/affilSubsets/inference_0.25affils_rand"), methodsWithParams, methodsWithoutParams,
																				fileFilter = "inference", whichTHats = best_tHats)

		allData = cbind(origData, flippedData, subsetMaxData, subsetRandData, subsetMinData)
		return(t(allData))
	} else {
		baseDir = "~/Documents/dissertation/binary-ndim/data2020/newsgroups"
		groups = c('alt.atheism', 'comp.graphics', 'comp.os.ms-windows.misc', 'comp.sys.ibm.pc.hardware',
				'comp.sys.mac.hardware', 'comp.windows.x', 'misc.forsale', 'rec.autos', 'rec.motorcycles',
				'rec.sport.baseball', 'rec.sport.hockey', 'sci.crypt', 'sci.electronics', 'sci.med', 'sci.space',
				'soc.religion.christian', 'talk.politics.guns', 'talk.politics.mideast', 'talk.politics.misc',
				'talk.religion.misc')
		allData = getAvgAUCsFromFiles(topDirsToAvgAcross=file.path(baseDir, groups), 
							infDirNames=c("inf_feb13", "infFlip_feb13", "inf_subsetMax.25", "inf_subsetRand.25", "inf_subsetMin.25"))
		return(allData)
	}
}

getRealityData = function(old=F) {
	if (old) {
		# reality data, old version
		#grandAvgDir = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/400trials/grand-avgs-all6"
		# (sorry, don't have affil subsets computed for all 6)

		grandAvgDir = "/Users/lfriedl/Documents/dissertation/binary-ndim/feb2015Expts/allPairsReal-realityMining/400trials/grand-avgs"
		best_tHats = .01

		flippedData = getCleanDataFromDir(paste0(grandAvgDir, "/inferenceFlip_allto6"), methodsWithParams, methodsWithoutParams, fileFilter = "inference", whichTHats = best_tHats)
		origData = getCleanDataFromDir(paste0(grandAvgDir, "/inference_allto6"), methodsWithParams, methodsWithoutParams, fileFilter = "inference", whichTHats = best_tHats)
		dataSmallT = getCleanDataFromDir(paste0(grandAvgDir, "/inference_moreTs"), methodsWithParams, c(), fileFilter = "inference", whichTHats = .001)

		origData[1:2,] = dataSmallT             # counting on model5 and its ci_ to stay in rows 1 & 2
		flippedData[1:2,] = dataSmallT
		best_tHats = .001       # for later use

		subsetMinData = getCleanDataFromDir(paste0(grandAvgDir, "/affilSubsets/inference_0.25affils_min"), methodsWithParams, methodsWithoutParams,
																			fileFilter = "inference", whichTHats = best_tHats)
		subsetMaxData = getCleanDataFromDir(paste0(grandAvgDir, "/affilSubsets/inference_0.25affils_max"), methodsWithParams, methodsWithoutParams,
																				fileFilter = "inference", whichTHats = best_tHats)
		subsetRandData = getCleanDataFromDir(paste0(grandAvgDir, "/affilSubsets/inference_0.25affils_rand"), methodsWithParams, methodsWithoutParams,
																				fileFilter = "inference", whichTHats = best_tHats)

		allData = cbind(origData, flippedData, subsetMaxData, subsetRandData, subsetMinData)
		return(t(allData))
	} else {
		baseDir = "~/Documents/dissertation/binary-ndim/data2020/reality"
		settings = c('allPairs-appsByDay', 'allPairs-appsByWeek', 'allPairs-bluetoothByDay', 'allPairs-bluetoothByWeek',
                'allPairs-cellTowersByDay', 'allPairs-cellTowersByWeek')
		allData = getAvgAUCsFromFiles(topDirsToAvgAcross=file.path(baseDir, settings), 
							infDirNames=c("inf_feb13", "infFlip_feb13", "inf_subsetMax.25", "inf_subsetRand.25", "inf_subsetMin.25"))
		return(allData)
	}

}

# All the get*Data functions return it in the order "Orig", "Flipped", "Max", "Rand", "Min". (Swapping 2:1 comes in outer function.)
getCongressData = function(old=F) {
	if (old) {
		# congress data, old version
		grandAvgDir = "/Users/lfriedl/Documents/dissertation/binary-ndim/congress/400trials/grand-avgs"

		flippedData = getUsualDataFromDir(paste0(grandAvgDir, "/inferenceFlip_allto6"), methodsWithParams, methodsWithoutParams, fileFilter = "inference")
		origData = getUsualDataFromDir(paste0(grandAvgDir, "/inference_allto6"), methodsWithParams, methodsWithoutParams, fileFilter = "inference")
		# observe: in these grand avgs, the best tHat turns out to be the lowest, .001
		best_tHats = origData["tHat_model5LogLR",]

		# if we add affil subsets, need to get rid of these extra rows of tHats that were just added
		flippedData = flippedData[1:(length(methodsWithParams) + length(methodsWithoutParams)),]
		origData = origData[1:(length(methodsWithParams) + length(methodsWithoutParams)),]

		subsetMinData = getCleanDataFromDir(paste0(grandAvgDir, "/affilSubsets/inference_0.25affils_min"), methodsWithParams, methodsWithoutParams,
																fileFilter = "inference", whichTHats = best_tHats)
		subsetMaxData = getCleanDataFromDir(paste0(grandAvgDir, "/affilSubsets/inference_0.25affils_max"), methodsWithParams, methodsWithoutParams,
																				fileFilter = "inference", whichTHats = best_tHats)
		subsetRandData = getCleanDataFromDir(paste0(grandAvgDir, "/affilSubsets/inference_0.25affils_rand"), methodsWithParams, methodsWithoutParams,
																				fileFilter = "inference", whichTHats = best_tHats)
		allData = cbind(origData, flippedData, subsetMaxData, subsetRandData, subsetMinData)
		return(t(allData))

	} else {

		# sort of like getAvgAUCsFromFiles(), except here it's all in 1 directory, plus we need special treatment of the different s_hats.
		resultsDir = "/Users/lfriedl/Documents/dissertation/binary-ndim/data2020/congress"
		settings = c("dem110", "dem111", "dem112", "dem113", "rep110", "rep111", "rep112", "rep113")	# note hack: order doesn't matter for avgs, but 1st file determines cols returned.
		filePatterns = c("Flip", "_subsetMax.25", "_subsetRand.25", "_subsetMin.25")
		filesInDir = list.files(resultsDir)
		allResultsFiles = filesInDir[grepl("results", filesInDir)]

		resultsList = list()
		# first the normal run
		filesToGet = paste0(resultsDir, "/results_", settings, ".txt")
		infData = getAUCsFromFiles2(filesToGet)
		infData = fixBestS(infData) 	# put the best s_hat into auc_mixed_pairs_0.001, before averaging
		resultsList[["orig"]] = as.list(apply(infData, 2, mean))

		for (pat in filePatterns) {
			filesToGet = allResultsFiles[grepl(pat, allResultsFiles)]
			infData = getAUCsFromFiles2(file.path(resultsDir, filesToGet))

			# handle s_hat and avg it
			infData = fixBestS(infData)
			resultsList[[pat]] = as.list(apply(infData, 2, mean))
		}

		return(setDF(rbindlist(resultsList, fill=T)))
	}
}

# Input: has at least one column starting "auc_mixed_pairs_". In each row, find max value in these columns, and save it to the column "auc_mixed_pairs_0.001".
fixBestS = function(data) {
	mixedPairsColIndices = which(grepl("^auc_mixed_pairs_", colnames(data)))
	data$auc_mixed_pairs_0.001 = apply(data[, mixedPairsColIndices], 1, max, na.rm=T)
	return(data)
}

# caution: only has 4 files (nothing to flip), so repeats the first one
getBrightkiteData = function() {
	baseDir = "~/Documents/dissertation/binary-ndim/data2020/brightkite_6friends"
	filenames = paste(baseDir, c("results_all.txt", "results_affils_max0.25.txt", "results_affils_rand0.25.txt", "results_affils_min0.25.txt"), sep="/")
	return(getAUCsFromFiles2(filenames))

}

getGowallaData = function() {
	baseDir = "~/Documents/dissertation/binary-ndim/data2020/gowalla_6friends"
	filenames = paste(baseDir, c("results_all.txt", "results_affils_max0.25.txt", "results_affils_rand0.25.txt", "results_affils_min0.25.txt"), sep="/")
	return(getAUCsFromFiles2(filenames))

}
getAllGowallaData = function() {
	baseDir = "~/Documents/dissertation/binary-ndim/data2020/gowalla_6friends"
	filenames = paste(baseDir, c("results_all.txt", paste0("results_affils_max.", 9:1, ".txt")), sep="/")
	return(getAUCsFromFiles2(filenames))

}
plotAllGowallaData = function() {
	data = getAllGowallaData()

	par(mgp=c(1.5, .5, 0), mar=c(2.5, 2.5, 2, 1), cex=.85)
    plot(1:nrow(data), rep(0, nrow(data)), ylim=c(.5, 1), ylab="AUCs", xlab="", type='n') # set up plot w/y-axis

#	plot(1:nrow(data), 
	addDataToPlot(data, startAtX=1)
}

# Returns a data.table with columns that look like "auc_shared_size" (etc) and rows corresponding to dataFiles. 
# Columns are limited to ("auc_") methods found in the 1st file.
getAUCsFromFiles = function(dataFiles) {
	# set up variable names in columns of an empty data.table
	testData = fread(dataFiles[1])	# will have the method names in V1
	# keep only those that begin with auc
	methsToKeep = grep("auc_", testData$V1)		# vector of indices

	realData = data.table(matrix(vector(), length(dataFiles), length(methsToKeep),		# we know the row count, but ok to add them dynamically
							dimnames=list(c(), testData$V1[methsToKeep])))
	for (i in 1:length(dataFiles)) {
		newData = fread(dataFiles[i])

		# just in case there are different variables or a different order...
		for (var in colnames(realData)) {
			if (sum(newData$V1==var) == 1) {
				realData[[var]][i] = newData$V2[newData$V1==var]
			}
		}
	}
	setDF(realData)		# need it to be a data frame to act right in the plots (at least for coexisting with the other data)
	return(realData)
}

# (this version improved: not restricted to columns seen in first file)
# Returns a data.frame with columns that look like "auc_shared_size" (etc) and rows corresponding to dataFiles. 
# Columns are limited to ("auc_") methods. 
getAUCsFromFiles2 = function(dataFiles) {
	resultsList = list()
	for (i in 1:length(dataFiles)) {
		newData = fread(dataFiles[i])
		rowsToKeep = grepl("auc_", newData$V1) # keep only those that begin with auc
		toStore = newData$V2[rowsToKeep]
		names(toStore) = newData$V1[rowsToKeep]
		resultsList[[i]] = as.list(toStore)
	}	
	return(setDF(rbindlist(resultsList, fill=T)))
}

# topDirsToAvgAcross: full paths
# infDirNames: name of inference dir within each top-level dir. Each should contain a file avgs.txt. These dirNames will be the rownames of data.frame returned.
getAvgAUCsFromFiles = function(topDirsToAvgAcross, infDirNames) {
	resultsList = list()
	for (infDir in infDirNames) {
		# get this data for all top-level dirs
		filesToGet = paste0(topDirsToAvgAcross, "/", infDir, "/avgs.txt")
		infData = getAUCsFromFiles(filesToGet) 

		# avg it
		resultsList[[infDir]] = as.list(apply(infData, 2, mean))
	}
	return(setDF(rbindlist(resultsList, fill=T)))
}
