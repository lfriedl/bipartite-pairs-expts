
dataDir = "~/Documents/dissertation/binary-ndim/data2020/synth"
saveFigureWithInset = function(outdirName="./") {
        pdf(paste(outdirName, "manyMethods69settings_wInset.pdf", sep="/"), width=10, height=7)
		#dev.new(width=10, height=7)	# while experimenting live; units default to "in"
		tmp = makeBigPlot(dataDir, doFlipped=T, savePDF=F)
        tmp = makeInnerPlot(dataDir, finalXSubset=23:65)
        dev.off()
}

# Returns a list of the main vars below (name => value).
# (Could modify this fn to grab CIs too.)
loadData = function(dataDir, totalNumSettings=79, doFlipped=T) {
	mainVarNames = c('jaccard', 'cosine', 'cosineIDF', 'shared_size', 'hamming', 'pearson',
				   'shared_weight11', 'shared_weight1100', 'adamic_adar', 'newman', 'mixed_pairs_0.2',
				   'weighted_corr')
	# load data!
	source(file.path(dataDir, "results_synth.txt"))

	# or other data!
	if (doFlipped) { # replace aucs with their flipped versions
		newVals = splitFlippedAndCompare(dataDir, totalNumSettings, reportDiffs=T, varsToSplice=mainVarNames)	
		for (i in 1:length(newVals)) {
			# e.g., newVals=(jaccard=c(.1, .2, .3))
			assign(names(newVals)[i], newVals[i][[1]]) 
		}
	}

	retVal = list()
	for (i in 1:length(mainVarNames)) {
		retVal[[mainVarNames[i]]] = get(mainVarNames[i])
	}
	return(retVal)
}

error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
        if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
        stop("vectors must be same length")
        arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
} 

# function copied from real-data plotting:
# default pch: 20 or 16 (bigger)? Use 16 to match the squares.
addLine = function(paramsList, xdata, ydata, name, col, lty=1, pch=16, sortOrder=sortOrder, errorBarData=NULL) {     
	lines(xdata, ydata, col=col, type='o', lty=lty, pch=pch)  #pch=arrangePlotShapes(baseShape=pch, sortOrder=sortOrder))
	paramsList$cols = c(paramsList$cols, col)
	paramsList$lty = c(paramsList$lty, lty)
	paramsList$names = c(paramsList$names, name)
	paramsList$pch = c(paramsList$pch, pch)
	if (length(errorBarData)) {
		error.bar(xdata, ydata, upper=errorBarData, length=.05)
	}
	return(paramsList)
}

arrangePlotShapes = function(baseShape=20, constShape=6, sortOrder=sortOrder) {
	numbers = paste(1:69, "t", sep="")
	shapes = rep(c(constShape, baseShape), times=c(9, 60))
	plotShapes = shapes[order(numbers)]
	return(plotShapes[sortOrder]) 
}

# returns the order by pi_vector medians
getSortOrder = function(numToKeepPerGroup = 6) {
	piSourceFile = "/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs-expts/data-prep/synthetic_data/params/pi_vectors_forExpts.Rdata"
	load(piSourceFile)	# provides us with pi_vectors, a 79x100 matrix
	# hack to make sure the constant settings sort in the desired order
	pi_vectors[6:9,] = pi_vectors[6:9,] - .00001

	pi_vectors_flipped = pmin(pi_vectors, 1 - pi_vectors)           # need to flip them for the ordering to make sense
	pi_medians_flipped = apply(pi_vectors_flipped, 1, median)

	# Want to order the data by pi_medians_flipped. e.g., return(order(pi_medians_flipped))	
	# However,
	# 1. Also want to use only the first n=6(?) of each group. (But this ordering will be applied to a vector of full length.)
	# 2. Need to return the order to use for filename variables, so need to order(numbers) too.
	settingsToKeep = rep(F, 79)
	settingsToKeep[1:9] = rep(T, 9)
	for (i in seq(from=10, to=70, by=10)) {
		settingsToKeep[i:(i+numToKeepPerGroup-1)] = rep(T, numToKeepPerGroup) 
	}
	settingsToKeepOrderedLikeFile = settingsToKeep[order(numbers)]

	pi_medians_orderedLikeFile = pi_medians_flipped[order(numbers)]
	pi_medians_sortTheFile = order(pi_medians_orderedLikeFile)	# vector of indices, tells how to reorder stuff in the file
	pi_medians_orderingFinalToKeep = pi_medians_sortTheFile[settingsToKeepOrderedLikeFile[order(pi_medians_orderedLikeFile)]]	# shorter vector of indices

	return(pi_medians_orderingFinalToKeep) # use this in brackets after a vector to get the order for plotting
}

splitFlippedAndCompare = function(dataDir, totalNumSettings, reportDiffs=T, varsToSplice=NULL) {

	# *** after manually renaming *** variables in these two files to begin with "flipped"
	source(file.path(dataDir, "results_synthFlip.txt"))

	# for splicing flipped results into regular vars (now we do actually run flipped trials for all settings)
	hasFlippedExpt = rep(T, totalNumSettings) # ordered numerically
	# recreate the way the dirs are sorted. (Note: assumes dir names for expts are the same for flipped/unflipped.)
	hasFlipped_dataOrder = hasFlippedExpt[order(numbers)]

	if (is.null(varsToSplice)) { # splice flipped versions into *all* variables
		flippedVarNames = ls(pattern="flipped_")
		varsToSplice = substr(flippedVarNames, start=9, stop=nchar(flippedVarNames))
	} else {	# just use a small list
		flippedVarNames = paste("flipped_", varsToSplice, sep="")
	}

	epsilon = .0001
	epsilonBigger = .001

	for (i in 1:length(flippedVarNames)) {
		flippedVar = get(flippedVarNames[i])
		mainVar = get(mainVarNames[i])

		# add a check monitoring whether flipped is actually higher than orig
		if (reportDiffs) {
			flippedWins = sum(flippedVar > mainVar[hasFlipped_dataOrder] + epsilon)
			flippedWinsBigger = sum(flippedVar > mainVar[hasFlipped_dataOrder] + epsilonBigger)
			flippedLosesBig = sum(mainVar[hasFlipped_dataOrder] > flippedVar + epsilonBigger)
			print(paste("method", mainVarNames[i], ": flipped wins", flippedWins, "times (of", sum(hasFlippedExpt), ");",
				"wins bigger", flippedWinsBigger, "times; loses big", flippedLosesBig, "times"))
		}

		mainVar[hasFlipped_dataOrder] = flippedVar
		assign(mainVarNames[i], mainVar)	# save the modified var
	}
	# when this is inside a function, the "assign" call doesn't affect the varnames outside the fn. Need to actually return them.
	retVal = list()
	for (i in 1:length(mainVarNames)) {
		retVal[[mainVarNames[i]]] = get(mainVarNames[i])
	}
	return(retVal)
}


makeBigPlot = function(dataDir, totalNumSettings=79, doFlipped=T, savePDF=T) {

	#doFlipped = T	# if T, uses flipped version and reports how it differs from orig
					# (Could have *just* used flipped version, except we want to compare them.)
	# Must match number of p_i settings in the data file.
	numbers = paste(1:totalNumSettings, "t", sep="")	
	# note: data[order(numbers)] makes data (orig) ordered by 1-69 match the alphabetized data in the files. class(numbers) = character().

	# Construct x axis labels
	# labeling from original figure:
	# datasetLabelsNumericOrder = c(seq(.1, .9, by=.1), rep(c("C", "B", "A", "F", "E", "D"), each=10))
	# new labeling that more closely matches median(p_i):
	datasetLabelsNumericOrder = c(seq(.1, .9, by=.1), rep(c("F", "E", "D", "C", "B", "A", "G"), each=10))
	colorsNumericOrder = rep(c("tomato", "brown", "slateblue", "green"), times=c(9, 30, 30, 10))	# colors of "A", "B", etc. labels on x-axis
	datasetLabels = datasetLabelsNumericOrder[order(numbers)]	# now to match the order of data in the file
	labelColors = colorsNumericOrder[order(numbers)]

	cl = rainbow(10)	# to match line colors in a different plot
	numToKeepPerGroup = 8
	xAxis = 1:(9 + numToKeepPerGroup * 7)	
	sortOrder = getSortOrder(numToKeepPerGroup)

	data = loadData(dataDir, totalNumSettings, doFlipped)

	if (savePDF) {
		if (doFlipped) {
			pdf("manyMethods69settings_v8_flipped.pdf", width=10, height=7)
		} else {
			pdf("manyMethods69settings_v8_noflip.pdf", width=10, height=7)
		}
	}

	# start plotting!
	par(mar=c(3,3,.5,.5), mgp=c(.5, .5, 0)) # leave extra space in the right margin, and make axis labels tight 

	minY = min(data$hamming) - .03

	# Set up the axes and labels (but plot mixedPairs last).
	# (Lines to leave for last-ish: model5, d, idf)
	plot(xAxis, data$mixed_pairs_0.2[sortOrder], type='n',
			xaxt='n',	# turn off x-axis ticks
			ylim=range(data$mixed_pairs_0.2, minY, .6, .95), 
			ylab="AUC", xlab=expression(paste("Setting of ", phi, ", ordered by median(", p[i], ")", sep="")))
	text(xAxis, rep(minY, length(xAxis)) + c(0,.01, .02), labels=datasetLabels[sortOrder], col=labelColors[sortOrder], cex=.8)
	abline(v=c(10,20,30,40,50,60), lty=3)

	paramsList = list(cols=c("black"), lty=c(1), names=c("MixedPairs"), pch=c(16))

	# (see prev versions for a maybe-better order of plotting)

	# order of plotting in which each group is together
	paramsList = addLine(paramsList, xAxis, data$weighted_corr[sortOrder], "WeightedCorr", "darkgreen", lty=1, sortOrder=sortOrder)
	paramsList = addLine(paramsList, xAxis, data$shared_weight1100[sortOrder], "SharedWeight10", "orange", lty=1, sortOrder=sortOrder)	
	paramsList = addLine(paramsList, xAxis, data$cosineIDF[sortOrder], "CosineIDF", cl[9], sortOrder=sortOrder)

	paramsList = addLine(paramsList, xAxis, data$pearson[sortOrder], "Pearson", "darkgreen", lty=4, pch=10, sortOrder=sortOrder)
	paramsList = addLine(paramsList, xAxis, data$jaccard[sortOrder], "Jaccard", "turquoise", pch=10, sortOrder=sortOrder)

	lines(xAxis, data$adamic_adar[sortOrder], col="green", type='o', pch=10) #pch=arrangePlotShapes(baseShape=15, sortOrder=sortOrder))	# line w/o legend
	paramsList = addLine(paramsList, xAxis, data$shared_weight11[sortOrder], "SharedWeight1", "orange", lty=2, pch=15, sortOrder=sortOrder)
	paramsList = addLine(paramsList, xAxis, data$newman[sortOrder], "Newman", "coral4", pch=15, sortOrder=sortOrder)
	paramsList = addLine(paramsList, xAxis, rep(-1, length(xAxis)), "Adamic / Adar", "green", pch=15)	# legend w/o line

	paramsList = addLine(paramsList, xAxis, data$shared_size[sortOrder], "SharedSize", "blue", pch=22, sortOrder=sortOrder)
	paramsList = addLine(paramsList, xAxis, data$hamming[sortOrder], "Hamming", "red", pch=22, sortOrder=sortOrder)

	# model5 last so line is visible: line w/o legend
	addLine(paramsList, xAxis, data$mixed_pairs_0.2[sortOrder], "", "black", lty=1, sortOrder=sortOrder)

	legend("topleft", legend=paramsList$names, lty=paramsList$lty, col=paramsList$cols, pch=paramsList$pch,
		bg="white",
		cex=.88, pt.cex=1.3, seg.len=2.5)	# make the box fit
		#cex=.95, pt.cex=1.3, seg.len=2.5)

	if (savePDF) {
		dev.off()
	}
}

makeInnerPlot = function(dataDir, totalNumSettings=79, finalXSubset=25:65) {

	numbers = paste(1:totalNumSettings, "t", sep="")	
	# note: data[order(numbers)] makes data (orig) ordered by 1-69 match the alphabetized data in the files. class(numbers) = character().

	# Construct x axis labels
	# labeling from original figure:
	# datasetLabelsNumericOrder = c(seq(.1, .9, by=.1), rep(c("C", "B", "A", "F", "E", "D"), each=10))
	# new labeling that more closely matches median(p_i):
	datasetLabelsNumericOrder = c(seq(.1, .9, by=.1), rep(c("F", "E", "D", "C", "B", "A", "G"), each=10))
	colorsNumericOrder = rep(c("tomato", "brown", "slateblue", "green"), times=c(9, 30, 30, 10))	# colors of "A", "B", etc. labels on x-axis
	datasetLabels = datasetLabelsNumericOrder[order(numbers)]	# now to match the order of data in the file
	labelColors = colorsNumericOrder[order(numbers)]

	cl = rainbow(10)	# to match line colors in a different plot
	numToKeepPerGroup = 8
	xAxisFull = (1:(9 + numToKeepPerGroup * 7)  )
	xAxis = xAxisFull[finalXSubset]
	sortOrder = getSortOrder(numToKeepPerGroup)

	data = loadData(dataDir, totalNumSettings, doFlipped=F)

	oldY = c(.63, .93)		# range of values we want to plot
	newY = c(.61, .75)		# where we're going to put them (in existing plot)
	shiftFn = function(y) { (y - oldY[1]) * (newY[2] - newY[1]) / (oldY[2] - oldY[1]) + newY[1] }	# converts actual values into box (keeping existing plot/coords)

	# set up new "axes" in same plot, w/o changing coord system. 
	#axis(side=2, pos=finalXSubset[1] - 1, cex=.8,
	axis(side=2, pos=finalXSubset[1] - 1 + .25, cex=.8,		# hack because I like having finalXSubset start at 23, but then the labels collide unpleasantly with the dotted line
		# what to write at tick marks
		# let's have the y-values c(.65, .7, .75, .8, .85, .9) if they fit
		labels=seq(from=.65, to=.9, by=.05),
		at=shiftFn(seq(from=.65, to=.9, by=.05))		# position of tick marks. also dictates bounds of the (axis's) line segment.
	)
	segments(x0=finalXSubset[1] - 1, x1=66, y0=shiftFn(.62))		# blank "x axis"
	#segments(x0=finalXSubset[1] - 1, x1=66, y0=shiftFn(.93))		# maybe a top line too? nah, not enough room.
	text(x=45, y=shiftFn(.65), labels="Without Flipping")

    addLine(paramsList, xAxis, shiftFn(data$weighted_corr[sortOrder][finalXSubset]), "WeightedCorr", "darkgreen", lty=1, sortOrder=sortOrder)
    addLine(paramsList, xAxis, shiftFn(data$shared_weight1100[sortOrder][finalXSubset]), "SharedWeight10", "orange", lty=1, sortOrder=sortOrder)
    addLine(paramsList, xAxis, shiftFn(data$cosineIDF[sortOrder][finalXSubset]), "CosineIDF", cl[9], sortOrder=sortOrder)

    addLine(paramsList, xAxis, shiftFn(data$pearson[sortOrder][finalXSubset]), "Pearson", "darkgreen", lty=4, pch=10, sortOrder=sortOrder)
    addLine(paramsList, xAxis, shiftFn(data$jaccard[sortOrder][finalXSubset]), "Jaccard", "turquoise", pch=10, sortOrder=sortOrder)

    lines(xAxis, shiftFn(data$adamic_adar[sortOrder][finalXSubset]), col="green", type='o', pch=10) #pch=arrangePlotShapes(baseShape=15, sortOrder=sortOrder)) # line w/o legend
    addLine(paramsList, xAxis, shiftFn(data$shared_weight11[sortOrder][finalXSubset]), "SharedWeight1", "orange", lty=2, pch=15, sortOrder=sortOrder)
    addLine(paramsList, xAxis, shiftFn(data$newman[sortOrder][finalXSubset]), "Newman", "coral4", pch=15, sortOrder=sortOrder)

    addLine(paramsList, xAxis, shiftFn(data$shared_size[sortOrder][finalXSubset]), "SharedSize", "blue", pch=22, sortOrder=sortOrder)
    addLine(paramsList, xAxis, shiftFn(data$hamming[sortOrder][finalXSubset]), "Hamming", "red", pch=22, sortOrder=sortOrder)

    # model5 last so line is visible
    addLine(paramsList, xAxis, shiftFn(data$mixed_pairs_0.2[sortOrder][finalXSubset]), "", "black", lty=1, sortOrder=sortOrder)

#	par(new=T)
#	# now that the data is where we want, put in the plot box & axes
#    plot(xAxis, shiftFn(data$mixed_pairs_0.2[sortOrder][finalXSubset]), type='n',
#            xaxt='n',   # turn off x-axis ticks
#			yaxt='n', xlab="", ylab="")
#			
#            #ylim=range(.63, .93),
#            ylab="Without Flipping", xlab="")

# testing...
}
