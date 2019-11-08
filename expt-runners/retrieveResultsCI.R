
computeAvgsVars = function(directory=".") {
	prevWD = setwd(directory)

	filesInDir = list.files(path=".", pattern="^results[0-9]+\\.txt$")		# will have the form paste0("results", i, ".txt")

	i = 1
	for (filename in filesInDir) {
		x = read.table(filename, as.is=T)

		# special handling in case rows differ across files
		if (i == 1) {
			# store rownames, and make sure all future ones contain at least these
			rownamesUsed = x[,1]
			# results go here. row = measure, column = results of 1 run
			dataf = data.frame(row.names=rownamesUsed)
		} 

		# find the positions of rownamesUsed in current file
		rownumsWanted = match(rownamesUsed, x[,1])
		if (sum(is.na(rownumsWanted)) > 0) {
			#stop(paste("Didn't find all the usual rownames in file", filename))
			warning(paste("Didn't find all the usual rownames in file", filename))

			# update the data frame to use only these rows from now on 
			rownamesUsed = intersect(rownamesUsed, x[,1])
			dataf = dataf[rownames(dataf) %in% rownamesUsed,]

			rownumsWanted = match(rownamesUsed, x[,1])
		}

		x = x[rownumsWanted,]
		
		if (0) {
			#rowsToKeep = (substr(x[,1], 1,4)=="auc_" | substr(x[,1], 1,4)=="pred" | substr(x[,1], 1,4)=="entr")
			#x = x[rowsToKeep,]
		} 
		dataf[,i] = as.numeric(x[,2])
		i = i + 1
	}


	avgs = apply(dataf, 1, mean, na.rm=T)
	names(avgs) = x[,1]
	write.table(avgs, file="avgs.txt", col.names=F, quote=F)

	vars = apply(dataf, 1, var, na.rm=T)
	numNonNAs = apply(dataf, 1, countNonNAs)
	confIntervals = 1.96 * sqrt(vars / numNonNAs)
	names(confIntervals) = x[,1]
	write.table(confIntervals, file="vars.txt", col.names=F, quote=F)
	write(paste("Computed averages for", length(filesInDir), "results files in", directory), stderr())
	setwd(prevWD)
}

countNonNAs = function(v) {
	return(sum(!is.na(v)))
}

