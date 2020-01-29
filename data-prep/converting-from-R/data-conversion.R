library(Matrix)

convertDataFile = function(infile, outdir, fileNum, varsWanted = "all") {
	varsFound = load(infile)
	if ("all" %in% varsWanted) {
		varsWanted = c("dataItems", "phi", "numPositivePairs")
		# varsFound could also contain tForModel5, but I don't think we'll ever need to convert that
	}
	if ("dataItems" %in% varsWanted) {
		if ("dataItems" %in% varsFound) {
			outfile = file.path(outdir, paste0("data", fileNum, "_adjMat", ".mtx"))
			writeMM(dataItems, outfile)
			system(paste("gzip", outfile))
		} else {
			warning("no dataItems found in ", infile)
		}
	}
	if ("phi" %in% varsWanted) {
		if ("phi" %in% varsFound) {
			outfile = file.path(outdir, paste0("data", fileNum, "_phi", ".txt.gz"))
			outConn = gzfile(outfile)
			write(as.vector(phi@componentProbs), outConn, ncolumns=1)
			close(outConn)
		} else {
			warning("no phi found in ", infile)
		}
	}
	if ("numPositivePairs" %in% varsWanted) {
		if ("numPositivePairs" %in% varsFound) {
			outfile = file.path(outdir, paste0("data", fileNum, "_numPos", ".txt"))
			write(numPositivePairs, outfile)
		} else {
			warning("no numPositivePairs found in", infile)
		}
	}

}

converDirDataFiles = function(inDir, outSubdir, whichVars="all") {
	# list files of pattern data*.Rdata in inDir
	files = list.files(path=inDir, pattern="^data.*Rdata$")

	# create outSubdir
	if (!dir.exists(file.path(inDir, outSubdir))) {
		dir.create(file.path(inDir, outSubdir))
	}

	# foreach file, construct names of new files, save adj matrix (and maybe other vars) there
	for (file in files) {
		fileNum = substr(file, 5, nchar(file) - 6)
		print(paste0("converting #", fileNum))
		convertDataFile(file.path(inDir, file), file.path(inDir, outSubdir), fileNum, whichVars)

	}

}

# Just a stub for now
# outfile: send it as .csv, but it'll actually be saved as .csv.gz
convertScoredPairsFile = function(infile, outfile, oldFormat=F) {
	varsFound = load(infile)
	if ("scoredPairs" %in% varsFound) {
		# keep any of the following columns
		if (oldFormat) {
			colsWanted = c("label", "m", "d", "adamicAdar", "unweightedCosine", "idf", "jaccardSim", "one_over_log_p_m11", "one_over_log_p_m1100", "model5LogLR_t0.01")
		} else {
			colsWanted = c("label", "m", "d", "one_over_log_p_m11", "one_over_log_p_m1100", "model5LogLR_t0.01", "pearsonWeighted", "pearsonCorrZero", "adamicFixed", 
							"newmanCollab", "unweighted_cosineZero", "idfZero", "jaccardSimZero")
		}  
		colsToKeep = intersect(colnames(scoredPairs), colsWanted)
		write.csv(scoredPairs[, colsToKeep], file=outfile, row.names=F)
		system(paste("gzip", outfile))

	} else {
		print("Did not find 'scoredPairs' in the infile")
	}

}

# outfile: send it as .csv.gz
getPhiFromScoredPairsFile = function(infile, outfile) {
	varsFound = load(infile)
	if (! "phi" %in% varsFound) {
		warning("Did not find 'phi' in the infile")
		return()
	}
	outConn = gzfile(outfile)
	write(as.vector(phi@componentProbs), outConn, ncolumns=1)
	close(outConn)

}


# logs
thingsActuallyConverted = function() {
	# on achtung
	converDirDataFiles("/home/lfriedl/ASOUND-bipartite/expts/newsgroups/alt.atheism", "converted-data")	# and result is much smaller than orig! 21M, vs an estimated 83M.


	# locally, one-offs
	convertDataFile(infile="/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/reality_appweek_50/data50.Rdata", 
					outdir="/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/reality_appweek_50/", fileNum=50, varsWanted = "all")
	
	# results file on local hard drive
	convertScoredPairsFile("/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/ng_aa_data2/data2-inferenceFeb.Rdata", 
						   "/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/ng_aa_data2/data2-inferenceFeb.csv", oldFormat=T)
	convertScoredPairsFile("/Users/lfriedl/from-other-machines/from-swarm-aug2017/storage/binary-ndim-expts/realDataAllPairs75/alt.atheism/inferenceFlip_allto6/data2.Rdata",
						   "/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/ng_aa_data2/data2-inferenceFlip.scoredPairs.csv", oldFormat=F)
	convertScoredPairsFile(infile="/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/reality_appweek_50/data50-inference-allto6.Rdata",
						   outfile="/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/reality_appweek_50/data50-inference-allto6.scoredPairs.csv")
	convertScoredPairsFile(infile="/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/reality_appweek_50/data50-inferenceFlip.Rdata",
						   outfile="/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/reality_appweek_50/data50-inferenceFlip.scoredPairs.csv")
	getPhiFromScoredPairsFile(infile="/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/reality_appweek_50/data50-inference-allto6.Rdata",
							outfile="/Users/lfriedl/Documents/dissertation/binary-ndim/bipartite-pairs/tests/reality_appweek_50/data50-inference-allto6.phi.csv.gz")
}

# With newsgroups, most expts were done with "1/4 affils", to get rid of ceiling effects. 
specialCaseNG = function() {

	# Shrink the already-saved adj matrix to these affils. (I don't particularly need the phi, since I always re-estimate it from adj matrix anyway.)
	library(Matrix)
	adj = readMM("tests/ng_aa_data2/data2_adjMat.mtx.gz")

	affilsToKeep = c(T, F, F, F)
	adj2 = adj[, affilsToKeep]

	outfile = "tests/ng_aa_data2/data2_adjMat_quarterAffils.mtx"
	writeMM(adj2, outfile)
	system(paste("gzip", outfile))

}
