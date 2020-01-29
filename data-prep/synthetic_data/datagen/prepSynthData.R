
library(Matrix)
source("reloadClassDefns.R")
source("../../converting-from-R/prepExpt.R", chdir=T)	# provides: convertDataDir(newDirTopLevel, oldExptPath, oldTar=NULL, clobberOk=F)

genAllSynthData = function() {
	outdir = "/home/lfriedl/ASOUND-bipartite/expts/synth/new1000"
	numTrials = 1000
	for (i in 1:79) {
		genSynthDataOneSetting(outdir, i, numTrials)
	}

}

# Function to freshly generate all synthetic data into a python-readable format.
# For each setting, generates data in Rdata format, converts it to the python format, and deletes Rdata version (since it has many large files).
genSynthDataOneSetting = function(outdir, settingNum, numTrials) {

	load("../params/pi_vectors_forExpts.Rdata")		# to get pi_vectors

	settingPath = paste0("pi", settingNum, "t0.2")
	path_for_R = file.path(outdir, "R", settingPath)
	#path_for_python = file.path(outdir, settingPath)
	dir.create(path_for_R, recursive=T)
	#dir.create(path_for_python, recursive=T)

	# generate data files
	for (trial in 1:numTrials) {
		genSynthData(pi_vectors[settingNum,], numPositivePairs=5, constructAllPairsFromMDocs=75, tForModel5=.2,
					outputDir=path_for_R, outfileNum=trial)
	}

	# convert
	convertDataDir(outdir, path_for_R)

	# delete the Rdata
	unlink(path_for_R, recursive=T)

}


genSynthData = function(pi_vector, numPositivePairs, constructAllPairsFromMDocs=0, tForModel5,
                    outputDir, outfileNum=0) {

	totNumPts = constructAllPairsFromMDocs
    numPosPts = 2 * numPositivePairs
    numNegPts = totNumPts - numPosPts
    
    if (numNegPts <= 0) {
        warning("incompatible combo of MDocs and numPositivePairs")
        return()
    }

    # create phi
    phi = new("multivariateBernoulli", componentProbs=pi_vector)
    
    # housekeeping
    tic=proc.time()[3]
    print(paste("generating synthetic data with params: pi_vector of length", length(pi_vector), 
                ", t =", tForModel5, ",", numPositivePairs, "positives"))
    
    # generate labeled data: 
    # create positive distribution object and pairs from it
    posDist = new("positiveBinaryPairDistribution5", singleton_phi=phi, t=tForModel5)
    listOfPosPairs = generateItem(posDist, numPositivePairs)

    # construct the singletons
    # This call returns a matrix, where each point is a row
    singletons = generateItem(phi, numNegPts)

    print(paste("generated data; tot time = ", proc.time()[3] - tic, "secs"))

    # change pairs + singletons into a matrix of dataItems, which is what we want to store
    dataItems = Matrix(0, nrow=totNumPts, ncol=length(pi_vector))
	for (posIdx in 1:numPositivePairs) {
		dataItems[2*posIdx-1, ] = listOfPosPairs[[posIdx]]@obs1
		dataItems[2*posIdx, ] = listOfPosPairs[[posIdx]]@obs2
	}
	dataItems[(1+2*numPositivePairs):totNumPts,] = singletons

    # save it!    
    save(dataItems, numPositivePairs, phi, tForModel5, 
         file=paste(outputDir, "/data", outfileNum, ".Rdata", sep=""))
}

