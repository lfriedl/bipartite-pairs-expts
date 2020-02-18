
source("prepSamplesRealData.R", chdir=T)						# for createRealDataReality()
source("../converting-from-R/prepExptDirWithTar.R", chdir=T)	# for convertDataDirToTar()

exptDirsBaseR = "/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new1000R"
exptDirsBase = "/home/lfriedl/ASOUND-bipartite/expts/reality-mining/new1000"
realityDataDirBase = "/home/lfriedl/ASOUND-bipartite/data-prep/reality-mining/data/final-bipartite"
outDirForMatrices = "/home/lfriedl/ASOUND-bipartite/data-prep/reality-mining/realityAdjacencyMatrices"
groups = c("allPairs-appsByDay",  "allPairs-appsByWeek",  "allPairs-bluetoothByDay",  "allPairs-bluetoothByWeek",  "allPairs-cellTowersByDay",  "allPairs-cellTowersByWeek")
numPositivePairs = 5 
numSingletons = 75		# (Whoops--had planned on 65 to match synthetic, but this makes sense too. Total population is 88, and this gives us 85 people every time.)

for (gp in groups) {
	createRealDataReality(exptDirFullPath = file.path(exptDirsBaseR, gp),
						  numPositivePairs, numSingletons, minTrialNum=1, maxTrialNum=1000,		# test with 1 trial
						  realityDataDir=file.path(realityDataDirBase, substr(gp, 10, nchar(gp))),
						  saveBipartiteOutfileBase=file.path(outDirForMatrices, gp))
	convertDataDirToTar(newDirTopLevel=exptDirsBase, oldExptPath=file.path(exptDirsBaseR, gp))
}
