
library(Matrix)
library(yaml)
library(jsonlite)

# try this for speed:
library(compiler)
enableJIT(1)  # higher numbers for more aggressive JITting
# (reminder: this library causes odd messages of the form "no visible global function definition...")

source("legislators.R")
legisDataDir = "/Users/lfriedl/Documents/lazer-lab/githubUnitedStates-and-otherExternal"
voteDataDir = "/Users/lfriedl/Documents/dissertation/real-data/congress/vote_data"
cosponsDataDir = "/Users/lfriedl/Documents/dissertation/real-data/congress/cosponsor_data"
# sourcefiles are in paths like: .../vote_data/111/votes/2009/h106/data.json 

# Function saves one matrix for dems, another one for reps. Rows are people, columns are votes, entries are 1 if they voted yes.
# -given a "Congress" (session) number, look in data dir to get the years for it (and num votes each year)
# -read legislator data (yaml) to get list of representatives for that session (and their parties)
# -create (sparse) Matrix to store all results
# -for each (vote) file, compute its column in the matrix
# -remove non-voting members, subdivide matrix rows by party, and save
createVoteMatrix = function(congressNum, outDir) {

	years = getYearsFromCongressNum(congressNum)

	voteBaseDir = paste(voteDataDir, congressNum, "votes", sep="/")
	yearDirs = paste(voteBaseDir, years, sep="/")
	allBills = c(list.files(yearDirs[1], full.names=T), list.files(yearDirs[2], full.names=T))
	print(paste("For Congress ", congressNum, ", found years ", paste(years, collapse=" "), ", and found ",
			length(allBills), " bills for those years", sep=""))


	# legislator data will look like this, with one row per year active:
	#  bioguide name        year    party   state   district
	#  B000944  Sherrod Brown 2006  D       OH      13

	minDateWeCareAbout = paste(min(years), "-01-04", sep="")
	legisData = getRepresentativeTermInfo(legisDataDir, minDateWeCareAbout, addOtherIDs=F)
	# gives warnings about Sablan (fine to leave as Democrat, since was Indep before) and Parker Griffith. 
	# Griffith really did change from D to R halfway through his single term (including changing his voting pattern). 
	# Let's just let him leave him be (as R), knowing that he won't end up looking like much of anyone else during the first term.

	# legislators active during our years?
	bioguides = sort(unique(legisData$bioguide[legisData$year %in% years]))
	# subdiv by party (will use later)
	bioguides_dem = sort(unique(legisData$bioguide[legisData$year %in% years & legisData$party=="D"]))
	bioguides_rep = sort(unique(legisData$bioguide[legisData$year %in% years & legisData$party=="R"]))

	# time to read and store data
	allVotes = Matrix(0, nrow=length(bioguides), ncol=length(allBills), 
					dimnames=list(bioguides, allBills))

	for (billDir in allBills) {
		message(paste("processing", billDir))
		billFile = paste(billDir, "data.json", sep="/")
		orderedBillData = readOneVoteFaster(billFile, cmptNames=bioguides)

		allVotes[, billDir] = orderedBillData
	}
	message("have all the bill data in a matrix...")


	# remove [rows and] columns that are all-zero
	# rows: non-voting members <-- no, don't remove rows, since we need rownames to match across data sets
	# columns: votes that were unanimous or didn't have a "Y" option. 
	allVotes = allVotes[, colSums(allVotes) > 0]

	# divide matrix by party
	demVotes = allVotes[rownames(allVotes) %in% bioguides_dem,]
	repVotes = allVotes[rownames(allVotes) %in% bioguides_rep,]

	# save
	outfileDem = paste(outDir, "/demVotes", congressNum, ".Rdata", sep="")
	save(demVotes, file=outfileDem)
	outfileRep = paste(outDir, "/repVotes", congressNum, ".Rdata", sep="")
	save(repVotes, file=outfileRep)

}

# Does it by looking at votes data (which we must have locally). Pulled out 
# into its own function b/c cosponsors data wants to use it too.
getYearsFromCongressNum = function(congressNum) {
	# congress ID --> years
	voteBaseDir = paste(voteDataDir, congressNum, "votes", sep="/")
	years = as.numeric(list.files(voteBaseDir))
	if (length(years) != 2) {
		stop(paste("Didn't find two 'years' in directory", voteBaseDir))
	}
	return(years)
}


# Returns a binary vector in which the names of the components are the elements of bioguides.
# Puts in 1's for people who voted "Y".
readOneVoteFaster = function(billFile, cmptNames) {
	retVec = Matrix(0, nrow=1, ncol=length(cmptNames), dimnames=list(c(), cmptNames))

	billData = fromJSON(billFile)

	# "yes" vote is listed either as "Yea" or "Aye". Govtrack explains that they mean the same thing, but the House
	# uses Yea/Nay only when voting on final passage of bills.
	indexOfYes = which(names(billData$votes) %in% c("Yea", "Aye"))

	if (length(indexOfYes) < 1) {
		# this is not expected
		message(paste("Didn't find Yes votes in bill", billFile, "having category", billData$category))
		return(retVec)
	}

	# now, billData$votes[[indexOfYes]] is a data frame. We can grab the column and set the vector in a single operation.
	voterBioguides = billData$votes[[indexOfYes]]$id
	if (length(voterBioguides) >= 1) {
		retVec[, which(colnames(retVec) %in% voterBioguides)] = 1
	}

	return(retVec[1,])
}

# Much like createVoteMatrix(); see above.
createCosponsMatrix = function(congressNum, outDir) {

	years = getYearsFromCongressNum(congressNum)

	congressDirs = paste(cosponsDataDir, congressNum, c("hr", "hjres"), sep="/")
	allBills = c(list.files(congressDirs[1], full.names=T), list.files(congressDirs[2], full.names=T))
	print(paste("For Congress ", congressNum, ", it occurs in years ", paste(years, collapse=" "), ", and we found ",
			length(allBills), " bills", sep=""))


	# (extended) legislator data will look like this, with one row per year active:
	#   bioguide               name year party state district thomasID votesmartID icpsr
	#   A000055 Robert B. Aderholt 2009     R    AL        4    01460         441 29701

	minDateWeCareAbout = paste(min(years), "-01-04", sep="")
	legisData = getRepresentativeTermInfo(legisDataDir, minDateWeCareAbout, addOtherIDs=T)

	# legislators active during our years?
	twoCols = unique(legisData[legisData$year %in% years, c("thomasID", "bioguide")])
	thomasIDs = twoCols[order(twoCols$thomasID), "thomasID"]
	matching_bioguides = twoCols[order(twoCols$thomasID), "bioguide"]

	# subdiv by party (will use later)
	bioguides_dem = sort(unique(legisData$bioguide[legisData$year %in% years & legisData$party=="D"]))
	bioguides_rep = sort(unique(legisData$bioguide[legisData$year %in% years & legisData$party=="R"]))

	# time to read and store data
	allSpons = Matrix(0, nrow=length(thomasIDs), ncol=length(allBills), 
					dimnames=list(thomasIDs, allBills))

	for (billDir in allBills) {
		message(paste("processing", billDir))
		billFile = paste(billDir, "data.json", sep="/")
		orderedBillData = readOneBill(billFile, cmptNames=thomasIDs)

		allSpons[, billDir] = orderedBillData
	}
	message("have all the bill data in a matrix...")

	# replace thomasIDs with matching bioguides, for being able to match this data against other.
	# (Important: we want the row names to match exactly.)
	rownames(allSpons) = matching_bioguides

	# remove any columns that are all-zero (don't expect any, but it's possible)
	allSpons = allSpons[, colSums(allSpons) > 0]

	# divide matrix by party
	demSpons = allSpons[rownames(allSpons) %in% bioguides_dem,]
	repSpons = allSpons[rownames(allSpons) %in% bioguides_rep,]

	# save
	outfileDem = paste(outDir, "/demCospons", congressNum, ".Rdata", sep="")
	save(demSpons, file=outfileDem)
	outfileRep = paste(outDir, "/repCospons", congressNum, ".Rdata", sep="")
	save(repSpons, file=outfileRep)
}

# Just like readOneVote, returns a binary vector, in this case with 1's for people who participate in a given bill.
# No distinction made between sponsors and cosponsors.
# In this case, cmptNames need to be Thomas IDs, because that's what the sponsorship data uses.
readOneBill = function(billFile, cmptNames) {
	retVec = Matrix(0, nrow=1, ncol=length(cmptNames), dimnames=list(c(), cmptNames))

	billData = fromJSON(billFile)

	sponsor = billData$sponsor$thomas_id
	if (!is.null(sponsor)) {
		retVec[, sponsor] = 1
	}

	# billData$cosponsors is a data frame
	if (length(billData$cosponsors) == 0) {
		return(retVec)
	}

	# ignore any rows that later withdrew their support
	cospons = billData$cosponsors[is.na(billData$cosponsors$withdrawn_at),]
	if (nrow(cospons) == 0) {
		return(retVec)
	}

	cosponsIDs = cospons$thomas_id
	retVec[, which(colnames(retVec) %in% cosponsIDs)] = 1

	return(retVec[1,])
}

