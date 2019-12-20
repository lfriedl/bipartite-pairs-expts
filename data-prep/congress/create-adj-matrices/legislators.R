
library(yaml)


# We'll return a data frame with one row per rep per year:
# 
#  bioguide name	year	party	state	district
#  B000944  Sherrod Brown 2006	D	OH	13
#
# If addOtherIDs=T, it has these columns. 
#   bioguide               name year party state district thomasID votesmartID icpsr
#   A000055 Robert B. Aderholt 2009     R    AL        4    01460         441 29701
#   A000055 Robert B. Aderholt 2010     R    AL        4    01460         441 29701
#   A000055 Robert B. Aderholt 2011     R    AL        4    01460         441 29701

# In case of errors: "replacement has length zero" can happen if any field we try to grab isn't present. Could
# test them more thoroughly with is.null().
getRepresentativeTermInfo = function(legisDir, minDateWeCareAbout, addOtherIDs=F, byQuarterNotYear=F) {
	legisCurr = yaml.load_file(paste(legisDir, "/legislators-current.yaml", sep=""))
	legisHist = yaml.load_file(paste(legisDir, "/legislators-historical.yaml", sep=""))

	# Most legislators serve for 2 years
	legisBioguide = vector(mode="character", length=2 * (length(legisCurr) + length(legisHist)))
	legisName = vector(mode="character", length=2 * (length(legisCurr) + length(legisHist)))
	legisYear = vector(mode="numeric", length=2 * (length(legisCurr) + length(legisHist)))
	legisParty = vector(mode="character", length=2 * (length(legisCurr) + length(legisHist)))
	legisState = vector(mode="character", length=2 * (length(legisCurr) + length(legisHist)))
	legisDistrict = vector(mode="numeric", length=2 * (length(legisCurr) + length(legisHist)))
	legisThomas = vector(mode="numeric", length=2 * (length(legisCurr) + length(legisHist)))
	legisVotesmart = vector(mode="numeric", length=2 * (length(legisCurr) + length(legisHist)))
	legisICPSR = vector(mode="numeric", length=2 * (length(legisCurr) + length(legisHist)))
	count = 1

	for (record in c(legisCurr, legisHist)) {
		# bioguide & name
		bioguide = record$id$bioguide
		if (is.null(record$name$official_full)) {
			name = paste(record$name$first, record$name$last)
		} else {
			name = record$name$official_full
		}

		# other ids
		if (addOtherIDs) {
			thomas = record$id$thomas
			if (!is.null(record$id$votesmart)) {
				votesmart = record$id$votesmart
			} else {
				votesmart = NA
			}

			if (!is.null(record$id$icpsr)) {
				icpsr = record$id$icpsr
			} else {
				icpsr = NA
			}
		}

		currYear = 0	# re-initialize for each rep
	
		# collect data for terms as a representative within our time period
		for (term in record$terms) {
			if (term$type == "rep") {
				state = term$state
				district = term$district
				party = substr(term$party, 1, 1)	# just the first letter

				if (term$end >= minDateWeCareAbout) {
					startYear = as.numeric(substr(term$start, 1, 4))
					endYear = as.numeric(substr(term$end, 1, 4))

					# usually endYear is in January, two years after startYear, 
					# but we don't want to include that final year iff it was early January
					if (substr(term$end, 6, 9) == "01-0") {
						endYear = endYear - 1
					}

					if ("party_affiliations" %in% names(term)) {
						print(paste("warning from getRepresentativeTermInfo:", name, "switched parties within a term; needs special handling"))
						# data will probably contain a party anyway, but party_affiliations contains additional info we're ignoring here
					}
			
					# Special handling: if a rep comes in mid-term, there's a possibility we'd try to list the same year twice. 
					# Avoid this by checking previously used currYear for a given rep.
					currYear = max(currYear, startYear)		# currYear has already been incremented from its last use
					while (currYear <= endYear) {
						legisBioguide[count] = bioguide
						legisName[count] = name
						legisYear[count] = currYear
						legisParty[count] = party
						legisState[count] = state
						legisDistrict[count] = district

						if (addOtherIDs) {
							legisThomas[count] = thomas
							legisVotesmart[count] = votesmart 
							legisICPSR[count] = icpsr 
						}
						
						count = count + 1
						currYear = currYear + 1
					}
				}
			}
		}
	}

	legisBioguide = legisBioguide[1:(count-1)]
	legisName = legisName[1:(count-1)]
	legisYear = legisYear[1:(count-1)]
	legisParty = legisParty[1:(count-1)]
	legisState = legisState[1:(count-1)]
	legisDistrict = legisDistrict[1:(count-1)]
	legisThomas = legisThomas[1:(count-1)]
	legisVotesmart = legisVotesmart[1:(count-1)]
	legisICPSR = legisICPSR[1:(count-1)]

	if (addOtherIDs) {
		legisData = data.frame(bioguide=legisBioguide, name=legisName, year=legisYear, 
							   party=legisParty, state=legisState, district=legisDistrict, 
							   thomasID=legisThomas, votesmartID=legisVotesmart, icpsr=legisICPSR)
	} else {
		legisData = data.frame(bioguide=legisBioguide, name=legisName, year=legisYear, 
							   party=legisParty, state=legisState, district=legisDistrict)
	}

	if (byQuarterNotYear) {
		colnames(legisData)[which(colnames(legisData)=="year")] = "quarter"
	}
		
	return(legisData)

}


