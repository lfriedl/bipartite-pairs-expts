
source("data-conversion.R")

# newDirTopLevel: folder all the expt settings and trials will go under.
#	Normally, the result of this call will go into a folder just under this. But if there's an oldTar file, then the result can be several levels of folders down.
# oldExptPath: under the old folder, the setting we want to convert.
# oldTar: if all the old data is stored (compressed) in a single tar file, its path
# 
# new location for data will be
convertDataDirToTar = function(newDirTopLevel, oldExptPath, oldTar=NULL, clobberOk=F) {
	# oldExptPath: if there's a tar file, new location includes the full path of oldExptPath. If there's no tar file, new location includes just its final dirname.

	if (! is.null(oldTar)) {
		newExptPath = file.path(newDirTopLevel, oldExptPath)
	} else {
		newExptPath = file.path(newDirTopLevel, basename(oldExptPath))
	}

	if (dir.exists(newExptPath)) {
		if (clobberOk) {
			unlink(newExptPath, recursive=T)
		} else {
			warning("directory ", newExptPath, " already exists; remove it or set clobberOk=T")
			return()
		}
	}


	# copy over the top-level data*.Rdata files
	if (! is.null(oldTar)) {
		# tell tar to get everything except "inf*" subdirectories (for "inference")
		tarArgs = paste("--exclude='inf*' -zf", oldTar, "-C", newDirTopLevel, "-x", oldExptPath)
		errCode = system2("tar", strsplit(tarArgs, " ")[[1]])
	} else { # make a copy
		dir.create(newExptPath)
		# tell cp we want files that look like "data*"
		cpArgs = paste("-p", file.path(oldExptPath, "data*"), newExptPath)
		errCode = system2("cp", strsplit(cpArgs, " ")[[1]])
	}
	if (errCode) {
		warning("convertDataDirToTar(): couldn't find source data")
		return()
	}

	# extract whatever variables are inside (any of "dataItems", "phi", "numPositivePairs") to simpler file formats
	converDirDataFiles(newExptPath, ".")

	# remove the Rdata files
	system2("rm", file.path(newExptPath, "*Rdata"))

	# tar up all those new files
	tarArgs = paste("-czf", file.path(newExptPath, "..", "allInputDataFiles.tgz"), "-C", newExptPath, "--sort=name", ".")
	system2("tar", strsplit(tarArgs, " ")[[1]])
	# note: wildcard expansion wasn't working (due to -C, probably), so created the tar file outside the dir, one level up. Now move it.
	system2("mv", c(file.path(newExptPath, "..", "allInputDataFiles.tgz"), newExptPath))

	# remove the individual data files
	system2("rm",  file.path(newExptPath, "data*_*"))

}
