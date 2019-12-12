from datetime import datetime, timedelta
print("loading printEvents")

# orig data looks like this:
#>>> reality['s'][0][1]['device_date'][0][2]
#732338.70673611108
#>>> reality['s'][0][1]['device_macs'][0][2]
#array([[  6.19649792e+10],
#       [  5.83019958e+10]])
# i.e., arrays of same length for dates and macs, but each macs entry may contain multiple macs
def printBluetoothMACs(dateList, macList, personID, outfile, byWeek=False):
		devicesToday = {}
		prevDate = 1	# initialized to ridiculous
		for j in range(len(dateList)):
			date = dateList[j]
			# ignore this entry if date is 0 (there won't be any mac addresses)
			if (date == 0):
				continue

			python_date = convertDate(date).date()
			if (byWeek):
				# if we want to aggregate by week instead of day:
				python_date = str(python_date.isocalendar()[0]) + "." + str(python_date.isocalendar()[1])

			if (python_date != prevDate):
				# time to print out summary of prevDate's activity
				for macAddress in devicesToday.keys():
					outfile.write(", ".join([str(personID), str(prevDate), str(macAddress), str(devicesToday[macAddress])]) + "\n")
				prevDate = python_date
				devicesToday = {}

			macArray = macList[j]
			for macAddress in macArray:
				if (macAddress[0] in devicesToday):
					devicesToday[macAddress[0]] = devicesToday[macAddress[0]] + 1
				else:
					devicesToday[macAddress[0]] = 1

		# print things from the final day
		for macAddress in devicesToday.keys():
			outfile.write(", ".join([str(personID), str(prevDate), str(macAddress), str(devicesToday[macAddress])]) + "\n")


# Much like Bluetooth. Except each entry in 'apps' only contains one app, I believe.
# orig data looks like this:
#>>> reality['s'][0][1]['app_dates'][0][2]
#732338.69438657409
#>>> reality['s'][0][1]['apps'][0][2]
#array([u'context_log'], 
#      dtype='<U11')
def printApps(dateList, appList, personID, outfile, byWeek=False):
		appsToday = {}
		prevDate = 1	# initialized to ridiculous
		for j in range(len(dateList)):
			date = dateList[j]

			# ignore this entry if date is 0 (but does this ever happen?)
			#if (date == 0):
			#	continue

			python_date = convertDate(date).date()
			if (byWeek):
				# if we want to aggregate by week instead of day:
				python_date = str(python_date.isocalendar()[0]) + "." + str(python_date.isocalendar()[1])

			if (python_date != prevDate):
				# time to print out summary of prevDate's activity
				# the "encode" thing is because, at least once, the app got scrambled into some kind of unicode mess, which we still want
				# to print out in some form.
				for app in appsToday.keys():
					outfile.write(", ".join([str(personID), str(prevDate), app.encode("ascii", "replace"), str(appsToday[app])]) + "\n")
				prevDate = python_date
				appsToday = {}

			# add the current entry to appsToday
			thisApp = appList[j][0]
			if (thisApp in appsToday):
				appsToday[thisApp] = appsToday[thisApp] + 1
			else:
				appsToday[thisApp] = 1

		# print things from the final day
		for app in appsToday.keys():
			outfile.write(", ".join([str(personID), str(prevDate), app.encode("ascii", "replace"), str(appsToday[app])]) + "\n")


def printCellTowers(locList, personID, outfile, byWeek=False):
		towersToday = {}
		prevDate = 1	# initialized to ridiculous
		for j in range(len(locList)):
			event = locList[j]
			date = event[0]
			python_date = convertDate(date).date()
			if (byWeek):
				# if we want to aggregate by week instead of day:
				python_date = str(python_date.isocalendar()[0]) + "." + str(python_date.isocalendar()[1])

			if (python_date != prevDate):
				# time to print out summary of prevDate's activity
				for cellTower in towersToday.keys():
					areaID = int(cellTower)
					towerID = int(round(100000 * (cellTower - areaID)))		# the actual ids seem to be 5 digits
					outfile.write(", ".join([str(personID), str(prevDate), str(areaID), str(towerID), str(towersToday[cellTower])]) + "\n")
				prevDate = python_date
				towersToday = {}

			cellTower = event[1]								# note: cellTower can also be 0 for "no signal"
			if (cellTower in towersToday):
				towersToday[cellTower] = towersToday[cellTower] + 1
			else:
				towersToday[cellTower] = 1

		# print things from the final day
		for cellTower in towersToday.keys():
			areaID = int(cellTower)
			towerID = int(round(100000 * (cellTower - areaID)))		# the actual ids seem to be 5 digits
			outfile.write(", ".join([str(personID), str(prevDate), str(areaID), str(towerID), str(towersToday[cellTower])]) + "\n")


def convertDate(date):
	# Let's round off the time to be in seconds
	day_fraction = timedelta(days=date%1)
	day_fraction_rounded = timedelta(day_fraction.days, day_fraction.seconds + round(day_fraction.microseconds / float(1000000)), 0)
	python_datetime = datetime.fromordinal(int(date)) + day_fraction_rounded - timedelta(days = 366)
	return(python_datetime)

