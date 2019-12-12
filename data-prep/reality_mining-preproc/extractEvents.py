import scipy.io
from datetime import datetime, timedelta
import printEvents

reality = scipy.io.loadmat('realitymining.mat')

#outfile = open("cellTowersByDay.txt", "w")
#outfile = open("cellTowersByWeek.txt", "w")
#outfile = open("bluetoothDevicesByDay.txt", "w")
#outfile = open("bluetoothDevicesByWeek.txt", "w")
outfile = open("appsByDay.txt", "w")
#outfile = open("appsByWeek.txt", "w")

for i in range(len(reality['s'][0])):
    person = reality['s'][0][i]     # person_id is i+1
    
    # key critera for deciding which records have actual data
    if (person['surveydata'].shape[0] == 0):
        continue
    
    #if (person['locs'].shape[0] > 0 and person['locs'].shape[1] > 0):
    #    #printEvents.printCellTowers(person['locs'], i+1, outfile)
    #    printEvents.printCellTowers(person['locs'], i+1, outfile, True)
    
    #if (len(person['device_date']) > 0 and len(person['device_macs']) > 0):
    #    #printEvents.printBluetoothMACs(person['device_date'][0], person['device_macs'][0], i+1, outfile)
    #    printEvents.printBluetoothMACs(person['device_date'][0], person['device_macs'][0], i+1, outfile, True)
    
    if (len(person['apps']) > 0 and len(person['app_dates']) > 0):
        printEvents.printApps(person['app_dates'][0], person['apps'][0], i+1, outfile)
        #printEvents.printApps(person['app_dates'][0], person['apps'][0], i+1, outfile, True)

outfile.close()

