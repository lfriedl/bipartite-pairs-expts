We use 3 types of affiliations from the Reality Mining data -- apps, bluetooth events, and cell towers -- and we aggregate each of them by day and by week.

To extract and preprocess the data:

1. Run `python extractEvents.py` six times, each time uncommenting 1 `outfile` line and the corresponding `printEvents` line. (Each run creates 1 file, and each line of the file describes 1 event.)
2. Run `changeAffilEventsToVectors.pl` six times. Example command line syntax: `perl changeAffilEventsToVectors.pl appsByDay.txt appsByDay`.

The result will be 6 directories, each containing 88 files. Each file is labeled with the ID of 1 person (from `pers3.txt` to `pers106.txt`). Each line contains personID (matches the file name), date (or week number), and a list of the (deduplicated) affiliations (apps, bluetooth devices or cell towers) for that person + date.
