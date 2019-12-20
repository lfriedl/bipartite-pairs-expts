Data obtained from <https://www.github.com/unitedstates> project, also known as 
<https://theunitedstates.io/>.

## Legislators
Files contain biographical info plus the dates they're in office. Need both files to get the full data. The function `getRepresentativeTermInfo()` in `legislators.R` does this.

These files were downloaded from <https://github.com/unitedstates/congress-legislators> on Sept 24, 2014. 

## Votes and bill sponsorships

The repository <https://github.com/unitedstates/congress> has tools (scrapers) to let people fetch this data on demand.

The steps I ran were something like:

1. Download that repo.
2. Following its instructions, run inside a virtualenv:

```
> bash
> mkvirtualenv congress
> workon congress
> pip install -r requirements.txt
```

3. Vote data for the House of Representatives, for Congresses 110-113. 

```
> ./run votes --congress=110 --session=2007 --chamber=house
> ./run votes --congress=110 --session=2008 --chamber=house
[...]
```
Downloaded in May 2015. The file `congress-votes.tgz` is an archive of the resulting directory.

4. Bill sponsorship data. Includes House bills (H.R.) and House joint resolutions (H.J.Res.) for Congresses 110-113.

The file `congress-bill-sponsors.tgz` is an archive of the data collected in August 2014. 

At the time, I used rsync to get the data from govtrack.us; a file inside `congress-bill-sponsors.tgz` shows the rsync commands. (Using rsync directly was faster and transferred less data than running commands of the form `./run bills --bill_type=hr --congress=110`.) 

Currently (2019), data from prior to the 113th Congress is no longer available via govtrack.us or (as a result) <https://github.com/unitedstates/congress>.
