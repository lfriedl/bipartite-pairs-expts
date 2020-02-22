
use strict;
use Cwd;

# Usage: $0 <directory>* > outfile.txt 
#	where each directory has avgs.txt and vars.txt files.
# 	Ouputs (prints) a large set of R-readable vectors.

# Grabs results across multiple data directories, for a single value of t.
# This should be called from the same directory where the retrieveResults scripts are located
# @ARGV: directories containing results we want to aggregate. Note: might not be in order by t, but results need to be.
my @dirs = @ARGV;	# must be subdirs of the current directory
my @dirsRun = ();
my $origDir = getcwd();	
my %data = ();
my @statNames = ();
foreach my $dir (@dirs) {
	$dir =~ s/\/$//;
	chdir($dir);
	# expect to find avgs.txt and vars.txt files

	open(AVGS, "<avgs.txt") || die($!);
	# modification for if not all dirs may have results files
	#if (! open(AVGS, "<avgs.txt")) {
	#	warn "no data found in $dir; skipping\n";
	#	chdir($origDir);
	#	next;
	#}
	push(@dirsRun, $dir);

	my @lines, my $line;
	while ($line = <AVGS>) {
		push(@lines, $line);
	}
	close(AVGS);

	open(VARS, "<vars.txt") || die($!);
	my @varlines;
	while ($line = <VARS>) {
		push(@varlines, $line);
	}
	close(VARS);

	chdir($origDir);

	# how to get t out of current directory structure:
	my ($thisT) = ($dir =~ /t(\d*\.\d*)($|\/)/);
	#my ($thisT) = ($dir =~ /(\d*\.\d*)$/);

	# go through avgs and vars lines twice: once to get the stat names, the second time to get the numbers
	# first pass
	if (!@statNames) {
		foreach my $line (@lines) {
			my ($stat) = ($line =~ /auc/) ? ($line =~ /auc_(\S+)/) : ($line =~ /^(\S+)/);
			#$stat =~ s/\"//;	# no longer necessary -- no more quotes in avgs.txt
			$stat =~ s/_t$thisT//;	# remove value of t provided it's the true param
			push(@statNames, $stat);
		}
		foreach my $line (@varlines) {
			my ($stat) = ($line =~ /auc/) ? ($line =~ /auc_(\S+)/) : ($line =~ /^(\S+)/);
			$stat = "ci_$stat";
			push(@statNames, $stat);
		}
	}

	# second pass
	my @valsThisDir = ();
	foreach my $line (@lines, @varlines) {
		#my ($value) = ($line =~ / ([\de\.\-]+)$/);
		my ($value) = ($line =~ / ((NA)|([\de\.\-]+))$/);	# could also be NA
		push(@valsThisDir, $value);
	}

	$data{$dir} = [@valsThisDir];

}

# data is stored. Now, write it out. (Construct strings for an R program.)
my %outputLines = ();
# start them
foreach my $stat (@statNames) {
	$outputLines{$stat} = "$stat = c(";
}
$outputLines{"setting"} = "dirs = c(";
# put in values
foreach my $t (@dirsRun) {
	foreach my $statIdx (0..$#statNames) {
		$outputLines{$statNames[$statIdx]} .= $data{$t}->[$statIdx] . ", ";
	}
	$outputLines{"setting"} .= "'$t', ";
}
# close them up
foreach my $stat (@statNames) {
	$outputLines{$stat} =~ s/, $/)/;
	$outputLines{$stat} .= "\n";
}
$outputLines{"setting"} =~ s/, $/)/;
$outputLines{"setting"} .= "\n";

# Write to stdout.
print $outputLines{"setting"};
foreach my $stat (@statNames) {
	print $outputLines{$stat};
}
