
use strict;

my $Usage = "$0 inFile outDir [minOccurrences]\n";
# Takes file of events -- 1 row per person, time, and id -- and changes to
# files in a directory -- 1 file per person, 1 row per time, with a list of ids.

unless (scalar(@ARGV) >= 2) { die $Usage; }
my $infile = shift(@ARGV);
my $outdir = shift(@ARGV);
my $minOccurrences = 0;
if (@ARGV) {
	$minOccurrences = shift(@ARGV);
}

unless (-e $outdir || mkdir($outdir)) { die("Couldn't make out directory: $!"); }
open(IN, "<$infile") || die($!);
my %personData;
my $prevPerson;
my $line;
while ($line = <IN>) {
	my @fields = split(", ", $line);
	my $person = $fields[0];

	if ($person != $prevPerson) {
		# write out %personData from last time
		saveToFile($outdir, $prevPerson, \%personData) if ($prevPerson);

		%personData = ();
		$prevPerson = $person;
	}

	my $date = $fields[1];
	my $occurrences = $fields[-1];	# final one
	next unless ($occurrences >= $minOccurrences);
	my $affil;
	if (scalar(@fields) == 5) {
		# cell towers: paste areaID and towerID back together
		$affil = $fields[2] . "." . $fields[3];
	} else {
		# bluetooth id
		$affil = $fields[2];
		# get rid of that trailing ".0" already
		$affil =~ s/\.0$//;
	}
	$personData{$date}->{$affil} = 1;
}
saveToFile($outdir, $prevPerson, \%personData);
print "all done!\n";


sub saveToFile {
	my $outdir = shift(@_);
	my $prevPerson = shift(@_);
	my $personData = shift(@_);

	my $outfile = "$outdir/pers$prevPerson.txt";
	open(OUT, ">$outfile") || die($!);
	print OUT "person\tdate\taffils\n";
	foreach my $prevDate (sort { cmp_in_order($a, $b); } keys %$personData) {
		print OUT "$prevPerson\t$prevDate\t" . 
			join(",", sort { cmp_in_order($a, $b); } keys %{$personData->{$prevDate}} ) . "\n";
	}
	close(OUT);
}

# alternative comparison function for sorting, which makes "asdf" come before "asd1"
sub cmp_in_order {
        my ($a_rem, $b_rem) = @_;

        my ($a_now, $b_now, @temp, $a_tmp, $b_tmp);
        while ($a_rem || $b_rem) {
                ($a_now, $a_tmp) = ($a_rem =~ /^(\D*)(.*)$/);
                ($a_now, $a_tmp) = ($a_rem =~ /^(\d*)(.*)$/) unless ($a_now);
                $a_rem = $a_tmp;
                ($b_now, $b_tmp) = ($b_rem =~ /^(\D*)(.*)$/);
                ($b_now, $b_tmp) = ($b_rem =~ /^(\d*)(.*)$/) unless ($b_now);
                $b_rem = $b_tmp;

                next if ($a_now eq $b_now);

                # numeric?
                if ($a_now =~ /^\d+$/ && $b_now =~ /^\d+$/) {
                        return ($a_now <=> $b_now);
                }
                # letters come first if each is present
                elsif ($a_now =~ /^\d+$/ || $b_now =~ /^\d+$/) {
                        if ($a_now =~ /^\d+$/) {
                                return 1;       # $b first
                        }
                        return -1;
                }

                # the usual: alphabetical
                else {
                        return ($a_now cmp $b_now);
                }
        }

        # both null
        return 0;

}
