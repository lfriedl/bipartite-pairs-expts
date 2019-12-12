
use strict;
#use warnings;

my $Usage = "perl $0 <directory> <outfile> [file1Index (1-based)]";
if (@ARGV < 2) {
	die $Usage;
}
my $dir = $ARGV[0];
my $outfile = $ARGV[1];
if (@ARGV == 3) {
	my $i = $ARGV[2];
	$outfile .= "_$i";
	findLongestSubstringAllPairs($dir, $outfile, $i);
} else {
	# will do all pairs this way
	findLongestSubstringAllPairs($dir, $outfile);
}


# modified from https://en.wikibooks.org/wiki/Algorithm_implementation/Strings/Longest_common_substring#Perl
sub longestCommonWordSubstring {
  my ($file1, $file2) = @_; 
  my $l_length = 0; # length of longest common substring

  my @words1 = ("", getWordsFromFile($file1)); # $str1 as array of words, indexed from 1
  my @words2 = ("", getWordsFromFile($file2)); # $str2 as array of words, indexed from 1
  my @lc_suffix; # "longest common suffix" table
  my %substrings; # list of common substrings of length $l_length
 
  for my $n1 ( 1 .. scalar(@words1) ) { 
    for my $n2 ( 1 .. scalar(@words2) ) { 
      if ($words1[$n1] eq $words2[$n2]) {
        # We have found a matching word. Is this the first matching word, or a
        # continuation of previous matching words? If the former, then the length of
        # the previous matching portion was undefined; set it to zero.
        $lc_suffix[$n1-1][$n2-1] ||= 0;
        # In either case, declare the match to be one longer than the match of
        # words preceding this word.
        $lc_suffix[$n1][$n2] = $lc_suffix[$n1-1][$n2-1] + 1;
        # If the resulting substring is longer than our previously recorded max length ...
        if ($lc_suffix[$n1][$n2] > $l_length) {
          # ... we record its length as our new max length ...
          $l_length = $lc_suffix[$n1][$n2];
          # ... and clear our result list of shorter substrings.
          %substrings = ();
        }
        # If this substring is equal to our longest ...
        if ($lc_suffix[$n1][$n2] == $l_length) {
          # ... add it to our list of solutions.
          #push @substrings, substr($str1, ($n1-$l_length), $l_length);
          $substrings{join(" ", @words1[($n1-$l_length+1)..$n1])} = 1;
        }
      }
    }
  }   
 
  return ($l_length, keys(%substrings));
}

# returns an array of words
sub getWordsFromFile {
	my $filename = shift(@_);
	my @preprocessed = ();

    # copy the pre-processing I do in R:
	open(FILE, "<$filename") || die($!);
	my $line = <FILE>;
	my $doneWithHeader = 0;
	while ($line) {
		# trim off headers -- everything until "Lines:"
		#if ($line =~ /^Lines:/) {
		#	$doneWithHeader = 1;
		#}
		# now: header goes up to first blank line.
		if ($line !~ /\S/) {
			$doneWithHeader = 1;
		}
		elsif ($doneWithHeader && ($line =~ /\S/)) {
			# split on spaces, put in lower case, then remove punctuation
			my @words = split(/\s+/, $line);
			@words = map { lc($_); } @words;
			map { s/\W//g; } @words;	# modifies the original
			@words = grep { /\S/; } @words;
			@preprocessed = (@preprocessed, @words);
		}

		$line = <FILE>;
	}
	close(FILE);
	return @preprocessed;
	
}

sub findLongestSubstringAllPairs {
	my $directory = shift(@_);
	my $outfile = shift(@_);
	my $file1Index = shift(@_) if (@_);

	opendir(DIR, $directory) or die($!);
	my @files = grep { !/^\./ } sort(readdir(DIR));
	closedir(DIR);

	open(OUT, ">$outfile") or die($!);

	my @iList = 0..$#files;
	if ($file1Index) {
		@iList = ($file1Index - 1);
	}

	foreach my $i (@iList) {
		next if ($files[$i] =~ /^\./);	# skip those starting with dot
		print "i = $i\n";
		foreach my $j (($i + 1) .. $#files) {
			next if ($files[$j] =~ /^\./);	# skip those starting with dot

			my @wordsShared = longestCommonWordSubstring("$directory/$files[$i]", "$directory/$files[$j]");
			my $numWordsShared = $wordsShared[0];
			print OUT "$files[$i]\t$files[$j]\t$numWordsShared\n";

		}
	}
	close(OUT);

	print "all done\n";

}
