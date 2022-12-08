#!/usr/bin/perl
#use strict;
#use warnings;

#get columns we want from occurrence data file
open(FILE, "anage_data.txt") or die("Cannot open file");
open (OUT1, ">Fish_traits.txt") or die("Cannot write to outfile");
open (OUT2, ">Amphibia_traits.txt") or die("Cannot write to outfile");
open (OUT3, ">Aves_traits.txt") or die("Cannot write to outfile");
open (OUT4, ">Mammalia_traits.txt") or die("Cannot write to outfile");
open (OUT5, ">Reptilia_traits.txt") or die("Cannot write to outfile");

while ($line = <FILE>) {
#get rid of tab characters in the array
	@array = split('\t',$line);
	
#	print "$array[3]\n";
	
	if (($array[3] =~ /Actinopterygii/) || ($array[3] =~ /Cephalaspidomorphi/) || ($array[3] =~ /Chondrichthyes/)|| ($array[3] =~ /Sarcopterygii/)) {
		print OUT1 $line;
		}
		
	if ($array[3] =~ /Amphibia/) {
		print OUT2 $line;
		}		

	if ($array[3] =~ /Aves/) {
		print OUT3 $line;
		}

	if ($array[3] =~ /Mammalia/) {
		print OUT4 $line;
		}

	if ($array[3] =~ /Reptilia/) {
		print OUT5 $line;
		}
}

