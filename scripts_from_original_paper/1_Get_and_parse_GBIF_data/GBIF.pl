#!/usr/bin/perl
#use strict;
#use warnings;

#get columns we want from occurrence data file
open(FILE1, "occurrence.txt") or die("Cannot open file");
open (OUT1, ">DATAtemp.txt") or die("Cannot write to outfile");

while( <FILE1> ) {
    my @columns = split /\t/, $_;
    print OUT1 "$columns[218]\t$columns[99]\t$columns[92]\t$columns[156]\t$columns[66]\t$columns[163]\t$columns[127]\t$columns[60]\t$columns[77]\t$columns[78]\t$columns[199]\t$columns[201]\n";
}

close FILE1;
close OUT1 or die $!;

#get occurrences with associatedsequence data
open(FILE2, "DATAtemp.txt") or die("Cannot open file");
open (OUT2, ">DATA.txt") or die("Cannot write to outfile");
open (OUT3, ">Accessionstemp.txt") or die("Cannot write to outfile");
open (OUT4, ">GPStemp.txt") or die("Cannot write to outfile");

#define regular expression (accession number) to search for
#my $accession = /(\w{2}\d{6})/;

#read each line
while ($line = <FILE2>) {
#get rid of tab characters in the array
	@array = split('\t',$line);
#look at number associated sequence column
	$associatedsequence = $array[7];
		
 	while ($associatedsequence =~ /(\w{2}\d{6})/g) {
		print OUT2 "$array[0]\t$array[1]\t$array[2]\t$array[3]\t$array[4]\t$array[5]\t$array[6]\t$1\t$array[8]\t$array[9]\t$array[10]\t$array[11]";
		print OUT3 "$1\n";
		print OUT4 "$array[0]\t$1\t$array[8]\t$array[9]\n";
		}
}	

close(FILE2);
close(OUT2);
close(OUT3);
close(OUT4);

$cmdA = q{awk '!seen[$0]++' Accessionstemp.txt >Accessions.txt};
$cmdG = q{awk '!seen[$0]++' GPStemp.txt >GPS.txt};
system($cmdA);
system($cmdG);

system("wc -l occurrence.txt");
system("wc -l DATA.txt");
system("wc -l Accessions.txt");
system("wc -l GPS.txt");

#delete temp datafile
system("rm DATAtemp.txt");
system("rm Accessionstemp.txt");
system("rm GPStemp.txt");
system("rm citations.txt");
system("rm meta.xml");
system("rm metadata.xml");
system("rm multimedia.txt");
system("rm rights.txt");
system("rm verbatim.txt");
system("rm -r dataset");
