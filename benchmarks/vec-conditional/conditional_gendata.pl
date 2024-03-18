#!/usr/bin/perl -w
#==========================================================================
# conditional_gendata.pl
#
# Author: Generated
# Date: Today
#
(our $usageMsg = <<'ENDMSG') =~ s/^\#//gm;
#
# Simple script which creates an input data set and the reference data
# for the given conditional operation.
#
ENDMSG

use strict "vars";
use warnings;
no  warnings("once");
use Getopt::Long;

#--------------------------------------------------------------------------
# Command line processing
#--------------------------------------------------------------------------

our %opts;

sub usage()
{

  print "\n";
  print " Usage: conditional_gendata.pl [options] \n";
  print "\n";
  print " Options:\n";
  print "  --help  print this message\n";
  print "  --size  size of input data [1000]\n";
  print "  --seed  random seed [1]\n";
  print "$usageMsg";

  exit();
}

sub processCommandLine()
{

  $opts{"help"} = 0;
  $opts{"size"} = 1000;
  $opts{"seed"} = 1;
  Getopt::Long::GetOptions( \%opts, 'help|?', 'size:i', 'seed:i' ) or usage();
  $opts{"help"} and usage();

}

#--------------------------------------------------------------------------
# Helper Functions
#--------------------------------------------------------------------------
sub printArray
{
  my $arrayName = $_[0];
  my $arrayRef  = $_[1];
  my $type      = $_[2];

  my $numCols = 20;
  my $arrayLen = scalar(@{$arrayRef});

  print $type." ".$arrayName."[DATA_SIZE] = \n";
  print "{\n";

  if ( $arrayLen <= $numCols ) {
    print "  ";
    for ( my $i = 0; $i < $arrayLen; $i++ ) {
      print sprintf("%3d",$arrayRef->[$i]);
      if ( $i != $arrayLen-1 ) {
        print ", ";
      }
    }
    print "\n";
  }

  else {
    my $numRows = int($arrayLen/$numCols);
    for ( my $j = 0; $j < $numRows; $j++ ) {
      print "  ";
      for ( my $i = 0; $i < $numCols; $i++ ) {
        my $index = $j*$numCols + $i;
        print sprintf("%3d",$arrayRef->[$index]);
        if ( $index != $arrayLen-1 ) {
          print ", ";
        }
      }
      print "\n";
    }

    if ( $arrayLen > ($numRows*$numCols) ) {
      print "  ";
      for ( my $i = 0; $i < ($arrayLen-($numRows*$numCols)); $i++ ) {
        my $index = $numCols*$numRows + $i;
        print sprintf("%3d",$arrayRef->[$index]);
        if ( $index != $arrayLen-1 ) {
          print ", ";
        }
      }
      print "\n";
    }

  }

  print  "};\n\n";
}

#--------------------------------------------------------------------------
# Main
#--------------------------------------------------------------------------

sub main()
{

  processCommandLine();
  srand($opts{"seed"});

  my @input1_data; # x
  my @input2_data; # a
  my @input3_data; # b
  my @verify_data; # z
  for ( my $i = 0; $i < $opts{"size"}; $i++ ) {
    my $valueX = int(rand(10)); # x
    my $valueA = int(rand(999)); # a
    my $valueB = int(rand(999)); # b

    push( @input1_data, $valueX );
    push( @input2_data, $valueA );
    push( @input3_data, $valueB );
    push( @verify_data, ($valueX < 5) ? $valueA : $valueB );
  }

  print "\n\#define DATA_SIZE ".$opts{"size"}." \n\n";
  printArray( "input1_data", \@input1_data, "int8_t" ); # x
  printArray( "input2_data", \@input2_data, "int16_t" ); # a
  printArray( "input3_data", \@input3_data, "int16_t" ); # b
  printArray( "verify_data", \@verify_data, "int16_t" ); # z

}

main();