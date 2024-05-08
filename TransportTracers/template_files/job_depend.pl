#!/usr/bin/perl -w

#------------------------------------------------------------------------------
#                  GEOS-Chem Global Chemical Transport Model                  !
#------------------------------------------------------------------------------
#BOP
#
# !IROUTINE: job_depend.pl
#
# !DESCRIPTION: Submits several jobs to the SLURM scheduler, such that
#  each job will wait for the previous job to be done.  Handy for
#  submitting stages of long model runs.
#\\
#\\
# !REMARKS:
#  Add the job script names to the @jobs array.
#
# !REVISION HISTORY:
#  16 Mar 2018 - R. Yantosca - Initial version
#EOP
#------------------------------------------------------------------------------
#BOC
#
# !LOCAL VARIABLES:
#

# Scalars
my $count   = 0;

# Strings
my $depend = "";
my $result = "";
my $job    = "";
my $jobId  = "";

# Arrays
my @subStr = ();
my @jobs   = qw/GCC_14.0.0-rc.3-TransportTracers.2009
                GCC_14.0.0-rc.3-TransportTracers.2010
                GCC_14.0.0-rc.3-TransportTracers.2011
                GCC_14.0.0-rc.3-TransportTracers.2012
                GCC_14.0.0-rc.3-TransportTracers.2013
                GCC_14.0.0-rc.3-TransportTracers.2014
                GCC_14.0.0-rc.3-TransportTracers.2015
                GCC_14.0.0-rc.3-TransportTracers.2016
                GCC_14.0.0-rc.3-TransportTracers.2017
                GCC_14.0.0-rc.3-TransportTracers.2018
                GCC_14.0.0-rc.3-TransportTracers.2019/;

# Loop over jobs
foreach $job ( @jobs ) {

  # Add the dependency string after the first job is submitted
  if ( $count > 0 ) { $depend = "--dependency=afterok:$jobId "; }

  # Submit the job
  $cmd    = "sbatch $depend $job";
  print "$cmd\n";
  $result = qx/$cmd/;

  # The job ID Is the last substring 
  @subStr = split( ' ', $result );
  $jobId  = $subStr[3];
  
  # Increment count
  $count++;
}


