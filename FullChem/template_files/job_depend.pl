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
my @jobs   = qw/GCC_X.Y.Z-rc.N.201807
                GCC_X.Y.Z-rc.N.201808
                GCC_X.Y.Z-rc.N.201809
                GCC_X.Y.Z-rc.N.201810
                GCC_X.Y.Z-rc.N.201811
                GCC_X.Y.Z-rc.N.201812
                GCC_X.Y.Z-rc.N.201901
                GCC_X.Y.Z-rc.N.201902
                GCC_X.Y.Z-rc.N.201903
                GCC_X.Y.Z-rc.N.201904
                GCC_X.Y.Z-rc.N.201905
                GCC_X.Y.Z-rc.N.201906
                GCC_X.Y.Z-rc.N.201907
                GCC_X.Y.Z-rc.N.201908
                GCC_X.Y.Z-rc.N.201909
                GCC_X.Y.Z-rc.N.201910
                GCC_X.Y.Z-rc.N.201911
                GCC_X.Y.Z-rc.N.201912
/;

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


