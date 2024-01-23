#!/bin/bash

#------------------------------------------------------------------------------
#          Harvard University Atmospheric Chemistry Modeling Group            !
#------------------------------------------------------------------------------
#BOP
#
# !MODULE: move_output
#
# !DESCRIPTION: Moves 1-year benchmark output files to the appropriate
#  location
#\\
#\\
# !CALLING SEQUENCE:
#  move_output OUTDIR
#
# !REVISION HISTORY: 
#  23 Sep 2016 - M. Sulprizio- Initial version
#EOP
#------------------------------------------------------------------------------
#BOC

# Make sure the version string is passed
if [ $# != 1 ]; then 
   echo "Usage: move_output OUTDIR"
   exit 0
fi

outdir=$1

# Make sure the directory exists
if [ ! -d $outdir ]; then
   echo "$outdir does not exist!"
   exit 0
fi

echo "Moving 1-year benchmark output to $outdir"

#----------------------------------------------------
# Model vs Observation plots
#----------------------------------------------------
subdir=$outdir'/ModelVsObs'
if [ ! -d $subdir ]; then
  echo "Creating $subdir"
  mkdir $subdir
fi
mv -v Aerosol*.pdf $subdir
mv -v aircraft*.pdf $subdir
mv -v BrO*.pdf  $subdir
mv -v *O3*.pdf $subdir
mv -v *CO*.pdf $subdir
mv -v cmdl*.pdf $subdir
mv -v PAN*.pdf  $subdir

# Clear variables
unset outdir
unset subdir

# Exit normally
exit 0
#EOC
