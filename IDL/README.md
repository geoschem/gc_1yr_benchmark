# IDL scripts for creating benchmark plots

This repository contains the IDL scripts for creating plots from the
1-year benchmark simulations. 

## Instructions

1. For each new benchmark, add a new input file `input/GC_X.Y.Z-rc.N.1yr`, where `X.Y.Z` is the version number and `N` is the release candidate number.  Edit the settings in the file as follows:
   
```console
##### Parameters for 1st model #####
V1: GCC_X.Y.Z                           # version number of Ref model
D1: /path/to/Ref/model/run-directory/OutputDir/
L1: GEOSChem.SpeciesConc.2019
M1: MERRA2
R1: 4
Y1: 2019

##### Parameters for 2nd model #####
V2: GCC_X.Y.Z                           # version number of Dev model
D2: /path/to/Dev/model/run-directory/OutputDir/
L2: GEOSChem.SpeciesConc.2019
M2: MERRA2
R2: 4
Y2: 2019
```
   
2. Use these commands to generate the plots.
    - NOTE: PDF output is now always turned on, so there is no need to manually set `/DO_PDF`.
    - Also, due to a SLURM bug on Harvard Cannon that prevents certain X11 windows from opening in interactive sessions, you must run the `benchmark_1yr.pro` script on the login node.
   
```console
$ benchmark_1yr, 'input/GC_X.Y.Z-rc.N.1yr', /DO_Ox
$ benchmark_1yr, 'input/GC_X.Y.Z-rc.N.1yr', /DO_CO
$ benchmark_1yr, 'input/GC_X.Y.Z-rc.N.1yr', /DO_MOZAIC
$ benchmark_1yr, 'input/GC_X.Y.Z-rc.N.1yr', /DO_AIRCAFT
$ benchmark_1yr, 'input/GC_X.Y.Z-rc.N.1yr', /DO_PAN
$ benchmark_1yr, 'input/GC_X.Y.Z-rc.N.1yr', /DO_AEROSOL
```

3. Run the `move_output.sh` script, which will move the files to the `ModelVsObs` subdirectory of the benchmark plots folder that you specify:

```console
$ cd output
$ ./move_output.sh /path/to/benchmark/results/folder
```
