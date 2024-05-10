# gc_1yr_benchmark_scripts

This repository contains run directory setup scripts and model vs observation IDL scripts for GEOS-Chem Classic 1-year benchmarks

## Contents

### FullChem

This folder contains scripts used to set up GEOS-Chem Classic 1-year and 10-year) full-chemistry benchmark simulations.

`template_scripts`

- Contains bash scripts for setting up, running, and compresing 1-year FullChem benchmark run directories.

`IDL`

- Contains the `benchmark_1yr.pro` driver routine for generating model vs observation plots from GEOS-Chem 1-year benchmark output. This directory also contains the following subdirectories:

  - `data`
    - Contains observational data for various species and from various campaigns
  - `input`
    - Contains text files specifying which model versions to compare
  - `output`
    - Stores PS, PDF, and txt files produced by the IDL scripts
  - `subroutines`
    - Contains IDL subroutines for each plot as referenced by `benchmark_1yr.pro`


### TransportTracers

`template_scripts`

- Contains bash scripts for setting up, running, and compresing 1-year TransportTracers benchmark run directories.

`IDL`

- Contains the `benchmark_1yr_transport.pro` driver routine for generating model vs observation plots from GEOS-Chem 1-year benchmark output. This directory also contains the following subdirectories.  These scripts may be obsolete.


## Authors

- Melissa Sulprizio (Harvard)
- Bob Yantosca (Harvard)
- Others as noted within individual script comments
