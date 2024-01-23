# gc_1yr_benchmark_scripts

This repository contains run directory setup scripts and model vs observation IDL scripts for GEOS-Chem 1-year benchmarks

## Contents

The template_scripts directory contains bash scripts for setting up, running, and compresing 1-year benchmar run directories.

The IDL directory contains the benchmark_1yr.pro driver routine for generating
model vs observation plots from GEOS-Chem 1-year benchmark output. This
directory also contains:

  - data        : Contains observational data for various species and from various campaigns
  - input       : Contains text files specifying which model versions to compare
  - output      : Stores PS, PDF, and txt files produced by the IDL scripts
  - subroutines : Contains IDL subroutines for each plot as referenced by benchmark_1yr.pro

## Authors

- Melissa Sulprizio (Harvard)
- Bob Yantosca (Harvard)
- Others as noted within individual script comments