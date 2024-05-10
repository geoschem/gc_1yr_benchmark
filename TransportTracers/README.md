# Run a GEOS-Chem Classic 1-year TransportTracers benchmark
- Written by Melissa Sulprizio (@msulprizio), January 2024
- Updated and ported to GitHub by Bob Yantosca (@yantosca), April 2024

## Set up and run the simulation

1. Navigate to `GCClassic/run`.  Create a MERRA-2 4x5 72L TransportTracers run directory by using `createRunDir.sh` by following the prompts.

   For consistency with past benchmarks and for ease of storage, it is recommended that you use a directory structure with version tag first (e.g. `14.3.0-rc.0`), containing `GCClassic` and `GCHP` subdirectories, both of which can include `FullChem` and/or `TransportTracer` subdirectories (the actual run directories). For example:

   ```console
   -----------------------------------------------------------
   Enter path where the run directory will be created:
   -----------------------------------------------------------
   >>> /path/to/X.Y.Z-rc.N/GCClassic

   -----------------------------------------------------------
   Enter run directory name, or press return to use default:

   NOTE: This will be a subfolder of the path you entered above.
   -----------------------------------------------------------
   >>> TransportTracers
   ```
   NOTE: Replace `X.Y.Z-rc.N` with the version label for this benchmark.
   


2. Clone or pull this repository to a folder in your home disk space, e.g.:

   ```console
   $ cd ~/GC/benchmarks
   $ git clone https://github.com/geoschem/gc_1yr_benchmark_scripts 1yr
   ```


3. Copy the setup script to your benchmark run directory., e.g.:

   ```console
   $ cd /path/to/GCClassic/TransportTracers
   $ cp ~/GC/benchmarks/1yr/TransportTracers/template_files/setup_1yr_transport_bm .
   ```

4. Open the `setup_1yr_transport_bm` script in your favorite editor.  Edit the configurable settings for this particular benchmark:

   ```bash
   BM_VERSION="GCC_X.Y.Z-rc.N-TransportTracers"  # Benchmark long version name

   GCPY_ROOT="$HOME/gcpy"

   THIS_REPO_ROOT="$HOME/GC/benchmarks/1yr"

   START_YR=2018
   START_MON=7
   N_MONTHS=18

   TEMP="temp.txt"
   ```

   Typically you will only need to edit `BM_VERSION` (the version string), `GCPY_ROOT` and (path to your GCPy top-level folder).


5. Copy the 2019/01/01 restart file from the last 1-year benchmark to the `Restarts/` folder.  This will be the initial conditions used to spin up the benchmarks simulation.  This file will be automatically renamed by the setup script.


6. Excecute the setup script:

   ```console
   $ ./setup_1yr_transport_bm
   ```

7. Navigate to the `RunScripts/` subdirectory and submit run scripts to the queue:

   ```console
   $ cd RunScripts
   $ ./job_depend.pl
   $ cd ..
   ```

   The `job_depend.pl` script to submits all of the run scripts with the proper dependencies.   The first run will compile GEOS-Chem and copy the executable to the run directory.

   NOTE: `The setup_1yr_transport_bm` script creates the monthly bash scripts in the `run/` subdirectory. If you need to modify the monthly bash scripts, you can modify the `run.template` file and then execute this command:

   ```console
   $ ./make_runscripts GCC_X.Y-Z-rc.N-TransportTracers 2009 11
   ```

   The run scripts will use 48 CPUs on `sapphire`,`huce_cascade`,`seas_compute`, or `shared` partitions, whichever has available nodes first.


## Create the benchmark plots and tables

1. Navigate to the `GCClassic/TransportTracers/BenchmarkResults` folder.

   ```console
   $ cd /path/to/GCClassic/TransportTracers/BenchmarkResults
   ```

   The `setup_1yr_transport_bm` script should have copied the benchmark plotting configuration file `1yr_tt_benchmark.yml` and a SLURM job script `benchmark_slurm.sh` to this folder.  If these file were not copied successfully, use these commands to manually copy them over:

   ```console
   $ cp /path/to/gcpy/gcpy/benchmark/config/1yr_tt_benchmark.yml .
   $ cp /path/to/gcpy/gcpy/benchmark/benchmark_slurm.sh .
   ```

2. Open the `1yr_tt_benchmark.yml` configuration file in your favorite editor.  Then select the plots and tables that you wish to make.

   If the 1-yr GCHP FullChem benchmark simulation has not yet finished, set `gcc_vs_gcc: True` and set the other comparisons to `False`.  You can plot the GCHP vs GCC and  diff-of-diffs comparisons after the GCHP benchmark finishes.


3. Edit the `benchmark_slurm.sh` script to change the requested time, memory, or partitions.  Also make sure to change these lines from:
   
   ```console
   config="1yr_fullchem_benchmark.yml"
   #config="1yr_tt_benchmark.yml"
   ```
   to
   ```console
   #config="1yr_fullchem_benchmark.yml"
   config="1yr_tt_benchmark.yml"
   ```
   so that GCPy will be given the proper configuration file for plotting TransportTracers benchmark results.
   
   Once you are satisfied with your selections, submit the script to the SLURM scheduler:

   ```console
   $ sbatch benchmark_slurm.sh
   ```

   A log file will be created to document the progress of the benchmark plotting.  The following folders will be created:

   ```console
   Benchmark_Results/GCC_version_comparison     # If gcc_vs_gcc was selected
   Benchmark_Results/GCHP_vs_GCC_comparison     # If gchp_vs_gcc was selected
   Benchmark_Results/GCHP_vs_GCHP_comparison    # If gchp_vs_gchp was selected
   Benchmark_Results/GCHP_vs_GCC_diff_of_diffs  # If diff_of_diffs was selected
   ```

   Each of these folders will be further organized into subfolders  containing the various plots and tables.


## Archiving results

1. Navigate back to the 1-year benchmark run directory

   ```console
   $ cd /path/to/GCClassic/TransportTracers
   ```

2. Update the README file to document added to this version. See past version README files for examples on formatting and information to include.


3. Compress the 1-year benchmark run directory:

   ```console
   $ ./pack_1yr_transport_bm
   ```

   NOTE: Out of caution, the lines to remove each directory after compressing it into a tarball are currently commented out, so you will need to remove the uncompressed directories manually after making sure there have been no issues with this script. If you’re feeling brave, you can uncomment those lines.


4. Create a new directory for this benchmark on gcgrid, so that the benchmark files can be seen via FTP/HTTP.

   ```console
   $ cd /n/holyscratch01/external_repos/GEOS-Chem/gcgrid/1yr_benchmarks
   $ mkdir -p X.Y.Z-rc.N/GCClassic/TransportTracers   # Replace X, Y, Z, N w/ version numbers
   ```

5. Navigate back to the 1-year benchmark run directory and execute the copy script. This will copy the packed files to the path on `gcgrid`.

   ```console
   $ cd /path/to/GCClassic/TransportTracers
   $ ./copy_1yr_transport_bm /n/holyscratch01/external_repos/GEOS-Chem/gcgrid/1yr_benchmarks/X.Y.Z-rc.N/GCClassic/TransportTracers
   ```

   Permissions should be changed automatically so that the `jacob_gcst` group has permission to read/write/execute as needed. The benchmark files will sync to ftp.as.harvard.edu hourly (usually at ~15 min past the hour).


6. Create and fill out 1-year benchmark sections and tables on GEOS-Chem wiki

   TIP: Copy wiki text from previous benchmark version, paste into an editor, and use replace feature to change version number (or use replace function in browser while editing the wiki page). Otherwise, there are a lot of file paths to update for the benchmark plots table!


7. Send a benchmark email to developers and GCSC asking for approval.
