# Run a GEOS-Chem Classic 1-year full-chemistry benchmark
- Written by Melissa Sulprizio (@msulprizio), January 2024
- Updated and ported to GitHub by Bob Yantosca (@yantosca), April 2024

## Set up and run the simulation

1. Navigate to `GCClassic/run`.  Create a MERRA-2 4x5 72L benchmark run directory by using `createRunDir.sh` by following the prompts.

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
   >>> FullChem
   ```
   NOTE: Replace `X.Y.Z-rc.N` with the version label for this benchmark.
   

2. If you haven't already, clone this repository to a folder in your home disk space:

   ```console
   $ cd ~/GC/benchmarks
   $ git clone https://github.com/geoschem/gc_1yr_benchmark_scripts 1yr
   ```


3. Copy the setup script to your benchmark run directory., e.g.:

   ```console
   $ cd /path/to/GCClassic/FullChem
   $ cp ~/GC/benchmarks/1yr/FullChem/template_files/setup_1yr_run .
   ```

4. Open the `setup_1yr_run` script in your favorite editor.  Edit the configurable settings for this particular benchmark:

   ```bash
   BM_VERSION="GCC_X.Y.Z-rc.N"  # Benchmark long version name

   GCPY_ROOT="$HOME/gcpy"

   THIS_REPO_ROOT="$HOME/GC/benchmarks/1yr"

   START_YR=2018
   START_MON=7
   N_MONTHS=18

   TEMP="temp.txt"
   ```

   Typically you will only need to edit `BM_VERSION` (the version string), `GCPY_ROOT` and (path to your GCPy top-level folder).


5. Excecute the setup script:

   ```console
   $ ./setup_1yr_run
   ```

6. Copy the 2019/07/01 restart file from the last 1-year benchmark to the `Restarts/` folder.  This will be the initial conditions used to spin up the benchmarks simulation.  Rename this file to 2018/07/01:

   ```console
   $ mv GEOSChem.Restart.20190701_0000z.nc4 GEOSChem.Restart.20180701_0000z.nc4
   ```


7. Navigate to the `RunScripts/` subdirectory and submit run scripts to the queue:

   ```console
   $ cd RunScripts
   $ ./job_depend.pl
   $ cd ..
   ```

   The `job_depend.pl` script to submits all of the run scripts with the proper dependencies.   The first run will compile GEOS-Chem and copy the executable to the run directory.

   NOTE: `The setup_1yr_run` script creates the monthly bash scripts in the `run/` subdirectory. If you need to modify the monthly bash scripts, you can modify the `run.template` file and then execute this command:

   ```console
   $ ./make_runscripts GCC_X.Y-Z-rc.N 2018 7 18
   ```

   The run scripts will use 48 CPUs on `sapphire`,`huce_cascade`,`seas_compute`, or `shared` partitions, whichever has available nodes first.


## Create the benchmark plots and tables

1. Navigate to the `GCClassic/FullChem/BenchmarkResults` folder.

   ```console
   $ cd /path/to/GCClassic/FullChem/BenchmarkResults
   ```

   The `setup_1yr_run` script should have copied the benchmark plotting configuration file `1yr_fullchem_benchmark.yml` and a SLURM job script `benchmark_slurm.sh` to this folder.  If these file were not copied successfully, use these commands to manually copy them over:

   ```console
   $ cp /path/to/gcpy/gcpy/benchmark/config/1yr_fullchem_benchmark.yml .
   $ cp /path/to/gcpy/gcpy/benchmark/benchmark_slurm.sh .
   ```

2. Open the `1yr_fullchem_benchmark.yml` configuration file in your favorite editor.  Then select the plots and tables that you wish to make.

   If the 1-yr GCHP FullChem benchmark simulation has not yet finished, set `gcc_vs_gcc: True` and set the other comparisons to `False`.  You can plot the GCHP vs GCC and  diff-of-diffs comparisons after the GCHP benchmark finishes.


3. Edit the `benchmark_slurm.sh` script to change the requested time, memory, or partitions.  Once you are satisfied with your selections, submit the script to the SLURM scheduler:

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


4. Generate additional "Model vs. Observations" plots with IDL.

   First, navigate to the `IDL/input` folder in your clone of this repository:

   ```console
   $ cd ~/GC/benchmarks/1yr/IDL/input
   ```


5. Navigate to the `IDL/input` folder and create a new configuration file for the current benchmark version.:

   ```console
   $ cd input

   $ cp GCC_14.3.0-rc.0.1yr GCC_X.Y.Z-rc.N.1yr  # Replace X, Y, Z, N with version numbers
   ```
   Then open `GCC_X.Y.Z-rc.N.1yr` in your favorite editor and update version numbers `V1` and `V2`.


6. Run the following commands.  (Replace `X.Y.Z-rc.N` with the benchmark version number).

   ```console
   $ cd ..     # Navigates back to the IDL folder
   $ idl
   IDL> benchmark_1yr, ‘input/GCC_X.Y.Z-rc.N.1yr’, /DO_PDF, /DO_Ox
   IDL> benchmark_1yr, ‘input/GCC_X.Y.Z-rc.N.1yr’, /DO_PDF, /DO_CO
   IDL> benchmark_1yr, ‘input/GCC_X.Y.Z-rc.N.1yr’, /DO_PDF, /DO_MOZAIC
   IDL> benchmark_1yr, ‘input/GCC_X.Y.Z-rc.N.1yr’, /DO_PDF, /DO_AIRCRAFT
   IDL> benchmark_1yr, ‘input/GCC_X.Y.Z-rc.N.1yr’, /DO_PDF, /DO_AEROSOL
   IDL> benchmark_1yr, ‘input/GCC_X.Y.Z-rc.N.1yr’, /DO_PDF, /DO_PAN
   IDL> exit
   ```

7. After creating all model vs observation benchmark plots, move the PDF files to the `Benchmark_Results/GCC_version_comparison/ModelVsObs` folder:

   ```console
   $ cd move_output
   $ ./move_output.sh /path/to/GCClassic/FullChem/BenchmarkResults/GCC_version_comparison
   ```

## Archiving results

1. Navigate back to the 1-year benchmark run directory

   ```console
   $ cd /path/to/GCClassic/FullChem
   ```

2. Update the README file to document added to this version. See past version README files for examples on formatting and information to include.


3. Compress the 1-year benchmark run directory:

   ```console
   $ ./pack_1yr_run
   ```

   NOTE: Out of caution, the lines to remove each directory after compressing it into a tarball are currently commented out, so you will need to remove the uncompressed directories manually after making sure there have been no issues with this script. If you’re feeling brave, you can uncomment those lines.


4. Create a new directory for this benchmark on gcgrid, so that the benchmark files can be seen via FTP/HTTP.

   ```console
   $ cd /n/holyscratch01/external_repos/GEOS-Chem/gcgrid/1yr_benchmarks
   $ mkdir -p X.Y.Z-rc.N/GCClassic/FullChem   # Replace X, Y, Z, N w/ version numbers
   ```

5. Navigate back to the 1-year benchmark run directory and execute the copy script. This will copy the packed files to the path on `gcgrid`.

   ```console
   $ cd /path/to/GCClassic/FullChem
   $ ./copy_1yr_run /n/holyscratch01/external_repos/GEOS-Chem/gcgrid/1yr_benchmarks/X.Y.Z-rc.N/GCClassic/FullChem
   ```

   Permissions should be changed automatically so that the `jacob_gcst` group has permission to read/write/execute as needed. The benchmark files will sync to ftp.as.harvard.edu hourly (usually at ~15 min past the hour).


6. Create and fill out 1-year benchmark sections and tables on GEOS-Chem wiki

   TIP: Copy wiki text from previous benchmark version, paste into an editor, and use replace feature to change version number (or use replace function in browser while editing the wiki page). Otherwise, there are a lot of file paths to update for the benchmark plots table!


7. Send a benchmark email to developers and GCSC asking for approval.
