;------------------------------------------------------------------------------
;+
; NAME:
;        BENCHMARK_1YR_TRANSPORT
;
; PURPOSE:
;        Produces tables and maps for 1-year Transport benchmark simulations
;        (for model validation).
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        BENCHMARK_1YR_TRANSPORT
;
; MODIFICATION HISTORY:
;        mps, 07 Mar 2016: Initial Version
;        mps, 08 Feb 2019: Rename from RnPbBe to Transport
;------------------------------------------------------------------------------


pro Benchmark_1Yr_Transport

   ;--------------------------------------------------------------------
   ; Configurable settings
   ;--------------------------------------------------------------------

   ; Met field
   Year = '2016'
   Met  = 'GEOSFP'
   Res  = 4

;------------------------------------------------------------------------------
; NLPBL is now the default (2/8/19)   
;   ; PBL mixing scheme (choose one)
;   pblmix = 'TURBDAY'
;   pblmix = 'NLPBL'
;------------------------------------------------------------------------------

   ; Version
   Ver_str1 = 'GC_12.2.0'
   Ver_str2 = 'TransportTracers'
   Version  = Ver_str1 + '-' + Ver_str2 ;+ '-' + pblmix
   
   ; Input directory (modify for your needs)
   RDir  = '/n/regal/jacob_lab/msulprizio/GC/benchmarks/1yr/'
   InDir = RDir + Ver_str1 + '/' + Ver_str2 + '/' ;+ pblmix + '/'

   ; Input file
   InFile = InDir + 'bpch/trac_avg.' + Version + '.' + Year + '01010000'

   ; Output directory
   OutDir = InDir + 'output/'

   ;--------------------------------------------------------------------
   ; Set links to the proper diaginfo.dat, tracerinfo.dat files
   ;--------------------------------------------------------------------

   ; Remove existing file links
   Cmd = 'rm -f diaginfo.dat tracerinfo.dat'
   Spawn, Cmd

   ; Link to the diaginfo.dat file
   Cmd = 'ln -s ' + StrTrim( InDir, 2 )+ 'diaginfo.dat .'
   print, Cmd
   Spawn, Cmd

   ; Link to the tracerinfo.dat file
   Cmd = 'ln -s ' + StrTrim( InDir, 2 )+ 'tracerinfo.dat .'
   print, Cmd
   Spawn, Cmd

   ;--------------------------------------------------------------------
   ; Create the RnPbBe benchmark tables and plots
   ;--------------------------------------------------------------------

   ; Compute Pb210 and Be7 budgets
   Print, 'Computing Pb210 and Be7 budgets'
   geoschem_pbbe_budget_benchmark, Year,  Met,    Res,   Version, $
                                   InDir, InFile, OutDir

   ; Create RnPbBe zonal mean plots
   Print, 'Creating zonal mean plots'
   rnpbbe_annual_zonalmean,  Year, Met, Res, Version, InDir, InFile, OutDir, /PS
   rnpbbe_monthly_zonalmean, Year, Met, Res, Version, InDir, InFile, OutDir, /PS

   ;--------------------------------------------------------------------
   ; Create the transport tracer plots
   ;--------------------------------------------------------------------

   ; Create bpch files with Jan and July output
   Print, 'Creating bpch files for Jan and Jul'
   OutJan = InDir + 'bpch/trac_avg.' + Version + '.Jan'
   OutJul = InDir + 'bpch/trac_avg.' + Version + '.Jul'

   if ( Year eq '2013' ) then begin
      Bpch_Sep, InFile, OutJan, tau0 = nymd2tau(20130101)
      Bpch_Sep, InFile, OutJul, tau0 = nymd2tau(20130701)
   endif else if ( Year eq '2016' ) then begin
      Bpch_Sep, InFile, OutJan, tau0 = nymd2tau(20160101)
      Bpch_Sep, InFile, OutJul, tau0 = nymd2tau(20160701)
   endif

   ; Create 1-month benchmark plots for Jan and Jul
   Print, 'Creating 1-month benchmark plots for Jan'
   Input = './input/' + Version + '.Jan.1mon'
   print, input
   Plot_1mon, Input, /dyn

   Print, 'Creating 1-month benchmark plots for Jul'
   Input = './input/' + Version + '.Jul.1mon'
   Plot_1mon, Input, /dyn

   ;--------------------------------------------------------------------
   ; Create the passive tracer mass table
   ;--------------------------------------------------------------------

   ; Echo info
   Message, 'Calculating total tracer mass ...', /Info

   ; Input file with 6-hr mass
   MassFile = 'species_mass_kg.txt'

   ; Output file name
   OutFile = OutDir + Version + '.passive_mass.txt'

   ; Compute monthly tracer mass
   Monthly_Mass, InDir, MassFile, Year, Version, $
                 OutFile=OutFile, _EXTRA=e

   ;--------------------------------------------------------------------
   ; Clean up and quit
   ;--------------------------------------------------------------------

   ; Create PDF files from the postscript files
   Make_PDF, InDir + 'output/'

   ; Remove PS files and only keep PDF files
   spawn, 'rm -v '+OutDir+'*.ps'

end
