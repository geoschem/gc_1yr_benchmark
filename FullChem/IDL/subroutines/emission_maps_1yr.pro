;-----------------------------------------------------------------------
;+
; NAME:
;        EMISSION_MAPS_1YR
;
; PURPOSE:
;        Creates emission plots of various tracers from 1-year
;        GEOS-Chem benchmark simulation  output.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        EMISSION_MAPS_1YR, FILES, VERSIONS, MONTH [, Keywords ]
;
; INPUTS:
;        FILES -> A 3-element vector containing the names of files
;             from the "old" and "new" GEOS-Chem model versions
;             that are to be compared. 
;
;        VERSIONS -> A 3-element vector containing the version
;             numbers for the "old" and "new" GEOS-Chem model
;             versions.
;
;        MONTH -> A string containing month name
;
; KEYWORD PARAMETERS:
;        /PS -> Set this switch to generate PostScript output.
;
;        DIR -> If /PS is set, then EMISSION_MAPS_1YR will
;             create PostScript files in this directory.
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines Included:
;        ===========================================
;        CreateEmPlots
;        PlotEmissions
;
;        External Subroutines Required:
;        ============================================
;        OPEN_DEVICE     CLOSE_DEVICE
;        MULTIPANEL      COLORBAR_NDIV    (function)
;        TVMAP           CHKSTRU          (function)
;        UNDEFINE        EXTRACT_FILENAME (function)  
;        CTM_GET_DATA    ADD_SEPARATOR    (function)
;     
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR.
;
; EXAMPLES:
;        FILES    = [ 'ctm.bpch.v10-01e-geosfp-Run1.2013010100', $
;                     'ctm.bpch.v10-01h-geosfp-Run1.2013010100', $
;                     'ctm.bpch.v10-01i-geosfp-Run0.2013010100'  ]
;        VERSIONS = [ 'v10-01e-geosfp-Run1', $
;                     'v10-01h-geosfp-Run2', $
;                     'v10-01i-geosfp-Run0'  ]
; 
;        EMISSION_MAPS_1YR, FILES, VERSIONS, /PS, MONTH='Jan', $
;                           DIR='v10-01/output/'
;
;             ; Creates emission maps of three GEOS-CHEM versions
;             ; (in this case v10-01e, v10-01h, and v10-01i) for Jan 2013.
;
; MODIFICATION HISTORY:
;        mps, 27 Apr 2015: Initial version based on emission_maps.pro
;                          from GAMAP v2-18
;        mps, 27 Jan 2017: - Update to allow for comparison of 2 versions,
;                            intead of the default 3 versions
;        bmy, 15 May 2018: Make lots of calls to CTM_CLEANUP, /NO_GC to
;                          avoid running out of available file LUNs
;
;-
; Copyright (C) 2011, GEOS-Chem Support Team, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to geos-chem-support@seas.harvard.edu
; with subject "IDL routine emission maps"
;-----------------------------------------------------------------------

pro PlotEmissions, Data, PlotTitle, Grid, Unit, PS=PS, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PlotEmissions plots various emissions data
   ;====================================================================

   ; Don't plot the polar latitudes
   XMid      = Grid.XMid
   YMid      = Grid.YMid[ 1:Grid.JMX-2 ]
   Data      = Data[ *, 1:Grid.JMX-2 ]

   ; Number of colorbar tickmarks
   Divisions = ColorBar_NDiv( 2 )

   ; We need to set the colorbar a little bit lower for PostScript
   if ( Keyword_Set( PS ) ) $
     then CbPos = [ 0.05, -0.02, 0.95, 0.01 ] $
     else CbPos = [ 0.05,  0.00, 0.95, 0.03 ]

   ; Plot maps over a world map
   TvMap, Data, XMid, Ymid,                                            $
      /Countries,         /Coasts,             /Cbar,                  $
      Division=Divisions, /Sample,             /Grid,                  $
      Title=PlotTitle,    CBFormat='(e10.2)',  Unit=Unit,              $
      _EXTRA=e

end

;------------------------------------------------------------------------------

pro CreateEmPlots, Files, Versions, Category, Category_bf, Tracer, Tracer_bf, $
                   Title, TopTitle, Month,    Lev,         Unit,   PS=PS,     $
                   _EXTRA=e

   ;====================================================================
   ; Internal routine CreateEmPlots reads the data from files and then
   ; calls PlotEmissions to add the plot to the output file.
   ;====================================================================

   ; Number of model versions to compare
   nVersions = N_Elements( Versions )
   
   ; Number of rows & columns for the plot
   Rows = 2
   Cols = 2

   ; Set multiple panels per page
   if ( nVersions eq 3 ) then begin
      MultiPanel, Nplots=3, Margin=[ 0.03, 0.03, 0.03, 0.03 ]
   endif else begin
      MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]
   endelse
   
   ; Loop over each diagnostic quantity
   for D = 0L, N_Elements( Category )-1L do begin

      ; Assume that a specified level greater than 1 is calling 
      ; for the total of the column data to be printed
      if ( Lev[D] gt 1 )           $
         then LLev = [ 1, Lev[D] ] $
         else LLev = [ 1, 1      ]

      ; Echo info
      print, 'Plotting emission maps for ' + $
             StrTrim( Category[D], 2 )       + $
             ', Tracer # '                   + $
             StrTrim( String( Tracer[D] ), 2 )

      ; Create the title for the top of the plot panel
      if ( StrPos( Title[D], 'sink' ) ge 0 )    $
      then BotTitle = StrTrim( Title[D], 2 ) +  $
                      ' for '   + Month         $
      else BotTitle = StrTrim( Title[D], 2 ) +  $
                      ' emissions for ' + Month

      ;--------------------------------------------------------------------
      ; Read and plot data from Version 1 file
      ;--------------------------------------------------------------------
      Success = CTM_Get_DataBlock( Data1,          Category[D],            $
                                   File=Files[0],                          $
                                   Lon=Lon,        Lat=Lat,                $
                                   Lev=LLev,       Tracer=Tracer[D],       $
                                   /Quiet,         /NoPrint,               $
                                   GridInfo=Grid,  _EXTRA=e )

      ; Error msg
      if ( not Success ) then begin
         ErrMsg = 'EMISSION_MAPS: Cannot read ' + StrTrim( Category, 2 ) + $
                  ' from '                      + StrTrim( Files[0], 2 ) + $
                  ' for tracer '                + Tracer
         Message, ErrMsg
      endif

      ; If this is a 3-D data block, then calculate the column emissions.
      if ( LLev[1] gt 1 ) then begin
         Data1 = Total( Data1, 3 )
      endif

      if ( keyword_set( AddBiofuel ) ) then begin

         ; Get biofuel emissions for the surface level
         Lev_bf  = [ 1, 1 ]
         Success = CTM_Get_DataBlock( Data1_bf,       Category_bf,         $
                                      File=Files[0],                       $
                                      Lon=Lon,        Lat=Lat,             $
                                      Lev=Lev_bf,     Tracer=Tracer_bf,    $
                                      /Quiet,         /NoPrint,            $
                                      GridInfo=Grid,  _EXTRA=e )

         ; Error msg
         if ( not Success ) then begin
         ErrMsg = 'EMISSION_MAPS: Cannot read ' + StrTrim( Category_bf, 2 ) + $
                  ' from '                      + StrTrim( Files[0], 2 )    + $
                  ' for tracer '                + Tracer
         Message, ErrMsg
         endif

         ; Compute anthro + biofuel emissions
         Data1 =  Data1 + Data1_bf

      endif

      ; Create the emissions map
      VerStr    = Versions[0]
      PlotTitle = VerStr +  '!C!C' + BotTitle
      PlotEmissions, Data1, PlotTitle, Grid, Unit, PS=PS, _EXTRA=e

      ;--------------------------------------------------------------------
      ; Read and plot data from Version2 file
      ;--------------------------------------------------------------------
      Success = CTM_Get_DataBlock( Data2,         Category[D],             $
                                   File=Files[1],                          $
                                   Lon=Lon,       Lat=Lat,                 $
                                   Lev=LLev,      Tracer=Tracer[D],        $
                                   /Quiet,        /NoPrint,                $
                                   GridInfo=Grid, _EXTRA=e )

      ; Error msg
      if ( not Success ) then begin
         ErrMsg = 'EMISSION_MAPS: Cannot read ' + StrTrim( Category, 2 ) + $
                  ' from '                      + StrTrim( Files[1], 2 ) + $
                  ' for tracer '                + Tracer
         Message, ErrMsg
      endif

      ; If this is a 3-D data block, then calculate the column emissions.
      if ( LLev[1] gt 1 ) then begin
         Data2 = Total( Data2, 3 )
      endif

      if ( keyword_set( AddBiofuel ) ) then begin

         ; Get biofuel emissions for the surface level
         Lev_bf  = [ 1, 1 ]
         Success = CTM_Get_DataBlock( Data2_bf,       Category_bf,         $
                                      File=Files[1],                       $
                                      Lon=Lon,        Lat=Lat,             $
                                      Lev=Lev_bf,     Tracer=Tracer_bf,    $
                                      /Quiet,         /NoPrint,            $
                                      GridInfo=Grid,  _EXTRA=e )

         ; Error msg
         if ( not Success ) then begin
         ErrMsg = 'EMISSION_MAPS: Cannot read ' + StrTrim( Category_bf, 2 ) + $
                  ' from '                      + StrTrim( Files[1], 2 )    + $
                  ' for tracer '                + Tracer
         Message, ErrMsg
         endif

         ; Compute anthro + biofuel emissions
         Data2 =  Data2 + Data2_bf

      endif

      ; Create the emissions map
      VerStr    = Versions[1]
      PlotTitle = VerStr +  '!C!C' + BotTitle
      PlotEmissions, Data2, PlotTitle, Grid, Unit, PS=PS, _EXTRA=e

      if ( nVersions eq 3 ) then begin

         ;--------------------------------------------------------------------
         ; Read and plot data from Version3 file
         ;--------------------------------------------------------------------
         Success = CTM_Get_DataBlock( Data3,         Category[D],             $
                                      File=Files[2],                          $
                                      Lon=Lon,       Lat=Lat,                 $
                                      Lev=LLev,      Tracer=Tracer[D],        $
                                      /Quiet,        /NoPrint,                $
                                      GridInfo=Grid, _EXTRA=e )
   
         ; Error msg
         if ( not Success ) then begin
            ErrMsg = 'EMISSION_MAPS: Cannot read ' + StrTrim( Category, 2 ) + $
                     ' from '                      + StrTrim( Files[2], 2 ) + $
                     ' for tracer '                + Tracer
            Message, ErrMsg
         endif
   
         ; If this is a 3-D data block, then calculate the column emissions.
         if ( LLev[1] gt 1 ) then begin
            Data3 = Total( Data3, 3 )
         endif
   
         if ( keyword_set( AddBiofuel ) ) then begin
   
            ; Get biofuel emissions for the surface level
            Lev_bf  = [ 1, 1 ]
            Success = CTM_Get_DataBlock( Data3_bf,       Category_bf,         $
                                         File=Files[2],                       $
                                         Lon=Lon,        Lat=Lat,             $
                                         Lev=Lev_bf,     Tracer=Tracer_bf,    $
                                         /Quiet,         /NoPrint,            $
                                         GridInfo=Grid,  _EXTRA=e )
   
            ; Error msg
            if ( not Success ) then begin
            ErrMsg = 'EMISSION_MAPS: Cannot read '+StrTrim( Category_bf, 2 ) + $
                     ' from '                     +StrTrim( Files[2], 2 )    + $
                     ' for tracer '               +Tracer
            Message, ErrMsg
            endif
   
            ; Compute anthro + biofuel emissions
            Data3 =  Data3 + Data3_bf
   
         endif
   
         ; Create the emissions map
         VerStr    = Versions[2]
         PlotTitle = VerStr +  '!C!C' + BotTitle
         PlotEmissions, Data3, PlotTitle, Grid, Unit, PS=PS, _EXTRA=e

      endif
      
      ; Plot the top title on each page  
      if ( D*4 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif

      ; Undefine variables
      Undefine, Grid
      Undefine, Data1
      Undefine, Data1_bf
      Undefine, Data2
      Undefine, Data2_bf
      Undefine, Data3
      Undefine, Data3_bf

   endfor

   ; Cancel multipanel settings
   Multipanel, /Off

end

;------------------------------------------------------------------------------

pro Emission_Maps_1yr, Files,    Versions,  PS=PS,  Month=Month,   $
                       Dir=Dir,  _EXTRA=e
 
   ;====================================================================
   ; Initialization
   ;====================================================================

   ; External functions
   FORWARD_FUNCTION Add_Separator, ChkStru, ColorBar_NDiv, Extract_FileName

   ; Arguments
   if ( N_Elements( Files ) ne N_Elements( Versions ) ) then $
      Message, 'Number of FILES does not equal number of VERSIONS!'

   ; Keywords
   if ( N_Elements( Month    ) ne 1 ) then Month = ''
   if ( N_Elements( Dir      ) ne 1 ) then Dir   = './'
   
   ; Number of model versions to compare
   nVersions = N_Elements( Versions )

   ; Save OUTDIR to a local shadow variable and 
   ; make sure that it ends with a path separator 
   OutDir   = Add_Separator( StrTrim( Dir, 2 ) )

   ; Define version string for output files
   if ( nVersions eq 2 ) then begin
      OutVersion = Versions[1]
   endif else begin
      OutVersion = Versions[2]
   endelse
   
   ; Top-of-plot title string
   TopTitle = 'GEOS-Chem Emission Maps!C!C'

   ; Save original color table
   TvLct, R, G, B, /Get

   ; Save current !MYCT sysvar settings 
   if ( ChkStru( !MYCT ) ) then Myct_Orig = !MYCT

   ; Load modified spectrum, extended to 12 colors
   MyCt, /ModSpec, NColors=12

   ; Use Postscript font
   !p.font = 0
   
   ;====================================================================
   ; Create emission plots for NOx
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'NO_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps' 
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'NO-AN-$',             $
                   'NO-AC-$',             $
                   'NO-BIOB',             $
                   'NO-FERT',             $
                   'NO-SOIL',             $
                   'NO-LI-$'              ]
   Title       = [ 'NO column anth+biof', $
                   'NO column aircraft',  $
                   'NO biomass',          $
                   'NO fertilizer',       $
                   'NO soil',             $
                   'NO column lightning'  ]
   Tracer      = [ 1,  1,  1, 1, 1, 1  ]
   MaxLev      = [ 72, 72, 1, 1, 1, 72 ]

   ; Define units
   Unit        = 'molec/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'NO-BIOF'
   Tracer_bf   = 1

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close the plot device (saves PS file to disk)
   Close_Device

   
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for CO
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'CO_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps' 
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'CO--SRCE',            $
                   'CO--SRCE',            $
                   'CO--SRCE',            $
                   'CO--SRCE'             ]
   Title       = [ 'CO column anth+biof', $
                   'CO biomass',          $
                   'CO from MONOT',       $
                   'CO ship'              ]
   Tracer      = [ 1,  2, 5, 6]
   MaxLev      = [ 72, 1, 1, 1]

   ; Define units
   Unit        = 'molec/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'CO--SRCE'
   Tracer_bf   = 3

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close the plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for ALK4
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'ALK4_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps' 
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'ANTHSRCE',              'BIOBSRCE'     ]
   Title       = [ 'ALK4 column anth+biof', 'ALK4 biomass' ]
   Tracer      = [  5,                       5             ]
   MaxLev      = [  72,                      1             ]

   ; Define units
   Unit        = 'atoms C/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'BIOFSRCE'
   Tracer_bf   = 5

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close the plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC   

   ;====================================================================
   ; Create emission plots for ISOP
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'ISOP_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'BIOGSRCE'      ]
   Title       = [ 'ISOP biogenic' ]
   Tracer      = [  1              ]
   MaxLev      = [  1              ]

   ; Define units
   Unit        = 'atoms C/cm2/s'

   ; No biofuel emissions
   Category_bf = ''
   Tracer_bf   = 0 

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close the plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for ACET
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'ACET_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps' 
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'ANTHSRCE',              $
                   'BIOBSRCE',              $
                   'BIOGSRCE',              $
                   'ACETSRCE',              $
                   'ACETSRCE',              $
                   'ACETSRCE',              $
                   'ACETSRCE'               ]
   Title       = [ 'ACET column anth+biof', $
                   'ACET biomass',          $
                   'ACET biogenic',         $
                   'ACET from MONOT',       $
                   'ACET from MBO',         $
                   'ACET direct',           $
                   'ACET ocean source'      ]
   Tracer      = [ 9,  9, 2, 1, 2, 3, 4 ]
   MaxLev      = [ 72, 1, 1, 1, 1, 1, 1 ]

   ; Define units
   Unit        = 'atoms C/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'BIOFSRCE'
   Tracer_bf   = 9

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for MEK
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'MEK_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps' 
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'ANTHSRCE',              'BIOBSRCE'    ] 
   Title       = [ 'MEK column anth+biofl', 'MEK biomass' ]
   Tracer      = [  10,                      10           ] 
   MaxLev      = [  72,                      1            ]

   ; Define units
   Unit        = 'atoms C/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'BIOFSRCE'
   Tracer_bf   = 10

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for ALD2
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'ALD2_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'ANTHSRCE',              $
                   'BIOBSRCE',              $
                   'BIOGSRCE'];,              $
;------------------------------------------------------------------------------
; Uncomment when v11-02a is version 1 (mps, 5/10/17)
;                   'BIOGSRCE',              $
;                   'BIOGSRCE'               ]
;------------------------------------------------------------------------------
   Title       = [ 'ALD2 column anth+biof', $
                   'ALD2 biomass',          $
                   'ALD2 biogenic'];,         $
;------------------------------------------------------------------------------
; Uncomment when v11-02a is version 1 (mps, 5/10/17)
;                   'ALD2 senescing',        $
;                   'ALD2 ocean source'      ]
;------------------------------------------------------------------------------
   Tracer      = [  11, 11, 16];, 27, 29 ]
   MaxLev      = [  72, 1,  1];,  1,  1  ]

   ; Define units
   Unit        = 'atoms C/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'BIOFSRCE'
   Tracer_bf   = 11

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

;------------------------------------------------------------------------------
; Uncomment when v11-02a is version 1 (mps, 5/10/17)
;   ;====================================================================
;   ; Create emission plots for RCHO
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'RCHO_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category    = [ 'ANTHSRCE' ]
;   Title       = [ 'RCHO anthro' ]
;   Tracer      = [  12 ]
;   MaxLev      = [  72 ]
;
;   ; Define units
;   Unit        = 'molec/cm2/s'
;
;   ; No biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0 
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for MACR
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'MACR_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category    = [ 'ANTHSRCE' ]
;   Title       = [ 'MACR anthro' ]
;   Tracer      = [  14 ]
;   MaxLev      = [  72 ]
;
;   ; Define units
;   Unit        = 'molec/cm2/s'
;
;   ; No biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0 
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;------------------------------------------------------------------------------

   ;====================================================================
   ; Create emission plots for PRPE
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'PRPE_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'ANTHSRCE',              $
                   'BIOBSRCE',              $
                   'BIOGSRCE'               ]
   Title       = [ 'PRPE column anth+biof', $
                   'PRPE biomass',          $
                   'PRPE biogenic'          ]
   Tracer      = [  18, 18, 3 ]
   MaxLev      = [  72, 1,  1 ]

   ; Define units
   Unit        = 'atoms C/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'BIOFSRCE'
   Tracer_bf   = 18

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for C3H8
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'C3H8_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'ANTHSRCE',               'BIOBSRCE'     ]
   Title       = [ 'C3H8 column anth+biofl', 'C3H8 biomass' ]
   Tracer      = [  19,                       19            ]
   MaxLev      = [  72,                       1             ]

   ; Define units
   Unit        = 'atoms C/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'BIOFSRCE'
   Tracer_bf   = 19

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for CH2O
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'CH2O_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'ANTHSRCE',              'BIOBSRCE'     ]
   Title       = [ 'CH2O column anth+biof', 'CH2O biomass' ]
   Tracer      = [  20,                      20            ]
   MaxLev      = [  72,                      1             ]

   ; Define units
   Unit        = 'molec/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'BIOFSRCE'
   Tracer_bf   = 20

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for C2H6
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'C2H6_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'ANTHSRCE',              'BIOBSRCE'     ]
   Title       = [ 'C2H6 column anth+biof', 'CH2O biomass' ]
   Tracer      = [  21,                      21            ]
   MaxLev      = [  72,                      1             ]

   ; Define units
   Unit        = 'atoms C/cm2/s'

   ; Info for biofuel emissions
   Category_bf = 'BIOFSRCE'
   Tracer_bf   = 21

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for DMS
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'DMS_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles and tracer numbers
   Category    = [ 'DMS-BIOG'     ]
   Title       = [ 'DMS biogenic' ]
   Tracer      = [  25            ]
   MaxLev      = [  1             ]

   ; Define units
   Unit        = 'kg S'

   ; No biofuel emissions
   Category_bf = ''
   Tracer_bf   = 0 

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for SO2
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'SO2_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'SO2-AN-$',                $
                   'SO2-AC-$',                $
                   'SO2-BIOF',                $
                   'SO2-BIOB',                $
                   'SO2-NV-$',                $
                   'SO2-EV-$',                $
                   'SO2-SHIP'                 ]
   Title       = [ 'SO2 column anthro',       $
                   'SO2 column aircraft',     $
                   'SO2 biofuel',             $
                   'SO2 biomass',             $
                   'SO2 column nonerup volc', $
                   'SO2 column erup volc',    $
                   'SO2 ship'                 ]   
   Tracer      = [ 26, 26, 26, 26, 26, 26, 26 ]
   MaxLev      = [ 72, 72, 1,  1,  72, 72, 1  ]

   ; Define units
   Unit        = 'kg S'

   ; Info for biofuel emissions
   Category_bf = 'SO2-BIOF'
   Tracer_bf   = 26

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for SO4
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'SO4_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'SO4-AN-$'             ]
   Title       = [ 'SO4 column anth+biof' ]
   Tracer      = [  27 ]
   MaxLev      = [  72 ]

   ; Define units
   Unit        = 'kg S'

   ; Info for biofuel emissions
   Category_bf = 'SO4-BIOF'
   Tracer_bf   = 27

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for NH3
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'NH3_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'NH3-ANTH',           $
                   'NH3-BIOF',           $
                   'NH3-BIOB',           $
                   'NH3-NATU'            ]
   Title       = [ 'NH3 column anthro',  $
                   'NH3 biofuel',        $
                   'NH3 biomass',        $
                   'NH3 natural src'     ]
   Tracer      = [  30, 30, 30, 30 ]
   MaxLev      = [  72, 1,  1,  1  ]

   ; Define units
   Unit        = 'kg'

   ; Biofuel emissions are handled above
   Category_bf = ''
   Tracer_bf   = 0

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for BC (black carbon)
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'BC_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'BC-ANTH',          $
                   'BC-BIOF',          $
                   'BC-BIOB'           ]
   Title       = [ 'BC column anthro', $
                   'BC biofuel',       $
                   'BC biomass'        ]
   Tracer      = [  34, 34, 34 ]
   MaxLev      = [  72, 1,  1  ]

   ; Define units
   Unit        = 'kg C'

   ; Biofuel emissions are handled above
   Category_bf = ''
   Tracer_bf   = 0

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for OC (organic carbon)
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'OC_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'OC-ANTH',           $
                   'OC-BIOF',           $
                   'OC-BIOB',           $
                   'OC-BIOG'            ]
   Title       = [ 'OC column anthro',  $
                   'OC biofuel',        $
                   'OC biomass',        $
                   'OC biogenic'        ]
   Tracer      = [  35, 35, 35, 35 ]
   MaxLev      = [  72, 1,  1,  1  ]

   ; Define units
   Unit        = 'kg C'

   ; Biofuel emissions are handled above
   Category_bf = ''
   Tracer_bf   = 0

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for DST1-4 (dust aerosol)
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'DUST_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'DUSTSRCE', 'DUSTSRCE', 'DUSTSRCE', 'DUSTSRCE'  ]
   Title       = [ 'DST1',     'DST2',     'DST3',     'DST4'      ]
   Tracer      = [  38,         39,         40,         41         ]
   MaxLev      = [  1,          1,          1,          1          ]

   ; Define units
   Unit        = 'kg'

   ; No biofuel emissions
   Category_bf = ''
   Tracer_bf   = 0

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e
              
   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for SALA, SALC (sea salt aerosol)
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'SALT_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'SALTSRCE',            'SALTSRCE'             ]
   Title       = [ 'Accum-mode sea salt', 'Coarse-mode sea salt' ]
   Tracer      = [  42,                    43                    ]
   MaxLev      = [  1,                     1                     ]

   ; Define units
   Unit        = 'kg'

   ; No biofuel emissions
   Category_bf = ''
   Tracer_bf   = 0

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for CHBr3
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'CHBr3_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'BIOGSRCE'       ]
   Title       = [ 'CHBr3 biogenic' ]
   Tracer      = [  23              ]
   MaxLev      = [  1               ]

   ; Define units
   Unit        = 'kg/m2/s'

   ; No biofuel emissions
   Category_bf = ''
   Tracer_bf   = 0

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for CH2Br2
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'CH2Br2_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'BIOGSRCE'        ]
   Title       = [ 'CH2Br2 biogenic' ]
   Tracer      = [  24               ]
   MaxLev      = [  1                ]

   ; Define units
   Unit        = 'kg/m2/s'

   ; No biofuel emissions
   Category_bf = ''
   Tracer_bf   = 0

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

   ;====================================================================
   ; Create emission plots for Br2
   ;====================================================================

   ; Open the plot device and initialize the page
   OutFileName = OutDir + 'Br2_emission_maps_' + Month + '.'  $
               + OutVersion + '.ps'
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Category, plot titles, tracer numbers, and levels
   Category    = [ 'BIOGSRCE'       ]
   Title       = [ 'Br2 biogenic'   ]
   Tracer      = [  25              ]
   MaxLev      = [  1               ]

   ; Define units
   Unit        = 'kg/m2/s'

   ; No biofuel emissions
   Category_bf = ''
   Tracer_bf   = 0

   ; Create the emission plots
   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
                  _EXTRA=e

   ; Close plot device (saves PS file to disk)
   Close_Device
   CTM_Cleanup, /No_GC

;------------------------------------------------------------------------------
; Uncomment when v11-02a is version 1 (mps, 5/10/17)
;   ;====================================================================
;   ; Create emission plots for NO2
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'NO2_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category    = [ 'ANTHSRCE' ]
;   Title       = [ 'NO2 anthro' ]
;   Tracer      = [  12 ]
;   MaxLev      = [  72 ]
;
;   ; Define units
;   Unit        = 'molec/cm2/s'
;
;   ; No biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0 
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for HNO2
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'HNO2_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category    = [ 'ANTHSRCE' ]
;   Title       = [ 'HNO2 anthro' ]
;   Tracer      = [  12 ]
;   MaxLev      = [  72 ]
;
;   ; Define units
;   Unit        = 'molec/cm2/s'
;
;   ; No biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0 
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for MTPA
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'MTPA_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category    = [ 'BIOBSRCE',     $
;                   'OC-MTPA'       ]
;   Title       = [ 'MTPA biomass', $
;                   'MTPA biogenic' ]
;   Tracer      = [  99, 99 ]
;   MaxLev      = [  1,  1  ]
;
;   ; Define units
;   Unit        = 'kg'
;
;   ; No biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for LIMO
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'LIMO_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category    = [ 'OC-LIMO'        ]
;   Title       = [ 'LIMO biogenic'  ]
;   Tracer      = [  100             ]
;   MaxLev      = [  1               ]
;
;   ; Define units
;   Unit        = 'kg'
;
;   ; No biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for MTPO
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'MTPO_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category    = [ 'OC-MTPO'        ]
;   Title       = [ 'MTPO biogenic'  ]
;   Tracer      = [  101             ]
;   MaxLev      = [  1               ]
;
;   ; Define units
;   Unit        = 'kg'
;
;   ; No biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for BENZ
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'BENZ_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'ANTHSRCE',              $
;                'BIOBSRCE'               ]
;   Title    = [ 'BENZ column anth+biof', $
;                'BENZ biomass'           ]
;   Tracer   = [  116, 116 ]
;   MaxLev   = [  72,  1   ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = 'BIOFSRCE'
;   Tracer_bf   = 116
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for TOLU
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'TOLU_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'ANTHSRCE',              $
;                'BIOBSRCE'               ]
;   Title    = [ 'TOLU column anth+biof', $
;                'TOLU biomass'           ]
;   Tracer   = [  117, 117 ]
;   MaxLev   = [  72,  1   ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = 'BIOFSRCE'
;   Tracer_bf   = 117
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for XYLE
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'XYLE_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'ANTHSRCE',              $
;                'BIOBSRCE'               ]
;   Title    = [ 'XYLE column anth+biof', $
;                'XYLE biomass'           ]
;   Tracer   = [  118, 118 ]
;   MaxLev   = [  72,  1   ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = 'BIOFSRCE'
;   Tracer_bf   = 118
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for EOH
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'EOH_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'ANTHSRCE',             $
;                'BIOBSRCE',             $
;                'BIOGSRCE',             $
;                'BIOGSRCE'              ]
;   Title    = [ 'EOH column anth+biof', $
;                'EOH biomass',          $
;                'EOH biogenic',         $
;                'EOH senescing'         ]
;   Tracer   = [  126, 126, 19, 28 ]
;   MaxLev   = [  72,  1,   1,  1  ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = 'BIOFSRCE'
;   Tracer_bf   = 126
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for MGLY
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'MGLY_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOBSRCE'     ]
;   Title    = [ 'MGLY biomass' ]
;   Tracer   = [  127           ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for Total Monoterpenes
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'MONX_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'MONX biogenic']
;   Tracer   = [  4             ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for Methyl butenol
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'MBOX_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'MBOX biogenic']
;   Tracer   = [  5             ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for Alpha-pinene
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'APIN_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'APIN biogenic']
;   Tracer   = [  7             ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for Beta-pinene
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'BPIN_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'BPIN biogenic']
;   Tracer   = [  8             ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for Limonene
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'LIMO_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'LIMO biogenic']
;   Tracer   = [  9             ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for Sabinene
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'SABI_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'SABI biogenic']
;   Tracer   = [  10            ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for Myrcene
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'MYRC_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'MYRC biogenic']
;   Tracer   = [  11            ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for 3-Carene
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'CARE_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'CARE biogenic']
;   Tracer   = [  12            ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for 3-Carene
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'CARE_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'CARE biogenic']
;   Tracer   = [  12            ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for Other Monoterpenes
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'OMON_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'OMON biogenic']
;   Tracer   = [  17            ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for a-Farnesene
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'FARN_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'FARN biogenic']
;   Tracer   = [  20            ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for b-Caryophyllene
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'BCAR_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'BCAR biogenic']
;   Tracer   = [  21            ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;
;   ;====================================================================
;   ; Create emission plots for Other Sesquiterpenes
;   ;====================================================================
;
;   ; Open the plot device and initialize the page
;   OutFileName = OutDir + 'OSQT_emission_maps_' + Month + '.'  $
;               + OutVersion + '.ps'
;   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
;
;   ; Category, plot titles, tracer numbers, and levels
;   Category = [ 'BIOGSRCE'     ]
;   Title    = [ 'OSQT biogenic']
;   Tracer   = [  22            ]
;   MaxLev   = [  1             ]
;
;   ; Define units
;   Unit        = 'atoms C/cm2/s'
;
;   ; Info for biofuel emissions
;   Category_bf = ''
;   Tracer_bf   = 0
;
;   ; Create the emission plots
;   CreateEmPlots, Files,   Versions, Category, Category_bf, Tracer, Tracer_bf, $
;                  Title,   TopTitle, Month,    MaxLev,      Unit,   PS=PS,     $
;                  _EXTRA=e
;
;   ; Close plot device (saves PS file to disk)
;   Close_Device
;------------------------------------------------------------------------------

   ;====================================================================
   ; Cleanup and quit
   ;====================================================================
Quit:

   ; Restore original color table
   TvLct, R, G, B

   ; Restore previous !MYCT sysvar settings
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig

   ; Quit
   return
end
 
