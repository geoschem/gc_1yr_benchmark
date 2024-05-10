;-----------------------------------------------------------------------
;+
; NAME:
;        PLOT_PASSIVE
;
; PURPOSE:
;        Produces maps of tracers and other quantities from a GEOS-Chem
;        RnPbBePassive benchmark simulation (for model validation).
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        PLOT_PASSIVE, INPUTFILE, [, Keywords ]
;
; INPUTS:
;        INPUTFILE -> A file containing default values for version 
;             numbers, directories, model names, resolutions, etc.
;             Default is "input_bm.1yr"
;
; KEYWORD PARAMETERS:
;        By default, PLOT_PASSIVE will produce the following types
;        of output:
;          (a) Table of total tracer mass
;          (b) Maps of tracer concentration @ surface and 500 hPa
;          (c) Maps of zonal tracer concentration
;
;        Each of these types of output can be turned off individually
;        with the following keywords:
;
;        /NO_MASS -> Do not calculate total tracer mass
;
;        /NO_CONC_MAPS -> Do not create the plot the maps of tracer
;             concentrations @ sfc and 500 hPa altitude.
;
;        /NO_ZONALCONC -> Do not create the maps of zonal tracer concentrations
;
;        Additional keywords:
;        --------------------
;
;        /DEBUG -> Set this switch to print the values of the various
;             input variables.  Use this to make sure that all values
;             have been created corectly.
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines:
;        ====================================================
;        GetSfc500Levels (function)  
;
;        External Subroutines Required:
;        ====================================================
;        CTM_NAMEXT (function)    CTM_TYPE (function) 
;        DIFFERENCES              FREQ_DIST         
;        FULLCHEM_BUDGET          FULLCHEM_EMISSIONS
;        JV_RATIO                 PROFILES
;        MAPS                     NYMD2TAU (function)
;        RATIOS                   REPLACE_TOKEN
;        STRUADDVAR (function)    MCF_LIFETIME (function)
;
; REQUIREMENTS:
;        References routines from the GAMAP package.
;
; NOTES:
;        PLOT_PASSIVE assumes that the following GEOS-Chem
;        diagnostic quantities have been archived:
;
;          (a) ND45 ("IJ-AVG-$")
;
; EXAMPLES:
;        PLOT_PASSIVE, 'input.1yr'
;
;            ; Produces the full suite of benchmark output plots.
;
;        PLOT_PASSIVE, 'input.1yr', /DEBUG, /NO_MASS
;
;            ; Will produce the standard plots except for the
;            ; total tracer mass.  Also will cause extra debug
;            ; information to be echoed to the screen.
;
; MODIFICATION HISTORY:
;        mps, 23 Nov 2015: Initial version based on "benchmark_1mon.pro"
;------------------------------------------------------------------------------

function GetSfc500Levels, ModelName

   ;====================================================================
   ; Internal function GetSfc500Level returns the surface and 500hPa 
   ; levels, depending on the met field type (bmy, 11/14/07)
   ;====================================================================

   ; Surface level
   Level_Sfc  = 1

   ; 500hPa level
   case ( ModelName ) of
      'GEOSFP'     : Level_500 = 23
      'GEOSFP_47L' : Level_500 = 23
      'MERRA'      : Level_500 = 23
      'MERRA_47L'  : Level_500 = 23
      'MERRA2'     : Level_500 = 23
      'MERRA2_47L' : Level_500 = 23
      'GEOS5'      : Level_500 = 23
      'GEOS5_47L'  : Level_500 = 23
      'GEOS4_30L'  : Level_500 = 8
      else         : Level_500 = -1
   endcase

   ; Return to calling program
   return, [ Level_Sfc, Level_500 ]
end

;-----------------------------------------------------------------------------

pro Plot_Passive, InputFile,                    $
                  Debug        = Debug,         $
                  No_Mass      = No_Mass,       $
                  No_Conc_Maps = No_Conc_Maps,  $
                  No_ZonalConc = No_ZonalConc,  $
                  _EXTRA=e

   ;====================================================================
   ; Initialization -- read default parameters from input file 
   ;====================================================================

   ; External Functions
   FORWARD_FUNCTION CTM_NamExt,    CTM_Type,  Nymd2Tau, $
                    Replace_Token, StruAddVar

   ; Arguments
   if ( N_Elements( InputFile ) eq 0 ) then InputFile = 'input_bm.1yr'
   
   ; Keywords
   Debug        =  Keyword_Set( Debug        )
   Do_Mass      = ~Keyword_Set( No_Mass      )
   Do_Conc_Maps = ~Keyword_Set( No_Conc_Maps ) 
   Do_ZonalConc = ~Keyword_Set( No_ZonalConc )

   ;====================================================================
   ; Read default settings from INPUTFILE
   ;====================================================================

   ; Open the input file
   Open_File, InputFile, Ilun, /Get_Lun

   ; Define string variable
   Line = ''
   
   ; Loop thru file
   while ( not EOF( Ilun ) ) do begin

      ; Read line and break on colon
      ReadF, Ilun, Line
      Result = StrTrim( StrBreak( Line, ':' ), 2 )

      ; Parse value of line into individual variables
      case ( StrUpCase( Result[0] ) ) of 
         'VERSION'    : Version      = Result[1]
         'MODEL'      : Model        = Result[1]
         'RES'        : Res          = Long( Result[1] )
         'YEAR'       : Year         = Long( Result[1] )
         'RUNDIR'     : RunDir       = Result[1]
         'FILE'       : File         = Result[1]
         'OUTPUTDIR'  : OutputDir    = Result[1]
         'TRAC_MASS'  : Trac_Mass    = Result[1]
         'CONC_MAPS'  : Conc_Plots   = Result[1]
         'ZONAL_CONC' : ZonalC_Plots = Result[1]
         else : ; Do nothing
      endcase
   endwhile

   ; Close input file
   Close,    Ilun
   Free_Lun, Ilun

   ;====================================================================
   ; Create directory, filename, and other relevant variables
   ;====================================================================

   ; Define structure for token replacement
   Token       = { VERSION:Version,  MODEL:Model }

   ; Replace tokens in run directory variables
   RunDir      = Replace_Token( RunDir, Token )

   ; Add run directory variables to TOKEN structure
   Token       = StruAddVar( Token, { RUNDIR:RunDir } )

   ; Replace tokens in output directory variable
   OutputDir   = Replace_Token( OutputDir,   Token )

   ; Add output directory variables to TOKEN structure
   Token       = StruAddVar( Token, { OUTPUTDIR:OutputDir } )
   
   ; Replace tokens in the rest of the variables
   File         = Replace_Token( File,         Token )
   Trac_Mass    = Replace_Token( Trac_Mass,    Token )
   Conc_Plots   = Replace_Token( Conc_Plots,   Token )
   ZonalC_Plots = Replace_Token( ZonalC_Plots, Token )
   
   ;====================================================================
   ; Define some plotting variables
   ;====================================================================

   ; Surface and 500hPa levels from both models ( sfc1, 500 )
   Levels_1    = GetSfc500Levels( Model )
   Levels      = [ Levels_1[0], Levels_1[1] ]
   
   ; Model and grid info
   ModelInfo = CTM_Type( Model, Res=Res )
   GridInfo  = CTM_Grid( ModelInfo )
   Dlat      = GridInfo.DJ
   Dlon      = GridInfo.DI
   Ptop      = GridInfo.Pedge[ GridInfo.LMX ]
   Nalt      = GridInfo.LMX

   ; RUNNAME is the tag for the PostScript files
   if ( N_Elements( RunName ) eq 0 ) then RunName = Version

   ; Redirect PostScript output (end w/ slash)
   if ( N_Elements( PSDir ) eq 0 ) then PSDir   = './'

   ; Plot only PASV tracer (in v/v dry air, v/v total air, and kg/kg total air)
   Tracers = [ 4, 5, 6 ]
   
   ;====================================================================
   ; Debug output
   ;====================================================================
   if ( Debug ) then begin

      print, '%%% Model Info %%%'
      print, 'Version     : ', Version
      print, 'Model       : ', Model
      print, 'Res         : ', Res
      print, 'Year        : ', Year
      print, 'RunDir      : ', RunDir
      print, 'File        : ', File
      print
      print, '%%% For Plotting %%%'
      print, 'OutputDir   : ', OutputDir   
      print, 'Trac_Mass   : ', Trac_Mass
      print, 'Conc_Plots  : ', Conc_Plots 
      print, 'ZonalC_Plots: ', ZonalC_Plots

   endif
   
   ;====================================================================
   ; Use the metadata from the diaginfo.dat and tracerinfo.dat
   ; files contained in the run directory.  Refresh every time.
   ;====================================================================

   ; File names 
   DInfo = StrTrim( RunDir, 2 ) + '/diaginfo.dat'
   TInfo = StrTrim( RunDir, 2 ) + '/tracerinfo.dat' 

   ; Load metadata from diaginfo.dat (if we find it)
   if ( File_Exist( Dinfo ) )                                           $
      then CTM_DiagInfo, FileName=DInfo, /Force                         $ 
      else Message, 'Could not find the proper diaginfo.dat file!'

   ; Load metadata from tracerinfo.dat (if we find it)
   if ( File_Exist( Tinfo ) )                                           $
      then CTM_TracerInfo, FileName=Tinfo, /Force                       $ 
      else Message, 'Could not find the proper diaginfo.dat file!'

   ;====================================================================
   ; Make the plots
   ;====================================================================

   ;--------------------------------------------------------------------
   ; Tracer mass for each month
   ;--------------------------------------------------------------------
   if ( Do_Mass ) then begin

      ; Input file with 6-hr mass
      MassFile = 'tracer_mass_kg.txt'
      
      ; Echo info
      Message, 'Calculating total tracer mass ...', /Info

      ; Compute monthly tracer mass
      Monthly_Mass, RunDir, MassFile, Year, Version, $
                    OutFile=Trac_Mass, _EXTRA=e

   endif
   
;   ;--------------------------------------------------------------------
;   ; Tracer concentration maps  @ sfc & 500hPa
;   ;--------------------------------------------------------------------
;   if ( Do_Conc_Maps ) then begin
;
;      ; Echo info
;      Message, 'Generating tracer concentration maps ...', /Info
;
;      ; Create tracer concentration maps at surface and 500 hPa
;      Season_Maps, File, Levels, Year, Tracers, Version, /PS, $
;                   OutFile=Conc_Plots, _EXTRA=e
;
;   endif
;
;   ;--------------------------------------------------------------------
;   ; Zonal mean concentration plots
;   ;--------------------------------------------------------------------
;   if ( Do_ZonalConc ) then begin
;
;      ; Echo info
;      Message, 'Generating zonal mean concentration plots ...', /Info
;
;      Lons = 0
;
;      ; Create zonal mean concentration plots
;      Season_Zonal, File, Lons, Year, Tracers, Version, /PS,       $
;                    OutFile=ZonalC_Plots,  /Pressure, $
;                    _EXTRA=e
;                                                            
;   endif

   ; Create PDF files from PS files
   ;make_pdf, OutputDir
   
   ;====================================================================
   ; Cleanup and Quit
   ;====================================================================
Quit:

end
