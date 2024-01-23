;-----------------------------------------------------------------------
;+
; NAME:
;        STRAT_DIFF_1YR
;
; PURPOSE:
;        Creates zonal mean absolute and percent difference plots
;        of tracers in the stratosphere (~400 hPa to max lev) from the GEOS-Chem
;        1-year benchmark simulations.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        STRAT_DIFF_1YR, FILES, TRACERS, VERSIONS, [, Keywords ]
;
; INPUTS:
;        FILES -> A 2-element vector containing the names of files
;             from the "old" and "new" GEOS-Chem model versions
;             that are to be compared. 
;
;        TRACERS -> The list of transported tracers (i.e. diagnostic
;             category "IJ-AVG-$").
;
;        VERSIONS -> A 2-element vector containing the version
;             numbers for the "old" and "new" GEOS-Chem model
;             versions.
;
; KEYWORD PARAMETERS:
;        /DYNRANGE -> Set this switch to create plots using the whole
;             dynamic range of the data.  Default is to restrict
;             the plot range to predetermined values as returned
;             by routine GET_DIFF_RANGE.
;
;        /PS -> Set this switch to generate PostScript output.
;
;        OUTFILENAME -> If /PS is set, will write PostScript output 
;             to a file whose name is specified by this keyword.
;             Default is "tracer_ratio.pro".
;
;        ZDFORMAT -> This keyword passes a colorbar format string
;             (Fortran-style) to the COLORBAR routine (via TVPLOT).
;             This keyword is purposely not named CBFORMAT, in order
;             to avoid passing this quantity to other routines.             
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines Included:
;        ==================================================
;        PlotStratAbsDiff      PlotStratPctDiff
;
;        External Subroutines Required:
;        ==================================================
;        CLOSE_DEVICE          COLORBAR_NDIV    (function)   
;        CTM_GET_DATA          GET_DIFF_RANGE   (function)   
;        GETMODELANDGRIDINFO   EXTRACT_FILENAME (function)   
;        MULTIPANEL            CHKSTRU          (function)   
;        MYCT                  OPEN_DEVICE                   
;        TVPLOT                UNDEFINE   
;     
; REQUIREMENTS:
;        References routines from the GAMAP package.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR.
;
; EXAMPLE:
;        FILES    = [ PREF1+'0101.nc', PREF2+'0101.nc', PREF3+'0101.nc' ]
;        TRACERS  = INDGEN( 43 ) + 1
;        VERSIONS = [ VERS1, VERS2, VERS3 ]
;        PSNAME   = PSDIR + 'Strat_Differences_Jan.' + RUNNAME + '.ps'
;
;        STRAT_DIFF_1YR, FILES, TRACERS, VERSIONS, $
;             /PS, OUTFILENAME=PSNAME
;
;             ; Creates zonal mean difference plots from 3 different model 
;             ; versions using netCDF output files from the various GEOS-Chem
;             ; 1-yr benchmark simulations.  (NOTE: this is the actual
;             ; calling sequence from driver routine BENCHMARK_1YR.)
;             
;        STRAT_DIFF_1YR, FILES, TRACERS, VERSIONS, $
;             /DYNRANGE, /PS, OUTFILENAME=PSNAME
;
;             ; Same as the above, but will create zonal mean difference
;             ; maps  using the full dynamic range of the data (centered
;             ; around zero) instead of using pre-defined min & max values.
;
; MODIFICATION HISTORY:
;        mps, 04 Dec 2015: - Initial version, based on strat_diff.pro
;        mps, 27 Jan 2017: - Update to allow for comparison of 2 versions,
;                            intead of the default 3 versions
;        ewl, 28 Jun 2017: - Use Model1 diagnostics for Model2 if GCHP benchmark
;        mps, 14 Mar 2018: - Now plot data above 400 hPa to capture entire
;                            stratosphere
; 
;-
; Copyright (C) 2011-2012, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to yantosca@seas.harvard.edu
; or with subject "IDL routine strat_diff"
;-----------------------------------------------------------------------

pro PlotStratAbsDiff, Data1,       Data2,     Version1,  Version2,          $
                      TracerName,  GridInfo,  DynRange,  Month,             $
                      Unit,        _EXTRA=e
   
   ;====================================================================
   ; Internal routine PlotStratAbsDiff plots the zonal mean absolute
   ; difference for a given tracer between two benchmark simulations.
   ;====================================================================

   ; Keywords
   Percent     = Keyword_Set( Percent  )

   ; Version string
   VerStr = Version2 + ' - ' + Version1

   if ( N_Elements( Format ) eq 0 ) then Format = '(e13.3)' 

   ; Grid dimension parameters
   S           = Size( Data1, /Dim )
   Lats        = GridInfo.YMid
   N_Lon       = Double( S[0] )

   ; Set Y-axis coordinate for pressure
   YMid = GridInfo.PMid[0:S[2]-1L]

   ; Select data for the stratosphere
   if ( TracerName eq 'OH' or TracerName eq 'HO2' ) then begin
      YStrat      = YMid[26:58]
      Strat1      = Data1[*,*,26:58]
      Strat2      = Data2[*,*,26:58]
   endif else begin
      YStrat      = YMid[34:71]
      Strat1      = Data1[*,*,26:71]
      Strat2      = Data2[*,*,26:71]
   endelse
   
   ; Missing data value
   FILLVALUE   = -9.99e30

   ;-----------------------------------------------------------------
   ; GEOS-Chem advected tracers are defined at all vertical levels,
   ; so we don't have to neglect the stratospheric boxes.
   ;-----------------------------------------------------------------

   ; Compute absolute differences
   AbsDiff   = Strat2 - Strat1

   ; Compute zonal mean absolute differences
   ZAbsDiff  = Total( AbsDiff, 1 ) / N_Lon

   ; Undefine
   Undefine, BotOut
   UnDefine, AbsDiff

   ; Replace denormal data with a missing value 
   Ind         = Where( ~Finite( ZAbsDiff ) )
   if ( Ind[0] ge 0 ) then ZAbsDiff[Ind] = FILLVALUE

   ;====================================================================
   ; Set plot parameters
   ;====================================================================

   ; Parameters common to both dyn range and pre-defined range plots
   XTickV         = [ -90, -60, -30, 0, 30, 60, 90 ]
   XTicks         = N_Elements( XTickV )-1L
   XMinor         = 3
   XTitle         = 'Latitude'
   yTitle         = 'Pressure (hPa)'
   Title          = VerStr     + '!C!C'                        +       $
                    TracerName + ' - Strat zonal mean abs diff for ' + Month 
   Div            = Colorbar_Ndiv( 6 )

   ; Get the difference range and unit
   ; NOTE: For dynamic range, plots, the RANGE will be ignored
   Get_Diff_Range, TracerName, Range, Unit

   if ( DynRange ) then begin

      ;-----------------------------------------------------------------
      ; Set plot parameters for the full dynamic range of the data
      ; Center the range around zero symmetrically
      ;-----------------------------------------------------------------
      Good     =  Where( ZAbsDiff gt FILLVALUE )
      MinData  =  Min( ZAbsDiff[Good], Max=MaxData )
      Extreme  =  Max( [ Abs( MinData ), Abs( MaxData ) ] )
      MinData  = -Extreme
      MaxData  =  Extreme
      BotOut   =  !MYCT.BOTTOM
      Triangle =  1
      NoGap    =  1
      Upos     =  1.1
      
      ; We need to set the colorbar a little bit lower for PostScript
      CBPos = [ 0.10, -0.02, 0.90, 0.01 ]

   endif else begin

      ;-----------------------------------------------------------------
      ; Set plot parameters using the pre-defined min & max values from
      ; function GET_DIFF_RANGE.  This is the default.
      ;-----------------------------------------------------------------
      MinData  =  Range[0] 
      MaxData  =  Range[1]
      BotOut   =  !MYCT.BOTTOM
      Triangle =  1
      NoGap    =  1
      UPos     =  1.02
      
      ; We need to set the colorbar a little bit lower for PostScript  
      CBPos = [ 0.05, -0.02, 0.95, 0.01 ]

   endelse

   ; For OH, let's rescale the unit for clarity
   if ( TracerName eq 'OH' ) then begin
      ZAbsDiff = ZAbsDiff / 1e5
      MinData  = MinData  / 1e5
      MaxData  = MaxData  / 1e5
      Unit     = '1e5 molec/cm3'
   endif

   ; For HO2, let's rescale the unit for clarity
   if ( TracerName eq 'HO2' ) then begin
      ZAbsDiff = ZAbsDiff / 1e-12
      MinData  = MinData  / 1e-12
      MaxData  = MaxData  / 1e-12
      Unit     = 'pptv'
   endif

   ; Use exponents to avoid colorbars with ranges 0.0000-0.0000
   if ( MaxData lt 0.0001 ) then begin
      Format = '(e13.3)'
   endif else if ( MaxData gt 10000.0 ) then begin
      Format = '(f13.0)'
   endif else begin
      Format = '(f13.4)'
   endelse

   ;====================================================================
   ; Create the plot!
   ;====================================================================

   ; Plot zonal mean differences
   TvPlot, ZAbsDiff, Lats, YStrat,                                          $
           /Cbar,             Division=Div,      /Sample,                   $
           Title=Title,       MinData=MinData,   MaxData=MaxData,           $
           CBFormat=Format,   BOR_Label=' ',     Unit=Unit,                 $
           /XStyle,           XTickV=XTickV,     XTicks=XTicks,             $
           XMinor=XMinor,     XTitle=Xtitle,     YTitle=YTitle,             $
           /YStyle,           Triangle=Triangle, NoGap=NoGap,               $
           CBPosition=CBPos,  UPos=UPos,         BotOut=BotOut,             $
           Min_Valid=MinData, _EXTRA=e

end

;------------------------------------------------------------------------------

pro PlotStratPctDiff, Data1,       Data2,     Version1,    Version2,        $
                      TracerName,  GridInfo,  DynRange,    Month,           $
                      Unit,        _EXTRA=e
   
   ;====================================================================
   ; Internal routine PlotStratPctDiff plots the zonal mean percent
   ; difference for a given tracer between two benchmark simulations.
   ;====================================================================

   ; Keywords
   Percent     = Keyword_Set( Percent )

   ; Version string
   VerStr = Version2 + ' / ' + Version1

   if ( N_Elements( Format ) eq 0 ) then Format = '(f14.4)' 

   ; Index arrays
   S           = Size( Data1, /Dim )
   Lats        = GridInfo.YMid
   N_Lon       = Double( S[0] )

   ; Set Y-axis coordinate for pressure or altitude grid (bmy, 7/18/11)
   YMid = GridInfo.PMid[0:S[2]-1L]

   ; Select data for the stratosphere
   if ( TracerName eq 'OH' or TracerName eq 'HO2' ) then begin
      YStrat      = YMid[26:58]
      Strat1      = Data1[*,*,26:58]
      Strat2      = Data2[*,*,26:58]
   endif else begin
      YStrat      = YMid[34:71]
      Strat1      = Data1[*,*,26:71]
      Strat2      = Data2[*,*,26:71]
   endelse
   
   ; Missing data value
   FILLVALUE   = -9.99e30

   ;-----------------------------------------------------------------
   ; GEOS-Chem advected tracers are defined at all vertical levels,
   ; so we don't have to neglect the stratospheric boxes.
   ;-----------------------------------------------------------------

   ; Compute percent differences
   PctDiff     = ( ( Strat2 - Strat1 ) / Strat1 ) * 100d0

   ; Compute zonal mean percent differences
   ZPctDiff    = Total( PctDiff, 1 ) / N_Lon

   ; Replace denormal data with a missing value 
   Ind         = Where( ~Finite( ZPctDiff ) )
   if ( Ind[0] ge 0 ) then ZPctDiff[Ind] = FILLVALUE

   ;=================================================================
   ; Plot percent difference
   ;=================================================================
   
   ; Parameters common to both dyn range and pre-defined range plots
   XTickV      = [ -90, -60, -30, 0, 30, 60, 90 ]
   XTicks      = N_Elements( XTickV )-1L
   XMinor      = 6
   XTitle      = 'Latitude'
   yTitle      = 'Pressure (hPa)'
   Title       = VerStr     + '!C!C'                        +       $
                 TracerName + ' - Strat zonal mean % diff for ' + Month 
   Div         = Colorbar_Ndiv( 6 )

   if ( DynRange ) then begin

      ;-----------------------------------------------------------------
      ; Parameters specific to the dynamic range plots
      ; Center the range around zero symmetrically
      ;-----------------------------------------------------------------
      Good     =  Where( ZPctDiff gt FILLVALUE )

      MinData  =  Min( ZPctDiff[Good], Max=MaxData )
      Extreme  =  Max( [ Abs( MinData ), Abs( MaxData ) ] )

      ; Prevent ratios from going out of range by capping at 1000%
      ; (mpayer, 8/7/13)
      if ( Extreme gt 1000d0 ) then Extreme = 1000d0
      MinData  = -Extreme
      MaxData  =  Extreme

      BotOut   =  !MYCT.BOTTOM
      Triangle =  1
      NoGap    =  1
      Upos     =  1.1
      
      ; We need to set the colorbar a little bit lower for PostScript
      CBPos = [ 0.10, -0.02, 0.90, 0.01 ]

   endif else begin

      ;-----------------------------------------------------------------
      ; Parameters specific to the plots w/ pre-defined min & max 
      ; limits (i.e. returned by function GET_DIFF_RANGE.
      ;-----------------------------------------------------------------
      MinData  = -30
      MaxData  = +30
      BotOut   =  !MYCT.BOTTOM
      Triangle =  1
      NoGap    =  1
      UPos     =  1.02
      
      ; We need to set the colorbar a little bit lower for PostScript  
      CBPos = [ 0.05, -0.02, 0.95, 0.01 ]
                    
   endelse
         
   ;====================================================================
   ; Create the plot!
   ;====================================================================
  
   ; Plot percent difference 
   TvPlot, ZPctDiff, Lats, YStrat,                                           $
           /Cbar,              Division=Div,      /Sample,                   $
           Title=Title,        MinData=MinData,   MaxData=MaxData,           $
           CBFormat='(f14.4)', BOR_Label=' ',     Unit='%',                  $
           /XStyle,            XTickV=XTickV,     XTicks=XTicks,             $
           XMinor=XMinor,      XTitle=XTitle,     YTitle=YTitle,             $
           /YStyle,            Triangle=Triangle, NoGap=NoGap,               $
           CBPosition=CBPos,   UPos=UPos,         BotOut=BotOut,             $
           Min_Valid=MinData,  _EXTRA=e
           
end

;------------------------------------------------------------------------------

pro Strat_Diff_1Yr, Files,              Tracers,  Versions, Do_GCHP,         $
                    DynRange=Dynrange,  PS=PS,    OutFileName=OutFileName, $
                    Month=Month,        _EXTRA=e
 
   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ColorBar_NDiv, Get_Diff_Range, Extract_FileName

   ; Arguments
   if ( N_Elements( Files ) ne N_Elements( Versions ) ) then $
      Message, 'Number of FILES does not equal number of VERSIONS!'
   
   ; Keywords
   DynRange = Keyword_Set( DynRange )
   if ( N_Elements( OutFileName ) ne 1 )                                    $
      then OutFileName = 'Strat_Differences.ps'

   ; Number of model versions to compare
   nVersions = N_Elements( Versions )
   
   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem Stratospheric Zonal Difference Maps!C!C'

   ; Save original color table
   TvLct, R, G, B, /Get

   ; Save the original settings of the !MYCT sysvar
   if ( ChkStru( !MYCT ) ) then Myct_Orig = !MYCT

   ; Load Blue-White-White-Red colortable w/ 12 colors
   MyCt, /BuWhWhRd, NColors=12

   ;====================================================================
   ; Read boxheight from the files
   ;====================================================================

   ; Boxheight from 1st file (red data pts)
   CTM_Get_Data, BoxHeight_1, 'BXHGHT-$', $
      File=Files[0], Tracer=1, /Quiet

   ; Boxheight from 2nd file (green data pts)
   if ( Do_GCHP ) then begin
      ; If GCHP use boxheight from 1st file since diag not available
      BoxHeight_2 = BoxHeight_1
   endif else begin
      CTM_Get_Data, BoxHeight_2, 'BXHGHT-$', $
         File=Files[1], Tracer=1, /Quiet
   endelse

   if ( nVersions eq 3 ) then begin
   ; Boxheight from 3rd file (blue data pts)
   CTM_Get_Data, BoxHeight_3, 'BXHGHT-$', $
      File=Files[2], Tracer=1, /Quiet
   endif
   
   ;------------------------------
   ; Error checks!
   ;------------------------------
   
   ; Stop if both DATAINFOs are incompatible
   if ( nVersions eq 2 ) then begin

      if ( N_Elements( BoxHeight_1 ) ne N_Elements( BoxHeight_2 ) ) $
         then Message, 'Files are incompatible!'

   endif else begin

      if ( N_Elements( BoxHeight_1 ) ne N_Elements( BoxHeight_3 ) ) $
         then Message, '1st & 3rd files are incompatible!'

      if ( N_Elements( BoxHeight_2 ) ne N_Elements( BoxHeight_3 ) ) $
         then Message, '2nd & 3rd files are incompatible!'

   endelse

   ;====================================================================
   ; Read airmass from the files
   ;====================================================================

   ; Airmass from 1st file (red data pts)
   CTM_Get_Data, AirMass_1, 'BXHGHT-$', $
      File=Files[0], Tracer=2, /Quiet

   ; Airmass from 2nd file (green data pts)
   if ( Do_GCHP ) then begin
      ; If GCHP use airmass from 1st file since diag not available
      AirMass_2 = AirMass_1
   endif else begin
      CTM_Get_Data, AirMass_2, 'BXHGHT-$', $
         File=Files[1], Tracer=2, /Quiet
   endelse

   if ( nVersions eq 3 ) then begin
   ; Airmass from 3rd file (blue data pts
   CTM_Get_Data, AirMass_3, 'BXHGHT-$', $
      File=Files[2], Tracer=2, /Quiet
   endif
   
   ;------------------------------
   ; Error checks!
   ;------------------------------

   ; Stop if both DATAINFOs are incompatible
   if ( nVersions eq 2 ) then begin
   
      if ( N_Elements( AirMass_1 ) ne N_Elements( AirMass_2 ) ) $
         then Message, 'Files are incompatible!'

   endif else begin
      
      if ( N_Elements( AirMass_1 ) ne N_Elements( AirMass_3 ) ) $
         then Message, '1st & 3rd files are incompatible!'

      if ( N_Elements( AirMass_2 ) ne N_Elements( AirMass_3 ) ) $
         then Message, '2nd & 3rd files are incompatible!'

   endelse
      
   ;====================================================================
   ; Read data from the files
   ;====================================================================
   
   ; Read tracers from the 1st file (red data pts)
   CTM_Get_Data, DataInfo_1, 'IJ-AVG-$', $
      File=Files[0], Tracer=Tracers, /Quiet

   ; Read tracers from the 2nd file (green data pts)
   CTM_Get_Data, DataInfo_2, 'IJ-AVG-$', $
      File=Files[1], Tracer=Tracers, /Quiet

   if ( nVersions eq 3 ) then begin
   ; Read tracers from the 3rd file (blue data pts)
   CTM_Get_Data, DataInfo_3, 'IJ-AVG-$', $
      File=Files[2], Tracer=Tracers, /Quiet
   endif
   
   ;------------------------------
   ; Read OH
   ;------------------------------

   ; Skip OH if doing GCHP
   if ( ~ Do_GCHP ) then begin

      ; Read OH from the 1st file (red data pts)
      CTM_Get_Data, DataInfo, 'CHEM-L=$', $
         File=Files[0], Tracer=1, /Quiet
      DataInfo_1 = [ DataInfo_1, DataInfo ]
      UnDefine, DataInfo
      
      ; Read OH from the 2nd file (green data pts)
      CTM_Get_Data, DataInfo, 'CHEM-L=$', $
         File=Files[1], Tracer=1, /Quiet
      DataInfo_2 = [ DataInfo_2, DataInfo ]
      UnDefine, DataInfo
      
      if ( nVersions eq 3 ) then begin
         ; Read OH from the 3rd file (blue data pts)
         CTM_Get_Data, DataInfo, 'CHEM-L=$', $
            File=Files[2], Tracer=1, /Quiet
         DataInfo_3 = [ DataInfo_3, DataInfo ]
         UnDefine, DataInfo
      endif

   endif

   ;------------------------------
   ; Read HO2
   ;------------------------------

   ; Skip HO2 if doing GCHP
   if ( ~ Do_GCHP ) then begin

      ; Read HO2 from the 1st file (red data pts)
      CTM_Get_Data, DataInfo, 'CHEM-L=$', $
         File=Files[0], Tracer=3, /Quiet
      DataInfo_1 = [ DataInfo_1, DataInfo ]
      UnDefine, DataInfo
      
      ; Read HO2 from the 2nd file (green data pts)
      CTM_Get_Data, DataInfo, 'CHEM-L=$', $
         File=Files[1], Tracer=3, /Quiet
      DataInfo_2 = [ DataInfo_2, DataInfo ]
      UnDefine, DataInfo
      
      if ( nVersions eq 3 ) then begin
         ; Read HO2 from the 3rd file (blue data pts)
         CTM_Get_Data, DataInfo, 'CHEM-L=$', $
            File=Files[2], Tracer=3, /Quiet
         DataInfo_3 = [ DataInfo_3, DataInfo ]
         UnDefine, DataInfo
      endif
   
   endif

   ;------------------------------
   ; Error checks!
   ;------------------------------

   ; Stop if both DATAINFOs are incompatible
   if ( nVersions eq 2 ) then begin

      if ( N_Elements( DataInfo_1 ) ne N_Elements( DataInfo_2 ) ) $
         then Message, 'Files are incompatible!'
      
   endif else begin
      
      if ( N_Elements( DataInfo_1 ) ne N_Elements( DataInfo_3 ) ) $
         then Message, '1st & 3rd files are incompatible!'

      if ( N_Elements( DataInfo_2 ) ne N_Elements( DataInfo_3 ) ) $
         then Message, '2nd & 3rd files are incompatible!'

   endelse

   ;====================================================================
   ; Extract box height arrays for surface and 500 hPa
   ;====================================================================

   ; Get MODELINFO and GRIDINFO structures
   GetModelAndGridInfo, BoxHeight_1[0], ModelInfo_1, GridInfo_1
   GetModelAndGridInfo, BoxHeight_2[0], ModelInfo_2, GridInfo_2
   if ( nVersions eq 3 ) then begin
   GetModelAndGridInfo, BoxHeight_3[0], ModelInfo_3, GridInfo_3
   endif
   
   ; Get full-sized airmass arrays
   BxHt_1     = *( BoxHeight_1[0].Data )
   BxHt_2     = *( BoxHeight_2[0].Data )
   if ( nVersions eq 3 ) then begin
   BxHt_3     = *( BoxHeight_3[0].Data )
   endif
  
   ;====================================================================
   ; Extract air mass arrays for surface and 500 hPa
   ;====================================================================

   ; Get MODELINFO and GRIDINFO structures
   GetModelAndGridInfo, AirMass_1[0], ModelInfo_1, GridInfo_1
   GetModelAndGridInfo, AirMass_2[0], ModelInfo_2, GridInfo_2
   if ( nVersions eq 3 ) then begin
   GetModelAndGridInfo, AirMass_3[0], ModelInfo_3, GridInfo_3
   endif

   ; Get full-sized airmass arrays
   Air_1     = *( AirMass_1[0].Data )
   Air_2     = *( AirMass_2[0].Data )
   if ( nVersions eq 3 ) then begin
   Air_3     = *( AirMass_3[0].Data )
   endif
   
   ;====================================================================
   ; Process data and create difference plots!
   ;====================================================================

   ; Number of rows & colums on the plot
      if ( nVersions eq 2 ) then begin
      Rows = 3
      Cols = 2
   endif else begin
      Rows = 2
      Cols = 2  
   endelse

   ; Use Postscript font
   !p.font=0

   ; Open the plot device and initialize the page
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e

   ; Multiple panels per page
   MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]

   ; Loop over the # of categories
   for D = 0L, N_Elements( DataInfo_1 )-1L do begin
      
      ;-----------------------------------------------------------------
      ; Error check grid, tracer name, and data block sizes 
      ;-----------------------------------------------------------------

      ; Get MODELINFO and GRIDINFO structures
      GetModelAndGridInfo, DataInfo_1[D], ModelInfo_1, GridInfo_1
      GetModelAndGridInfo, DataInfo_2[D], ModelInfo_2, GridInfo_2
      if ( nVersions eq 3 ) then begin
      GetModelAndGridInfo, DataInfo_3[D], ModelInfo_3, GridInfo_3
      endif
      
;      ; Can't plot if the vertical grids are different!
;      if ( Modelinfo_1.Name ne ModelInfo_2.Name ) then begin
;         
;         ; First determine if both grids are equivalent even if the
;         ; model names differ (e.g. GEOS-5.2, MERRA, GEOS-5.7)
;         Models = [ Modelinfo_1.Name,  ModelInfo_2.Name ]
;         F0     = ( Where( Models eq 'GEOS5_47L'  ) ge 0 )
;         F1     = ( Where( Models eq 'MERRA_47L'  ) ge 0 )
;         F2     = ( Where( Models eq 'GEOSFP_47L' ) ge 0 )
;         F3     = ( Where( Models eq 'MERRA2_47L' ) ge 0 )
;
;         ; If both grids are not equivalent, then stop w/ error message
;         if ( ( F0 + F1 + F2 ) ne 2 ) then  begin
;
;            ; Now check models using native grid
;            F4     = ( Where( Models eq 'GEOS5'  ) ge 0 )
;            F5     = ( Where( Models eq 'MERRA'  ) ge 0 )
;            F6     = ( Where( Models eq 'GEOSFP' ) ge 0 )
;            F7     = ( Where( Models eq 'MERRA2' ) ge 0 )
;
;            if ( ( F3 + F4 + F5 ) ne 2 ) then  begin
;               Message, $
;               'The two models have different vertical grids!  Cannot plot!'
;            endif
;         endif
;      endif

      ; Make sure grids are compatible
      if ( nVersions eq 2 ) then begin

         if ( GridInfo_1.IMX ne GridInfo_2.IMX  OR $
              GridInfo_1.JMX ne GridInfo_2.JMX )   $
            then Message, 'Resolution mismatch!'
         
      endif else begin

         if ( GridInfo_1.IMX ne GridInfo_3.IMX  OR $
              GridInfo_1.JMX ne GridInfo_3.JMX )   $
            then Message, '1-3 resolution mismatch!'

         if ( GridInfo_2.IMX ne GridInfo_3.IMX  OR $
              GridInfo_2.JMX ne GridInfo_3.JMX )   $
            then Message, '2-3 resolution mismatch!'

      endelse

      ; Get corresponding tracers
      TracerName_1 = DataInfo_1[D].TracerName
      for D2 = 0L, N_Elements( DataInfo_2 )-1L do begin
         GetModelAndGridInfo, DataInfo_2[D2], ModelInfo_2, GridInfo_2
         TracerName_2 = DataInfo_2[D2].TracerName
         if ( TracerName_2 eq TracerName_1 ) then begin
            break
         endif
      endfor
      if ( nVersions eq 3 ) then begin
         for D3 = 0L, N_Elements( DataInfo_3 )-1L do begin
            GetModelAndGridInfo, DataInfo_3[D3], ModelInfo_3, GridInfo_3
            TracerName_3 = DataInfo_3[D3].TracerName
            if (( TracerName_1 eq TracerName_3 ) and $ 
                (TracerName_2 eq TracerName_3)) then begin
                break
            endif
         endfor
      endif
      
      ; Check that matching tracers were found
      if ( nVersions eq 2 ) then begin
         if ( TracerName_1 ne TracerName_2 ) then Message,'Tracer mismatch!'
      endif else begin
         if ( TracerName_1 ne TracerName_3 ) then Message,'1-3 Tracer mismatch!'
         if ( TracerName_2 ne TracerName_3 ) then Message,'2-3 Tracer mismatch!'
      endelse

      ; Get full-sized data arrays
      Data_1 = *( DataInfo_1[D].Data )
      Data_2 = *( DataInfo_2[D2].Data )
      if ( nVersions eq 3 ) then begin
      Data_3 = *( DataInfo_3[D3].Data )
      endif
      
      ; Get the dimensions of the data arrays
      Size_1 = Size( Data_1, /Dim )
      Size_2 = Size( Data_2, /Dim )
      if ( nVersions eq 3 ) then begin
      Size_3 = Size( Data_3, /Dim )
      endif
      
      ; Stop the run if the data block sizes don't agree
      if ( nVersions eq 2 ) then begin
         if ( Size_1[0] ne Size_2[0] ) then Message, 'Longitude mismatch!'
         if ( Size_1[1] ne Size_2[1] ) then Message, 'Latitude mismatch!'
      endif else begin
         if ( Size_1[0] ne Size_3[0] ) then Message, '1-3 Longitude mismatch!'
         if ( Size_1[1] ne Size_3[1] ) then Message, '1-3 Latitude mismatch!'
         if ( Size_2[0] ne Size_3[0] ) then Message, '2-3 Longitude mismatch!'
         if ( Size_2[1] ne Size_3[1] ) then Message, '2-3 Latitude mismatch!'
      endelse
      
      ; Get unit of data
      Unit = DataInfo_1[D].Unit
      
      ;-----------------------------------------------------------------
      ; Convert aerosol tracers from ppbv to ug/m3
      ;-----------------------------------------------------------------
      if ( Is_Aerosol( TracerName_1 ) ) then begin
      
         ; Convert arrays to [ug/m3]
         Convert_To_UgM3, Data_1,   ModelInfo_1, GridInfo_1,             $ 
                          TracerName_1, BxHt_1,  Air_1
      
         Convert_To_UgM3, Data_2,   ModelInfo_2, GridInfo_2,             $
                          TracerName_2, BxHt_2,  Air_2
      
         if ( nVersions eq 3 ) then begin
         Convert_To_UgM3, Data_3,   ModelInfo_3, GridInfo_3,             $
                          TracerName_3, BxHt_3,  Air_3
         endif
         
      endif

      ;-----------------------------------------------------------------
      ; Plot the data!
      ;-----------------------------------------------------------------

      if ( nVersions eq 2 ) then begin

         ; Version 2 - Version 1 Absolute diff
         PlotStratAbsDiff, Data_1,       Data_2,     Versions[0], Versions[1], $
                           TracerName_1, GridInfo_1, DynRange,    Month,       $
                           Unit,         _EXTRA=e

         ; Version 2 / Version 1 Percent diff
         PlotStratPctDiff, Data_1,       Data_2,     Versions[0], Versions[1], $
                           TracerName_1, GridInfo_1, DynRange,    Month,       $
                           Unit,         _EXTRA=e
         
      endif else begin
      
         ; "Blue" - "Green" Absolute diff
         PlotStratAbsDiff, Data_2,       Data_3,     Versions[1], Versions[2], $
                           TracerName_1, GridInfo_1, DynRange,    Month,       $
                           Unit,         _EXTRA=e

         ; "Blue" / "Green" Percent diff
         PlotStratPctDiff, Data_2,       Data_3,     Versions[1], Versions[2], $
                           TracerName_1, GridInfo_1, DynRange,    Month,       $
                           Unit,         _EXTRA=e

         ; "Blue" - "Red" Absolute diff
         PlotStratAbsDiff, Data_1,       Data_3,     Versions[0], Versions[2], $
                           TracerName_1, GridInfo_1, DynRange,    Month,       $
                           Unit,         _EXTRA=e

         ; "Blue" / "Red" Percent diff
         PlotStratPctDiff, Data_1,       Data_3,     Versions[0], Versions[2], $
                           TracerName_1, GridInfo_1, DynRange,    Month,       $
                           Unit,         _EXTRA=e

      endelse

      ; Plot the top title on each page  
      if ( D*4 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif

      ;-----------------------------------------------------------------
      ; Undefine stuff for next iteration
      ;-----------------------------------------------------------------
      UnDefine, Data_1
      UnDefine, Data_2
      UnDefine, GridInfo_1
      UnDefine, GridInfo_2
      UnDefine, ModelInfo_1
      UnDefine, ModelInfo_2
      UnDefine, Size_1
      UnDefine, Size_2
      UnDefine, TracerName_1
      UnDefine, TracerName_2
      if ( nVersions eq 3 ) then begin
      UnDefine, Data_3
      UnDefine, GridInfo_3
      UnDefine, ModelInfo_3
      UnDefine, Size_3
      UnDefine, TracerName_3
      endif
      
   endfor

   ;====================================================================
   ; Cleanup and quit
   ;====================================================================

   ; Cancel previous MULTIPANEL settings
   MultiPanel, /Off

   ; Close plot device
   Close_Device

   ; Restore original color table
   TvLct, R, G, B

   ; Restore !MYCT sysvar to original settings
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig

   ; Quit
   return
end
 
