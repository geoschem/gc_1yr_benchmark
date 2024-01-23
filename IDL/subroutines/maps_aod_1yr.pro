;-----------------------------------------------------------------------
;+
; NAME:
;        MAPS_AOD_1YR
;
; PURPOSE:
;        Creates difference maps of tracer at the surface and 500hPa
;        levels from 1-year GEOS-Chem benchmark simulation  output.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        MAPS_AOD_1YR, FILES, TRACERS, VERSIONS, [, Keywords ]
;
; INPUTS:
;        FILES -> A 3-element vector containing the names of files
;             from the "red", 'green", and "blue" GEOS-Chem model 
;             versions that are to be compared. 
;
;        TRACERS -> The list of transported tracers (i.e. diagnostic
;             category "IJ-AVG-$") to be plotted.
;
;        VERSIONS ->  A 3-element vector containing the model version
;             names from the "red", 'green", and "blue" simulations. 
;
; KEYWORD PARAMETERS:
;        /DYNRANGE -> Set this switch to create plots using the entire
;             dynamic range of the data (centered around zero).  The
;             default is to use pre-defined data ranges.
;
;        /PS -> Set this switch to generate PostScript output.
;
;        OUTFILENAME -> If /PS is set, will write PostScript output 
;             to a file whose name is specified by this keyword.
;             Default is "tracer_ratio.pro".
;
;        MONTH -> Month for which to create plots.
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines Provided:
;        =========================================
;        PLOTDIFF
;
;        External Subroutines Required:
;        =========================================
;        OPEN_DEVICE     CLOSE_DEVICE
;        MULTIPANEL      COLORBAR_NDIV (function)
;        CTM_PLOT        CHKSTRU       (function)
;     
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR.
;
; EXAMPLE:
;        FILES    = [ PREF1+'0101.nc', PREF2+'0101.nc', PREF3+'0101.nc' ]
;        TRACERS  = [ 1, 2, 4 ]
;        ALTRANGE = [ 0, 20 ]
;        VERSIONS = [ VERS1, VERS2, VERS3 ]
;        PSNAME   = PSDIR + 'Differences_Jan.' + RUNNAME + '.ps'
;
;        DIFFERENCES_1YR, FILES, ALTRANGE, TRACERS, VERSIONS, $
;             /PS, OUTFILENAME=PSNAME
;
;             ; Creates difference maps from 3 different model versions
;             ; using netCDF output files from the various GEOS-Chem
;             ; 1-yr benchmark simulations.  (NOTE: this is the actual
;             ; calling sequence from driver routine BENCHMARK_1YR.)
;
;        DIFFERENCES_1YR, FILES, ALTRANGE, TRACERS, VERSIONS, $
;             /DYNRANGE, /PS, OUTFILENAME=PSNAME
;
;             ; Same as above, but will create difference maps  using
;             ; the full dynamic range of the data (centered around zero)
;             ; instead of using pre-defined min & max values.

;
; MODIFICATION HISTORY:
;        bmy, 05 Sep 2012: VERSION 1.01
;                          - Initial version
;        mps, 27 Jan 2017: - Update to allow for comparison of 2 versions,
;                            intead of the default 3 versions
;
;-
; Copyright (C) 2012, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine differences"
;-----------------------------------------------------------------------


pro PlotAODMap, Data,     Version,  TracerName, $
                GridInfo, DynRange, Month,      $
                _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTAODMAP plots AOD concentrations
   ;
   ; Initialize color table
   ;====================================================================

   ; Save original color table and !MYCT sysvar
   TvLct, R, G, B, /Get
   Myct_Orig = !MYCT

   ; Load White-Red colortable
   MyCt, /WhGrYlRd, _EXTRA=e

   ;====================================================================
   ; Plot settings
   ;====================================================================

   ; Version string
   VerStr = Version

   ; Pick new tracer name
   case ( StrUpCase( StrTrim( TracerName, 2 ) ) ) of 
      'OPD'   : NewTracerName = 'Dust: Column AOD map for '
      'OPSO4' : NewTracerName = 'SO4: Column AOD map for '
      'OPBC'  : NewTracerName = 'BC: Column AOD map for '
      'OPOC'  : NewTracerName = 'OC: Column AOD map for '
      'OPSSA' : NewTracerName = 'SALA: Column AOD map for '
      'OPSSC' : NewTracerName = 'SALC: Column AOD map for '
      ; For v10-01i and later
      'OPSO4550' : NewTracerName = 'SO4: Column AOD map for '
      'OPBC550'  : NewTracerName = 'BC: Column AOD map for '
      'OPOC550'  : NewTracerName = 'OC: Column AOD map for '
      'OPSSA550' : NewTracerName = 'SALA: Column AOD map for '
      'OPSSC550' : NewTracerName = 'SALC: Column AOD map for '
   endcase

   ; Plot title
   Title = VerStr + '!C!C' + NewTracerName + Month 

   ; Number of colorbar tickmarks
   Divisions = ColorBar_NDiv( 8 )

   ; Compute difference, but don't plot the polar latitudes
   XMid = GridInfo.XMid
   YMid = GridInfo.YMid[ 1:GridInfo.JMX-2 ]

   ; Get the difference range and unit
   ; NOTE: For dynamic range, plots, the RANGE will be ignored
   Get_Conc_Range, TracerName, Lo_Bound, Hi_Bound, Unit

   if ( DynRange ) then begin

      ;=================================================================
      ; Create plots using the full dynamic range of the data (centered
      ; around zero) if the /DYNRANGE keyword is set.
      ;=================================================================
      MinData  =  Min( Data, Max=MaxData )
      Triangle =  0
      NoGap    =  0
      Upos     =  1.1
      CbPos    =  [ 0.10, 0.01, 0.90, 0.04 ]

   endif else begin

      ;=================================================================
      ; Create plots using the pre-defined min & max values (from
      ; function GET_DIFF_RANGE).  This is the default.
      ;=================================================================
      MinData  =  Lo_Bound
      MaxData  =  Hi_Bound
      BotOut   =  !MYCT.BOTTOM
      Triangle =  1
      NoGap    =  1
      UPos     =  1.02
      CbPos    =  [ 0.05, 0.01, 0.95, 0.04 ]

   endelse

   ; Plot differences over a world map
   TvMap, Data[ *, 1:GridInfo.JMX-2 ], XMid, Ymid,          $
      /Countries,         /Coasts,         /Cbar,           $
      Division=Divisions, /Sample,         /Grid,           $
      Title=Title,        MinData=MinData, MaxData=MaxData, $
      CBFormat='(f13.3)', BOR_Label=' ',   Unit=Unit,       $
      Triangle=Triangle,  NoGap=NoGap,     BotOut=BotOut,   $
      CBPosition=CBPos,   TcsFac=1.0,      CsFac=0.8,       $
      UPos=UPos,          _EXTRA=e

   ; Restore original color table
   TvLct, R, G, B

   ; Restore !MYCT sysvar to defaults
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig

end

;------------------------------------------------------------------------------

pro Maps_AOD_1yr, Files,             Tracers, Versions,                $
                  DynRange=DynRange, PS=PS,   OutFileName=OutFileName, $
                  Month=Month,       _EXTRA=e
 
   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ChkStru, ColorBar_NDiv, Extract_FileName

   ; Arguments
   if ( N_Elements( Files ) ne N_Elements( Versions ) ) then $
      Message, 'Number of FILES does not equal number of VERSIONS!'
   
   ; Arguments
   DynRange = Keyword_Set( DynRange )
   if ( N_Elements( Month       ) ne 1 ) then Month       = ''
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'aod_maps.ps'

   ; Number of model versions to compare
   nVersions = N_Elements( Versions )
   
   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem AOD Maps at surface and 500 hPa!C!C'
   
   ;====================================================================
   ; Read data from the files
   ;====================================================================
   
   ; Read tracers from the 1st file
   CTM_Get_Data, DataInfo_1, 'OD-MAP-$', $
      File=Files[0], Tracer=Tracers, /Quiet

   ; Read tracers from the 2nd file
   CTM_Get_Data, DataInfo_2, 'OD-MAP-$', $
      File=Files[1], Tracer=Tracers, /Quiet

   if ( nVersions eq 3 ) then begin
   ; Read tracers from the 3rd file
   CTM_Get_Data, DataInfo_3, 'OD-MAP-$', $
      File=Files[2], Tracer=Tracers, /Quiet
   endif
   
   ;------------------------------
   ; Error checks!
   ;------------------------------

   ; Stop if both DATAINFOs are incompatible
   if ( nVersions eq 2 ) then begin

      if ( N_Elements( DataInfo_1 ) ne N_Elements( DataInfo_2 ) ) $
         then Message, 'Files are incompatible!'

   endif else begin
   
      ; Stop if both DATAINFOs are incompatible
      if ( N_Elements( DataInfo_1 ) ne N_Elements( DataInfo_3 ) ) $
         then Message, '1st & 3rd files are incompatible!'

      if ( N_Elements( DataInfo_2 ) ne N_Elements( DataInfo_3 ) ) $
         then Message, '2nd & 3rd files are incompatible!'

   endelse

   ;====================================================================
   ; Process data and create profile plots with CTM_PLOT!
   ;====================================================================

   ; Number of rows & colums on the plot
   Rows = 2
   Cols = 2

   ; Use Postscript font
   !p.font = 0

   ; Open the plot device and initialize the page
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
  
   ; Multiple panels per page
   if ( nVersions eq 3 ) then begin
      MultiPanel, Nplots=3, Margin=[ 0.03, 0.03, 0.03, 0.03 ]
   endif else begin
      MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]
   endelse
      
   ; Max # of colorbar divisions
   Divisions = Colorbar_NDiv( 6 )

   ; Loop over the data blocks
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

      ; Make sure the tracers correspond to each other
      TracerName_1 = DataInfo_1[D].TracerName
      TracerName_2 = DataInfo_2[D].TracerName
      if ( nVersions eq 3 ) then begin
      TracerName_3 = DataInfo_3[D].TracerName
      endif

      if ( nVersions eq 2 ) then begin
      if ( TracerName_1 ne TracerName_2 ) then Message, 'Tracer mismatch!'
      endif else begin
      if ( TracerName_1 ne TracerName_3 ) then Message, '1-3 Tracer mismatch!'
      if ( TracerName_2 ne TracerName_3 ) then Message, '2-3 Tracer mismatch!'
      endelse

      ; Get full-sized data arrays
      Data_1 = *( DataInfo_1[D].Data )
      Data_2 = *( DataInfo_2[D].Data )
      if ( nVersions eq 3 ) then begin
      Data_3 = *( DataInfo_3[D].Data )
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
      ; Extract column data arraysys for surface and 500 hPa
      ;-----------------------------------------------------------------

      ; Sum arrays in the vertical
      Data_Col_1 = Total( Data_1, 3 )
      Data_Col_2 = Total( Data_2, 3 )
      if ( nVersions eq 3 ) then begin
      Data_Col_3 = Total( Data_3, 3 )
      endif
      
      ; We no longer need the large arrays
      UnDefine, Data_1
      UnDefine, Data_2
      if ( nVersions eq 3 ) then begin
      UnDefine, Data_3
      endif
      
      ;-----------------------------------------------------------------
      ; Plot the data!
      ;-----------------------------------------------------------------

      if ( nVersions eq 2 ) then begin

         ; Version 1
         PlotAODMap, Data_Col_1, Versions[0], TracerName_1, $
                     GridInfo_1, DynRange,    Month,        $
                     _EXTRA = e

         ; Version 2
         PlotAODMap, Data_Col_2, Versions[1], TracerName_1, $
                     GridInfo_1, DynRange,    Month,        $
                     _EXTRA = e
         
      endif else begin
         
         ; "Blue" model
         PlotAODMap, Data_Col_1, Versions[0], TracerName_1, $
                     GridInfo_1, DynRange,    Month,        $
                     _EXTRA = e

         ; "Blue" model
         PlotAODMap, Data_Col_2, Versions[1], TracerName_1, $
                     GridInfo_1, DynRange,    Month,        $
                     _EXTRA = e

         ; "Blue" model
         PlotAODMap, Data_Col_3, Versions[2], TracerName_1, $
                     GridInfo_1, DynRange,    Month,        $
                     _EXTRA = e

      endelse
         
      ; Plot the top title on each page  
      if ( D*4 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif
   endfor

   ;====================================================================
   ; Cleanup and quit
   ;====================================================================

   ; Turn off multi-panel settings
   Multipanel, /Off

   ; Close plot device
   Close_Device

   ; Quit
   return
end
 
