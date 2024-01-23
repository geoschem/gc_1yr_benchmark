;------------------------------------------------------------------------------
;+
; NAME:
;        CLOUD_DIFFERENCES_1YR
;
; PURPOSE:
;        Creates absolute difference plots ( New - Old ) for cloud fraction
;        (CLDTOT) and cloud optical depth (OPTD) in the surface - 500 hPa
;        column from 1-year benchmark simulation output.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        CLOUD_DIFFERENCES, FILES, TAUS, TRACERS, VERSIONS, [, Keywords ]
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
;        Internal Subroutines Included:
;        ===================================================
;        PlotAbsDiff
;        PlotPctDiff
;
;        External Subroutines Required:
;        ===================================================
;        CLOSE_DEVICE           COLORBAR_NDIV    (function)
;        CTM_GET_DATA           GET_DIFF_RANGE  
;        GETMODELANDGRIDINFO    EXTRACT_FILENAME (function)
;        MULTIPANEL             MYCT                       
;        OPEN_DEVICE            TVMAP                      
;        UNDEFINE        
;        
; REQUIREMENTS:
;        References routines from the GAMAP package.
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
;        TRACERS  = [ 1, 2 ]
;        PSNAME   = 'Cloud_Differences_Jan.v10-01i-geosfp-Run0.ps'
; 
;        CLOUD_DIFFERENCES_1YR, FILES, TRACERS, VERSIONS, Month='Jan', $
;                               /PS, OUTFILENAME=PSNAME
;
;             ; Creates cloud difference plots of three GEOS-CHEM
;             ; versions for Jan 2013.
;             ; Output is sent to PostScript file "myplot.ps".
;             ; The min and max of the data on each plot panel
;             ; corresponds to the dynamic range of the data.
;
; MODIFICATION HISTORY:
;        mps, 08 Apr 2016: - Initial version
;        mps, 27 Jan 2017: - Update to allow for comparison of 2 versions,
;                            intead of the default 3 versions
; 
;-
; Copyright (C) 2007-2011,
; Bob Yantosca and Philippe Le Sager, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine differences"
;------------------------------------------------------------------------------

pro PlotAbsDiff, Data1,    Data2, TracerName, Version1, Version2, $
                 GridInfo, Unit,  PS=PS,      _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTDIFF plots either the surface or 500 hPa
   ; ratio of tracer between old and new versions (bmy, 11/14/07)
   ;====================================================================

   ; Version string
   VerStr = Version2 + ' - ' + Version1
   
   ; Plot title
   Title = VerStr + '!C' + 'Abs Diff:!C' + TracerName  

   ; Number of colorbar tickmarks
   Divisions = ColorBar_NDiv( 6 )

   ; Don't plot the polar latitudes
   XMid   = GridInfo.XMid
   YMid   = GridInfo.YMid[ 1:GridInfo.JMX-2 ]
   Data_1 = Data1[ *, 1:GridInfo.JMX-2 ]
   Data_2 = Data2[ *, 1:GridInfo.JMX-2 ]

   ; Compute "new" - "old" difference
   Diff  = Data_2 - Data_1

   ;=================================================================
   ; Create plots using the full dynamic range of the data (centered
   ; around zero) if the /DYNRANGE keyword is set.
   ;=================================================================
   MinData  =  Min( Diff, Max=MaxData )
   Extreme  =  Max( [ Abs( MinData ), Abs( MaxData ) ] )
   MinData  = -Extreme
   MaxData  =  Extreme
   Triangle =  0
   NoGap    =  0
   Upos     =  1.1

   ; We need to set the colorbar a little bit lower for PostScript
   if ( Keyword_Set( PS ) )                     $
      then CBPos = [ 0.10, -0.02, 0.90, 0.01 ]  $
      else CBPos = [ 0.05,  0.05, 0.95, 0.08 ]

   ; Plot differences over a world map
   TvMap, Diff, XMid, Ymid,                                 $
      /Countries,         /Coasts,         /Cbar,           $
      Division=Divisions, /Sample,         /Grid,           $
      Title=Title,        MinData=MinData, MaxData=MaxData, $
      CBFormat='(f13.4)', BOR_Label=' ',   Unit=Unit,       $
      Triangle=Triangle,  NoGap=NoGap,     BotOut=BotOut,   $
      CbPosition=CbPos,   UPos=UPos,       _EXTRA=e

   Undefine, Data_1
   Undefine, Data_2
   
end

;------------------------------------------------------------------------------

pro PlotPctDiff, Data1,    Data2, TracerName, Version1, Version2, $
                 GridInfo, Unit,  PS=PS,      _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTDIFF plots either the surface or 500 hPa
   ; ratio of tracer between old and new versions (bmy, 11/14/07)
   ;====================================================================

   ; Version string
   VerStr = Version2 + ' / ' + Version1
   
   ; Plot title
   Title = VerStr + '!C' + 'Ratio:!C' + TracerName  

   ; Number of colorbar tickmarks
   Divisions = ColorBar_NDiv( 6 )

   ; Compute "new" / "old" difference
   Ratio  = Data2 / Data1

   ; Replace non-finite values with a missing data value
   Ind = Where( ~Finite( Ratio ) ) 
   if ( Ind[0] ge 0 ) then Ratio[Ind] = -9.99e30

   ;=================================================================
   ; Plot ratio data w/in limits of 0.5 - 2.0 (default)
   ; Values in the range of 0.9 - 1.1 will show up as white
   ;=================================================================

   ; Settings for plot
   MinData  = 0.5
   MaxData  = 2.0
   BotOut   = !MYCT.BOTTOM
   Triangle = 1
   NoGap    = 1
   Log      = 1                               
   Div      = 8
   Annote   = [ '0.50', '0.61', '0.74', '0.90',                     $
                '1.10', '1.34', '1.64', '2.00'  ]                   

   ; We need to set the colorbar a little bit lower for PostScript
   if ( Keyword_Set( PS ) )                     $
      then CBPos = [ 0.05, -0.02, 0.95, 0.01 ]  $
      else CBPos = [ 0.05,  0.05, 0.95, 0.08 ]

   ; Plot ratios over a world map
   TvMap, Ratio, XMid, Ymid,                                 $
      /Countries,         /Coasts,          /Cbar,           $
      Division=Div,       /Sample,          /Grid,           $
      Title=Title,        MinData=MinData,  MaxData=MaxData, $
      CBFormat='(f13.2)', BOR_Label=' ',                     $
      Triangle=Triangle,  NoGap=NoGap,      BotOut=BotOut,   $
      Log=Log,  CbPosition=CbPos, _EXTRA=e

end

;------------------------------------------------------------------------------

pro Cloud_Differences_1yr, Files, Tracers, Versions,         $
                           PS=PS, OutFileName=OutFileName,   $
                           _EXTRA=e

   ;====================================================================
   ; Initialize
   ;====================================================================

   ; External functions
   FORWARD_FUNCTION ColorBar_NDiv, Get_Diff_Range, Extract_FileName

   ; Arguments
   if ( N_Elements( Files ) ne N_Elements( Versions ) ) then $
      Message, 'Number of FILES does not equal number of VERSIONS!'
   
   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'cloud_diff.ps'

   ; Number of model versions to compare
   nVersions = N_Elements( Versions )
   
   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem Surface - 500 hPa column differences (dyn range)!C!C'

   ; Set levels
   MinLev = 1   ; Sfc
   MaxLev = 23  ; 500 hPa

   ; Save original color table and !MYCT sysvar
   TvLct, R, G, B, /Get
   Myct_Orig = !MYCT

   ; Load Blue-White-White-Red colortable
   MyCt, /BuWhWhRd, _EXTRA=e
   
   ;====================================================================
   ; Read data from the files
   ;====================================================================

   ; Read tracers from the 1st file
   CTM_Get_Data, OptdInfo_1, 'OD-MAP-$', $
                 File=Files[0], Tracer=Tracers[0], /Quiet
   CTM_Get_Data, CldfInfo_1, 'OD-MAP-$', $
                 File=Files[0], Tracer=Tracers[1], /Quiet
   
   ; Read tracers from the 2nd file
   CTM_Get_Data, OptdInfo_2, 'OD-MAP-$', $
                 File=Files[1], Tracer=Tracers[0], /Quiet
   CTM_Get_Data, CldfInfo_2, 'OD-MAP-$', $
                 File=Files[1], Tracer=Tracers[1], /Quiet

   if ( nVersions eq 3 ) then begin
   ; Read tracers from the 3rd file
   CTM_Get_Data, OptdInfo_3, 'OD-MAP-$', $
                 File=Files[2], Tracer=Tracers[0], /Quiet
   CTM_Get_Data, CldfInfo_3, 'OD-MAP-$', $
                 File=Files[2], Tracer=Tracers[1], /Quiet
   endif
   
   ;------------------------------
   ; Error checks!
   ;------------------------------

   if ( nVersions eq 2 ) then begin

      if ( N_Elements( OptdInfo_1 ) ne N_Elements( OptdInfo_2 ) ) $
         then Message, '1st & 3rd files are incompatible!'
      
   endif else begin
   
      if ( N_Elements( OptdInfo_1 ) ne N_Elements( OptdInfo_3 ) ) $
         then Message, '1st & 3rd files are incompatible!'

      if ( N_Elements( OptdInfo_2 ) ne N_Elements( OptdInfo_3 ) ) $
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
   MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]

   ; Max # of colorbar divisions
   Divisions = Colorbar_NDiv( 6 )

   ;-----------------------------------------------------------------
   ; Error check grid, tracer name, and data block sizes 
   ;-----------------------------------------------------------------

   ; Get MODELINFO and GRIDINFO structures
   GetModelAndGridInfo, OptdInfo_1, ModelInfo_1, GridInfo_1
   GetModelAndGridInfo, OptdInfo_2, ModelInfo_2, GridInfo_2
   if ( nVersions eq 3 ) then begin
   GetModelAndGridInfo, OptdInfo_3, ModelInfo_3, GridInfo_3
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
   OptdName_1 = OptdInfo_1.TracerName
   OptdName_2 = OptdInfo_2.TracerName
   if ( nVersions eq 3 ) then begin
   OptdName_3 = OptdInfo_3.TracerName
   endif
   
   if ( nVersions eq 2 ) then begin
      if ( OptdName_1 ne OptdName_2 ) then Message, 'Tracer mismatch!'
   endif else begin
      if ( OptdName_1 ne OptdName_3 ) then Message, '1-3 tracer mismatch!'
      if ( OptdName_2 ne OptdName_3 ) then Message, '2-3 tracer mismatch!'
   endelse
   
   CldfName_1 = CldfInfo_1.TracerName
   CldfName_2 = CldfInfo_2.TracerName
   if ( nVersions eq 3 ) then begin
   CldfName_3 = CldfInfo_3.TracerName
   endif
   
   if ( nVersions eq 2 ) then begin
      if ( CldfName_1 ne CldfName_2 ) then Message, '1-3 tracer mismatch!'
   endif else begin
      if ( CldfName_1 ne CldfName_3 ) then Message, '1-3 tracer mismatch!'
      if ( CldfName_2 ne CldfName_3 ) then Message, '2-3 tracer mismatch!'
   endelse
   
   ; Get full-sized data arrays
   Optd_1 = *( OptdInfo_1.Data )
   Cldf_1 = *( CldfInfo_1.Data )
   Optd_2 = *( OptdInfo_2.Data )
   Cldf_2 = *( CldfInfo_2.Data )
   if ( nVersions eq 3 ) then begin
   Optd_3 = *( OptdInfo_3.Data )
   Cldf_3 = *( CldfInfo_3.Data )
   endif
   
   ; Get the dimensions of the data arrays
   Size_1 = Size( Optd_1, /Dim )
   Size_2 = Size( Optd_2, /Dim )
   if ( nVersions eq 3 ) then begin
   Size_3 = Size( Optd_3, /Dim )
   endif
   
   ; Get dimensions of grid
   NI = GridInfo_1.IMX
   NJ = GridInfo_1.JMX
   NL = Size_1[2]

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
   Unit = OptdInfo_1.Unit

   ; Create arrays for summing data in column
   MaxCldf_1      = fltarr(NI,NJ)
   Optd_Col_1     = fltarr(NI,NJ)
   CalcOptd_Col_1 = fltarr(NI,NJ)
   MaxCldf_2      = fltarr(NI,NJ)
   Optd_Col_2     = fltarr(NI,NJ)
   CalcOptd_Col_2 = fltarr(NI,NJ)
   if ( nVersions eq 3 ) then begin
   MaxCldf_3      = fltarr(NI,NJ)
   Optd_Col_3     = fltarr(NI,NJ)
   CalcOptd_Col_3 = fltarr(NI,NJ)
   endif
   
   ;-----------------------------------------------------------------
   ; Extract column data arrays for surface to 500 hPa
   ;-----------------------------------------------------------------

   for I = 0, NI-1L do begin
   for J = 0, NJ-1L do begin

      ; Compute maximum cloud fraction in column
      MaxCldf_1[I,J] = Max(Cldf_1[I,J,0:NL-1L])
      MaxCldf_2[I,J] = Max(Cldf_2[I,J,0:NL-1L])
      if ( nVersions eq 3 ) then begin
      MaxCldf_3[I,J] = Max(Cldf_3[I,J,0:NL-1L])
      endif
      
   endfor
   endfor

   ; Loop over specified levels
   for Lev = MinLev-1L, MaxLev-1L do begin

      ; Sum data over specified levels
      Optd_Col_1     = Optd_Col_1 + Optd_1[*, *, Lev]
      Optd_Col_2     = Optd_Col_2 + Optd_2[*, *, Lev]
      if ( nVersions eq 3 ) then begin
      Optd_Col_3     = Optd_Col_3 + Optd_3[*, *, Lev]
      endif
      
      ; Compute cloud optical depth using OPTD * CLDF
      CalcOptd_Col_1 = CalcOptd_Col_1 + $
                       ( Optd_1[*,*,Lev] * Cldf_1[*,*,Lev] )
      CalcOptd_Col_2 = CalcOptd_Col_2 + $
                       ( Optd_1[*,*,Lev] * Cldf_2[*,*,Lev] )
      if ( nVersions eq 3 ) then begin
      CalcOptd_Col_3 = CalcOptd_Col_3 + $
                       ( Optd_3[*,*,Lev] * Cldf_3[*,*,Lev] )
      endif
      
   endfor
   
   ; We no longer need the large arrays
   UnDefine, Optd_1
   UnDefine, Cldf_1
   UnDefine, Optd_2
   UnDefine, Cldf_2
   if ( nVersions eq 3 ) then begin
   UnDefine, Optd_3
   UnDefine, Cldf_3
   endif
   
   ;-----------------------------------------------------------------
   ; Plot the differences
   ;-----------------------------------------------------------------
   Unit       = Unit

   if ( nVersions eq 2 ) then begin

      ; Cloud fraction -- Version 2 - Version 1
      TracerName = 'Max cloud fraction (' + CldfName_1 + ' ) in column'
      PlotAbsDiff, MaxCldf_1,   MaxCldf_2,   TracerName, $
                   Versions[0], Versions[1], GridInfo_1, $
                   Unit,        PS=PS,       _EXTRA=e
      PlotPctDiff, MaxCldf_1,   MaxCldf_1,   TracerName, $
                   Versions[0], Versions[1], GridInfo_1, $
                   Unit,      PS=PS,         _EXTRA=e
   
      ; Cloud optical depth -- Version 2 - Version 1
      TracerName = 'Sum opt depth (' + OptdName_1 + '*' + CldfName_1 + $
                   ') below 500hPa'
      PlotAbsDiff, CalcOptd_Col_1, CalcOptd_Col_2, TracerName, $
                   Versions[0],    Versions[1],    GridInfo_1, $
                   Unit,           PS=PS,          _EXTRA=e
      PlotPctDiff, CalcOptd_Col_1, CalcOptd_Col_2, TracerName, $
                   Versions[0],    Versions[1],    GridInfo_1, $
                   Unit,           PS=PS,          _EXTRA=e
      
   endif else begin
   
      ; Cloud fraction -- Version 3 - Version 2
      TracerName = 'Max cloud fraction (' + CldfName_1 + ' ) in column'
      PlotAbsDiff, MaxCldf_2,   MaxCldf_3,   TracerName, $
                   Versions[1], Versions[2], GridInfo_1, $
                   Unit,        PS=PS,       _EXTRA = e
      PlotPctDiff, MaxCldf_2,   MaxCldf_3,   TracerName, $
                   Versions[1], Versions[2], GridInfo_1, $
                  Unit,        PS=PS,       _EXTRA=e

      ; Cloud optical depth -- Version 3 - Version 2
      TracerName = 'Sum opt depth (' + OptdName_1 + '*' + CldfName_1 + $
                   ') below 500hPa'
      PlotAbsDiff, CalcOptd_Col_2, CalcOptd_Col_3, TracerName, $
                   Versions[1],    Versions[2],    GridInfo_1, $
                   Unit,           PS=PS,          _EXTRA=e
      PlotPctDiff, CalcOptd_Col_2, CalcOptd_Col_3, TracerName, $
                   Versions[1],    Versions[2],    GridInfo_1, $
                   Unit,           PS=PS,          _EXTRA=e
   
      ; Cloud fraction -- Version 3 - Version 1
      TracerName = 'Max cloud fraction (' + CldfName_1 + ' ) in column'
      PlotAbsDiff, MaxCldf_1,   MaxCldf_3,   TracerName, $
                   Versions[0], Versions[2], GridInfo_1, $
                   Unit,        PS=PS,       _EXTRA=e
      PlotPctDiff, MaxCldf_1,   MaxCldf_3,   TracerName, $
                   Versions[0], Versions[2], GridInfo_1, $
                   Unit,      PS=PS,         _EXTRA=e
   
      ; Cloud optical depth -- Version 3 - Version 1
      TracerName = 'Sum optical depth (' + OptdName_1 + '*' + CldfName_1 + $
                   ') below 500hPa'
      PlotAbsDiff, CalcOptd_Col_1, CalcOptd_Col_3, TracerName, $
                   Versions[0],    Versions[2],    GridInfo_1, $
                   Unit,           PS=PS,          _EXTRA=e
      PlotPctDiff, CalcOptd_Col_1, CalcOptd_Col_3, TracerName, $
                   Versions[0],    Versions[2],    GridInfo_1, $
                   Unit,           PS=PS,          _EXTRA=e
   endelse
   
   ; Plot the top title on each page  
   XYoutS, 0.5, 1.03, TopTitle, $
         /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5

   ; Undefine stuff
   Undefine, MaxCldf_1
   Undefine, Optd_Col_1
   Undefine, CalcOptd_Col_1
   Undefine, MaxCldf_2
   Undefine, Optd_Col_2
   Undefine, CalcOptd_Col_2
   if ( nVersions eq 3 ) then begin
   Undefine, MaxCldf_3
   Undefine, Optd_Col_3
   Undefine, CalcOptd_Col_3
   endif
   
   ;====================================================================
   ; Cleanup and quit
   ;====================================================================

   ; Turn off multi-panel settings
   Multipanel, /Off

   ; Close plot device
   Close_Device

   ; Restore original color table
   TvLct, R, G, B

   ; Restore !MYCT sysvar to defaults
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig

   ; Quit
   return
end
