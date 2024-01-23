;-----------------------------------------------------------------------
;+
; NAME:
;        JV_RATIOS_1YR
;
; PURPOSE:
;        Creates ratio (new / old) plots of J-values at both
;        the surface and 500 hPa from 1-year benchmark simulation output. 
;       
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        JV_RATIOS_1YR, FILES, VERSIONS, [, Keywords ]
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
; KEYWORD PARAMETERS:
;        /PS -> Set this switch to generate PostScript output.
;
;        OUTFILENAME -> If /PS is set, will write PostScript output
;             to a file whose name is specified by this keyword.
;
;        MONTH -> Month for which to create plots.
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        External Subroutines Required:
;        ==================================================
;        PlotJVRatio
;
;        External Subroutines Required:
;        ==================================================
;        CLOSE_DEVICE          COLORBAR_NDIV    (function)
;        CTM_GET_DATA          EXTRACT_FILENAME (function)       
;        GETMODELANDGRIDINFO   MULTIPANEL         
;        MYCT                  OPEN_DEVICE   
;        TVMAP  
;
; REQUIREMENTS:
;        References routines from the GAMAP package.
;
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR
;
; EXAMPLE:
;        FILES    = [ 'ctm.bpch.v10-01e-geosfp-Run1.2013010100', $
;                     'ctm.bpch.v10-01h-geosfp-Run1.2013010100', $
;                     'ctm.bpch.v10-01i-geosfp-Run0.2013010100'  ]
;        VERSIONS = [ 'v10-01e-geosfp-Run1', $
;                     'v10-01h-geosfp-Run2', $
;                     'v10-01i-geosfp-Run0'  ]
;        PSNAME   = 'JValue_Ratios_Jan.v10-01i-geosfp-Run0.ps'
; 
;        JV_RATIOS_1YR, FILES, VERSIONS, Month='Jan', $
;                       /PS, OUTFILENAME=PSNAME
;
;             ; Creates J-value ratio plots of three GEOS-CHEM versions
;             ; (in this case v10-01i / v10-01e and v10-01i / v10-01h)
;             ; for Jan 2013.
;
; MODIFICATION HISTORY:
;        mps, 02 Dec 2013: - Initial version based on jv_ratio.pro from
;                            GAMAP v2-18
;        mps, 27 Jan 2017: - Update to allow for comparison of 2 versions,
;                            intead of the default 3 versions
; 
;-
; Copyright (C) 2007-2011, GEOS-Chem Support Team, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to geos-chem-support@seas.harvard.edu
; with subject "IDL routine JV ratios"
;-----------------------------------------------------------------------

pro PlotJVRatio, InData1,     InData2,     Version1, Version2, $
                 TracerName,  GridInfo,    Unit,     Month,    $
                 L_Sfc=L_Sfc, L_500=L_500, PS=PS,    _EXTRA=e
   
   ;====================================================================
   ; Internal routine PlotJVRatios plots either the surface or 500 hPa
   ; ratio of tracer between old and new versions (bmy, 11/9/07)
   ;====================================================================

   ; Version string
   VerStr = Version2 + ' - ' + Version1

   ; Plot title for surface
   if ( Keyword_Set( L_Sfc ) )                               $
      then Title = VerStr + '!C!C' + TracerName      +       $
                   ' - J-Value Ratio @ surface for ' + Month

   ; Plot title for 500 hPa
   if ( Keyword_Set( L_500 ) )                               $
      then Title = VerStr + '!C!C' + TracerName      +       $
                   ' - J-Value Ratio @ 500 hPa for ' + Month  

   ; Number of colorbar tickmarks
   Divisions = ColorBar_NDiv( 6 )
      
   ; Don't plot the polar latitudes
   XMid  = GridInfo.XMid
   YMid  = GridInfo.YMid[ 1:GridInfo.JMX-2 ]
   Data1 = InData1[ *, 1:GridInfo.JMX-2 ]
   Data2 = InData2[ *, 1:GridInfo.JMX-2 ]
  
   ; Compute "new" / "old" ratio
   Ratio = Data2 / Data1

   ;=================================================================
   ; Plot ratio data dusing the dynamic range of the data 
   ; Center the plot range symmetrically around zero
   ;=================================================================

   ; Settings for plot
   MinData  = 0.5
   MaxData  = 2.0
   BotOut   = !MYCT.BOTTOM
   Triangle = 1
   NoGap    = 1
   Log      = 1
   Div      = 8
   Annote   = [ '0.50', '0.61', '0.74', '0.90',  $
                '1.10', '1.34', '1.64', '2.00'  ]

   ; We need to set the colorbar a little bit lower for PostScript
   if ( Keyword_Set( PS ) )                   $
      then CBPos = [ 0.05, 0.01, 0.95, 0.04 ] $
      else CBPos = [ 0.05, 0.05, 0.95, 0.08 ]

   ; Plot ratios over a world map
   TvMap, Ratio, XMid, Ymid,                                  $
      /Countries,         /Coasts,         /Cbar,             $
      Divisions=Div,      /Sample,         /Grid,             $
      Title=Title,        MinData=MinData, MaxData=MaxData,   $
      CBFormat='(f13.2)', BOR_Label=' ',   Triangle=Triangle, $
      CbPosition=CBPos,   NoGap=NoGap,     BotOut=BotOut,     $
      Annotation=Annote,  Log=Log,         _EXTRA=e

end

;------------------------------------------------------------------------------

pro JV_Ratios_1yr, Files, Versions, Month=Month,$
                        OutFileName=OutFileName, $
                        PS=PS, _EXTRA=e

   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ColorBar_NDiv, Extract_FileName

   ; Arguments
   if ( N_Elements( Files ) ne N_Elements( Versions ) ) then $
      Message, 'Number of FILES does not equal number of VERSIONS!'

   ; Keywords
   if ( N_Elements( Month       ) ne 1 ) then Month       = ''
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'jv_ratio.ps'

   ; Number of model versions to compare
   nVersions = N_Elements( Versions )
   
   ; Define title for top of page
   TopTitle = 'GEOS-Chem J-Values at surface and 500 hPa!C!C'

   ; Save original color table and !MYCT sysvar
   TvLct, R, G, B, /Get
   Myct_Orig = !MYCT

   ; This colortable will show 0.9 - 1.0 in the white, and anything
   ; outside that in the red or blue.  Use 8 colorbar divisions to line
   ; everything up properly. (bmy, 5/21/10)
   ;
   ; NOTE: It is really 0.9057 - 1.0104.  Close enough for gov't work.
   MyCt, 'RdBu', NColors=14, /MidCol, /White, /Reverse, _EXTRA=e

   ;====================================================================
   ; Read data from the files
   ;====================================================================

;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;%%% Use this for benchmarks prior to v10-01c_trop (bmy, mps, 5/29/14)
;%%%   ; Tracer list prior to v10-01c_UCX:                            
;%%%   Tracer = [  1,  2,  3,  4,  5,  6, 23, 24, 25, 26, 27, 28     ]
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
;%%% Use this to compare v10-01c_trop to v10-01c_UCX (bmy, mps, 5/29/14)
;%%%   Tracer = [  1,  2,  3,  4,  5,  6,  9, 10, 11, 12, $
;%%%              13, 14, 17, 18, 23, 24, 25, 26, 27, 28 ]
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

   ; Tracer list for v10-01c_UCX and higher versions
   Tracer = [  1,  2,  3,  4,  5,  6,  9, 10, 11, 12, 13, 14, 15, $ 
              16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28 ]

   ; Read data from "old" file
   CTM_Get_Data, DataInfo_1, 'JV-MAP-$', File = Files[0], $
                 Tracer = Tracer, /Quiet

   CTM_Get_Data, DataInfo_2, 'JV-MAP-$', File = Files[1], $
                 Tracer = Tracer, /Quiet

   if ( nVersions eq 3 ) then begin
   CTM_Get_Data, DataInfo_3, 'JV-MAP-$', File = Files[2], $
                 Tracer = Tracer, /Quiet
   endif
   
   ; Error check
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
   ; Process tracers
   ;====================================================================

   ; Number of plots per page
   Rows = 2
   Cols = 2

   ; Use Postscript font
   !p.font = 0

   ; Initialize the plot device
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, FileName=OutFileName
 
   ; Number of panels per page
   MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]
  
   ; Loop over tracers
   for D = 0L, N_Elements( DataInfo_1 ) - 1L do begin

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
      ; Extract data arrays for surface and 500 hPa
      ;-----------------------------------------------------------------

      ; Extract data at surface -- 1st model
      Data_Sfc_1 = CTM_Extract( Data_1,                                      $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,  $
                                Lon=[-180,180], Lat=[-90,90], Lev=1 )

      ; Extract data at 500hPa -- 1st model
      Data_500_1 = CTM_Extract( Data_1,                                      $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,  $
                                Lon=[-180,180], Lat=[-90,90], Prange=500 )

      ; Extract data at surface -- 2nd model
      Data_Sfc_2 = CTM_Extract( Data_2,                                      $
                                ModelInfo=ModelInfo_2, GridInfo=GridInfo_2,  $
                                Lon=[-180,180], Lat=[-90,90], Lev=1 )

      ; Extract data at 500hPa -- 2nd model
      Data_500_2 = CTM_Extract( Data_2,                                      $
                                ModelInfo=ModelInfo_2, GridInfo=GridInfo_2,  $
                                Lon=[-180,180], Lat=[-90,90], Prange=500 )

      if ( nVersions eq 3 ) then begin
      ; Extract data at surface -- 3rd model
      Data_Sfc_3 = CTM_Extract( Data_3,                                      $
                                ModelInfo=ModelInfo_3, GridInfo=GridInfo_3,  $
                                Lon=[-180,180], Lat=[-90,90], Lev=1 )

      ; Extract data at 500hPa -- 3rd model
      Data_500_3 = CTM_Extract( Data_3,                                      $
                                ModelInfo=ModelInfo_3, GridInfo=GridInfo_3,  $
                                Lon=[-180,180], Lat=[-90,90], Prange=500 )
      endif
      
      ; We no longer need the large arrays
      UnDefine, Data_1
      UnDefine, Data_2
      if ( nVersions eq 3 ) then begin
      UnDefine, Data_3
      endif
      
      ;-----------------------------------------------------------------
      ; Compute the absolute ratios and plot the data
      ;-----------------------------------------------------------------

      ; Debug
      ;Print, 'Creating ', Month, ' J-value ratio maps for ', TracerName_1

      if ( nVersions eq 2 ) then begin

         ; Plot the surface ratios -- Version 2 / Version 1
         PlotJVRatio, Data_Sfc_1,   Data_Sfc_2, Versions[0], Versions[1], $
                      TracerName_1, GridInfo_1, Unit,        Month,       $
                      /L_Sfc,       PS=PS,      _EXTRA=e

         ; Plot the 500 hPa ratios -- Version 2 / Version 1
         PlotJVRatio, Data_500_1,   Data_500_2, Versions[0], Versions[1], $
                      TracerName_1, GridInfo_1, Unit,        Month,       $
                     /L_500,       PS=PS,      _EXTRA=e
         
      endif else begin
      
         ; Plot the surface ratios -- Version 3 / Version 2
         PlotJVRatio, Data_Sfc_2,   Data_Sfc_3, Versions[1], Versions[2], $
                      TracerName_1, GridInfo_1, Unit,        Month,       $
                      /L_Sfc,       PS=PS,      _EXTRA=e

         ; Plot the 500 hPa ratios -- Version 3 / Version 2
         PlotJVRatio, Data_500_2,   Data_500_3, Versions[1], Versions[2], $
                      TracerName_1, GridInfo_1, Unit,        Month,       $
                      /L_500,       PS=PS,      _EXTRA=e

         ; Plot the surface ratios -- Version 3 / Version 1
         PlotJVRatio, Data_Sfc_1,   Data_Sfc_3, Versions[0], Versions[2], $
                      TracerName_1, GridInfo_1, Unit,        Month,       $
                      /L_Sfc,       PS=PS,      _EXTRA=e

         ; Plot the 500 hPa ratios -- Version 3 / Version 1
         PlotJVRatio, Data_500_1,   Data_500_3, Versions[0], Versions[2], $
                      TracerName_1, GridInfo_1, Unit,        Month,       $
                     /L_500,       PS=PS,      _EXTRA=e
      endelse
         
      ; Plot the top title on each page
      if ( D*4 mod ( Rows * Cols ) eq 0 ) then begin
         XYOutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif
 
      ; Undefine stuff
      UnDefine, Data_500_1
      UnDefine, Data_500_2
      UnDefine, Data_Sfc_1
      UnDefine, Data_Sfc_2
      UnDefine, GridInfo_1
      UnDefine, GridInfo_2
      UnDefine, ModelInfo_1
      UnDefine, ModelInfo_2
      UnDefine, Size_1
      UnDefine, Size_2
      if ( nVersions eq 3 ) then begin
      UnDefine, Data_500_3
      UnDefine, Data_Sfc_3
      UnDefine, GridInfo_3
      UnDefine, ModelInfo_3
      UnDefine, Size_3
      endif
      
   endfor

   ;====================================================================
   ; Cleanup and quit
   ;====================================================================
Quit:

   ; Cancel previous MULTIPANEL settings
   MultiPanel, /Off

   ; Close device
   Close_Device

   ; Restore original color table
   TvLct, R, G, B

   ; Restore !MYCT sysvar to original settings
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig

   ; Quit
   return

end
 
