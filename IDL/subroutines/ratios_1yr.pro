;-----------------------------------------------------------------------
;+
; NAME:
;        RATIOS_1YR
;
; PURPOSE:
;        Creates ratio maps of tracer at the surface and 500hPa
;        levels from 1-year GEOS-Chem benchmark simulation  output.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        RATIOS_1YR, FILES, TRACERS, VERSIONS, [, Keywords ]
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
;        /PS -> Set this switch to generate PostScript output.
;
;        OUTFILENAME -> If /PS is set, will write PostScript output 
;             to a file whose name is specified by this keyword.
;             Default is "tracer_ratio.pro".
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines Provided:
;        =========================================
;        PLOTRATIOS
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
;        PROFILES_1YR, FILES, ALTRANGE, TRACERS, VERSIONS, $
;             /PS, OUTFILENAME=PSNAME
;
;             ; Creates ratio maps from 3 different model versions
;             ; using netCDF output files from the various GEOS-Chem
;             ; 1-yr benchmark simulations.  (NOTE: this is the actual
;             ; calling sequence from driver routine BENCHMARK_1YR.)
;             ; The 
;
;        PROFILES_1YR, FILES, ALTRANGE, TRACERS, VERSIONS, $
;             /DYNRANGE, /PS, OUTFILENAME=PSNAME
;
;             ; Same as above, but will create the plot using the 
;             ; dynamic range of the data (centered around zero).
;
; MODIFICATION HISTORY:
;        bmy, 09 Nov 2007: VERSION 1.01
;                          - Initial version
;        bmy, 20 Dec 2007: VERSION 1.02
;                          - Now pass the month as a keyword to
;                            put on the plot panel titles
;        bmy, 01 Jun 2011: VERSION 1.03
;                          - Make the colorbar a little wider
;                          - Reduce the character size CsFac to 0.75
;                            to better display long plot titles
;                          - Now compute the ratio within PLOTRATIOS
;                          - Remove subroutine COMPUTERATIO
;                          - Now use appropriate plot settings for
;                            creating ratio plots w/ dynamic range
;                          - Also restore the !MYCT sysvar to defaults
;        bmy, 31 Aug 2012: VERSION 1.04
;                          - Convert aerosols to ug/m3
;                          - Call GET_RATIO_RANGE to return upper and
;                            lower bounds for the plot
;                          - Now move calls to MYCT to PLOTRATIOS rotuine
;     mpayer, 07 Sep 2012: - Now plot OH ratios
;     mpayer, 29 Mar 2013: - Now plot HO2 ratios
;        mps, 27 Jan 2017: - Update to allow for comparison of 2 versions,
;                            intead of the default 3 versions
;        ewl, 28 Jun 2017: - Use Model1 diagnostics for Model2 if GCHP benchmark
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
;-----------------------------------------------------------------------

pro PlotRatios, Data1,       Data2,       Version1,  Version2,               $
                TracerName,  GridInfo,    DynRange,  Month,                  $
                L_Sfc=L_Sfc, L_500=L_500, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTRATIOS plots either the surface or 500 hPa
   ; ratio of tracer between old and new versions (bmy, 11/9/07)
   ;====================================================================

   ; Get the upper & lower bounds for the plot
   Get_Ratio_Range, TracerName, LoBound, HiBound, Unit

   ; Keywords for colorbar
   CBPos      = [ 0.05, 0.01, 0.95, 0.04 ]                ; Position vector
   BotOut     = !MYCT.GRAY50                              ; Out of range color

   ; Pick color table based on the upper/lower limits of the range
   if ( LoBound eq 0.1 AND HiBound eq 10.0 ) then begin
      
      ;---------------------------
      ; Range 0.1 to 10.0
      ;---------------------------
      NColors = 38                                        ; # of colors
      Div     = 20                                        ; # of divisions
      Annote  = [ '0.1', '',     '',     '0.2',                             $ 
                  '',    '',    '0.4', '',                                 $
                  '',    '0.9', '1.1',  '',                                 $
                  '',    '2.3', '',     '',                                 $
                  '4.8', '',    '',     '10.0'  ]        ; Colorbar labels
   endif else begin

      ;---------------------------
      ; Range 0.5 to 2.0
      ;---------------------------
      NColors = 14                                        ; # of colors
      Div     = 8                                         ; # of divisions
      Annote  = [ '0.50', '0.61', '0.74', '0.90',                            $
                  '1.10', '1.34', '1.64', '2.00'  ]       ; Colorbar labels

   endelse
 
   ;====================================================================
   ; Set up plot labels
   ;====================================================================

   ; Version string
   VerStr = Version2 + ' / ' + Version1
 
   ; Plot title for surface
   if ( Keyword_Set( L_Sfc ) )                                               $
      then Title = VerStr     + '!C!C'                  +                    $
                   TracerName + ' / Ratio @ Surface for ' + Month

   ; Plot title for 500 hPa
   if ( Keyword_Set( L_500 ) )                                               $
      then Title = VerStr     + '!C!C'                  +                    $
                   TracerName + '/ Ratio @ 500 hPa for ' + Month 

   ;=================================================================
   ; Compute ratio of two data blocks
   ;=================================================================

   ; Take the ratio "new" / "old"
   Ratio = Data2 / Data1

   ; Replace non-finite points with a missing-data value
   Ind = Where( ~Finite( Ratio ) )
   if ( Ind[0] ge 0 ) then Ratio[Ind] = -9.99e30

   ; Don't plot the polar latitudes
   XMid  = GridInfo.XMid
   YMid  = GridInfo.YMid[ 1:GridInfo.JMX-2 ]
   Ratio = Ratio[ *, 1:GridInfo.JMX-2 ]
   
   if ( DynRange ) then begin

      ;=================================================================
      ; Plot ratio data dusing the dynamic range of the data 
      ; Center the plot range symmetrically around zero
      ;=================================================================

      Ind = Where( Ratio gt 0 )

      ; Settings for plot
      MinData =  Min( Ratio[Ind], Max=MaxData )
      Extreme =  Max( [ Abs( MinData ), Abs( MaxData ) ] )
      ;MinData = -Extreme
      ;MaxData =  Extreme

      ; Plot data w/ country boundaries
      TvMap, Ratio, XMid, Ymid,                                              $
             /Countries,         /Coasts,           /Cbar,                   $
             Division=Div,       /Sample,           /Grid,                   $
             Title=Title,        MinData=MinData,   MaxData=MaxData,         $
             CBFormat='(f13.2)', TCsFac=1.0,        CsFac=0.8,               $
             CBPosition=CBPos,   BotOut=BotOut,     /Triangle,               $  
             /NoGap,             Min_Valid=MinData, _EXTRA=e

   endif else begin

      ;=================================================================
      ; Plot ratio data w/in specified limits
      ;=================================================================

      ; Settings for plot
      MinData = LoBound
      MaxData = HiBound
      BotOut  = !MYCT.GRAY50
      
      ; Plot data w/ country boundaries
      TvMap, Ratio, XMid, Ymid,                                              $
             /Countries,         /Coasts,         /Cbar,                     $
             Division=Div,       /Sample,         /Grid,                     $
             Title=Title,        MinData=MinData, MaxData=MaxData,           $
             CBFormat='(f13.3)', BOR_Label=' ',   /Triangle,                 $
             CBPosition=CBPos,   /NoGap,          BotOut=BotOut,             $
             Annotation=Annote,  TCsFac=1.0,      CsFac=0.8,                 $
             /Log,               _EXTRA=e

   endelse

end

;------------------------------------------------------------------------------

pro Ratios_1yr, Files,             Tracers,     Versions, Do_GCHP,           $
                DynRange=DynRange, PS=PS,       OutFileName=OutFileName,     $
                Month=Month,       DiagN=DiagN, _EXTRA=e
 
   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ChkStru, ColorBar_NDiv, Extract_FileName

   ; Arguments
   if ( N_Elements( DiagN    ) eq 0 ) then Diagn = 'IJ-AVG-$'
   if ( N_Elements( Files ) ne N_Elements( Versions ) ) then $
      Message, 'Number of FILES does not equal number of VERSIONS!'
   
   ; Arguments
   DynRange = Keyword_Set( DynRange )
   if ( N_Elements( Month       ) ne 1 ) then Month       = ''
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'ratios.ps'

   ; Number of model versions to compare
   nVersions = N_Elements( Versions )

   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem Ratio Maps at surface and 500 hPa!C!C'

   ; Save original color table
   TvLct, R, G, B, /Get
   Myct_Orig = !MYCT

   ; This colortable will show approx 0.9 - 1.0 in the white, 
   ; and anything outside that in the red or blue.
   ; NOTE: The spacings were empirically determined.
   MyCt, 'RdBu', NColors=NColors, /MidCol, /White, /Reverse, _EXTRA=e
   
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
   ; Boxheight from 3rd file (blue data pts
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
   
   ; Extract airmass at surface -- 1st model
   BxHt_Sfc_1 = CTM_Extract( BxHt_1,                                         $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Lev=1 )           
                                                                             
   ; Extract airmass at surface -- 1st model                                 
   BxHt_Sfc_2 = CTM_Extract( BxHt_2,                                         $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Lev=1 )           
                                                                             
   if ( nVersions eq 3 ) then begin
   ; Extract airmass at surface -- 1st model                                 
   BxHt_Sfc_3 = CTM_Extract( BxHt_3,                                         $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Lev=1 )           
   endif
   
   ; Extract airmass at surface -- 1st model                                 
   BxHt_500_1 = CTM_Extract( BxHt_1,                                         $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Prange=500 )      
                                                                             
   ; Extract airmass at surface -- 1st model                                 
   BxHt_500_2 = CTM_Extract( BxHt_2,                                         $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Prange=500 )      
                                                                             
   if ( nVersions eq 3 ) then begin
   ; Extract airmass at surface -- 1st model                                 
   BxHt_500_3 = CTM_Extract( BxHt_3,                                         $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Prange=500 )
   endif
   
   ; We no longer need the large arrays
   UnDefine, BxHt_1
   UnDefine, BxHt_2
   if ( nVersions eq 3 ) then begin
   UnDefine, BxHt_3
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
   
   ; Extract airmass at surface -- 1st model
   Air_Sfc_1 = CTM_Extract(  Air_1,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Lev=1 )           
                                                                             
   ; Extract airmass at surface -- 1st model                                 
   Air_Sfc_2 = CTM_Extract(  Air_2,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Lev=1 )           
                                                                             
   if ( nVersions eq 3 ) then begin
   ; Extract airmass at surface -- 1st model                                 
   Air_Sfc_3 = CTM_Extract(  Air_3,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Lev=1 )           
   endif
   
   ; Extract airmass at surface -- 1st model                                 
   Air_500_1 = CTM_Extract(  Air_1,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Prange=500 )      
                                                                             
   ; Extract airmass at surface -- 1st model                                 
   Air_500_2 = CTM_Extract(  Air_2,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Prange=500 )

   if ( nVersions eq 3 ) then begin
   ; Extract airmass at surface -- 1st model
   Air_500_3 = CTM_Extract(  Air_3,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Prange=500 )
   endif
   
   ; We no longer need the large arrays
   UnDefine, Air_1
   UnDefine, Air_2
   if ( nVersions eq 3 ) then begin
   UnDefine, Air_3
   endif
   
   ;====================================================================
   ; Process data and create profile plots with CTM_PLOT!
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
      ; Extract data arrays for surface and 500 hPa
      ;-----------------------------------------------------------------

      ; Extract data at surface -- 1st model
      Data_Sfc_1 = CTM_Extract( Data_1,                                      $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,  $
                                Lon=[-180,180], Lat=[-90,90], Lev=1 )

      ; Extract data at surface -- 2nd model
      Data_Sfc_2 = CTM_Extract( Data_2,                                      $
                                ModelInfo=ModelInfo_2, GridInfo=GridInfo_2,  $
                                Lon=[-180,180], Lat=[-90,90], Lev=1 )

      if ( nVersions eq 3 ) then begin
      ; Extract data at surface -- 3rd model
      Data_Sfc_3 = CTM_Extract( Data_3,                                      $
                                ModelInfo=ModelInfo_3, GridInfo=GridInfo_3,  $
                                Lon=[-180,180], Lat=[-90,90], Lev=1 )
      endif

      ; Extract data at 500hPa -- 1st model
      Data_500_1 = CTM_Extract( Data_1,                                      $
                                ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,  $
                                Lon=[-180,180], Lat=[-90,90], Prange=500 )

      ; Extract data at 500hPa -- 2nd model
      Data_500_2 = CTM_Extract( Data_2,                                      $
                                ModelInfo=ModelInfo_2, GridInfo=GridInfo_2,  $
                                Lon=[-180,180], Lat=[-90,90], Prange=500 )

      if ( nVersions eq 3 ) then begin
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
      ; Convert aerosol tracers from ppbv to ug/m3
      ;-----------------------------------------------------------------
      if ( Is_Aerosol( TracerName_1 ) ) then begin

         ; Convert arrays to [ug/m3]
         Convert_To_UgM3, Data_Sfc_1,   ModelInfo_1, GridInfo_1,             $ 
                          TracerName_1, BxHt_Sfc_1,  Air_Sfc_1

         Convert_To_UgM3, Data_Sfc_2,   ModelInfo_2, GridInfo_2,             $
                          TracerName_2, BxHt_Sfc_2,  Air_Sfc_2

         if ( nVersions eq 3 ) then begin
         Convert_To_UgM3, Data_Sfc_3,   ModelInfo_3, GridInfo_3,             $
                          TracerName_3, BxHt_Sfc_3,  Air_Sfc_3
         endif
         
         Convert_To_UgM3, Data_500_1,   ModelInfo_1, GridInfo_1,             $
                          TracerName_1, BxHt_500_1,  Air_Sfc_1

         Convert_To_UgM3, Data_500_2,   ModelInfo_2, GridInfo_2,             $
                          TracerName_2, BxHt_500_2,  Air_Sfc_2

         if ( nVersions eq 3 ) then begin
         Convert_To_UgM3, Data_500_3,   ModelInfo_3, GridInfo_3,             $
                          TracerName_3, BxHt_500_3,  Air_Sfc_3
         endif
         
      endif

      ;-----------------------------------------------------------------
      ; Plot the data!
      ;-----------------------------------------------------------------

      if ( nVersions eq 2 ) then begin
 
         ; Version 2 - Version 1 at Sfc
         PlotRatios, Data_Sfc_1,   Data_Sfc_2, Versions[0], Versions[1],     $
                     TracerName_1, GridInfo_1, DynRange,    Month,           $
                     /L_Sfc,       _EXTRA = e

         ; Version 2 - Version 1 at 500 hPa
         PlotRatios, Data_500_1,   Data_500_2, Versions[0], Versions[1],     $
                     TracerName_1, GridInfo_1, DynRange,    Month,           $
                     /L_500,       _EXTRA = e
         
      endif else begin
         
         ; "Blue" - "Green" at Sfc
         PlotRatios, Data_Sfc_2,   Data_Sfc_3, Versions[1], Versions[2],     $
                     TracerName_1, GridInfo_1, DynRange,    Month,           $
                     /L_Sfc,       _EXTRA = e

         ; "Blue" - "Green" at 500 hPa
         PlotRatios, Data_500_2,   Data_500_3, Versions[1], Versions[2],     $
                     TracerName_1, GridInfo_1, DynRange,    Month,           $
                     /L_500,      _EXTRA = e

         ; "Blue" - "Red" at Sfc
         PlotRatios, Data_Sfc_1,   Data_Sfc_3, Versions[0], Versions[2],     $
                     TracerName_1, GridInfo_1, DynRange,    Month,           $
                     /L_Sfc,       _EXTRA = e

         ; "Blue" - "Red" at 500 hPa
         PlotRatios, Data_500_1,   Data_500_3, Versions[0], Versions[2],     $
                     TracerName_1, GridInfo_1, DynRange,    Month,           $
                     /L_500,       _EXTRA = e

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

   ; Restore original color table
   TvLct, R, G, B

   ; Restore !MYCT sysvar to defaults
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig
   
   ; Quit
   return
end
 
