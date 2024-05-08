;-----------------------------------------------------------------------
;+
; NAME:
;        MAPS_1YR
;
; PURPOSE:
;        Creates lon-lat maps of GEOS-Chem tracers at the surface and 500 hPa
;        levels from 1-year GEOS-Chem benchmark simulation  output.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        MAPS_1YR, FILEs, TRACERS, VERSIONS, [, Keywords ]
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
;        ==================================================
;        PlotMap
;
;        External Subroutines Required:
;        ==================================================
;        CLOSE_DEVICE          COLORBAR_NDIV    (function)
;        CTM_GET_DATA          EXTRACT_FILENAME (function)
;        GETMODELANDGRIDINFO   MULTIPANEL
;        MYCT                  OPEN_DEVICE
;        TVMAP                 CHKSTRU          (function)
;        UNDEFINE
;        
; REQUIREMENTS:
;        References routines from the GAMAP package.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR.
;
; EXAMPLES:
;        FILES    = [ PREF1+'0101.nc', PREF2+'0101.nc', PREF3+'0101.nc' ]
;        TRACERS  = [ 1, 2, 4 ]
;        ALTRANGE = [ 0, 20 ]
;        VERSIONS = [ VERS1, VERS2, VERS3 ]
;        PSNAME   = PSDIR + 'Concentrations_Jan.' + RUNNAME + '.ps'
;
;        MAPS_1YR, FILES, TRACERS, VERSIONS, /PS, OUTFILENAME=PSNAME
;
;             ; Creates difference maps from 3 different model versions
;             ; using netCDF output files from the various GEOS-Chem
;             ; 1-yr benchmark simulations.  (NOTE: this is the actual
;             ; calling sequence from driver routine BENCHMARK_1YR.)
;
; MODIFICATION HISTORY:
;        mps, 21 Sep 2017: - Initial version based on maps.pro in GAMAP
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
; or phs@io.as.harvard.edu with subject "IDL routine maps"
;-----------------------------------------------------------------------


pro PlotMap, Data, Version, TracerName, Unit, GridInfo, Month, $
             L_Sfc=L_Sfc, L_500=L_500, PS=PS, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTMAP plots either the surface or 500 hPa
   ; map of tracer (bmy, 11/14/07)
   ;====================================================================

   ; Plot title for surface
   if ( Keyword_Set( L_Sfc ) )                        $
      then Title = Version    + '!C!C' + TracerName + $
                   ' - Species map @ surface for ' + Month

   ; Plot title for 500 hPa
   if ( Keyword_Set( L_500 ) )                        $
      then Title = Version    + '!C!C' + TracerName + $
                   ' - Species map @ 500 hPa for ' + Month 

   ; Number of colorbar tickmarks
   Divisions = ColorBar_NDiv( 6 )
      
   ; Don't plot the polar latitudes
   XMid = GridInfo.XMid
   YMid = GridInfo.YMid[ 1:GridInfo.JMX-2 ]
   Data = Data[ *, 1:GridInfo.JMX-2 ]

   ; For OH, let's rescale the unit for clarity
   if ( TracerName eq 'OH' ) then begin
      Data = Data / 1e5
      Unit = '1e5 molec/cm3'
   endif

   ; For HO2, let's rescale the unit for clarity
   if ( TracerName eq 'HO2' ) then begin
      Data = Data / 1e-12
      Unit = 'pptv'
   endif

   ; Use exponents to avoid colorbars with ranges 0.0000-0.0000
   if ( Max(Data) lt 0.0001 ) then begin
      Format = '(e13.3)'
   endif else if ( Max(Data) gt 10000.0 ) then begin
      Format = '(f13.0)'
   endif else begin
      Format = '(f13.4)'
   endelse

   ; Plot data w/ country boundaries
   TvMap, Data, XMid, Ymid,                              $
      /Countries,         /Coasts,            /Cbar,     $
      Division=Divisions, /Sample,            /Grid,     $
      Title=Title,        CBFormat=Format, Unit=Unit, $
      _EXTRA=e

end

;------------------------------------------------------------------------------

pro Maps_1yr, Files, Tracers, Versions, Do_GCHP, $
              Month=Month, PS=PS, OutFileName=OutFileName, _EXTRA=e
   
   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ChkStru, ColorBar_NDiv, Extract_FileName

   ; Arguments
   if ( N_Elements( Files ) ne N_Elements( Versions ) ) then $
      Message, 'Number of FILES does not equal number of VERSIONS!'

   ; Keywords
   if ( N_Elements( Month       ) ne 1 ) then Month       = ''
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'Concentrations.ps'

   ; Number of model versions to compare
   nVersions = N_Elements( Versions )

   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem Concentration Maps at Surface and 500 hPa!C!C'

   ; Save original color table information
   TvLct, R, G, B, /Get

   ; Save the original settings of the !MYCT sysvar
   if ( ChkStru( !MYCT ) ) then Myct_Orig = !MYCT

   ; Load modified spectrum, extended to 12 colors
   MyCt, /ModSpec, NColors=12

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
                                                                             
   ; Extract airmass at surface -- 2nd model                                 
   Air_Sfc_2 = CTM_Extract(  Air_2,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Lev=1 )           
                                                                             
   if ( nVersions eq 3 ) then begin
   ; Extract airmass at surface -- 3rd model                                 
   Air_Sfc_3 = CTM_Extract(  Air_3,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Lev=1 )           
   endif

   ; Extract airmass at 500 hPa -- 1st model                                 
   Air_500_1 = CTM_Extract(  Air_1,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Prange=500 )      
                                                                             
   ; Extract airmass at 500 hPa -- 2nd model                                 
   Air_500_2 = CTM_Extract(  Air_2,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Prange=500 )

   if ( nVersions eq 3 ) then begin
   ; Extract airmass at 500 hPa -- 3rd model
   Air_500_3 = CTM_Extract(  Air_3,                                          $
                             ModelInfo=ModelInfo_1, GridInfo=GridInfo_1,     $
                             Lon=[-180,180], Lat=[-90,90], Prange=500 )
   endif
   
   ; We no longer need the large arrays
   UnDefine, Air_1
   UnDefine, Air_2
   if ( nVersions eq 3 ) then UnDefine, Air_3

   ;====================================================================
   ; Process data and create profile plots with CTM_PLOT!
   ;====================================================================

   ; Number of rows & colums on the plot
   if ( nVersions eq 2 ) then begin
      Rows = 2
      Cols = 2
   endif else begin
      Rows = 3
      Cols = 2  
   endelse

   ; Use Postscript font
   !p.font = 0

   ; Open the plot device and initialize the page
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
 
   ; Multiple panels per page
   MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]

   ; Loop over all data blocks
   for D = 0L, N_Elements( DataInfo_2 )-1L do begin

      ;-----------------------------------------------------------------
      ; Error check grid, tracer name, and data block sizes 
      ;-----------------------------------------------------------------

      ; Get MODELINFO and GRIDINFO structures
      GetModelAndGridInfo, DataInfo_1[D], ModelInfo_1, GridInfo_1
      GetModelAndGridInfo, DataInfo_2[D], ModelInfo_2, GridInfo_2
      if ( nVersions eq 3 ) then $
      GetModelAndGridInfo, DataInfo_3[D], ModelInfo_3, GridInfo_3

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
         if ( TracerName_1 ne TracerName_2 ) then Message, 'Tracer mismatch!'
      endif else begin
         if ( TracerName_1 ne TracerName_3 ) then Message, '1-3 Tracer mismatch!'
         if ( TracerName_2 ne TracerName_3 ) then Message, '2-3 Tracer mismatch!'
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
         
        ; Set unit to ug/m3
         Unit = 'ug/m3'

      endif

      ;-----------------------------------------------------------------
      ; Plot the data!
      ;-----------------------------------------------------------------      

      if ( nVersions eq 2 ) then begin
         
         ; Version 1 at Sfc
         PlotMap, Data_Sfc_1,   Versions[0], $
                  TracerName_1, Unit,        $
                  GridInfo_1,   Month,       $
                  /L_Sfc,       _EXTRA=e

         ; Version 1 at 500 hPa
         PlotMap, Data_500_1,   Versions[0], $
                  TracerName_1, Unit,        $
                  GridInfo_1,   Month,       $
                  /L_500,       _EXTRA=e

         ; Version 2 at Sfc
         PlotMap, Data_Sfc_2,   Versions[1], $
                  TracerName_2, Unit,        $
                  GridInfo_2,   Month,       $
                  /L_Sfc,       _EXTRA=e

         ; Version 2 at 500 hPa
         PlotMap, Data_500_2,   Versions[1], $
                  TracerName_2, Unit,        $
                  GridInfo_2,   Month,       $
                  /L_500,       _EXTRA=e

      endif else begin
         
         ; Version 1 at Sfc
         PlotMap, Data_Sfc_1,   Versions[0], $
                  TracerName_1, Unit,        $
                  GridInfo_1,   Month,       $
                  /L_Sfc,       _EXTRA=e

         ; Version 1 at 500 hPa
         PlotMap, Data_500_1,   Versions[0], $
                  TracerName_1, Unit,        $
                  GridInfo_1,   Month,       $
                  /L_500,       _EXTRA=e

         ; Version 2 at Sfc
         PlotMap, Data_Sfc_2,   Versions[1], $
                  TracerName_2, Unit,        $
                  GridInfo_2,   Month,       $
                  /L_Sfc,       _EXTRA=e

         ; Version 2 at 500 hPa
         PlotMap, Data_500_2,   Versions[1], $
                  TracerName_2, Unit,        $
                  GridInfo_2,   Month,       $
                  /L_500,       _EXTRA=e

         ; Version 3 at Sfc
         PlotMap, Data_Sfc_3,   Versions[2], $
                  TracerName_3, Unit,        $
                  GridInfo_3,   Month,       $
                  /L_Sfc,       _EXTRA=e

         ; Version 3 at 500 hPa
         PlotMap, Data_500_3,   Versions[2], $
                  TracerName_3, Unit,        $
                  GridInfo_3,   Month,       $
                  /L_500,       _EXTRA=e

      endelse
      
      ; Plot the top title on each page  
      if ( D*6 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif

      ;-----------------------------------------------------------------
      ; Undefine stuff for next iteration
      ;-----------------------------------------------------------------
      UnDefine, Data_500_1
      UnDefine, Data_500_3
      UnDefine, Data_500_3
      UnDefine, Data_Sfc_1
      UnDefine, Data_Sfc_2
      UnDefine, Data_Sfc_3
      UnDefine, ModelInfo_1
      UnDefine, ModelInfo_2
      UnDefine, ModelInfo_3
      UnDefine, GridInfo_1
      UnDefine, GridInfo_2
      UnDefine, GridInfo_3
      UnDefine, TracerName_1
      UnDefine, TracerName_2
      UnDefine, TracerName_3
      UnDefine, Unit

   endfor

   ;====================================================================
   ; Cleanup & quit
   ;====================================================================

   ; Cancel previous MultiPanel Settings
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
 
