;-----------------------------------------------------------------------
;+
; NAME:
;        ZONAL_1YR
;
; PURPOSE:
;        Creates zonal-mean curtain plots of GEOS-Chem tracers from
;        1-year GEOS-Chem benchmark simulation  output.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        ZONAL_1YR, FILES, TRACERS, VERSIONS, [, Keywords ]
;
; INPUTS:
;        FILES -> A 3-element vector containing the names of files
;             from the "red", 'green", and "blue" GEOS-Chem model 
;             versions that are to be compared. 
;
;        TRACERS -> The list of transported tracers (i.e. diagnostic
;             category "IJ-AVG-$").
;
;        VERSIONS -> A 3-element vector containing the model version
;             names from the "red", 'green", and "blue" simulations.
;
; KEYWORD PARAMETERS:
;        /PRESSURE -> Set this switch to plot pressure on the Y-axis.
;             The default is to plot altitude on the Y-axis.
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
;        PlotZonal
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
;        VERSIONS = [ VERS1, VERS2, VERS3 ]
;        PSNAME   = PSDIR + 'Zonal_Concentrations_Jan.' + RUNNAME + '.ps'
;
;        ZONAL_1YR, FILES, TRACERS, VERSIONS, /PS, OUTFILENAME=PSNAME
;
;             ; Creates difference maps from 3 different model versions
;             ; using netCDF output files from the various GEOS-Chem
;             ; 1-yr benchmark simulations.  (NOTE: this is the actual
;             ; calling sequence from driver routine BENCHMARK_1YR.)
;
; MODIFICATION HISTORY:
;        mps, 21 Sep 2017: - Initial version based on zonal.pro in GAMAP
;
;-
; Copyright (C) 2012-2013,
; Bob Yantosca and Philippe Le Sager, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine maps"
;-----------------------------------------------------------------------


pro PlotZonal, Data, Version, Lons, TracerName, Unit, GridInfo, Month, $
               Pressure=Pressure, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTZONAL plots either the zonal mean or cross
   ; section of tracer (bmy, 11/14/07)
   ;====================================================================

   ; Keywords
   Pressure    = Keyword_Set( Pressure )

   ; Plot title
   if ( Lons ge 0 )                                                       $
      then Title = Version    + '!C!C' + TracerName +                     $
                   ' - Species Curtain @ '+ string(GridInfo.XMid[Lons]) + $
                   ' for ' + Month                                        $
      else Title = Version    + '!C!C' + TracerName +                     $
                   ' - Species Zonal Mean for ' + Month

   ; Index arrays
   S           = Size( Data, /Dim )
   Lats        = GridInfo.YMid

   ; Set Y-axis coordinate for pressure or altitude grid (bmy, 7/18/11)
   if ( Pressure ) then begin
      if ( Lons ge 0 )                                                      $
      then YMid = GridInfo.PMid[0:S[2]-1L]                                  $
      else YMid = GridInfo.PMid[0:S[1]-1L]
   endif else begin
      if ( Lons ge 0 )                                                      $
      then YMid = GridInfo.ZMid[0:S[2]-1L]                                  $
      else YMid = GridInfo.ZMid[0:S[1]-1L]
   endelse

   ; Parameters common to both dyn range and pre-defined range plots
   XTickV = [ -90, -60, -30, 0, 30, 60, 90 ]
   XTicks = N_Elements( XTickV )-1L
   XMinor = 6
   XTitle = 'Latitude'
   Div    = Colorbar_Ndiv( 6 )

   ; Set Y-axis coordinate for pressure or altitude grid (bmy, 7/18/11)
   if ( Pressure )                                                          $
      then YTitle = 'Pressure (hPa)'                                        $
      else YTitle = 'Altitude (km)'

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
   TvPlot, Data, Lats, Ymid,                                 $
           /Cbar,         Division=Div,       /Sample,       $
           Title=Title,   CBFormat=Format, Unit=Unit,     $
           /XStyle,       XTickV=XTickV,      XTicks=XTicks, $
           XMinor=XMinor, XTitle=Xtitle,      YTitle=YTitle, $
           /YStyle,       _EXTRA=e

end

;------------------------------------------------------------------------------

pro Zonal_1yr, Files, Tracers, Versions, Do_GCHP, Month=Month, $
               PS=PS, OutFileName=OutFileName, _EXTRA=e
   
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
   if ( N_Elements( OutFileName ) ne 1 ) $
      then OutFileName = 'Zonal_Concentrations.ps'

   ; Number of model versions to compare
   nVersions = N_Elements( Versions )

   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem Zonal Mean and Curtain at 180E !C!C'

   ; Save original color table information
   TvLct, R, G, B, /Get

   ; Save the original settings of the !MYCT sysvar
   if ( ChkStru( !MYCT ) ) then Myct_Orig = !MYCT

   ; Load modified spectrum, extended to 12 colors
   MyCt, /ModSpec, NColors=12

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
   ; Read data from the files
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
   for D = 0L, N_Elements( DataInfo_1 )-1L do begin
   
      ;-----------------------------------------------------------------
      ; Extract data 
      ;-----------------------------------------------------------------

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

      ;--------------------------------------------------------------
      ; Prior to 5/29/13:
      ; Can't seem to find the MEAN2 function in IDL, which may
      ; be part of the JHU astro library.  Reverse-engineer it
      ; with a simple command here. (bmy, 5/29/13)
      ;Data_Zonal   = Mean2( Data, 1 )
      ;--------------------------------------------------------------

      ; Get dimensions of data
      SData_1    = Size( Data_1, /Dim )
      SData_2    = Size( Data_2, /Dim )
      if ( nVersions eq 3 ) then begin
      SData_3    = Size( Data_3, /Dim )
      endif

      ; Split into zonal and cross section at 180E
      Data_1_Zonal = Total( Data_1, 1 ) / Float( SData_1[0] )
      Data_2_Zonal = Total( Data_2, 1 ) / Float( SData_2[0] )
      if ( nVersions eq 3 ) then begin
      Data_3_Zonal = Total( Data_3, 1 ) / Float( SData_3[0] )
      endif

      Lons = 0
      Data_1_Lon   = Data_1[ Lons, *, * ]
      Data_2_Lon   = Data_2[ Lons, *, * ]
      if ( nVersions eq 3 ) then begin
      Data_3_Lon   = Data_3[ Lons, *, * ]
      endif

      ; We no longer need the large data araray
      UnDefine, Data

      ;-----------------------------------------------------------------
      ; Plot the data!
      ;-----------------------------------------------------------------      

      if ( nVersions eq 2 ) then begin

         ; Plot the zonal mean for Version 1
         PlotZonal, Data_1_Zonal, Versions[0], -1, TracerName_1, $
                    Unit, GridInfo_1, Month, _EXTRA=e
      
         ; Plot the cross section for Version 1
         PlotZonal, Data_1_Lon, Versions[0], Lons, TracerName_1, $
                    Unit, GridInfo_1, Month, _EXTRA=e

         ; Plot the zonal mean for Version 2
         PlotZonal, Data_2_Zonal, Versions[1], -1, TracerName_2, $
                    Unit, GridInfo_2, Month, _EXTRA=e
      
         ; Plot the cross section for Version 2
         PlotZonal, Data_2_Lon, Versions[1], Lons, TracerName_2, $
                    Unit, GridInfo_2, Month, _EXTRA=e

      endif else begin

         ; Plot the zonal mean for Version 1
         PlotZonal, Data_1_Zonal, Versions[0], -1, TracerName_1, $
                    Unit, GridInfo_1, Month, _EXTRA=e
      
         ; Plot the cross section for Version 1
         PlotZonal, Data_1_Lon, Versions[0], Lons, TracerName_1, $
                    Unit, GridInfo_1, Month, _EXTRA=e

         ; Plot the zonal mean for Version 2
         PlotZonal, Data_2_Zonal, Versions[1], -1, TracerName_2, $
                    Unit, GridInfo_2, Month, _EXTRA=e
      
         ; Plot the cross section for Version 2
         PlotZonal, Data_2_Lon, Versions[1], Lons, TracerName_2, $
                    Unit, GridInfo_2, Month, _EXTRA=e

         ; Plot the zonal mean for Version 3
         PlotZonal, Data_3_Zonal, Versions[2], -1, TracerName_3, $
                    Unit, GridInfo_3, Month, _EXTRA=e
      
         ; Plot the cross section for Version 3
         PlotZonal, Data_3_Lon, Versions[2], Lons, TracerName_3, $
                    Unit, GridInfo_3, Month, _EXTRA=e

      endelse

      ; Plot the top title on each page  
      if ( D*4 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif

      ;-----------------------------------------------------------------
      ; Undefine stuff for next iteration
      ;-----------------------------------------------------------------
      UnDefine, Data_1_Zonal
      UnDefine, Data_2_Zonal
      UnDefine, Data_3_Zonal
      UnDefine, Data_1_Lon
      UnDefine, Data_2_Lon
      UnDefine, Data_3_Lon
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
 
