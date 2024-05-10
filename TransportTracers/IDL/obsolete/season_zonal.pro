;-----------------------------------------------------------------------
;+
; NAME:
;        SEASON_ZONAL
;
; PURPOSE:
;        Creates zonal-mean curtain plots of GEOS-Chem tracers for
;        Jan, Apr, Jul, and Oct.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        SEASON_ZONAL, FILE, LONS, YEAR, TRACERS, VERSION, [, Keywords ]
;
; INPUTS:
;        FILE -> The name of the file containing data to be plotted.
;
;        LONS -> Longitudes to plot
;
;        YEAR -> The YEAR corresponding to the data to be plotted.
;
;        TRACERS -> The list of transported tracers (i.e. diagnostic
;             category "IJ-AVG-$").
;
;        VERSION -> The model version number corresponding to the
;             data to be plotted.
;
; KEYWORD PARAMETERS:
;        /PRESSURE -> Set this switch to plot pressure on the Y-axis.
;             The default is to plot altitude on the Y-axis.
;
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
;        (1) Meant to be called from PLOT_PASSIVE.
;
; EXAMPLES:
;        FILE     = 'trac_avg.v11-01d'
;        LONS     = 0
;        YEAR     = 2013
;        TRACERS  = 4
;        VERSION  = 'v11-01d'
;
;        ZONAL, FILE, LONS, YEAR, TRACERS, VERSION, $
;             /PRESSURE, /PS, OUTFILENAME='myplot.ps'
;
; MODIFICATION HISTORY:
;        mps, 23 Nov 2015: Initial version based on "zonal.pro"
;-----------------------------------------------------------------------


pro PlotZonal, Data, Lons, TracerName, Month, Unit, GridInfo, $
               Pressure=Pressure, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTZONAL plots either the zonal mean or
   ; longitudinal map of tracer (bmy, 11/14/07)
   ;====================================================================

   ; Keywords
   Pressure    = Keyword_Set( Pressure )

   ; Plot title
   if ( Lons ge 0 )                                      $
      then Title = 'Tracer Curtain @ '+ string(GridInfo.XMid[Lons])+' - ' + TracerName + ' - ' + Month  $
      else Title = 'Tracer Curtain Zonal mean - ' + TracerName + ' - ' + Month

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

   ; Specify format based on unit
   if Unit eq 'kg/kg total air' then begin
      Format = '(e13.2)'
   endif else begin
      Format = '(f13.4)'
   endelse
   
   ; Set Y-axis coordinate for pressure or altitude grid (bmy, 7/18/11)
   if ( Pressure )                                                          $
      then YTitle = 'Pressure (hPa)'                                        $
      else YTitle = 'Altitude (km)'

   ; Plot data w/ country boundaries
   TvPlot, Data, Lats, Ymid,                                 $
           /Cbar,         Division=Div,       /Sample,       $
           Title=Title,   CBFormat=Format,    Unit=Unit,     $
           /XStyle,       XTickV=XTickV,      XTicks=XTicks, $
           XMinor=XMinor, XTitle=Xtitle,      YTitle=YTitle, $
           /YStyle,       _EXTRA=e

end

;------------------------------------------------------------------------------

pro Season_Zonal, File, Lons, Year, Tracers, Version, Pressure=Pressure, $
                  PS=PS, OutFileName=OutFileName, _EXTRA=e
   
   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ChkStru, ColorBar_NDiv, Extract_FileName

   ; Arguments
   if ( N_Elements( File    ) ne 1 ) then Message, 'Invalid FILES!'
   if ( N_Elements( Lons    ) ne 1 ) then Message, 'Invalid LONS'
   if ( N_Elements( Year    ) ne 1 ) then Message, 'Invalid YEAR!'
   if ( N_Elements( Version ) ne 1 ) then Message, 'Invalid VERSIONS!'

   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'zonal.ps'

   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem ' + Version            + $
      ' Zonal Mean and Curtain at 180E !C!C' + Extract_FileName( File )

   ; Save original color table information
   TvLct, R, G, B, /Get

   ; Save the original settings of the !MYCT sysvar
   if ( ChkStru( !MYCT ) ) then Myct_Orig = !MYCT

   ; Load modified spectrum, extended to 12 colors
   MyCt, /ModSpec, NColors=12

   ; Define TAU0 for each season
   Tau0_Jan =  Nymd2Tau( 20130101 )
   Tau0_Apr =  Nymd2Tau( 20130401 )
   Tau0_Jul =  Nymd2Tau( 20130701 )
   Tau0_Oct =  Nymd2Tau( 20131001 )

   ;====================================================================
   ; Read data from the files
   ;====================================================================
   
   ; Read transported tracers
   CTM_Get_Data, DataInfo_Jan, 'IJ-AVG-$', $
                 File=File, Tau0=Tau0_Jan, Tracer=Tracers, /Quiet

   ; Read transported tracers
   CTM_Get_Data, DataInfo_Apr, 'IJ-AVG-$', $
                 File=File, Tau0=Tau0_Apr, Tracer=Tracers, /Quiet

   ; Read transported tracers
   CTM_Get_Data, DataInfo_Jul, 'IJ-AVG-$', $
                 File=File, Tau0=Tau0_Jul, Tracer=Tracers, /Quiet

   ; Read transported tracers
   CTM_Get_Data, DataInfo_Oct, 'IJ-AVG-$', $
                 File=File, Tau0=Tau0_Oct, Tracer=Tracers, /Quiet

   ;====================================================================
   ; Create the plots
   ;====================================================================

   ; Number of rows & columns on the plot
   Rows = 3
   Cols = 2

   ; Open the plot device and initialize the page
   Open_Device, /Color, Bits=8, /Portrait, PS=PS, File=OutFileName, _EXTRA=e
 
   ; Multiple panels per page
   MultiPanel, Rows=Rows, Cols=Cols, Margin=[ 0.03, 0.03, 0.03, 0.03 ]

   ; Loop over all data blocks
   for D = 0L, N_Elements( DataInfo_Jan )-1L do begin

      ;-----------------------------------------------------------------
      ; Extract and plot data for January 
      ;-----------------------------------------------------------------
      
      ; Set month string
      Month = 'Jan'
      
      ; Get MODELINFO and GRIDINFO structures
      GetModelAndGridInfo, DataInfo_Jan[D], ModelInfo, GridInfo
      
      ; Get tracername and unit strings
      TracerName = DataInfo_Jan[D].TracerName
      Unit       = DataInfo_Jan[D].Unit

      ; Get data array
      Data       = *( DataInfo_Jan[D].Data )
      
      ; Get dimensions of data
      SData      = Size( Data, /Dim )
      
      ; Split into zonal and cross section at 180E
      Data_Zonal = Total( Data, 1 ) / Float( SData[0] )
      Data_Lon   = Data[ Lons, *, * ]
      
      ; We no longer need the large data araray
      UnDefine, Data
      
      ; Plot the zonal data
      PlotZonal, Data_Zonal, -1, TracerName, Month, Unit, GridInfo, $
                 Pressure=Pressure, _EXTRA = e
      
      ; Plot the longitudinal data
      PlotZonal, Data_Lon, Lons, TracerName, Month, Unit, GridInfo, $
                 Pressure=Pressure, _EXTRA=e
      
      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif
      
      ; Undefine stuff for next iteration
      UnDefine, Data_Zonal
      UnDefine, Data_Lon
      UnDefine, ModelInfo
      UnDefine, GridInfo
      UnDefine, TracerName
      UnDefine, Unit

   endfor
      
   ; Loop over all data blocks
   for D = 0L, N_Elements( DataInfo_Apr )-1L do begin
      
      ;-----------------------------------------------------------------
      ; Extract and plot data for April
      ;-----------------------------------------------------------------
      
      ; Set month string
      Month = 'Apr'
      
      ; Get MODELINFO and GRIDINFO structures
      GetModelAndGridInfo, DataInfo_Apr[D], ModelInfo, GridInfo
      
      ; Get tracername and unit strings
      TracerName = DataInfo_Apr[D].TracerName
      Unit       = DataInfo_Apr[D].Unit

      ; Get data array
      Data       = *( DataInfo_Apr[D].Data )
      
      ; Get dimensions of data
      SData      = Size( Data, /Dim )
      
      ; Split into zonal and cross section at 180E
      Data_Zonal = Total( Data, 1 ) / Float( SData[0] )
      Data_Lon   = Data[ Lons, *, * ]
      
      ; We no longer need the large data araray
      UnDefine, Data
      
      ; Plot the zonal data
      PlotZonal, Data_Zonal, -1, TracerName, Month, Unit, GridInfo, $
                 Pressure=Pressure, _EXTRA=e
      
      ; Plot the longitudinal data
      PlotZonal, Data_Lon, Lons, TracerName, Month, Unit, GridInfo, $
                 Pressure=Pressure, _EXTRA=e

      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif
      
      ; Undefine stuff for next iteration
      UnDefine, Data_Zonal
      UnDefine, Data_Lon
      UnDefine, ModelInfo
      UnDefine, GridInfo
      UnDefine, TracerName
      UnDefine, Unit

   endfor
      
   ; Loop over all data blocks
   for D = 0L, N_Elements( DataInfo_Jul )-1L do begin
      
      ;-----------------------------------------------------------------
      ; Extract and plot data for July
      ;-----------------------------------------------------------------
      
      ; Set month string
      Month = 'Jul'

      ; Get MODELINFO and GRIDINFO structures
      GetModelAndGridInfo, DataInfo_Jul[D], ModelInfo, GridInfo
      
      ; Get tracername and unit strings
      TracerName = DataInfo_Jul[D].TracerName
      Unit       = DataInfo_Jul[D].Unit
      
      ; Get data array
      Data       = *( DataInfo_Jul[D].Data )
      
      ; Get dimensions of data
      SData      = Size( Data, /Dim )
      
      ; Split into zonal and cross section at 180E
      Data_Zonal = Total( Data, 1 ) / Float( SData[0] )
      Data_Lon   = Data[ Lons, *, * ]
      
      ; We no longer need the large data araray
      UnDefine, Data
      
      ; Plot the zonal data
      PlotZonal, Data_Zonal, -1, TracerName, Month, Unit, GridInfo, $
                 Pressure=Pressure, _EXTRA=e
      
      ; Plot the longitudinal data
      PlotZonal, Data_Lon, Lons, TracerName, Month, Unit, GridInfo, $
                 Pressure=Pressure, _EXTRA=e

      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif
      
      ; Undefine stuff for next iteration
      UnDefine, Data_Zonal
      UnDefine, Data_Lon
      UnDefine, ModelInfo
      UnDefine, GridInfo
      UnDefine, TracerName
      UnDefine, Unit

   endfor
      
   ; Loop over all data blocks
   for D = 0L, N_Elements( DataInfo_Oct )-1L do begin
      
      ;-----------------------------------------------------------------
      ; Extract and plot data for October
      ;-----------------------------------------------------------------
      
      ; Set month string
      Month = 'Oct'

      ; Get MODELINFO and GRIDINFO structures
      GetModelAndGridInfo, DataInfo_Oct[D], ModelInfo, GridInfo
      
      ; Get tracername and unit strings
      TracerName = DataInfo_Oct[D].TracerName
      Unit       = DataInfo_Oct[D].Unit
      
      ; Get data array
      Data       = *( DataInfo_Oct[D].Data )
      
      ; Get dimensions of data
      SData      = Size( Data, /Dim )
      
      ; Split into zonal and cross section at 180E
      Data_Zonal = Total( Data, 1 ) / Float( SData[0] )
      Data_Lon   = Data[ Lons, *, * ]
      
      ; We no longer need the large data araray
      UnDefine, Data
      
      ; Plot the zonal data
      PlotZonal, Data_Zonal, -1, TracerName, Month, Unit, GridInfo, $
                 Pressure=Pressure, _EXTRA=e
      
      ; Plot the longitudinal data
      PlotZonal, Data_Lon, Lons, TracerName, Month, Unit, GridInfo, $
                 Pressure=Pressure, _EXTRA=e
      
      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif
      
      ; Undefine stuff
      UnDefine, Data_Zonal
      UnDefine, Data_Lon
      UnDefine, ModelInfo
      UnDefine, GridInfo
      UnDefine, TracerName
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
 
