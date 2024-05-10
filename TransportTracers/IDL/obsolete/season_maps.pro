;-----------------------------------------------------------------------
;+
; NAME:
;        SEASON_MAPS
;
; PURPOSE:
;        Creates lon-lat maps of GEOS-Chem tracers at the 
;        surface and 500 hPa levels for Jan, Apr, Jul, and Oct.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        SEASON_MAPS, FILE, LEVELS, YEAR, TRACERS, VERSION, [, Keywords ]
;
; INPUTS:
;        FILE -> The name of the file containing data to be plotted.
;
;        LEVELS -> A 4-element vector containing the level indices
;             for the GEOS-Chem surface layer and 500 hPa layer.
;             for both models (e.g. SFC_1, SFC_2, 500_1, 500_2).
;             NOTE: This is in Fortran notation (starting from 1!)
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
;        (1) Meant to be called from PLOT_PASSIVE.
;
; EXAMPLES:
;        FILE     = 'trac_avg.v11-01d'
;        LEVELS   = [ 1, 13 ]
;        YEAR     = 2013
;        TRACERS  = 4
;        VERSION  = 'v11-01d'
;
;        SEASON_MAPS, FILE, LEVELS, YEAR, TRACERS, VERSION, $
;                    /PS, OUTFILENAME='myplot.ps'
;
;             ; Creates seasonal tracer maps of GEOS-CHEM v11-01d.
;             ; Output is sent to PostScript file "myplot.ps".
;
; MODIFICATION HISTORY:
;        mps, 23 Nov 2015: Initial version based on "maps.pro"
;-----------------------------------------------------------------------

pro PlotMap, Data, Level, TracerName, Month, Unit, GridInfo, _EXTRA=e
   
   ;====================================================================
   ; Internal routine PLOTMAP plots either the surface or 500 hPa
   ; map of tracer (bmy, 11/14/07)
   ;====================================================================

   ; Plot title
   if ( Level gt 1 )                                      $
      then Title = 'Tracer Map @ 500 hPa - ' + TracerName + ' - ' +  Month $
      else Title = 'Tracer Map @ Surface - ' + TracerName + ' - ' +  Month

   ; Number of colorbar tickmarks
   Divisions = ColorBar_NDiv( 6 )
      
   ; Don't plot the polar latitudes
   XMid = GridInfo.XMid
   YMid = GridInfo.YMid[ 1:GridInfo.JMX-2 ]
   Data = Data[ *, 1:GridInfo.JMX-2 ]

   ; Specify format based on unit
   if Unit eq 'kg/kg total air' then begin
      Format = '(e13.2)'
   endif else begin
      Format = '(f13.4)'
   endelse
   
   ; Plot data w/ country boundaries
   TvMap, Data, XMid, Ymid,                              $
      /Countries,         /Coasts,            /Cbar,     $
      Division=Divisions, /Sample,            /Grid,     $
      Title=Title,        CBFormat=Format,    Unit=Unit, $
      _EXTRA=e

end

;------------------------------------------------------------------------------

pro Season_Maps, File, Levels, Year, Tracers, Version, PS=PS,     $
                 OutFileName=OutFileName, _EXTRA=e
   
   ;====================================================================
   ; Initialization
   ;====================================================================
   
   ; External functions
   FORWARD_FUNCTION ChkStru, ColorBar_NDiv, Extract_FileName

   ; Arguments
   if ( N_Elements( File    ) ne 1 ) then Message, 'Invalid FILE!'
   if ( N_Elements( Levels  ) ne 2 ) then Message, 'Invalid LEVELS'
   if ( N_Elements( Year    ) ne 1 ) then Message, 'Invalid YEAR!'
   if ( N_Elements( Version ) ne 1 ) then Message, 'Invalid VERSION!'
   
   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'maps.ps'

   ; Title for the top of the plot
   TopTitle = 'GEOS-Chem ' + Version            + $
      ' Tracer Maps at Surface and 500 hPa!C!C' + Extract_FileName( File )

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
      ; Extract and plot data for Janaury
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
      
      ; Split into sfc and 500hPa levels
      Data_Sfc   = Data[ *, *, Levels[0]-1L ]
      Data_500   = Data[ *, *, Levels[1]-1L ]
      
      ; We no longer need the large data array
      UnDefine, Data
      
      ; Plot the surface data
      PlotMap, Data_Sfc, Levels[0], TracerName, Month, Unit, GridInfo, _EXTRA=e
         
      ; Plot the 500hPa data
      PlotMap, Data_500, Levels[1], TracerName, Month, Unit, GridInfo, _EXTRA=e

      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif
      
      ; Undefine stuff for next iteration
      UnDefine, Data_500
      UnDefine, Data_Sfc
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

      ; Split into sfc and 500hPa levels
      Data_Sfc   = Data[ *, *, Levels[0]-1L ]
      Data_500   = Data[ *, *, Levels[1]-1L ]
      
      ; We no longer need the large data array
      UnDefine, Data
      
      ; Plot the surface data
      PlotMap, Data_Sfc, Levels[0], TracerName, Month, Unit, GridInfo, _EXTRA=e
         
      ; Plot the 500hPa data
      PlotMap, Data_500, Levels[1], TracerName, Month, Unit, GridInfo, _EXTRA=e
         
      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif

      ; Undefine stuff for next iteration
      UnDefine, Data_500
      UnDefine, Data_Sfc
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
      
      ; Split into sfc and 500hPa levels
      Data_Sfc   = Data[ *, *, Levels[0]-1L ]
      Data_500   = Data[ *, *, Levels[1]-1L ]
      
      ; We no longer need the large data array
      UnDefine, Data
      
      ; Plot the surface data
      PlotMap, Data_Sfc, Levels[0], TracerName, Month, Unit, GridInfo, _EXTRA=e
         
      ; Plot the 500hPa data
      PlotMap, Data_500, Levels[1], TracerName, Month, Unit, GridInfo, _EXTRA=e
         
      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif

      ; Undefine stuff for next iteration
      UnDefine, Data_500
      UnDefine, Data_Sfc
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

      ; Split into sfc and 500hPa levels
      Data_Sfc   = Data[ *, *, Levels[0]-1L ]
      Data_500   = Data[ *, *, Levels[1]-1L ]
     
      ; We no longer need the large data array
      UnDefine, Data
     
      ; Plot the surface data
      PlotMap, Data_Sfc, Levels[0], TracerName, Month, Unit, GridInfo, _EXTRA=e
        
      ; Plot the 500hPa data
      PlotMap, Data_500, Levels[1], TracerName, Month, Unit, GridInfo, _EXTRA=e
        
      ; Plot the top title on each page  
      if ( D*2 mod ( Rows * Cols ) eq 0 ) then begin
         XYoutS, 0.5, 1.03, TopTitle, $
            /Normal, Color=!MYCT.BLACK, CharSize=1.0, Align=0.5
      endif

      ; Undefine stuff
      UnDefine, Data_500
      UnDefine, Data_Sfc
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
 
