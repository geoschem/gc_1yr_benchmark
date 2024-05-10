;------------------------------------------------------------------------------
;+
; NAME:
;        PAN_PROFILES
;
; PURPOSE:
;        Calculate and plot vertical profiles of PAN against observations
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        PAN_profiles, Pref1, Model1, Dlat1, Dlon1, Year1,
;                      Pref2, Model2, Dlat2, Dlon2, Year2,
;                      Pref3, Model3, Dlat3, Dlon3, Year3,
;                      Title, PSName
;
; INPUTS:
;        PREF* -> Directory and label name of model to be plotted
;
;        MODEL* -> The model name (e.g. GEOS5, GEOSFP, MERRA2)
;
;        DLAT* -> Delta-J interval between grid box edges
;
;        DLON* -> Delta-I interval between grid box edges
;
;        YEAR* -> Year of model simulation
;
;        TITLE -> Plot title for top of page
;
;        PSNAME -> Name of output PS file

; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR.
;
; MODIFICATION HISTORY:
;        mps, 10 May 2017: Intial version from Emily Fischer
;------------------------------------------------------------------------------

pro PAN_profiles, Pref1, Model1, Dlat1, Dlon1, Year1, $
                  Pref2, Model2, Dlat2, Dlon2, Year2, $
                  Pref3, Model3, Dlat3, Dlon3, Year3, $
                  TopTitle, PSName

   ;=======================================================================
   ; Initialization
   ;=======================================================================

   ; Number of model versions to compare
   if ( Pref3 eq 'None' ) then nVersions = 2 $
                          else nVersions = 3

   nmon = 12
   Files1 = strarr(nmon)
   Files2 = strarr(nmon)
   if ( nVersions eq 3 ) then begin
   Files3 = strarr(nmon)
   endif

   ; Loop over months
   for i = 0, nmon-1 do begin

      mn=strtrim(String(fix(i+1)),2)
      if (strlen(mn) eq 1) then begin            
         mn='0'+mn
      endif

      Name1 = Pref1+'GEOSChem.SpeciesConc.'+year1+mn+'01_0000z.nc4'
      Name2 = Pref2+'GEOSChem.SpeciesConc.'+year2+mn+'01_0000z.nc4'
      if ( nVersions eq 3 ) then begin
      Name3 = Pref3+'GEOSChem.SpeciesConc.'+year3+mn+'01_0000z.nc4'
      endif
      
      Files1[i] = Name1
      Files2[i] = Name2
      if ( nVersions eq 3 ) then begin
      Files3[i] = Name3
      endif
      
   endfor

   ; Get MODELINFO structures for the 3 models
   Type1 = CTM_Type( Model1, Res=[ DLon1, DLat1 ] )
   Type2 = CTM_Type( Model2, Res=[ DLon2, DLat2 ] )
   if ( nVersions eq 3 ) then begin
   Type3 = CTM_Type( Model3, Res = [ DLon3, DLat3 ] )
   endif
   
   ; Get GRIDINFO structures for the 3 models
   Grid1 = CTM_Grid( Type1 )
   Grid2 = CTM_Grid( Type2 )
   if ( nVersions eq 3 ) then begin
   Grid3 = CTM_Grid( Type3 )
   endif
   
   ; Open postscript file
   open_device, olddevice, /ps, /color, filename=PSName, /portrait

   ; Get defaults
   X_OMARGIN   = !X.OMARGIN
   Y_OMARGIN   = !Y.OMARGIN
   X_MARGIN    = !X.MARGIN
   Y_MARGIN    = !Y.MARGIN
   P_CHARTHICK = !P.CHARTHICK
   P_THICK     = !P.THICK
   X_THICK     = !X.THICK
   Y_THICK     = !Y.THICK

   !X.OMARGIN = [7, 3]
   !Y.OMARGIN = [5, 2]
   !X.MARGIN  = [0, 0]
   !Y.MARGIN  = [1, 1]
   !P.CHARTHICK = 3
   !P.THICK = 4.0
   !X.THICK = 4.0
   !Y.THICK = 4.0
   
   ; Set up plotting parameters
   nrow = 5
   ncol = 5
   !P.Multi = [0, ncol, nrow, 0, 1]
   multipanel,rows=nrow,cols=ncol
   
   xrange    = [0, 1000]
   yrange    = [0, 12]
   charsize  = 1.5
   ytickv    = [0, 2, 4, 6, 8, 10, 12]
   yticks    = 6
   yminor    = 0
   xticks    = 3
   xminor    = 2
   xstyle    = 1
   ystyle    = 1
   color     = 1
   xtickv    = [0, 500, 1000]
   xtickname = replicate(' ', 4)
   ytickname = replicate(' ', 8)
   thick     = 4
   color_m1  = 2
   color_m2  = 3
   color_m3  = 4

   ; Define vertical levels
   alt     = CTM_GRID(CTM_TYPE('GEOS5_47L'), /ZMID)
   alt2    = alt[0:30]
   altdata = [0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, 8.5, 9.5, 10.5, 11.5]

   Month=[ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
           'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'  ]

   ;=======================================================================
   ; East Asian Outflow
   ;=======================================================================

   ;-----------------------------------------
   ; China Coast Trace P - March
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/ChinaCoast.PAN.new.tracep.wo.filt'
   ReadData, DataFile, Data, Header, Delim=' '

   Title = '1. China Coast'
   Lat   = [ 25,  40  ]
   Lon   = [ 122, 126 ]
   Mon   = 3

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species = 'SpeciesConcVV_PAN' )
   endif
   
   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1], IndLat[0]:IndLat[1], *]
   endif
   
   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif
   
   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif
   
   ; Top title (page 1)
   xyouts, 0.5, 1.0, TopTitle, /normal, align=0.5, charsize=1.2, color=1

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif
   
   ; Overplot the data with error bars
   oplot,    Data[2, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], Data[0, *], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 600, 11, Month[Mon-1],   color=1, charsize=1
   xyouts, -150, 6, 'Altitude (km)',color=1, charsize=1, alignment=0.5, orientation=90

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; S China Coast Trace P - March
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/SChinaCoast.PAN.new.tracep.wo.filt'
   ReadData, DataFile, Data, Header, Delim=' '

   Title = '2. S China Coast'
   Lat   = [ 13,  23  ]
   Lon   = [ 112, 126 ]
   Mon   = 3

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], Data[0, *], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 600, 11, Month[Mon-1],   color=1, charsize=1
   xyouts, -150, 6, 'Altitude (km)',color=1, charsize=1, alignment=0.5, orientation=90

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; SE China aase - February
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/SEChina.PAN.new.aase'
   ReadData, DataFile, Data, Header, Delim=' '

   Title = '3. SE China'
   Lat   = [ 20,  30  ]
   Lon   = [ 125, 150 ]
   Mon   = 2

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:9],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:9],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:9], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 600, 11, Month[Mon-1],   color=1, charsize=1
   xyouts, -150, 6, 'Altitude (km)',color=1, charsize=1, alignment=0.5, orientation=90

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; SE China gte - October
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/SEChina.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '

   Title = '4. SE China'
   Lat   = [ 15,  30  ]
   Lon   = [ 120, 140 ]
   Mon   = 10

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 600, 11, Month[Mon-1],   color=1, charsize=1
   xyouts, -150, 6, 'Altitude (km)',color=1, charsize=1, alignment=0.5, orientation=90

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; S Japan Trace P - March
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/SJapan.PAN.new.tracep.wo.filt'
   ReadData, DataFile, Data, Header, Delim=' '

   Title = '5. S Japan'
   Lat   = [ 25,  35  ]
   Lon   = [ 126, 140 ]
   Mon   = 3

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], Data[0, *], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 600, 11, Month[Mon-1],   color=1, charsize=1
   xyouts, -150, 6, 'Altitude (km)',color=1, charsize=1, alignment=0.5, orientation=90
   xyouts, 0,    -1, '0',    color=1, alignment=0.5, charsize=1
   xyouts, 500,  -1, '500',  color=1, alignment=0.5, charsize=1
   xyouts, 1000, -1, '1000', color=1, alignment=0.5, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; W trop Pacific Trace P - March
   ;-----------------------------------------

   xrange = [0, 700]
   xtickv = [0, 350, 700]

   DataFile = 'data/eval/aircraft/data/PAN/WTropPacific.PAN.new.tracep.wo.filt'
   ReadData, DataFile, Data, Header, Delim=' '

   Title = '6. W Trop Pacific'
   Lat   = [ 13,  25  ]
   Lon   = [ 126, 146 ]
   Mon   = 3

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], Data[0, *], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Japan gte - October
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/Japan.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '

   Title = '7. Japan'
   Lat   = [ 25,  40  ]
   Lon   = [ 140, 150 ]
   Mon   = 10

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1],  color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Japan Coast aase - February
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/JapanCoast.PAN.new.aase'
   ReadData, DataFile, Data, Header, Delim=' '

   Title = '8.  Japan Coast'
   Lat   = [ 30,  40  ]
   Lon   = [ 140, 150 ]
   Mon   = 2

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:8],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:8],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *],altdata[0:8], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1],  color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;=======================================================================
   ; Remote Pacific
   ;=======================================================================

   ;-----------------------------------------
   ; Fiji - March
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/Fiji.PAN.new.ptb'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '9. Fiji'
   Lat   = [  -25, -10  ]
   Lon1  = [  160, 175  ]
   Lon2  = [ -180, -170 ]
   Mon   = 3

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   ; Break longitude down into two ranges because we cross the edge of the
   ;  global domain
   CTM_Index, Type1, IndLon1, IndLat, Edge=[Lat[0],Lon1[0],Lat[1],Lon1[1]], /Non
   CTM_Index, Type1, IndLon2, IndLat, Edge=[Lat[0],Lon2[0],Lat[1],Lon2[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon1 = IndLon1 - 1
   IndLon2 = IndLon2 - 1
   IndLat  = IndLat  - 1

;   Print, IndLon1
;   Print, IndLon2
;   Print, IndLat

   ; KLUDGE: Hardwire longitude indices to get station selection to work
   ; (mps, 2/5/14)
   IndLon = [ 0, 1, 2, 68, 69, 70, 71 ]

   ; Select data for this station
   PAN1_box = PAN1[IndLon,IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon,IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon,IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], Data[0, *], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Hawaii - March
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/Hawaii.PAN.new.ptb'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '10. Hawaii'
   Lat   = [ 15,   25   ]
   Lon   = [ -160, -150 ]
   Mon   =  3

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], Data[0, *], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   ; x-axis labels
   xyouts, 350, -1, '350', color=1, alignment=0.5, charsize=1
   xyouts, 700, -1, '700', color=1, alignment=0.5, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Equatorail Pacific - March
   ;-----------------------------------------

   xrange = [0, 200]
   xtickv = [0, 100, 200]

   DataFile = 'data/eval/aircraft/data/PAN/EqPacific.PAN.new.ptb'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '11. Eq Pacific'
   Lat   = [  -10, 5    ]
   Lon1  = [  160, 175  ]
   Lon2  = [ -180, -140 ]
   Mon   = 3

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   ; Break longitude down into two ranges because we cross the edge of the
   ;  global domain
   CTM_Index, Type1, IndLon1, IndLat, Edge=[Lat[0],Lon1[0],Lat[1],Lon1[1]], /Non
   CTM_Index, Type1, IndLon2, IndLat, Edge=[Lat[0],Lon2[0],Lat[1],Lon2[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon1 = IndLon1 - 1
   IndLon2 = IndLon2 - 1
   IndLat  = IndLat  - 1

;   Print, IndLon1
;   Print, IndLon2
;   Print, IndLat

   ; KLUDGE: Hardwire longitude indices to get station selection to work
   ; (mps, 2/5/14)
   IndLon = [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 68, 69, 70, 71 ]

   ; Select data for this station
   PAN1_box = PAN1[IndLon,IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon,IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon,IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], Data[0, *], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 120, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Tahiti - March
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/Tahiti.PAN.new.ptb'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '12. Tahiti'
   Lat   = [ -30,  -10  ]
   Lon   = [ -160, -140 ]
   Mon   = 3

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 120, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Easter Island - March
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/EasterIs.PAN.new.ptb'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '13. Easter Is'
   Lat   = [ -30,  -10  ]
   Lon   = [ -130, -100 ]
   Mon   = 3

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 120, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Easter Island - Sept
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/EasterIs.PAN.new.pem.wo.filt'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '14. Easter Is'
   Lat   = [ -35,  -10  ]
   Lon   = [ -120, -105 ]
   Mon   = 9

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 120, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; New Zealand- Sept
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/NewZealand.PAN.new.pem.wo.filt'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '15. New Zealand'
   Lat   = [ -55,  -35 ]
   Lon   = [ -170, 170 ]
   Mon   = 9

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species = 'SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 120, 11, Month[Mon-1], color=1, charsize=1

   ; x-axis labels
   xyouts, 100, -1, '100', color=1, alignment=0.5, charsize=1
   xyouts, 200, -1, '200', color=1, alignment=0.5, charsize=1
   xyouts, 100, -3, 'PAN (pptv)', color=1, alignment=0.5, charsize=1.25

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; New Guinea- Feb
   ;-----------------------------------------

   xrange = [0, 700]
   xtickv = [0, 350, 700]

   DataFile = 'data/eval/aircraft/data/PAN/NewGuinea.PAN.new.aase'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '16. New Guinea'
   Lat   = [ -15, 0   ]
   Lon   = [ 140, 155 ]
   Mon   = 2

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Antarctica- Sept
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/Antarctic.PAN.new.pem.wo.filt'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '17. Antarctica'
   Lat   = [ -75,  -55 ]
   Lon   = [ -170, 170 ]
   Mon   = 9

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:10],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:10],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:10], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; INTEXB C130
   ;-----------------------------------------

   DataFile = 'data/PAN/intexb_c130_profiles.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim=','
   
   Title = '18. E Pacific'
   Lat   = [ 37,   53   ]
   Lon   = [ -141, -125 ]
   Mon   = 5

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; INTEXB DC8 North
   ;-----------------------------------------

   DataFile = 'data/PAN/intexb_dc8_profile_north.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim = ','
   
   Title = '19. E Pacific (>40N)'
   Lat   = [ 40,   62   ]
   Lon   = [ -185, -125 ]
   Mon   = 5

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; INTEXB DC8 South
   ;-----------------------------------------

   DataFile = 'data/PAN/intexb_dc8_profile_south.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim = ','
   
   Title = '20. E Pacific (<40N)'
   Lat   = [ 19,   40   ]
   Lon   = [ -185, -125 ]
   Mon   = 5

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   ; x-axis labels
   xyouts, 350, -1, '350', color=1, alignment=0.5, charsize=1
   xyouts, 700, -1, '700', color=1, alignment=0.5, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;=======================================================================
   ; Eastern Pacific to Western Atlantic - North America
   ;=======================================================================

   ;-----------------------------------------
   ; Phobea - April
   ;-----------------------------------------

   DataFile = 'data/PAN/phobea_bins.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim=','
   
   Title = '21. E Pacific'
   Lon   = [ -129, -125 ]
   Lat   = [   40,   48 ]
   Mon   = 4

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; ITCT2k2 - **Check to see if Apr or May**
   ;-----------------------------------------

   DataFile = 'data/PAN/itct2k2_profile.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim=','

   Title = '22. E Pacific'
   LAT   = [   30,   48 ] ; ITCT2k2
   LON   = [ -130, -125 ] ; ITCT2k2
   Mon   = 4

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Milagro - April
   ;-----------------------------------------

   DataFile = 'data/PAN/milagro.csv'
   ReadData, DataFile, Data, Header, /NoHeader, Cols=4, Delim=','
   
   Title = '23. Mexico'
   Lat   = [   14 , 40 ]
   Lon   = [ -123, -86 ]
   Mon   = 4

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Western US - August
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/WesternUS.PAN.new.gte'
   ReadData, DataFile, Data, Header, DELIM=' '
   
   Title = '24. West US'
   Lat   = [   30,   40 ]
   Lon   = [ -120, -105 ]
   Mon   = 8

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:6],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:6],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:6], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; US West Coast - August
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/USWCoast.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '25. US West Coast'
   Lon   = [ -135, -120 ]
   Lat   = [   25,   40 ]
   Mon   = 8

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:6],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:6],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:6], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   ; x-axis labels
   xyouts, 350, -1, '350', color=1, alignment=0.5, charsize=1
   xyouts, 700, -1, '700', color=1, alignment=0.5, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Intex A Northeast US - August
   ;-----------------------------------------

   ;INTEX-NA Northern North America
   DataMean   = [431, 293, 203, 167, 185, 252, 251, 272, 318, 275, 210, 106]
   DataStd    = [470, 278, 150, 110, 128, 141, 174, 171, 189, 128, 133, 151]
   DataMedian = [330, 203, 141, 147, 178, 208, 219, 220, 288, 241, 216, 25 ]
   
   Title = '26. NE US'
   Lat   = [   40,  50 ]
   Lon   = [ -105, -76 ]
   Mon   = 8

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    DataMean,   altdata,  color=1,  thick=thick
   oplot,    DataMedian, altdata,  color=1,  thick=thick,  linestyle=2
   errorbar, DataMean,   altdata, DataStd, /X,  color=1,  thick=thick

   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1
   xyouts, -100, 6, 'Altitude (km)',color=1, alignment=0.5, charsize=1, orientation=90

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3
   Undefine, DataMean
   Undefine, DataMedian
   Undefine, DataStd

   ;-----------------------------------------
   ; Intex A Southeast US - August
   ;-----------------------------------------

   ;INTEX-NA Southern North America
   DataMean   = [423, 318, 194, 262, 222, 265, 279, 314, 312, 320, 322, 372]
   DataStd    = [277, 214, 171, 294, 140, 163, 213, 180, 186, 208, 182, 217]
   DataMedian = [408, 268, 157, 191, 198, 241, 227, 291, 288, 249, 270, 352]
   
   Title = '27. SE US'
   Lat   = [   25,  40 ]
   Lon   = [ -105, -76 ]
   Mon   = 8

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    DataMean,   altdata,  color=1,  thick=thick
   oplot,    DataMedian, altdata,  color=1,  thick=thick,  linestyle=2
   errorbar, DataMean,   altdata, DataStd, /X,  color=1,  thick=thick

   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1
   xyouts, -100, 6, 'Altitude (km)',color=1, alignment=0.5, charsize=1, orientation=90

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3
   Undefine, DataMean
   Undefine, DataMedian
   Undefine, DataStd

   ;-----------------------------------------
   ; Intex A Offshore - August
   ;-----------------------------------------

   ;INTEX-NA Off Shore US
   DataMean   = [148, 123, 131, 209, 290, 305, 341, 431, 350, 376, 352, 176]
   DataStd    = [239, 127, 140, 239, 285, 182, 173, 335, 172, 247, 201,  41]
   DataMedian = [ 53, 75,  111, 158, 196, 257, 289, 360, 327, 290, 314, 179]
   
   Title = '28. Atlantic'
   Lat   = [  30,  55 ]
   Lon   = [ -76, -35 ]
   Mon   = 8


   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    DataMean,   altdata,  color=1,  thick=thick
   oplot,    DataMedian, altdata,  color=1,  thick=thick,  linestyle=2
   errorbar, DataMean,   altdata, DataStd, /X,  color=1,  thick=thick

   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1
   xyouts, -100, 6, 'Altitude (km)',color=1, alignment=0.5, charsize=1, orientation=90

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3
   Undefine, DataMean
   Undefine, DataMedian
   Undefine, DataStd

   ;-----------------------------------------
   ; Sonex Maine - October
   ;-----------------------------------------

   DataFile = 'data/PAN/Maine.PAN.new.sonex.wo.filt'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '29. Maine'
   Lon   = [ -72, -55]
   Lat   = [  41,  50]
   Mon   = 10

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick

   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1
   xyouts, -100, 6, 'Altitude (km)', color=1, alignment=0.5, charsize=1, orientation=90

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Amazon- ** Check to see if May or Sep **
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/Amazon.PAN.new.aase'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '30. Amazon'
   Lon   = [ -70, -55 ]
   Lat   = [  -8,   2 ]
   Mon   = 5

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,    $
                             xrange=xrange,       yrange=yrange,    $
                             yticks=yticks,       yminor=yminor,    $
                             xticks=xticks,       xminor=xminor,    $
                             xstyle=xstyle,       ystyle=ystyle,    $
                             xtickname=xtickname, ytickname=ytickv, $
                             color=color,         thick=thick,      $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:5],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:5],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:5], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   ; x-axis labels
   xyouts, 0,   -1,   '0', color=1, alignment=0.5, charsize=1
   xyouts, 350, -1, '350', color=1, alignment=0.5, charsize=1
   xyouts, 700, -1, '700', color=1, alignment=0.5, charsize=1
   xyouts, -100, 6, 'Altitude (km)', color=1, alignment=0.5, charsize=1, orientation=90

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; East Brazil - September
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/EasternBrazil.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '31. E Brazil'
   Lon   = [ -50, -40 ] ; East Brazil
   Lat   = [ -20,  -3 ] ; East Brazil
   Mon   = 9

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; South Brazil - September
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/SBrazilCoast.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '32. S Brazil'
   Lon   = [ -50, -40 ]
   Lat   = [ -38, -23 ]
   Mon   = 9

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:9],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:9],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:9], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Alaska - July
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/Alaska.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '33. Alaska'
   Lon   = [ -170, -150 ]
   Lat   = [   50,   75 ]
   Mon   = 7

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:6],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:6],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:6], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Eastern Canada - July
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/EasternCanada.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '34. E Canada'
   Lat   = [  49,  57 ]
   Lon   = [ -70, -55 ]
   Mon   = 7

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:5],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:5],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:5], Data[3, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Central Canada - July
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/CentralCanada.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '35. Cent Canada'
   Lat   = [  44,  57 ]
   Lon   = [ -90, -75 ]
   Mon   = 7

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:5],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:5],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:5], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   ; x-axis labels
   xyouts, 350, -1, '350', color=1, alignment=0.5, charsize=1
   xyouts, 700, -1, '700', color=1, alignment=0.5, charsize=1
   xyouts, 700, -3, 'PAN (pptv)', color=1, alignment=0.5, charsize=1.25

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Arctas Summer North - July
   ;-----------------------------------------

   DataFile = 'data/PAN/arctas_summer_north.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim=','
   
   Title = '36. NA Arctic (>60N)*'
   Lat   = [   65,  86 ]
   Lon   = [ -131, -38 ]
   Mon   = 7

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Arctas Summer South - July
   ;-----------------------------------------

   DataFile = 'data/PAN/arctas_summer_south.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim=','
   
   Title = '37. Canada*'
   Lat   = [   51,  60 ]
   Lon   = [ -122, -82 ]
   Mon   = 7

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, 0:9], Data[0, 0:9],  color=1,  thick=thick
   oplot,    Data[3, 0:9], Data[0, 0:9],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, 0:9], Data[0, 0:9], Data[2, 0:9], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Arctas Spring - April
   ;-----------------------------------------

   DataFile = 'data/PAN/arctas_spring.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim=','
   
   Title = '38. NA Arctic*'
   Lat   = [   60,  89 ]
   Lon   = [ -175, -30 ]
   Mon   = 4

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; ARCPAC - April
   ;-----------------------------------------

   DataFile = 'data/PAN/arcpac.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim=' '
   
   Title = '39. NA Arctic*'
   Lat   = [   60,   75 ]
   Lon   = [ -165, -125 ]
   Mon   = 4

   ; Read PAN from MODELS 1-3
   PAN1 = Get_Species_Geos( Files1[Mon-1], Species='SpeciesConcVV_PAN' )
   PAN2 = Get_Species_Geos( Files2[Mon-1], Species='SpeciesConcVV_PAN' )
   if ( nVersions eq 3 ) then begin
   PAN3 = Get_Species_Geos( Files3[Mon-1], Species='SpeciesConcVV_PAN' )
   endif

   ; Get the lon & lat indices corresponding to this station
   CTM_Index, Type1, IndLon, IndLat, Edge=[Lat[0],Lon[0],Lat[1],Lon[1]], /Non

   ; Convert from F90 to IDL notation
   IndLon = IndLon - 1
   IndLat = IndLat - 1

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box = PAN1_box*1000
   PAN2_box = PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1], color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; POLARCAT - July
   ;-----------------------------------------

   DataFile = 'data/PAN/polarcatPAN.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim=','

   Title = '40. Greenland'
   Lon   = [ -65, 15 ]
   Lat   = [  57, 81 ]
   Mon   = 7

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1

   ; x-axis labels
   xyouts, 350, -1, '350', color=1, alignment=0.5, charsize=1
   xyouts, 700, -1, '700', color=1, alignment=0.5, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; SONEX Ireland - October
   ;-----------------------------------------

   DataFile = 'data/PAN/Ireland.PAN.new.sonex.wo.filt'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '41. Ireland'
   Lon   = [ -13, -3 ]
   Lat   = [  49, 54 ]
   Mon   = 10

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Tropical S Atlantic - September
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/TrpSAtlantic.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '42. Trop S Atl'
   Lon   = [ -25, -10 ]
   Lat   = [ -20,   2 ]
   Mon   = 9

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Africa W coast - September
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/AfricaWCoast.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '43. Africa W Cst'
   Lon   = [   0, 10 ]
   Lat   = [ -20,  0 ]
   Mon   = 9

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; South Africa - September
   ;-----------------------------------------

   DataFile = 'data/eval/aircraft/data/PAN/SouthAfrica.PAN.new.gte'
   ReadData, DataFile, Data, Header, Delim=' '
   
   Title = '44. South Africa'
   Lon   = [  15,  30 ]
   Lat   = [ -30, -15 ]
   Mon   = 9

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[2, *], altdata[0:11],  color=1,  thick=thick
   oplot,    Data[3, *], altdata[0:11],  color=1,  thick=thick, linestyle=2
   errorbar, Data[2, *], altdata[0:11], Data[4, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3

   ;-----------------------------------------
   ; Central Africa (AMMA) - July
   ;-----------------------------------------

   DataFile = 'data/PAN/amma.csv'
   ReadData, DataFile, Data, /NoHeader, Cols=4, Delim=','
   
   Title = '45. Central Africa'
   Lon   = [    4, 19 ]
   Lat   = [ -2.7,  7 ]
   Mon   = 7

   ; Select data for this station
   PAN1_box = PAN1[IndLon[0]:IndLon[1],IndLat[0]:Indlat[1],*]
   PAN2_box = PAN2[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   if ( nVersions eq 3 ) then begin
   PAN3_box = PAN3[IndLon[0]:IndLon[1],IndLat[0]:IndLat[1],*]
   endif

   ; Now convert ppb PAN to pptv PAN
   PAN1_box =  PAN1_box*1000
   PAN2_box =  PAN2_box*1000
   if ( nVersions eq 3 ) then begin
   PAN3_box =  PAN3_box*1000
   endif

   ; Average the model output into vertical profiles
   vert_mean1 = fltarr(31)
   for x=0,30 do begin
     vert_mean1[x]=mean(PAN1_box[*,*,x])
   endfor
   
   vert_mean2 = fltarr(31)
   for x=0,30 do begin
     vert_mean2[x]=mean(PAN2_box[*,*,x])
   endfor
   
   if ( nVersions eq 3 ) then begin
   vert_mean3 = fltarr(31)
   for x=0,30 do begin
     vert_mean3[x]=mean(PAN3_box[*,*,x])
   endfor
   endif

   ; Plot PAN profiles
   plot,  vert_mean1, alt2,  title=Title,  $
                             charsize=charsize,   xtickv=xtickv,       $
                             xrange=xrange,       yrange=yrange,       $
                             yticks=yticks,       yminor=yminor,       $
                             xticks=xticks,       xminor=xminor,       $
                             xstyle=xstyle,       ystyle=ystyle,       $
                             xtickname=xtickname, ytickname=ytickname, $
                             color=color,         thick=thick,         $
                             /nodata

   ; 1st model
   oplot,  vert_mean1, alt2, color=color_m1, thick=thick

   ; 2nd model
   oplot,  vert_mean2, alt2, color=color_m2, thick=thick

   if ( nVersions eq 3 ) then begin
   ; 3rd model
   oplot,  vert_mean3, alt2, color=color_m3, thick=thick
   endif

   ; Overplot the data with error bars
   oplot,    Data[1, *], Data[0, *],  color=1,  thick=thick
   oplot,    Data[3, *], Data[0, *],  color=1,  thick=thick, linestyle=2
   errorbar, Data[1, *], Data[0, *], Data[2, *], /X,  color=1,  thick=thick
   xyouts, 400, 11, Month[Mon-1],   color=1, charsize=1

   ; x-axis labels
   xyouts, 350, -1, '350', color=1, alignment=0.5, charsize=1
   xyouts, 700, -1, '700', color=1, alignment=0.5, charsize=1

   ; Top title (page 2)
   xyouts, 0.5, 1.0, TopTitle, /normal, align=0.5, charsize=1.2, color=1

   Undefine, DataFile
   Undefine, Data
   Undefine, Title
   Undefine, Lat
   Undefine, Lon
   Undefine, Mon
   Undefine, PAN1_box
   Undefine, PAN2_box
   Undefine, PAN3_box
   Undefine, vert_mean1
   Undefine, vert_mean2
   Undefine, vert_mean3


   device, /close

   ; Restore defaults
   !X.OMARGIN   = X_OMARGIN   
   !Y.OMARGIN   = Y_OMARGIN   
   !X.MARGIN    = X_MARGIN    
   !Y.OMARGIN   = Y_MARGIN    
   !P.CHARTHICK = P_CHARTHICK 
   !P.THICK     = P_THICK     
   !X.THICK     = X_THICK     
   !Y.THICK     = Y_THICK 

end

