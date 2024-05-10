; $Id: plot_surface_o3_geos_3_models.pro,v 1.2 2008/03/11 17:47:25 bmy Exp $
pro plot_surface_o3_geos_3_models, pref1, ptop1,  dlat1, dlon1, model1, year1, $
                                   pref2, ptop2,  dlat2, dlon2, model2, year2, $
                                   pref3, ptop3,  dlat3, dlon3, model3, year3, $
                                   title, psname, Do_GCHP

; PURPOSE:
;        For a given set of stations compares O3 surface data from cmdl (black
;        solid line) with surface data from 3 models plotted with linestyles 1 
;        to 3 and colors red, green and blue correspondently
;
; MODIFICATION HISTORY:
;        bmy, 08 Mar 2005: Now uses GET_PRESSURE_GEOS and GET_SPECIES_GEOS which
;                          can read both GEOS-3 and GEOS-4 met fields.  Also
;                          updated comments and made cosmetic changes.
;        bmy, 11 Jul 2007: Now use CTM_INDEX to pick the lon & lat indices
;                          corresponding to each station
;        bmy, 11 Dec 2007: Now use correct station pressures as taken from the
;                          file "input/cmdl.sigma.levels"
;        mps, 27 Jan 2017: Update to allow for comparison of 2 versions,
;                          intead of the default 3 versions
;        ewl, 27 Jun 2017: Use Model1 instead of Model2 pressure if Do_GCHP

   ;=======================================================================
   ; Initialization
   ;=======================================================================

   ; Number of model versions to compare
   if ( Pref3 eq 'None' ) then nVersions = 2 $
                          else nVersions = 3
   
   ; Get MODELINFO structures for the 3 models
   Type1 = CTM_Type( Model1, Res=[ DLon1, DLat1 ] )
   Type2 = CTM_Type( Model2, Res=[ DLon2, DLat2 ] )
   if ( nVersions eq 3 ) then begin
   Type3 = CTM_Type( Model3, Res = [ DLon3, DLat3 ] )
   endif
   
   ; Get defaults (bmy, 6/7/11)
   X_OMARGIN   = !X.OMARGIN
   Y_OMARGIN   = !Y.OMARGIN
   X_MARGIN    = !X.MARGIN
   Y_MARGIN    = !Y.MARGIN
   P_CHARTHICK = !P.CHARTHICK
   P_THICK     = !P.THICK
   X_THICK     = !X.THICK
   Y_THICK     = !Y.THICK

   ; Plot parameters
   !X.OMARGIN=[8,6] 
   !Y.OMARGIN=[6,6]
   !X.THICK=4
   !Y.THICK=4
   !P.CHARTHICK=2.5
   !P.THICK=2.5

   ; Use Postscript font
   !P.FONT=0

   Species='O3'

   ;input data about stations
   filest='data/netCDF/Sites.ground.'+Species+'.remote.1' 

   ; Open file with stations
   openr, usta, filest, /get_lun
   iname_sta=''
   ititle_sta=''
   ipref_sta=''

   mmonth = strarr(12)
   mmonth=['Jan','Feb','Mar','Apr','May','Jun',$
           'Jul','Aug','Sep','Oct','Nov','Dec']

   ; Open PS device
   open_device, olddevice,/ps,/color,filename=psname ;color plot
   
   ; Specify directory with surface data 
   pre = 'data/surface.ozone/'

   ;====================================================================
   ; --- read station & indice ---
   ;====================================================================

   ; Set max_sta parameter
   max_sta=19                   ;real

   ; Read in information about stations to be plotted - 
   ; 3-letter name(capital), station name, latitude and longitude

   name_sta  = strarr(max_sta)
   lon_sta   = fltarr(max_sta)
   lat_sta   = fltarr(max_sta)
   lon_sta_1 = fltarr(max_sta)
   lat_sta_1 = fltarr(max_sta)
   title_sta = strarr(max_sta)
   pref_sta  = strarr(max_sta)

   for i=0,max_sta-1 do begin
      readf,usta, iname_sta,ititle_sta,  ilat, ilon, ipref_sta, $
         format='(2x,a3,7x,a12,5x,f6.2,3x,f6.2,4x,a3)'
      name_sta(i)  = iname_sta
      lon_sta_1(i) = round(ilon)
      lat_sta_1(i) = round(ilat)
      lon_sta(i)   = ilon
      lat_sta(i)   = ilat
      title_sta(i) = ititle_sta
      pref_sta[i]  = ipref_sta
   endfor

   nrow=4
   ncol=4
   !P.Multi = [0,nrow,ncol,1,0]
   
   ;==================================================================== 
   ; ---  open files ---
   ;====================================================================
   ncount=0

   ; --- loop for stations ---
   for k = 1, max_sta do begin

      ncount=ncount+1
    kk = k-1 
    ix = k
    file=''

    file=pre+'surf'+pref_sta(kk)+'.dat'

    ilun = k+50
    openr,ilun,file

    maxd = 12
    o3mean   = fltarr(maxd)
    o3mon = fltarr(maxd)

    ; Loop over months
    for i=0,11 do begin
       readf,ilun,                                             $
          io3mon,io3mean
       o3mon(i)    = io3mon
       o3mean(i)   = io3mean
    endfor
    close, ilun

    ; Put longitude in (-180,180) range
    if lon_sta_1(kk) gt 180 then lon_sta_1(kk)=lon_sta_1(kk)-360
    if lon_sta(kk) gt 180 then lon_sta(kk)=lon_sta(kk)-360

    ; Create station title
    ltitle=''
    ltitle = strtrim(title_sta(kk),2)+$
       ' ('+strtrim(string(fix(lat_sta_1(kk))),1)+$
       ' ,'+strtrim(string(fix(lon_sta_1(kk))),1)+' )'
    
    ;======================================================================
    ; Read data from first model 
    ;======================================================================
    out=fltarr(12)

    ; Loop over months
    for i=0,11 do begin
       
       ; Month name
       mn=strtrim(String(fix(i+1)),2)
       if (strlen(mn) eq 1) then begin
          mn='0'+mn
       endif
       name=pref1+'GEOSChem.SpeciesConc.'+year1+mn+'01_0000z.nc4'
       name_P=pref1+'GEOSChem.LevelEdgeDiags.'+year1+mn+'01_0000z.nc4'

       ; Get O3 & pressure
       O3       = Get_Species_Geos( name, Date=Date, Species='SpeciesConcVV_O3' )
       Pressure = Get_Pressure_Geos( name_P, PTOP=PTOP1 )

       ; Get the lon & lat indices corresponding to this station
       CTM_Index, Type1, IndLon, IndLat,  $
          Center=[ Lat_Sta[KK], Lon_Sta[KK] ], /Non

       ; Convert from F90 to IDL notation
       IndLon = IndLon - 1
       IndLat = IndLat - 1

       O3_box  = O3[Indlon,Indlat,0]
       out[i]  = mean(O3_box)

       ; NOTE: Use INTERPOL to find the level closest in pressure to
       ;       the given station's surface pressure.  Update the pressures
       ;       according to the cmdl.sigma.levels document (bmy, 12/11/07)
       
       if ( name_sta(kk) eq 'NWR') then begin
          out[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(660.))

       endif else if (name_sta(kk) eq 'IZO') then begin
          out[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(760.))

       endif else if (name_sta(kk) eq 'MLO') then begin
          out[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(670.))

       endif else if (name_sta(kk) eq 'SPO') then begin
          out[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(700.))

       endif else if (name_sta(kk) eq 'LEF') then begin
          out[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(913.))

       endif else if (name_sta(kk) eq 'UUM') then begin
          out[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(908.))

       endif else if (name_sta(kk) eq 'UTA') then begin
          out[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(865.))

       endif else if (name_sta(kk) eq 'CUI') then begin
          out[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(900.))

       endif else if (name_sta(kk) eq 'WLG') then begin
          out[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(640.))
       endif

    endfor

    ;======================================================================
    ; Read data from 2nd model 
    ;======================================================================
    out2=fltarr(12)

    ; Loop over months
    for i=0,11 do begin

       ; Month name
       mn=strtrim(String(fix(i+1)),2)
       if (strlen(mn) eq 1) then begin
          mn='0'+mn
       endif
       name=pref2+'GEOSChem.SpeciesConc.'+year2+mn+'01_0000z.nc4'
       if ( Do_GCHP ) then begin
          name_P=pref1+'GEOSChem.LevelEdgeDiags.'+year2+mn+'01_0000z.nc4'
       endif else begin
          name_P=pref2+'GEOSChem.LevelEdgeDiags.'+year2+mn+'01_0000z.nc4'
       endelse
       
       ; Get O3 and pressure
       O3       = Get_Species_Geos( name, Date=Date, Species='SpeciesConcVV_O3' )
       Pressure = Get_Pressure_Geos( name_P, PTOP=PTOP2 )

       ; Get the lon & lat indices corresponding to this station
       CTM_Index, Type2, IndLon, IndLat,  $
          Center=[ Lat_Sta[KK], Lon_Sta[KK] ], /Non

       ; Convert from F90 to IDL notation
       IndLon = IndLon - 1
       IndLat = IndLat - 1

       O3_box  = O3[Indlon,Indlat,0]
       out2[i] = mean(O3_box)

       ; NOTE: Use INTERPOL to find the level closest in pressure to
       ;       the given station's surface pressure.  Update the pressures
       ;       according to the cmdl.sigma.levels document (bmy, 12/11/07)
       
       if ( name_sta(kk) eq 'NWR') then begin
          out2[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(660.))

       endif else if (name_sta(kk) eq 'IZO') then begin
          out2[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(760.))

       endif else if (name_sta(kk) eq 'MLO') then begin
          out2[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(670.))

       endif else if (name_sta(kk) eq 'SPO') then begin
          out2[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(700.))

       endif else if (name_sta(kk) eq 'LEF') then begin
          out2[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(913.))

       endif else if (name_sta(kk) eq 'UUM') then begin
          out2[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(908.))

       endif else if (name_sta(kk) eq 'UTA') then begin
          out2[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(865.))

       endif else if (name_sta(kk) eq 'CUI') then begin
          out2[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(900.))

       endif else if (name_sta(kk) eq 'WLG') then begin
          out2[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(640.))
       endif

    endfor

    if ( nVersions eq 3 ) then begin

    ;======================================================================
    ; Read data from 3rd model 
    ;======================================================================
    out3=fltarr(12)

    for i=0,11 do begin

       ; Month name
       mn=strtrim(String(fix(i+1)),2)
       if (strlen(mn) eq 1) then begin
          mn='0'+mn
       endif
       name=pref3+'GEOSChem.SpeciesConc.'+year3+mn+'01_0000z.nc4'
       name_P=pref3+'GEOSChem.LevelEdgeDiags.'+year3+mn+'01_0000z.nc4'

       ; O3 & pressure
       O3       = Get_Species_Geos( name, Date=Date, Species='SpeciesConcVV_O3' )
       Pressure = Get_Pressure_Geos( name_P, PTOP=PTOP3 )

       ; Get the lon & lat indices corresponding to this station
       CTM_Index, Type3, IndLon, IndLat,  $
          Center=[ Lat_Sta[KK], Lon_Sta[KK] ], /Non

       ; Convert from F90 to IDL notation
       IndLon = IndLon - 1
       IndLat = IndLat - 1

       O3_box  = O3[Indlon,Indlat,0]
       out3[i] = mean(O3_box)

       ; NOTE: Use INTERPOL to find the level closest in pressure to
       ;       the given station's surface pressure.  Update the pressures
       ;       according to the cmdl.sigma.levels document (bmy, 12/11/07)
       
       if ( name_sta(kk) eq 'NWR') then begin
          out3[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(660.))

       endif else if (name_sta(kk) eq 'IZO') then begin
          out3[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(760.))

       endif else if (name_sta(kk) eq 'MLO') then begin
          out3[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(670.))

       endif else if (name_sta(kk) eq 'SPO') then begin
          out3[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(700.))

       endif else if (name_sta(kk) eq 'LEF') then begin
          out3[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(913.))

       endif else if (name_sta(kk) eq 'UUM') then begin
          out3[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(908.))

       endif else if (name_sta(kk) eq 'UTA') then begin
          out3[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(865.))

       endif else if (name_sta(kk) eq 'CUI') then begin
          out3[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(900.))

       endif else if (name_sta(kk) eq 'WLG') then begin
          out3[i]=interpol(O3[Indlon,Indlat,*],$
                          -alog10(Pressure[Indlon,Indlat,*]),-alog10(640.))
       endif
       
    endfor

    endif
    
    ;======================================================================
    ; Plot the data
    ;======================================================================

    ; Define the range for y axis
    loval=0 
    highval=60

    if  max([o3mean,out]) gt 60 then begin highval=80
    endif

    ; -- plot observed data --
    plot, findgen(12)+1, o3mean, xstyle=1,ystyle=1,$
       title=ltitle,linestyle=0,psym=-5,symsize=0.6, $
       xticks=13, min_val=-900, xrange=[0,13],yrange=[loval,highval],$
       charsize=1.5, xmargin=[3,2], ymargin=[3,1],color=1,$
         xtickname=[' ','J','F','M','A','M','J','J','A','S','O','N','D',' ']
    
    ; 1st model
    oplot, findgen(12)+1,out,linestyle=1,color=2
    oplot, findgen(12)+1,out,linestyle=1,psym=2,symsize=0.3,color=2   

    ; 2nd model
    oplot, findgen(12)+1,out2,linestyle=2,color=3
    oplot, findgen(12)+1,out2,linestyle=2,psym=2,symsize=0.3,color=3   

    if ( nVersions eq 3 ) then begin
    ; 3rd model
    oplot, findgen(12)+1,out3,linestyle=3,color=4
    oplot, findgen(12)+1,out3,linestyle=3,psym=2,symsize=0.3,color=4   
    endif
    
    xyouts, 0.04, 0.5, 'O3 (ppb)', /normal, align=0.5, orientation=90, $
       charsize=1.2,color=1
    xyouts, 0.5, 0.96,title, /normal, align=0.5, charsize=1.2,color=1
 endfor

 ; Cleanup & quit
 close_device, /TIMESTAMP
 
 close, /all

   ; Restore defaults (bmy, 6/7/11)
   !X.OMARGIN   = X_OMARGIN   
   !Y.OMARGIN   = Y_OMARGIN   
   !X.MARGIN    = X_MARGIN    
   !Y.OMARGIN   = Y_MARGIN    
   !P.CHARTHICK = P_CHARTHICK 
   !P.THICK     = P_THICK     
   !X.THICK     = X_THICK     
   !Y.THICK     = Y_THICK 

end


