; $Id: plot_4lev_co_geos_3_models_mozaic.pro,v 1.3 2010/10/04 15:21:23 bmy Exp $
pro plot_4lev_co_geos_3_models_mozaic, pref1, ptop1, dlat1, dlon1, model1, year1, $
                                       pref2, ptop2, dlat2, dlon2, model2, year2, $
                                       pref3, ptop3, dlat3, dlon3, model3, year3, $
                                       title, psname, max_station, filest, $
                                       Do_GCHP

; MODIFICATION HISTORY:
;        bmy, 07 Mar 2005: Now uses GET_PRESSURE_GEOS and GET_SPECIES_GEOS which
;                          can read both GEOS-3 and GEOS-4 met fields.  Also
;                          updated comments and made cosmetic changes.
;    iam,bmy, 07 Mar 2005: This version is for the 0.5 degree resolution data
;                          that comes with the MOZAIC Data set
;        bmy, 11 Jul 2007: Now pass MODELNAMES instead of NALT.  We can get the
;                          number of altitudes from the modelname
;        bmy, 11 Jul 2007: Now use CTM_INDEX to pick the lon & lat indices
;                          corresponding to each station
;        mps, 27 Jan 2017: Update to allow for comparison of 2 versions,
;                          intead of the default 3 versions
;        ewl, 27 Jun 2017: Use Model1 instead of Model2 pressure if Do_GCHP

   ;========================================================================
   ; Initialization
   ;========================================================================

   ; Number of model versions to compare
   if ( Pref3 eq 'None' ) then nVersions = 2 $
                          else nVersions = 3
   
   ; Get MODELINFO structures for the 3 models
   Type1 = CTM_Type( Model1, Res=[ DLon1, DLat1 ] )
   Type2 = CTM_Type( Model2, Res=[ DLon2, DLat2 ] )
   if ( nVersions eq 3 ) then begin
   Type3 = CTM_Type( Model3, Res=[ DLon3, DLat3 ] )
   endif
   
   ; Get GRIDINFO structures for the 3 models
   Grid1 = CTM_Grid( Type1 )
   Grid2 = CTM_Grid( Type2 )
   if ( nVersions eq 3 ) then begin
   Grid3 = CTM_Grid( Type3 )
   endif
   
   ; Number of altitudes
   NAlt1 = Grid1.LMX
   NAlt2 = Grid2.LMX
   if ( nVersions eq 3 ) then begin
   NAlt3 = Grid3.LMX
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
   !X.OMARGIN=[12,8] 
   !Y.OMARGIN=[10,8]
   !X.MARGIN=[0,0]
   !Y.MARGIN=[0,0]
   !P.CHARTHICK=4.0
   !P.THICK=4.0
   !X.THICK=4
   !Y.THICK=4

   ; Use Postscript font
   !P.FONT=0

   ; now set up plotting parameters
   nrow=4
   ncol=4
   !P.Multi = [0,nrow,ncol,0,1]

   Species='CO'

   mmonth = strarr(12)
   mmonth=['Jan','Feb','Mar','Apr','May','Jun',$
           'Jul','Aug','Sep','Oct','Nov','Dec']

   std_press=[891,708,482,304]
   
   open_device, olddevice,/ps,/color,filename=psname

   ;========================================================================
   ; --- read station & indice ---
   ;========================================================================
   name_sta=''
   num_sta=''
   pref_sta=''
   
   num_station=strarr(max_station)
   name_station=strarr(max_station)
   pref_station=strarr(max_station)
   lat_station=fltarr(max_station)
   lon_station=fltarr(max_station)
   lat_station_1=fltarr(max_station)
   lon_station_1=fltarr(max_station)

   ; Now read in information about stations
   openr, usta, filest, /get_lun
   for i=0,max_station-1 do begin
      readf,usta, pref_sta,                  $
         name_sta, lat_sta,         $
         lon_sta,num_sta,         $
   ; ccc 10/01/10  change format for updated station list
   ;      format='(2x,a3,9x,a16,1x,f6.2,3x,f6.2,3x,a3)'
         format='(a11,2x,a17,1x,f6.2,3x,f6.2,3x,a3)'
      pref_station(i) = pref_sta
      name_station(i) = name_sta
      lat_station(i) = lat_sta
      lon_station(i) = lon_sta
      lat_station_1(i) = round(lat_sta)
      lon_station_1(i) = round(lon_sta)
      num_station(i) = num_sta
      
   endfor

   ;========================================================================
   ; ---  open files ---
   ;========================================================================
   ncount=0

   for k = 1, max_station do begin

      ncount=ncount+1
      kk = k-1 
      ix = k
      file=''

      if lon_station_1(kk) gt 180 then lon_station_1(kk)=lon_station_1(kk)-360
      if lon_station(kk) gt 180 then lon_station(kk)=lon_station(kk)-360

      name_sonde='data/co.prof.for.gmi/co.prof.'+ num_station(kk)+'.0.5'

      ;=================================================================
      ; Read data from 1st model
      ;=================================================================

      ; Data array
      out=fltarr(12,4)

      ; Loop over months
      for i=0,11 do begin

         ; Month name
         mn=strtrim(String(fix(i+1)),2)
         if (strlen(mn) eq 1) then begin
            
            mn='0'+mn
         endif
         name=pref1+'GEOSChem.SpeciesConc.'+year1+mn+'01_0000z.nc4'
         name_P=pref1+'GEOSChem.LevelEdgeDiags.'+year1+mn+'01_0000z.nc4'

         ; Get CO & pressure
         Data     = Get_Species_Geos( name, Date=Date, Species='SpeciesConcVV_CO' )
         Pressure = Get_Pressure_Geos( name_P, PTOP=PTOP1 )

         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type1, IndLon, IndLat,  $
            Center=[ Lat_Station[KK], Lon_Station[KK] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1

         ; Define stuff
         CO_box       = Data[Indlon,Indlat,*]
         Pressure_box = Pressure[Indlon,Indlat,*]
         Nalt         = nalt1
         CO           = fltarr(Nalt)
         Pres         = fltarr(Nalt)
         
         for j=0,Nalt-1 do begin
            CO[j]=mean(CO_box[*,*,j])
            Pres[j]=mean( Pressure_box [*,*,j]) 
         endfor
         
         ; save in array
         out[i,*]=interpol( CO, alog10(Pres), alog10(std_press) )
         

         ; Undefine temp arrays
         UnDefine, CO_Box
         UnDefine, Pressure_Box
         UnDefine, CO
         UnDefine, Pres
         UnDefine, Data
         UnDefine, Pressure
      endfor

      ;=================================================================
      ; Read data from second model
      ;=================================================================

      ; Data array
      out2=fltarr(12,4)

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

         ; Get CO & pressure
         Data     = Get_Species_Geos( name, Date=Date, Species='SpeciesConcVV_CO' )
         Pressure = Get_Pressure_Geos( name_P, PTOP=PTOP2, Lat=Lat, Lon=Lon )
         
         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type2, IndLon, IndLat,  $
            Center=[ Lat_Station[KK], Lon_Station[KK] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1

         ; Define stuff
         CO_box       = Data[Indlon,Indlat,*]
         Pressure_box = Pressure[Indlon,Indlat,*]
         Nalt         = nalt2
         CO           = fltarr(Nalt)
         Pres         = fltarr(Nalt)
         
         for j=0,Nalt-1 do begin
            CO[j]=mean(CO_box[*,*,j])
            Pres[j]=mean( Pressure_box [*,*,j]) 
         endfor         

         ; Save in array
         out2[i,*]=interpol( CO, alog10(Pres), alog10(std_press) )
         
         ; Undefine temp arrays
         UnDefine, CO_Box
         UnDefine, Pressure_Box
         UnDefine, CO
         UnDefine, Pres
         UnDefine, Data
         UnDefine, Pressure
      endfor

      if ( nVersions eq 3 ) then begin

      ;===================================================================
      ; Read data from 3rd model
      ;===================================================================

      ; Data array
      out3=fltarr(12,4)
      
      ; Loop over months
      for i=0,11 do begin

         ; Month name
         mn=strtrim(String(fix(i+1)),2)
         if (strlen(mn) eq 1) then begin
            mn='0'+mn
         endif
         name=pref3+'GEOSChem.SpeciesConc.'+year3+mn+'01_0000z.nc4'
         name_P=pref3+'GEOSChem.LevelEdgeDiags.'+year3+mn+'01_0000z.nc4'
         
         ; Get CO & pressure
         Data     = Get_Species_Geos( name, Date=Date, Species='SpeciesConcVV_CO' )
         Pressure = Get_Pressure_Geos( name_P, PTOP=PTOP3 )

         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type3, IndLon, IndLat,  $
            Center=[ Lat_Station[KK], Lon_Station[KK] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1

         ; Define stuff
         CO_box       = Data[Indlon,Indlat,*]
         Pressure_box = Pressure[Indlon,Indlat,*]
         Nalt         = nalt3
         CO           = fltarr(Nalt)
         Pres         = fltarr(Nalt)
         
         for j=0,Nalt-1 do begin
            CO[j]=mean(CO_box[*,*,j])
            Pres[j]=mean( Pressure_box [*,*,j]) 
         endfor

         ; Save in array
         out3[i,*]=interpol( CO, alog10(Pres), alog10(std_press) )
         
         ; Undefine temp arrays
         UnDefine, CO_Box
         UnDefine, Pressure_Box
         UnDefine, CO
         UnDefine, Pres
         UnDefine, Data
         UnDefine, Pressure
      endfor

      endif
      
      ;=====================================================================
      ; Now read sonde means and standard deviations
      ;=====================================================================
      read_sondes_co_4lev_mozaic_0_5,name_sonde, sonde, std_sonde

      ;==================================================================
      ; First Plot Panel
      ;==================================================================
      ltitle=''
      ltitle = strtrim(name_station(kk),2)+$
         ' ('+strtrim(string(fix(lat_station_1(kk))),1)+$
         ' ,'+strtrim(string(fix(lon_station(kk))),1)+' )'
      
      ; -- plot observed data --
      loval=50 
      highval=200
      
      plot, findgen(12)+1, sonde[*,3], xstyle=1,ystyle=5,$
         title=ltitle,linestyle=0,psym=-5,symsize=0.6, $
         xticks=13, min_val=-900, xrange=[0,13],yrange=[loval,highval],$
         charsize=1.5,color=1,$
         xtickname=[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']

      ; Now plot standard deviations
      for w = 0, 11 do begin
         errbar = [sonde[w,3]-std_sonde[w,3],sonde[w,3]+std_sonde[w,3] ]
         oplot,  [w+1,w+1],errbar,$
            linestyle=0,color=1
      endfor

      ; 1st model
      oplot, findgen(12)+1,out[0:11,3],linestyle=0,color=2
      oplot, findgen(12)+1,out[0:11,3],linestyle=0,psym=-5,$
         symsize=0.6,color=2   
    
      ; 2nd model
      oplot, findgen(12)+1,out2[0:11,3],linestyle=0,color=3
      oplot, findgen(12)+1,out2[0:11,3],linestyle=0,psym=-5,$
         symsize=0.6,color=3 
 
      if ( nVersions eq 3 ) then begin
      ; 3rd model
      oplot, findgen(12)+1,out3[0:11,3],linestyle=0,color=4
      oplot, findgen(12)+1,out3[0:11,3],linestyle=0,psym=-5,$
         symsize=0.6,color=4
      endif
      
      ; Now plot standard deviations
      xyouts,1,0.9*highval, '305 hPa', charsize = 1,color=1

      yt1=['50','100','150','200']
      if lat_station(kk) ge 51 then begin
         yt1=[' 0 ','75','150','225','300']
      endif

      ; Axes
; ccc, 10/01/10 Simplify the if statement and make it more general.
;               Use loval, highval variables to define the yrange for
;               axis.
;
;      if ncount eq 1 or ncount eq 5 or ncount eq 9 or ncount eq 13 or ncount eq 17 or ncount eq 21 or ncount eq 25 or ncount eq 29 then begin  
;         axis, 0, yaxis=0, yticks=4, yrange=[0,highval],$
;            ytickv=findgen(4)*highval/3, ytickname=yt1,$
;            charsize=1.5,/ystyle,color=1
;      endif
;
;      if ncount ne 1 and ncount ne 5 and ncount ne 9 and ncount ne 13 and ncount ne 17 then begin  
;         axis, 0, yaxis=0, yticks=4, yrange=[0,highval],$
;            ytickv=findgen(4)*highval/3, ytickname=[' ',' ',' ',' ',' ',' '],$
;            charsize=1.5,/ystyle,color=1
;      endif
;
;      if ncount ne 1 and ncount ne 5 and ncount ne 9 and ncount ne 13 and ncount ne 17 then begin  
;         axis, 0, yaxis=0, yticks=6, yrange=[50,200],$
;            ytickv=[50,75,100,125,150], ytickname=[' ',' ',' ',' ',' ',' '],$
;            charsize=1.5,/ystyle,color=1
;      endif
;
;      axis,13, yaxis=1,yticks=6, yrange=[50,200],$
;         ytickv=[50,100,150,200], ytickname=[' ',' ',' ',' ',' ',' '],$
;         charsize=1.5,/ystyle,color=1
      

      if ( ncount mod 4 ) eq 1 then begin  
         axis, 0, yaxis=0, yticks=4, yrange=[0,highval],$
            ytickv=findgen(4)*highval/3, ytickname=yt1,$
            charsize=1.5,/ystyle,color=1
      endif

; ccc 10/01/10  I don't think it's needed anymore since it's
; overwritten by the next call to axis.
;
;      if ( ncount mod 4 ) ne 1 then begin  
;         axis, 0, yaxis=0, yticks=4, yrange=[loval,highval],$
;            ytickv=findgen(4)*highval/3, ytickname=[' ',' ',' ',' ',' ',' '],$
;            charsize=1.5,/ystyle,color=1
;      endif

      if ( ncount mod 4) ne 1 then begin  
         axis, 0, yaxis=0, yticks=6, yrange=[loval,highval],$
            ytickv=[50,75,100,125,150], ytickname=[' ',' ',' ',' ',' ',' '],$
            charsize=1.5,/ystyle,color=1
      endif

      axis,13, yaxis=1,yticks=6, yrange=[loval,highval],$
         ytickv=[50,75,100,125,150,200], ytickname=[' ',' ',' ',' ',' ',' '],$
         charsize=1.5,/ystyle,color=1
      
      ;==================================================================
      ; Second plot panel
      ;==================================================================
      loval=50 
      highval=200

      plot, findgen(12)+1, sonde[*,2], xstyle=1,ystyle=5,$
         linestyle=0,psym=-5,symsize=0.6, $
         xticks=13, min_val=-900, xrange=[0,13],yrange=[loval,highval],$
         charsize=1.5,color=1,$
         xtickname=[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']
    
      ; Now plot standard deviations
      for w = 0, 11 do begin
         errbar = [sonde[w,2]-std_sonde[w,2],sonde[w,2]+std_sonde[w,2] ]
         oplot,  [w+1,w+1],errbar,$
          linestyle=0,color=1
      endfor

      ; 1st model
      oplot, findgen(12)+1,out[0:11,2],linestyle=0,color=2
      oplot, findgen(12)+1,out[0:11,2],linestyle=0,psym=-5,$
         symsize=0.6,color=2   
    
      ; 2nd model
      oplot, findgen(12)+1,out2[0:11,2],linestyle=0,color=3
      oplot, findgen(12)+1,out2[0:11,2],linestyle=0,psym=-5,$
         symsize=0.6,color=3
  
      if ( nVersions eq 3 ) then begin
      ; 3rd model
      oplot, findgen(12)+1,out3[0:11,2],linestyle=0,color=4
      oplot, findgen(12)+1,out3[0:11,2],linestyle=0,psym=-5,$
         symsize=0.6,color=4
      endif
      
      ; Now plot standard deviations
      xyouts,1,0.9*highval, '480 hPa', charsize = 1,color=1

      yt1=['50','100','150','200']
      if lat_station(kk) ge 51 then begin
         yt1=[' 0 ','75','150','225','300']
      endif
    
      ; Axes
; ccc, 10/01/10 Simplify the if statement and make it more general.
;               Use loval, highval variables to define the yrange for
;               axis.
;
;      if ncount eq 1 or ncount eq 5 or ncount eq 9 or ncount eq 13 or ncount eq 17 or ncount eq 21 or ncount eq 25 or ncount eq 29 then begin  
;         axis, 0, yaxis=0, yticks=4, yrange=[0,highval],$
;            ytickv=findgen(4)*highval/3, ytickname=yt1,$
;            charsize=1.5,/ystyle,color=1
;      endif
;    
;      if ncount ne 1 and ncount ne 5 and ncount ne 9 and ncount ne 13 and ncount ne 17 then begin  
;         axis, 0, yaxis=0, yticks=4, yrange=[0,highval],$
;            ytickv=findgen(4)*highval/3, ytickname=[' ',' ',' ',' ',' ',' '],$
;            charsize=1.5,/ystyle,color=1
;      endif
;    
;      if ncount ne 1 and ncount ne 5 and ncount ne 9 and ncount ne 13 and ncount ne 17 then begin  
;         axis, 0, yaxis=0, yticks=6, yrange=[50,200],$
;            ytickv=[50,75,100,125,150], ytickname=[' ',' ',' ',' ',' ',' '],$
;            charsize=1.5,/ystyle,color=1
;      endif
;
;      axis,13, yaxis=1,yticks=6, yrange=[50,200],$
;         ytickv=[50,100,150,200], ytickname=[' ',' ',' ',' ',' ',' '],$
;         charsize=1.5,/ystyle,color=1
      
      if ( ncount mod 4 ) eq 1 then begin  
         axis, 0, yaxis=0, yticks=4, yrange=[0,highval],$
            ytickv=findgen(4)*highval/3, ytickname=yt1,$
            charsize=1.5,/ystyle,color=1
      endif
    
;ccc, 10/01/10
;      if ( ncount mod 4 ) ne 1 then begin  
;         axis, 0, yaxis=0, yticks=4, yrange=[loval,highval],$
;            ytickv=findgen(4)*highval/3, ytickname=[' ',' ',' ',' ',' ',' '],$
;            charsize=1.5,/ystyle,color=1
;      endif
    
      if ( ncount mod 4 ) ne 1 then begin  
         axis, 0, yaxis=0, yticks=6, yrange=[loval,highval],$
            ytickv=[50,75,100,125,150], ytickname=[' ',' ',' ',' ',' ',' '],$
            charsize=1.5,/ystyle,color=1
      endif

      axis,13, yaxis=1,yticks=6, yrange=[loval,highval],$
         ytickv=[50,75,100,125,150,200], ytickname=[' ',' ',' ',' ',' ',' '],$
         charsize=1.5,/ystyle,color=1
      
      ;=====================================================================
      ; Third plot panel
      ;=====================================================================
      loval=50 
      highval=200
    
      plot, findgen(12)+1, sonde[*,1], xstyle=1,ystyle=5,$
         linestyle=0,psym=-5,symsize=0.6, $
         xticks=13, min_val=-900, xrange=[0,13],yrange=[loval,highval],$
         charsize=1.5,color=1,$
         xtickname=[' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ',' ']

      ; Now plot standard deviations
      for w = 0, 11 do begin
         errbar = [sonde[w,1]-std_sonde[w,1],sonde[w,1]+std_sonde[w,1] ]
         oplot,  [w+1,w+1],errbar,$
            linestyle=0,color=1
      endfor

      ; 1st model
      oplot, findgen(12)+1,out[0:11,1],linestyle=0,color=2
      oplot, findgen(12)+1,out[0:11,1],linestyle=0,psym=-5,$
         symsize=0.6,color=2   
      
      ; 2nd model
      oplot, findgen(12)+1,out2[0:11,1],linestyle=0,color=3
      oplot, findgen(12)+1,out2[0:11,1],linestyle=0,psym=-5,$
         symsize=0.6,color=3
    
      if ( nVersions eq 3 ) then begin
      ; 3rd model
      oplot, findgen(12)+1,out3[0:11,1],linestyle=0,color=4
      oplot, findgen(12)+1,out3[0:11,1],linestyle=0,psym=-5,$
         symsize=0.6,color=4  
      endif

      xyouts, 1, 0.9*highval, '710 hPa', charsize = 1, color = 1
         
      ; Axes
; ccc, 10/01/10 Simplify the if statement and make it more general.
;               Use loval, highval variables to define the yrange for
;               axis.
;
;      if ncount eq 1 or ncount eq 5 or ncount eq 9 or ncount eq 13 or ncount eq 17 or ncount eq 21 or ncount eq 25 or ncount eq 29 then begin  
;         axis, 0, yaxis=0, yticks=4, yrange=[50,200],$
;            ytickv=[50,100,150,200], ytickname=['50','100','150','200'],$
;            charsize=1.5,/ystyle,color=1
;      endif
;
;      if ncount ne 1 and ncount ne 5 and ncount ne 9 and ncount ne 13 and ncount ne 17 then begin  
;         axis, 0, yaxis=0, yticks=4, yrange=[50,200],$
;            ytickv=[50,100,150,200], ytickname=[' ',' ',' ',' ',' '],$
;            charsize=1.5,/ystyle,color=1
;      endif
;
;      axis,13, yaxis=1,yticks=4, yrange=[50,200],$
;         ytickv=[50,100,150,200], ytickname=[' ',' ',' ',' ',' '],$
;         charsize=1.5,/ystyle,color=1
    
      if ( ncount mod 4 ) eq 1 then begin  
         axis, 0, yaxis=0, yticks=4, yrange=[loval,highval],$
            ytickv=[50,100,150,200], ytickname=['50','100','150','200'],$
            charsize=1.5,/ystyle,color=1
      endif

      if ( ncount mod 4 ) ne 1 then begin  
         axis, 0, yaxis=0, yticks=4, yrange=[loval,highval],$
            ytickv=[50,100,150,200], ytickname=[' ',' ',' ',' ',' '],$
            charsize=1.5,/ystyle,color=1
      endif

      axis,13, yaxis=1,yticks=4, yrange=[loval,highval],$
         ytickv=[50,100,150,200], ytickname=[' ',' ',' ',' ',' '],$
         charsize=1.5,/ystyle,color=1
    
      ;=====================================================================
      ; Fourth plot panel
      ;=====================================================================
; ccc, 10/01/10  Extend yrange for 890 hPa plot to lower values.
;      loval=100 
      loval=50 
      highval=300
    
      plot, findgen(12)+1, sonde[*,0], xstyle=1,ystyle=5,$
         linestyle=0,psym=-5,symsize=0.6, $
         xticks=13, min_val=-900, xrange=[0,13],yrange=[loval,highval],$
         charsize=1.5,color=1,$
         xtickname=[' ','J','F','M','A','M','J','J','A','S','O','N','D',' ']
 
      ; Now plot standard deviations
      for w = 0, 11 do begin
         errbar = [sonde[w,0]-std_sonde[w,0],sonde[w,0]+std_sonde[w,0] ]
         oplot,  [w+1,w+1],errbar,$
            linestyle=0,color=1
      endfor
    
      ; 1st model
      oplot, findgen(12)+1,out[0:11,0],linestyle=0,color=2
      oplot, findgen(12)+1,out[0:11,0],linestyle=0,psym=-5,$
         symsize=0.6,color=2   
    
      ; 2nd model
      oplot, findgen(12)+1,out2[0:11,0],linestyle=0,color=3
      oplot, findgen(12)+1,out2[0:11,0],linestyle=0,psym=-5,$
         symsize=0.6,color=3

      if ( nVersions eq 3 ) then begin
      ; 3rd model
      oplot, findgen(12)+1,out3[0:11,0],linestyle=0,color=4
      oplot, findgen(12)+1,out3[0:11,0],linestyle=0,psym=-5,$
         symsize=0.6,color=4  
      endif
      
      xyouts,1,0.9*highval, '890 hPa', charsize = 1,color=1    
      xyouts, 0.04, 0.5, 'CO (ppb)', /normal, align=0.5, orientation=90, $
         charsize=1.2, color=1
      
      ; Axes
; ccc, 10/01/10 Simplify the if statement and make it more general.
;               Use loval, highval variables to define the yrange for
;               axis.
;               
;               Adapt yaxis ticks to a range of [50, 300]
;
;     if ncount eq 1 or ncount eq 5 or ncount eq 9 or ncount eq 13 or ncount eq 17 or ncount eq 21 or ncount eq 25 or ncount eq 29 then begin  
;        axis, 0, yaxis=0, yticks=4, yrange=[100,300],$
;           ytickv=[100,150,200,250,300], $
;           ytickname=['100','150','200','250','300'],$
;           charsize=1.5,/ystyle,color=1
;     endif
;
;     if ncount ne 1 and ncount ne 5 and ncount ne 9 and ncount ne 13 and ncount ne 17 then begin  
;        axis, 0, yaxis=0, yticks=4, yrange=[100,300],$
;           ytickv=[100,150,200,250,300], ytickname=[' ',' ',' ',' ',' '],$
;           charsize=1.5,/ystyle,color=1
;     endif
;
;     axis,13, yaxis=1,yticks=4, yrange=[100,300],$
;        ytickv=[100,150,200,250,300], ytickname=[' ',' ',' ',' ',' '],$
;        charsize=1.5,/ystyle,color=1
      
      if ( ncount mod 4 ) eq 1 then begin  
         axis, 0, yaxis=0, yticks=3, yrange=[loval,highval],$
            ytickv=[50,100,200, 300],       $
            ytickname=['50', '100', '200', '300'], $
            charsize=1.5,/ystyle,color=1
      endif

      if ( ncount mod 4 ) ne 1 then begin  
         axis, 0, yaxis=0, yticks=2, yrange=[loval,highval],$
            ytickv=[100,200,300], ytickname=[' ',' ',' '],$
            charsize=1.5,/ystyle,color=1
      endif

      axis,13, yaxis=1,yticks=2, yrange=[loval,highval],$
         ytickv=[100,200,300], ytickname=[' ',' ',' '],$
         charsize=1.5,/ystyle,color=1

      ; Print overall title.
      xyouts, 0.5, 0.96,title, /normal, align=0.5, charsize=1.2,color=1
      
   endfor

   ; Cleanup and quit
   close_device,/TIMESTAMP
   
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


