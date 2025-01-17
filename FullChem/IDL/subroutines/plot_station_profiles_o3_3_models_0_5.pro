; $Id: plot_station_profiles_o3_3_models_0_5.pro,v 1.2 2008/03/11 17:47:25 bmy Exp $
pro plot_station_profiles_o3_3_models_0_5,  $
        mon1,  mon2,  mon3,  mon4,          $
        pref1, ptop1, dlat1, dlon1, model1, year1, $
        pref2, ptop2, dlat2, dlon2, model2, year2, $
        pref3, ptop3, dlat3, dlon3, model3, year3, $
        title, psname, max_station, filest, $
        Do_GCHP

; MODIFICATION HISTORY:
;        bmy, 07 Mar 2005: Now uses GET_PRESSURE_GEOS and GET_SPECIES_GEOS which
;                          can read both GEOS-3 and GEOS-4 met fields.  Also
;                          updated comments and made cosmetic changes.
;        bmy, 11 Jul 2007: Now define size arrays for each model independently
;        bmy, 11 Jul 2007: Now pass MODELNAMES instead of NALT.  We can get the
;                          number of altitudes from the modelname
;        bmy, 11 Jul 2007: Now use CTM_INDEX to pick the lon & lat indices
;                          corresponding to each station.  This is grid-
;                          independent 
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
   
   ; Open postscript file
   open_device, olddevice,/ps,/color,filename=psname,/portrait

   ; Get defaults (bmy, 6/7/11)
   X_OMARGIN   = !X.OMARGIN
   Y_OMARGIN   = !Y.OMARGIN
   X_MARGIN    = !X.MARGIN
   Y_MARGIN    = !Y.MARGIN
   P_CHARTHICK = !P.CHARTHICK
   P_THICK     = !P.THICK
   X_THICK     = !X.THICK
   Y_THICK     = !Y.THICK

   ; Plot settings
   !X.OMARGIN=[12,8] 
   !Y.OMARGIN=[10,8]
   !X.MARGIN=[0,0]
   !Y.MARGIN=[0,0]
   !P.CHARTHICK=4.0
   !P.THICK=4.0
   !X.THICK=4.5
   !Y.THICK=4.5

   ; Use Postscript font
   !P.FONT=0

   ; now set up plotting parameters
   nrow=4
   ncol=4
   !P.Multi = [0,nrow,ncol,0,1]

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

   ; Read in input station abbriviation, name, latitude, longitude and number
   openr, usta, filest, /get_lun
   for i=0,max_station-1 do begin
      readf,usta, pref_sta,                  $
         name_sta, lat_sta,         $
         lon_sta,num_sta,         $
         format='(2x,a3,9x,a16,1x,f6.2,3x,f6.2,3x,a3)'
      pref_station(i) = pref_sta
      name_station(i) = name_sta
      lat_station(i) = lat_sta
      lon_station(i) = lon_sta
      lat_station_1(i) = round(lat_sta)
      lon_station_1(i) = round(lon_sta)

      num_station(i) = num_sta
   endfor

   ; for test purpouses only, should be input parameters
   ;mon1=1
   ;mon2=4
   ;mon3=7
   ;mon4=10

   month=strarr(12)
   month=["JAN","FEB","MAR","APR","MAY","JUN",$
          "JUL","AUG","SEP","OCT","NOV","DEC"]

   pressure_sonde=fltarr(26)
   pressure_sonde=[962.35  ,891.25  ,825.41  ,764.42  ,707.94  ,$
                   655.64  ,607.20  ,562.34  ,520.80  ,482.32  ,$
                   446.69  ,413.69  ,383.12  ,354.81  ,328.60  ,$
                   304.32  ,281.84  ,261.02  ,241.73  ,223.87  ,$
                   207.33  ,192.01  ,177.82  ,164.69  ,152.52  ,$
                   141.26  ,130.82  ,121.15  ,112.20  ,103.91  ,$
                    96.23  , 89.12  , 82.54  , 76.44  , 70.79  ,$
                    65.57  , 60.72  , 56.23  , 52.08  , 48.23  ,$
                    44.67  , 41.37  , 38.31  , 35.48  , 32.86  ,$
                    30.43  , 28.18  , 26.10  , 24.17  , 22.38  ,$
                    20.73  , 19.20  , 17.78  , 16.47  , 15.25  ,$
                    14.12  , 13.08  , 12.12  , 11.22  , 10.39  ,$
                     9.62  ,  8.91  ,  8.25  ,  7.64  ,  7.08  ,$
                     6.56  ,  6.07  ,  5.62  ,  5.21  ,  4.82  ]

   ; Describe arrays for model results
   mod1=fltarr(20)
   mod2=fltarr(20)
   mod3=fltarr(20)
   mod4=fltarr(20)
   
   ncount=0

   ; Main loop over stations
   for i=0,max_station-1 do begin
      
      ncount=ncount+1

      ; Put longitude in (-180,180) range
      if lon_station_1(i) gt 180 then lon_station_1(i)=lon_station_1(i)-360
      if lon_station(i) gt 180 then lon_station(i)=lon_station(i)-360

      ;=====================================================================
      ; Get names of sonde and models files
      ;=====================================================================
      name_sonde='data/sondes.for.gmi/sonde'+num_station(i)+'.0.5'

      ; First month
      mn1=strtrim(String(fix(mon1)),2)
      if (strlen(mn1) eq 1) then begin
         mn1='0'+mn1
      endif

      if (i eq 5) then begin
         mn1='02'
      endif
      if (i ne 5) then begin
         mn1='01'
      endif

      name1=pref1+'GEOSChem.SpeciesConc.'+year1+mn1+'01_0000z.nc4'
      name1_2=pref2+'GEOSChem.SpeciesConc.'+year2+mn1+'01_0000z.nc4'
      if ( nVersions eq 3 ) then begin
      name1_3=pref3+'GEOSChem.SpeciesConc.'+year3+mn1+'01_0000z.nc4'
      endif

      name1_P=pref1+'GEOSChem.LevelEdgeDiags.'+year1+mn1+'01_0000z.nc4'
      name1_P2=pref2+'GEOSChem.LevelEdgeDiags.'+year2+mn1+'01_0000z.nc4'
      if ( nVersions eq 3 ) then begin
      name1_P3=pref3+'GEOSChem.LevelEdgeDiags.'+year3+mn1+'01_0000z.nc4'
      endif
      
      ; 2nd month
      mn2=strtrim(String(fix(mon2)),2)
      if (strlen(mn2) eq 1) then begin
         mn2='0'+mn2
      endif
      name2=pref1+'GEOSChem.SpeciesConc.'+year1+mn2+'01_0000z.nc4'
      name2_2=pref2+'GEOSChem.SpeciesConc.'+year2+mn2+'01_0000z.nc4'
      if ( nVersions eq 3 ) then begin
      name2_3=pref3+'GEOSChem.SpeciesConc.'+year3+mn2+'01_0000z.nc4'
      endif

      name2_P=pref1+'GEOSChem.LevelEdgeDiags.'+year1+mn2+'01_0000z.nc4'
      name2_P2=pref2+'GEOSChem.LevelEdgeDiags.'+year2+mn2+'01_0000z.nc4'
      if ( nVersions eq 3 ) then begin
      name2_P3=pref3+'GEOSChem.LevelEdgeDiags.'+year3+mn2+'01_0000z.nc4'
      endif
      
      ; 3rd month
      mn3=strtrim(String(fix(mon3)),2)
      if (strlen(mn3) eq 1) then begin
         mn3='0'+mn3
      endif
      name3=pref1+'GEOSChem.SpeciesConc.'+year1+mn3+'01_0000z.nc4'
      name3_2=pref2+'GEOSChem.SpeciesConc.'+year2+mn3+'01_0000z.nc4'
      if ( nVersions eq 3 ) then begin
      name3_3=pref3+'GEOSChem.SpeciesConc.'+year3+mn3+'01_0000z.nc4'
      endif

      name3_P=pref1+'GEOSChem.LevelEdgeDiags.'+year1+mn3+'01_0000z.nc4'
      name3_P2=pref2+'GEOSChem.LevelEdgeDiags.'+year2+mn3+'01_0000z.nc4'
      if ( nVersions eq 3 ) then begin
      name3_P3=pref3+'GEOSChem.LevelEdgeDiags.'+year3+mn3+'01_0000z.nc4'
      endif
      
      ; 4th month
      mn4=strtrim(String(fix(mon4)),2)
      if (strlen(mn4) eq 1) then begin
         mn4='0'+mn4
      endif
      name4=pref1+'GEOSChem.SpeciesConc.'+year1+mn4+'01_0000z.nc4'
      name4_2=pref2+'GEOSChem.SpeciesConc.'+year2+mn4+'01_0000z.nc4'
      if ( nVersions eq 3 ) then begin
      name4_3=pref3+'GEOSChem.SpeciesConc.'+year3+mn4+'01_0000z.nc4'
      endif

      name4_P=pref1+'GEOSChem.LevelEdgeDiags.'+year1+mn4+'01_0000z.nc4'
      name4_P2=pref2+'GEOSChem.LevelEdgeDiags.'+year2+mn4+'01_0000z.nc4'
      if ( nVersions eq 3 ) then begin
      name4_P3=pref3+'GEOSChem.LevelEdgeDiags.'+year3+mn4+'01_0000z.nc4'
      endif
      
      ;=====================================================================
      ; Now read sonde means and standard deviations
      ;=====================================================================
      read_sondes_0_5, name_sonde, mon1,   mon2, mon3,   mon4, $
                                   sonde1, std1, sonde2, std2, $
                                   sonde3, std3, sonde4, std4

      if (i eq 5) then begin
         read_sondes_0_5, name_sonde, 2,      mon2, mon3,   mon4, $
                                      sonde1, std1, sonde2, std2, $
                                      sonde3, std3, sonde4, std4
      endif

      ; Replace -999 for std with 0
      for l=0,24 do begin
         if (std1[l] eq -999) then std1[l]=0
         if (std2[l] eq -999) then std2[l]=0
         if (std3[l] eq -999) then std3[l]=0
         if (std4[l] eq -999) then std4[l]=0
      endfor

      if (lon_station(i) eq 178 and dlon1 eq 5) then lon_station(i)=-182

      ;=======================================================================
      ; Read O3 for 1st model and 1st month
      ;=======================================================================

      ; Get O3 & pressure
      O3       = Get_Species_Geos( name1, Date=Date, Species='SpeciesConcVV_O3' )
      Pressure = Get_Pressure_Geos( name1_P, PTOP=PTOP1 )

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type1, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt1 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor
      
      mod1      = Ozone
      pressure1 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=======================================================================
      ; Read O3 for 1st model and 2nd month
      ;=======================================================================
      
      ; Get O3 & pressure
      O3       = Get_Species_Geos( name2, Date=Date, Species='SpeciesConcVV_O3')
      Pressure = Get_Pressure_Geos( name2_P, PTOP=PTOP1 )

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type1, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt1 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor
      
      mod2      = Ozone
      pressure2 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=======================================================================
      ; Read O3 for 1st model and 3rd month
      ;=======================================================================

      ; Get O3 & pressure
      O3       = Get_Species_Geos( name3, Date=Date, Species='SpeciesConcVV_O3' )
      Pressure = Get_Pressure_Geos( name3_P, PTOP=PTOP1, Lat=Lat, Lon=Lon )

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type1, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt1 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor
            
      mod3      = Ozone
      pressure3 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=======================================================================
      ; Read O3 for 1st model and 4th month
      ;=======================================================================

      ; Get O3 & pressure
      O3       = Get_Species_Geos( name4, Date=Date, Species='SpeciesConcVV_O3' )
      Pressure = Get_Pressure_Geos( name4_P, PTOP=PTOP1 )

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type1, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt1 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor
                  
      mod4 = Ozone
      pressure4 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=====================================================================
      ; Set indices
      ;=====================================================================
      ind1 = Where(pressure1 ge 200)
      ind2 = Where(pressure2 ge 200)
      ind3 = Where(pressure3 ge 200)
      ind4 = Where(pressure4 ge 200)
      
      inds1 = Where(pressure_sonde ge 200 and sonde1 gt 0)
      inds2 = Where(pressure_sonde ge 200 and sonde2 gt 0)
      inds3 = Where(pressure_sonde ge 200 and sonde3 gt 0)
      inds4 = Where(pressure_sonde ge 200 and sonde4 gt 0)

      ; Select data below 200 mbar
      pressure1_p=pressure1[ind1]
      pressure2_p=pressure2[ind2]
      pressure3_p=pressure3[ind3]
      pressure4_p=pressure4[ind4]
      pressure_sonde1_p=pressure_sonde[inds1]
      pressure_sonde2_p=pressure_sonde[inds2]
      pressure_sonde3_p=pressure_sonde[inds3]
      pressure_sonde4_p=pressure_sonde[inds4]

      mod1_p=mod1[ind1]
      mod2_p=mod2[ind2]
      mod3_p=mod3[ind3]
      mod4_p=mod4[ind4]
      
      if ( nVersions eq 3 ) then begin

      ;=====================================================================
      ; Read O3 from 3rd model and 1st month
      ;=====================================================================
      
      ; Get O3 & pressure
      O3       = Get_Species_Geos( name1_3, Date=Date, Species='SpeciesConcVV_O3' )
      Pressure = Get_Pressure_Geos( name1_P3, PTOP=PTOP3 )

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type3, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt3 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor

      mod1_3      = Ozone
      pressure1_3 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=====================================================================
      ; Read O3 from 3rd model and 2nd month
      ;===================================================================== 

      ; Get O3 & pressure
      O3       = Get_Species_Geos( name2_3, Date=Date, Species='SpeciesConcVV_O3' )
      Pressure = Get_Pressure_Geos( name2_P3, PTOP=PTOP3, Lat=Lat, Lon=Lon )

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type3, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt3 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor
      
      mod2_3 = Ozone
      pressure2_3 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=====================================================================
      ; Read O3 from 3rd model and 3rd month
      ;===================================================================== 
      
      ; Get O3 & pressure
      O3       = Get_Species_Geos( name3_3, Date=Date, Species='SpeciesConcVV_O3' )
      Pressure = Get_Pressure_Geos( name3_P3, PTOP=PTOP3, Lat=Lat, Lon=Lon )

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type3, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt3 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor
      
      mod3_3 = Ozone
      pressure3_3 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres      

      ;=====================================================================
      ; Read O3 from 3rd model and 4th month
      ;=====================================================================

      ; Get O3 & pressure
      O3       = Get_Species_Geos( name4_3, Date=Date, Species='SpeciesConcVV_O3' )
      Pressure = Get_Pressure_Geos( name4_P3, PTOP=PTOP3 )

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type3, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt3 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor
      
      mod4_3 = Ozone
      pressure4_3 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=====================================================================
      ; Set up indices
      ;===================================================================== 

      ind1 = Where(pressure1_3 ge 200)
      ind2 = Where(pressure2_3 ge 200)
      ind3 = Where(pressure3_3 ge 200)
      ind4 = Where(pressure4_3 ge 200)
      
      inds = Where(pressure_sonde ge 200)

      ; Select data below 200 mbar
      pressure1_3_p=pressure1_3[ind1]
      pressure2_3_p=pressure2_3[ind2]
      pressure3_3_p=pressure3_3[ind3]
      pressure4_3_p=pressure4_3[ind4]
      pressure_sonde_p=pressure_sonde[inds]
      
      mod1_3_p=mod1_3[ind1]
      mod2_3_p=mod2_3[ind2]
      mod3_3_p=mod3_3[ind3]
      mod4_3_p=mod4_3[ind4]

      endif  ; nVersions=3
      
      if (lon_station(i) eq -182) then lon_station(i)=178
      if (lon_station(i) eq 178 and dlon2 eq 5) then lon_station(i)=-182

      ;=====================================================================
      ; Read O3 from 2nd model and 1st month
      ;===================================================================== 
      
      ; Get O3
      O3       = Get_Species_Geos( name1_2, Date=Date, Species='SpeciesConcVV_O3' )

      ; Get pressure
      if ( Do_GCHP ) then begin
         Pressure = Get_Pressure_Geos( name1_P,  PTOP=PTOP2 )
      endif else begin
         Pressure = Get_Pressure_Geos( name1_P2,  PTOP=PTOP2 )
      endelse

      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type2, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt2 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor

      mod1_2 = Ozone
      pressure1_2 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=====================================================================
      ; Read O3 from 2nd model and 2nd month
      ;===================================================================== 
      
      ; Get O3
      O3       = Get_Species_Geos( name2_2, Date=Date, Species='SpeciesConcVV_O3' )
      
      ; Get pressure
      if ( Do_GCHP ) then begin
         Pressure = Get_Pressure_Geos( name2_P, PTOP=PTOP2, Lat=Lat, Lon=Lon )
      endif else begin
         Pressure = Get_Pressure_Geos( name2_P2, PTOP=PTOP2, Lat=Lat, Lon=Lon )
      endelse


      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type2, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt2 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor

      mod2_2 = Ozone
      pressure2_2 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=====================================================================
      ; Read O3 from 2nd model and 3rd month
      ;===================================================================== 

      ; Get O3
      O3       = Get_Species_Geos( name3_2, Date=Date, Species='SpeciesConcVV_O3' )

      ; Get pressure
      if ( Do_GCHP ) then begin
         Pressure = Get_Pressure_Geos( name3_P, PTOP=PTOP2 )
      endif else begin
         Pressure = Get_Pressure_Geos( name3_P2, PTOP=PTOP2 )
      endelse


      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type2, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt2 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor
      
      mod3_2 = Ozone
      pressure3_2 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres

      ;=====================================================================
      ; Read O3 from 2nd model and 4th month
      ;===================================================================== 

      ; Get O3
      O3       = Get_Species_Geos( name4_2, Date=Date, Species='SpeciesConcVV_O3' )

      ; Get pressure
      if ( Do_GCHP ) then begin
         Pressure = Get_Pressure_Geos( name4_P, PTOP=PTOP2 )
      endif else begin
         Pressure = Get_Pressure_Geos( name4_P2, PTOP=PTOP2 )
      endelse


      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type2, IndLon, IndLat,  $
         Center=[ Lat_Station[I], Lon_Station[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = O3[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Nalt         = nalt2 
      Ozone        = fltarr(Nalt)
      pres         = fltarr(Nalt)

      for j=0,Nalt-1 do begin
         Ozone[j]=mean(O3_box[*,*,j])
         Pres[j]=mean( Pressure_box [*,*,j]) 
      endfor
      
      mod4_2 = Ozone
      pressure4_2 = Pres

      ; Undefine stuff
      UnDefine, O3_box
      UnDefine, Pressure_Box
      UnDefine, Ozone
      UnDefine, Pres
      
      ;=====================================================================
      ; Set up indices
      ;===================================================================== 

      ind1 = Where(pressure1_2 ge 200)
      ind2 = Where(pressure2_2 ge 200)
      ind3 = Where(pressure3_2 ge 200)
      ind4 = Where(pressure4_2 ge 200)
      
      inds1 = Where(pressure_sonde ge 200 and sonde1>0)
      inds2 = Where(pressure_sonde ge 200 and sonde2>0)
      inds3 = Where(pressure_sonde ge 200 and sonde3>0)
      inds4 = Where(pressure_sonde ge 200 and sonde4>0)

      ; Select data below 200 mbar
      pressure1_p=pressure1[ind1]
      pressure2_p=pressure2[ind2]
      pressure3_p=pressure3[ind3]
      pressure4_p=pressure4[ind4]
      pressure1_2_p=pressure1_2[ind1]
      pressure2_2_p=pressure2_2[ind2]
      pressure3_2_p=pressure3_2[ind3]
      pressure4_2_p=pressure4_2[ind4]
      
      pressure_sonde1_p=pressure_sonde[inds1]
      pressure_sonde2_p=pressure_sonde[inds2]
      pressure_sonde3_p=pressure_sonde[inds3]
      pressure_sonde4_p=pressure_sonde[inds4]
      
      mod1_p=mod1[ind1]
      mod2_p=mod2[ind2]
      mod3_p=mod3[ind3]
      mod4_p=mod4[ind4]
      
      mod1_2_p=mod1_2[ind1]
      mod2_2_p=mod2_2[ind2]
      mod3_2_p=mod3_2[ind3]
      mod4_2_p=mod4_2[ind4]
      
      sonde1_p=sonde1[inds1]
      sonde2_p=sonde2[inds2]
      sonde3_p=sonde3[inds3]
      sonde4_p=sonde4[inds4]
      std1_p=std1[inds1]
      std2_p=std2[inds2]
      std3_p=std3[inds3]
      std4_p=std4[inds4]

      loval=0
      
      highval1=160
      highval2=160
      highval3=160
      highval4=160
     
      ;=====================================================================
      ; First plot panel
      ;=====================================================================

      ; Create station title
      ltitle=''
      ltitle = strtrim(name_station(i),2)+$
         ' ( '+strtrim(string(fix(lat_station_1(i))),1)+' )'

      ; Plot sonde data
      plot, sonde1_p, -alog10(pressure_sonde1_p), xstyle=1,ystyle=5,$
         title=ltitle,linestyle=0,psym=-5,symsize=0.06,color=1,$ 
         xticks=8, min_val=-900, yrange=[-3,-alog10(200)], $
         xrange=[loval,highval1],$
         charsize=1.5, $
         xtickname=[' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']

      oplot,sonde1_p,-alog10(pressure_sonde1_p),linestyle=0,$
         psym=6,symsize=0.006,color=1 

      ; 1st model
      oplot, mod1_p,-alog10(pressure1_p),linestyle=0,psym=4,$
         symsize=0.3,color=2 
      oplot, mod1_p,-alog10(pressure1_p),linestyle=0,color=2 

      ; 2nd model
      oplot, mod1_2_p,-alog10(pressure1_2_p),linestyle=0,$
         psym=4,symsize=0.3,color=3
      oplot, mod1_2_p,-alog10(pressure1_2_p),linestyle=0,color=3 

      if ( nVersions eq 3 ) then begin
      ; 3rd model
      oplot, mod1_3_p,-alog10(pressure1_3_p),linestyle=0,$
         psym=4,symsize=0.3,color=4
      oplot, mod1_3_p,-alog10(pressure1_3_p),linestyle=0,color=4 
      endif
      
      ; Get pressure labels
      ytickv = [1000,900,800,700,600,500,400,300,200]
      ytickname = ['1000','','800','','600','','400','','200']

      pres=[1000,900,800,700,600,500,400,300,200]
      logpres=-alog10(pres)
      pres1=strtrim(string(pres),2)
      pres1[1]=" "
      pres1[2]=" "
      pres2 = replicate(' ',n_elements(pres1))
      
      pres1=strtrim(string(pres),2)
      pres1[1]=" "

      ; Axes
      if ncount eq 1 or ncount eq 5 or ncount eq 9 or ncount eq 13 or ncount eq 17 or ncount eq 21 or ncount eq 25 or ncount eq 29 then begin  
         axis, loval, yaxis=0, yticks=9, yrange=[-3,-alog10(200)],$
            ytickv=logpres, ytickname=pres1,charsize=1.5,/ystyle,color=1
      endif

      if ncount ne 1 and ncount ne 5 and ncount and 9 or ncount and 13 and ncount ne 17 then begin  
         axis, loval, yaxis=1, yticks=9, yrange=[-3,-alog10(200)],$
            ytickv=logpres, ytickname=pres2, /ystyle ,color=1
      endif

      axis, highval1, yaxis=1, yticks=9, yrange=[-3,-alog10(200)],$
         ytickv=logpres, ytickname=pres2, /ystyle ,color=1
      
      ; Put error bars (one standard deviation) for sonde data
      levs=n_elements(sonde1_p)
      for w = 0, levs-1 do begin
         errbar = [sonde1_p(w)-std1(w), sonde1_p(w)+std1(w)]
         oplot, errbar, $
            [-alog10(pressure_sonde1_p(w)),-alog10(pressure_sonde1_p(w))],$
            linestyle=0,color=1
      endfor
      
      ; Write the month on the plot
      if (i ne 5) then begin
         xyouts,120,-alog10(250), month[mon1-1], charsize = 1, /data, color=1
      endif

      if (i eq 5) then begin
         xyouts,120,-alog10(250), month[1], charsize = 1, /data, color=1
      endif

      ;=====================================================================
      ; Second plot panel
      ;=====================================================================
      ltitle= '' 

      ; Plot sonde data
      plot, sonde2_p, -alog10(pressure_sonde2_p), xstyle=1,ystyle=5,$
         title=ltitle,linestyle=0,psym=-5,symsize=0.06, color=1,$
         xticks=8, min_val=-900, yrange=[-3,-alog10(200)], $
         xrange=[loval,highval2],$
         charsize=1.5, xtickname=[ ' ', ' ', ' ', ' ',' ', ' ', ' ', ' ', ' ']

      oplot, sonde2_p,-alog10(pressure_sonde2_p),linestyle=0,$
         psym=6,symsize=0.06,color=1

      ; 1st model
      oplot, mod2_p,-alog10(pressure2_p),linestyle=0,psym=4,$
         symsize=0.3,color=2 
      oplot, mod2_p,-alog10(pressure2_p),linestyle=0,color=2

      ; 2nd model
      oplot, mod2_2_p,-alog10(pressure2_2_p),linestyle=0,psym=4,$
         symsize=0.3,color=3
      oplot, mod2_2_p,-alog10(pressure2_2_p),linestyle=0,color=3

      if ( nVersions eq 3 ) then begin
      ; 3rd model
      oplot, mod2_3_p,-alog10(pressure2_3_p),linestyle=0,$
         psym=4,symsize=0.3,color=4
      oplot, mod2_3_p,-alog10(pressure2_3_p),linestyle=0,color=4
      endif
      
      ; Put axes
      if ncount eq 1 or ncount eq 5 or ncount eq 9 or ncount eq 13 or ncount eq 17 or ncount eq 21 or ncount eq 25 or ncount eq 29 then begin  
         axis, loval, yaxis=0, yticks=9, yrange=[-3,-alog10(200)],$
            ytickv=logpres, ytickname=pres1,charsize=1.5,/ystyle,color=1
      endif

      if ncount ne 1 and ncount ne 5 and ncount and 9 or ncount and 13 and ncount ne 17 then begin  
         axis, loval, yaxis=1, yticks=9, yrange=[-3,-alog10(200)],$
            ytickv=logpres, ytickname=pres2, /ystyle ,color=1
      endif

      axis, highval2, yaxis=1, yticks=n_elements(ytickv)-1, $
         yrange=[-3,-alog10(200)],$
         ytickv=logpres, ytickname=pres2, /ystyle,color=1 
      
      ; Add error bars (one standard deviation) for sonde data
      levs=n_elements(sonde2_p)
      for w = 0, levs-1 do begin
         errbar = [sonde2_p(w)-std2(w), sonde2_p(w)+std2(w)]
         oplot, errbar,$
            [-alog10(pressure_sonde2_p(w)),-alog10(pressure_sonde2_p(w))],$
            linestyle=0,color=1
      endfor
      
      ; Put month on the plot
      xyouts,120,-alog10(250), month[mon2-1], charsize = 1, /data,color=1

      ;=====================================================================
      ; Third plot panel
      ;=====================================================================
      ltitle='' 

      ; Plot sonde data
      plot, sonde3_p, -alog10(pressure_sonde3_p), xstyle=1,ystyle=5,$
         title=ltitle,linestyle=0,psym=-5,symsize=0.06, color=1,$
         xticks=8, min_val=-900, yrange=[-3,-alog10(200)], $
         xrange=[loval,highval3],$
         charsize=1.5, xtickname=[ ' ', ' ', ' ', ' ',' ', ' ', ' ', ' ', ' ']

      oplot,sonde3_p,-alog10(pressure_sonde3_p),linestyle=0,$
         psym=6,symsize=0.06,color=1 
      
      ; 1st model
      oplot, mod3_p,-alog10(pressure3_p),linestyle=0,$
         psym=4,symsize=0.3,color=2 
      oplot, mod3_p,-alog10(pressure3_p),linestyle=0,color=2

      ; 2nd model
      oplot, mod3_2_p,-alog10(pressure3_2_p),linestyle=0,$
         psym=4,symsize=0.3,color=3
      oplot, mod3_2_p,-alog10(pressure3_2_p),linestyle=0,color=3

      if ( nVersions eq 3 ) then begin
      ; 3rd model
      oplot, mod3_3_p,-alog10(pressure3_3_p),linestyle=0,$
         psym=4,symsize=0.3,color=4
      oplot, mod3_3_p,-alog10(pressure3_3_p),linestyle=0,color=4
      endif
      
      ; Add axes
      if ncount eq 1 or ncount eq 5 or ncount eq 9 or ncount eq 13 or ncount eq 17 or ncount eq 21 or ncount eq 25 or ncount eq 29 then begin  
         axis, loval, yaxis=0, yticks=9, yrange=[-3,-alog10(200)],$
            ytickv=logpres, ytickname=pres1,charsize=1.5,/ystyle,color=1
      endif

      if ncount ne 1 and ncount ne 5 and ncount and 9 or ncount and 13 and ncount ne 17 then begin  
         axis, loval, yaxis=1, yticks=9, yrange=[-3,-alog10(200)],$
            ytickv=logpres, ytickname=pres2, /ystyle,color=1 
      endif

      axis, highval3, yaxis=1, yticks=n_elements(ytickv)-1, $
         yrange=[-3,-alog10(200)],$
         ytickv=logpres, ytickname=pres2, /ystyle,color=1 
      
      ; Put error bars (one standard deviation) sonde data
      levs=n_elements(sonde3_p)
      for w = 0, levs-1 do begin
         errbar = [sonde3_p(w)-std3(w), sonde3_p(w)+std3(w)]
         oplot, errbar, $
            [-alog10(pressure_sonde3_p(w)),-alog10(pressure_sonde3_p(w))],$
            linestyle=0,color=1
      endfor

      xyouts,120,-alog10(250), month[mon3-1], charsize = 1, /data, color=1

      ;=====================================================================
      ; Fourth plot panel
      ;=====================================================================
      ltitle= ''  
      
      ; Plot sonde data
      plot, sonde4_p, -alog10(pressure_sonde4_p), xstyle=1,ystyle=5,$
         title=ltitle,linestyle=0,psym=-5,symsize=0.06, color=1,$
         xticks=8, min_val=-900, yrange=[-3,-alog10(200)], $
         xrange=[loval,highval4],$
         charsize=1.5,$
         xtickname=[ ' ', ' ', '40', ' ','80', ' ', '120', ' ', '160' ]
      
      oplot, sonde4_p,-alog10(pressure_sonde4_p),$
         linestyle=0,psym=6,symsize=0.06,color=1

      ; 1st model
      oplot, mod4_p,-alog10(pressure4_p),linestyle=0,$
         psym=4,symsize=0.3,color=2
      oplot, mod4_p,-alog10(pressure4_p),linestyle=0,color=2

      ; 2nd model
      oplot, mod4_2_p,-alog10(pressure4_2_p),linestyle=0,color=3
      oplot, mod4_2_p,-alog10(pressure4_2_p),linestyle=0,$
         psym=4,symsize=0.3,color=3

      if ( nVersions eq 3 ) then begin
      ; 3rd model
      oplot, mod4_3_p,-alog10(pressure4_3_p),linestyle=0,color=4
      oplot, mod4_3_p,-alog10(pressure4_3_p),linestyle=0,$
         psym=4,symsize=0.3,color=4
      endif
      
      ; Put error bars (one standard deviation) for geos model
      levs=n_elements(mod4_p)

      ; Put error bars (one standard deviation) for sonde data
      levs=n_elements(sonde4_p)
      for w = 0, levs-1 do begin
         errbar = [sonde4_p(w)-std4(w), sonde4_p(w)+std4(w)]
         oplot, errbar, $
            [-alog10(pressure_sonde4_p(w)),-alog10(pressure_sonde4_p(w))],$
            linestyle=0,color=1
      endfor
      xyouts,120,-alog10(250), month[mon4-1], charsize = 1, /data

      ; Add axes
      if ncount eq 1 or ncount eq 5 or ncount eq 9 or ncount eq 13 or ncount eq 17 or ncount eq 21 or ncount eq 25 or ncount eq 29 then begin  
         axis, loval, yaxis=0, yticks=9, yrange=[-3,-alog10(200)],$
            ytickv=logpres, ytickname=pres1,charsize=1.5,/ystyle,color=1
      endif

      if ncount ne 1 and ncount ne 5 and ncount and 9 or ncount and 13 and ncount ne 17 then begin  
         axis, loval, yaxis=1, yticks=9, yrange=[-3,-alog10(200)],$
            ytickv=logpres, ytickname=pres2, /ystyle , color=1
      endif

      axis, highval4, yaxis=1, yticks=n_elements(ytickv)-1, $
         yrange=[-3,-alog10(200)],$
         ytickv=logpres, ytickname=pres2, /ystyle ,color=1

      xyouts,120,-alog10(250), month[mon4-1], charsize = 1, /data, color=1
      xyouts, 0.04, 0.5, 'Pressure (hPa)', /normal, align=0.5, $
         orientation=90, charsize=1.2,color=1
      xyouts, 0.5, 0.05, 'O3 (ppb)', /normal, align=0.5, charsize=1.,color=1
      xyouts, 0.5, 0.96,title, /normal, align=0.5, charsize=1.2,color=1

   endfor

   close_device, /TIMESTAMP
   close,/all

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
