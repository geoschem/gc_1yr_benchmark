; $Id: test_stations_geos.pro,v 1.1 2009/07/10 16:33:07 bmy Exp $
pro test_stations_geos, species1, species2, species, max_sta, pref, indyear, $
                        ptop,     dlon,     dlat,    model,   ext

   ; NOTE: Now pass NALT via the arg list (bmy, 3/29/04)
   ;
   ; NOTE: Now pass DLON, DLAT, MODEL and remove NALT. (bmy, 7/11/07)
   ;
   ; NOTE: Now use CTM_INDEX to return the Lon & Lat indices for each
   ;       station.  This will work for all grids. (bmy, 7/11/07)   

   ; Get MODELINFO & GRIDINFO structure
   Type1 = CTM_Type( Model, Res=[ DLon, DLat ] )
   Grid1 = CTM_Grid( Type1 )

   ; Get vertical dimension
   Nalt  = Grid1.LMX

   ; Read station data
   filest      = 'data/netCDF/'+species1+'.stations'
   openr, usta, filest, /get_lun
   iname_sta   =''
   ititle_sta  =''

   name_sta = strarr(max_sta)
   month    = strarr(max_sta)
   lol      = fltarr(max_sta)
   lor      = fltarr(max_sta)
   lad      = fltarr(max_sta)
   lau      = fltarr(max_sta)
   H        = fltarr(max_sta)
   year     = intarr(max_sta)
   
   for i=0,max_sta-1 do begin
      readf,usta, iname_sta,              $
         ilol, ilor, ilad, ilau,          $
         imonth , iH, iyear, ititle_sta,  $
         format='(a36,1x,i4,1x,i4,1x,i4,1x,i4,1x,i4,1x,i4,1x,i2,1x,a20)'
      name_sta(i) = iname_sta
      month(i)    = imonth
      lol(i)      = ilol
      lor(i)      = ilor
      lad(i)      = ilad
      lau(i)      = ilau
      H(i)        = iH
      year(i)     = iyear  
   endfor
 
   ; Now extract proper profile for the stations
   ; proper name will be given later, now we read from just one file
   for i=0,max_sta-1 do begin
      
      ; Prior to 3/25/08:
      ; There is a simpler way to do this (bmy, 3/25/08)
      ;mn=strtrim(String(month[i]),2)
      ;mn=strtrim(String(fix(month[i])),2)
      ;if (strlen(mn) eq 1) then begin
      ;   mn='0'+mn
      ;endif
     
      ; Month string
      Mn = String( Month[i], Format='(i2.2)' )

      ; netCDF file name
      name=pref+mn+'01.nc'

      if (indyear eq 1)  then begin
         yr=Strtrim(String(fix(year[i]+1900)),1)
         if (year[i] lt 50) then begin
            yr=Strtrim(String(fix(year[i]+2000)),1)
         endif
         name=pref+yr+mn+'01.nc'
      endif

      ;=================================================================
      ; Read DATA
      ;=================================================================

      ; Get data & pressure
      Data     = Get_Species_Geos( Name, Species=Species )
      Pressure = Get_Pressure_Geos( Name, PTOP=PTOP )
      
      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type1, IndLon, IndLat,  $
         Edge=[ Lad[I], Lol[I], Lau[I], Lor[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Define stuff
      O3_box       = Data[Indlon,Indlat,*]
      Pressure_box = Pressure[Indlon,Indlat,*]
      Ozone        = fltarr(Nalt)
      Col          = fltarr(Nalt)
      
      ; NOTE: In case some model grids straddle the boundary betweeen
      ; 2 grid boxes, take the mean of the model data.
      for j=0,Nalt-1 do begin
         Ozone[j]  = Mean( O3_box[*,*,j]        )
         Col[j]    = Mean( Pressure_box [*,*,j] ) 
      endfor

      ; Write column data to a text file
      fileout = 'temp/' + strtrim(name_sta(i),2) + ext
      fileout = Replace_Token( Fileout, 'HNO3', Species2, Delim='' )

      iunit = i+50
      openw,iunit,fileout
      
      for n = 0, Nalt-1 do begin
         printf, iunit, Col[n] , Ozone[n]
      endfor
      close, iunit
   endfor

   ; Quit
   close,/all
   close_device

end
