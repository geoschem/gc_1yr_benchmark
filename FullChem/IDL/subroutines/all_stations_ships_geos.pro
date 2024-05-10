; $Id: all_stations_ships_geos.pro,v 1.3 2008/03/31 18:51:06 bmy Exp $
pro all_stations_ships_geos, species1, species, max_sta, pref,  inyear, $
                             ptop,     dlon,    dlat,    model, ext

   ; ALL_STATIONS_SHIPS_GEOS: Saves out files of "average" GEOS-Chem data
   ; over the same area as various ship tracks.  Originally written by 
   ; Inna Megretskaia, modified by Bob Yantosca and Philippe Le Sager.
   ;
   ; NOTE: Now use CTM_INDEX to return the Lon & Lat indices for each
   ;       station.  This will work for all grids. (bmy, 7/11/07)
   
   ; Get MODELINFO structure
   Type1 = CTM_Type( Model, Res=[ DLon, DLat ] )

   ; Define stuff
   filest     = 'data/netCDF/'+species1+'.ships'
   openr, usta, filest, /get_lun
   iname_sta  = ''
   ititle_sta = ''

   name_sta = strarr(max_sta)
   month    = strarr(max_sta)
   lol      = fltarr(max_sta)
   lor      = fltarr(max_sta)
   lad      = fltarr(max_sta)
   lau      = fltarr(max_sta)
   H        = fltarr(max_sta)
   year     = intarr(max_sta)

   for i=0,max_sta-1 do begin
      readf,usta, iname_sta,                    $ 
         ilol, ilor, ilad, ilau,                $
         imonth , iH, iyear, ititle_sta,         $
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
   for i = 0, max_sta-1 do begin
      for j=1, 12        do begin

         ; Month string
         mn = String( J, format='(i2.2)' )

         ; netCDF file name
         name = pref+'GEOSChem.SpeciesConc.'+inyear+mn+'01_0000z.nc4'

         ; Get data from the model
         Data = Get_Species_Geos( name, Species=Species )

         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type1, IndLon, IndLat,  $
                    Edge=[ Lad[I], Lol[I], Lau[I], Lor[I] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1

         ; Bug fix - Average over the area instead of two opposite corners
         ; and take care of the dateline (phs, 3/26/08)
         ; Note, we take the surface model layer since these are ship
         ; observations (bmy, 3/31/08)
         if ( IndLon[1] lt IndLon[0] ) then begin
            Data_box1 = Data[ Indlon[0]:sz[1]-1L,  Indlat[0]:IndLat[1], 0 ]
            Data_box2 = Data[        0:IndLon[1], Indlat[0]:IndLat[1],  0 ]
            Data_box  = [ Data_box1, Data_box2 ]
         endif else begin
            Data_box  = Data[ Indlon[0]:IndLon[1], Indlat[0]:IndLat[1], 0]
         endelse
      
         ; Average the model boxes in the range of lons/lats
         ; that were traversed by the ship (phs, bmy, 3/31/08)
         Data_Avg = Mean( Data_Box )

         ; Output file name
         fileout = 'temp/' + strtrim(name_sta(i), 2)+ext

         ; Open file for writing
         iunit = i+50
         if ( j eq 1 ) then begin 
            openw, iunit, fileout
         endif
         
         ; Write to file
         printf, iunit, Data_Avg
      endfor

      close, iunit
   endfor
   
   close,/all
   close_device
end
