; $Id: all_stations_geos.pro,v 1.3 2008/03/31 18:51:06 bmy Exp $
pro all_stations_geos, species1, species,  max_sta, pref_spc, pref_p,  $
                       inyear,  ptop,     dlon,    dlat,     model, ext

   ; ALL_STATIONS_GEOS: Saves out files of "average" GEOS-Chem data
   ; over the same area as various aircraft campaigns.  Originally
   ; written by Inna Megretskaia, modified by Bob Yantosca and
   ; Philippe Le Sager.
   ;
   ; NOTE: Now pass NALT via the arg list (bmy, 3/29/04)
   ;
   ; NOTE: Now pass DLON, DLAT, MODEL and remove NALT. (bmy, 7/11/07)
   ;
   ; NOTE: Now use CTM_INDEX to return the Lon & Lat indices for each
   ;       station.  This will work for all grids. (bmy, 7/11/07)   
   ;
   ; NOTE: Now average over the area instead of just taking the corner
   ;       boxes of the region (phs, 3/31/08)
   ;
   ; NOTE: Now read in separate file prefixes for species and pressure
   ;       sources to allow read-in from different models for GCHP 
   ;       (ewl, 6/28/17)

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
      
      ; Month string
      Mn = String( Month[i], Format='(i2.2)' )

      ; netCDF file name
      name_spc=pref_spc+'GEOSChem.SpeciesConc.'+inyear+mn+'01_0000z.nc4'
      name_p=pref_p+'GEOSChem.LevelEdgeDiags.'+inyear+mn+'01_0000z.nc4'

      ;=================================================================
      ; Read DATA
      ;=================================================================

      ; Get data & pressure
      Data     = Get_Species_Geos( Name_spc, Species=Species )
      Pressure = Get_Pressure_Geos( Name_p, PTOP=PTOP )
      
      ; Get the lon & lat indices corresponding to this station
      CTM_Index, Type1, IndLon, IndLat,  $
         Edge=[ Lad[I], Lol[I], Lau[I], Lor[I] ], /Non

      ; Convert from F90 to IDL notation
      IndLon = IndLon - 1
      IndLat = IndLat - 1

      ; Get size of the output (phs, 3/31/08)
      sz = size( data )

      ; Will print only the surface level if 2D (phs, 3/31/08)
      if ( sz[0] eq 2 ) then Nalt = 1 

      ; Bug fix - Average over the area instead of two opposite corners
      ; and take care of the dateline (phs, 3/26/08)
      if ( IndLon[1] lt IndLon[0] ) then begin

         Data_box1    =     Data[Indlon[0]:sz[1]-1L, Indlat[0]:IndLat[1], *]
         Pr_box1      = Pressure[Indlon[0]:sz[1]-1L, Indlat[0]:IndLat[1], *]

         Data_box2    =     Data[0:IndLon[1], Indlat[0]:IndLat[1], *]
         Pr_box2      = Pressure[0:IndLon[1], Indlat[0]:IndLat[1], *]

         ; concatenate over the first dimension
         Data_box     = [ Data_box1, Data_box2 ]
         Pressure_box = [ Pr_box1,   Pr_box2   ]

      endif else begin
         Data_box     = Data[    Indlon[0]:IndLon[1], Indlat[0]:IndLat[1], *]
         Pressure_box = Pressure[Indlon[0]:IndLon[1], Indlat[0]:IndLat[1], *]
      endelse

      ; Arrays for level-averaged data
      Data_Avg     = fltarr(Nalt)
      Press_Avg    = fltarr(Nalt)
      
      ; NOTE: In case some model grids straddle the boundary betweeen
      ; 2 grid boxes, take the mean of the model data.
      for j=0,Nalt-1 do begin
         Data_Avg[j]  = Mean( Data_box[*,*,j]      )
         Press_Avg[j] = Mean( Pressure_box [*,*,j] ) 
      endfor

      ; Write level-averaged data over lon & lat
      fileout = 'temp/' + strtrim(name_sta(i),2) + ext

      iunit = i+50
      openw,iunit,fileout
      
      for n = 0, Nalt-1 do begin
         printf, iunit, Press_Avg[n] , Data_Avg[n]
      endfor
      close, iunit
   endfor

   ; Quit
   close,/all
   close_device

end
