; $Id: plot_seas_improve_geos_3_models.pro,v 1.1 2009/07/10 16:33:08 bmy Exp $
pro plot_seas_improve_geos_3_models, pref1,  ptop1,  dlat1,   dlon1, model1, year1, $
                                     pref2,  ptop2,  dlat2,   dlon2, model2, year2, $
                                     pref3,  ptop3,  dlat3,   dlon3, model3, year3, $
                                     title,  psname, max_sta, filest

; PURPOSE:
;        Compares 2005 IMPROVE speciated aerosol surface concentrations 
;        with GEOS-Chem monthly mean concentrations from 3 models, 
;        plotted with colors red, green and blue 
; 
;        Written by: Colette L. Heald (clh, 7/7/2009)
;
; MODIFICATION HISTORY:
;        clh, 07 Jul 2009: Initial version
;        mps, 27 Jan 2017: Update to allow for comparison of 2 versions,
;                          intead of the default 3 versions

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
   Type3 = CTM_Type( Model3, Res=[ DLon3, DLat3 ] )
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

   ; Plot settings
   !X.OMARGIN=[8,6] 
   !Y.OMARGIN=[3,6]
   !X.THICK=4
   !Y.THICK=4
   !P.CHARTHICK=2.5
   !P.THICK=2.5
   
   ; Use Postscript font
   !P.FONT=0

   mmonth = strarr(12)
   mmonth=['Jan','Feb','Mar','Apr','May','Jun',$
           'Jul','Aug','Sep','Oct','Nov','Dec']
   nmon = n_elements(mmonth)

   species = ['NH4f', 'ECf', 'OCf', 'NO3f', 'SO4f']
   nsp = n_elements(species)

   ; conversion from ppb to ug/m3 (must also multiply by MW)
   ; ug/m3 = vmr x na / Avg x MWx10^6
   ; ug/sm3 = ug/m3 x (T/STP_T) x (STP_P/P) = vmr x MWe6 / R x 100 x (STP_P/STP_T)
   STP_P = 1013.25
   STP_T = 298.
   ppb_ugm3 = 1e6 / 8.314 * 100. * STP_P/STP_T *1e-9 ; 4.1e-2 * MW
   MWaer = [18, 12, 12, 62, 96]

   ;color plot
   open_device, olddevice,/ps,/color,filename=psname 
   nrow=4
   ncol=4
   !P.Multi = [0,nrow,ncol,1,0]

   ;========================================================================
   ; --- read IMPROVE data ---
   ;========================================================================
   name_sta     = strarr(max_sta)
   id_sta     = strarr(max_sta)
   spec         = strarr(nsp)
   lon_sta      = fltarr(max_sta)
   lat_sta      = fltarr(max_sta)
   elev_sta     = fltarr(max_sta)
   aer_mean     = fltarr(max_sta, nmon, nsp)
   aer_std      = fltarr(max_sta, nmon, nsp)
   line =  ' '

   ; open file & header 
   openr, ilun_pos, filest, /get_lun
   readf, ilun_pos, line
   vars = strbreak(line, ',')

   ; loop for stations
   for i=0, max_sta-1 do begin
      for j=0, nsp-1 do begin
         readf, ilun_pos, line
         junk = strbreak(line, ',')
         
         name_sta(i) = junk(0)
         id_sta(i) = junk(1)
         spec(j) = junk(2)
         lat_sta(i) = float(junk(3))
         lon_sta(i) = float(junk(4))
         elev_sta(i) = float(junk(5))
         aer_mean(i, *, j) = float(junk(6:17))
         aer_std(i, *, j) = float(junk(18:29))
         
      endfor ; j
   endfor ; i

   close,  ilun_pos
   free_lun, ilun_pos

   print, 'Done reading IMPROVE data'

   ;========================================================================
   ; --- open model output and match data ---
   ;========================================================================
   gc_mean = fltarr(nVersions, max_sta, nmon, nsp)

   ;------------------------------------------------------------------------
   ; Read data from 1st model
   ;------------------------------------------------------------------------
   for mon=0, 11 do begin

      ctm_cleanup, /no_gc

      ; Month name & filename
      mn = strtrim(String(fix(mon+1)), 2)
      if(strlen(mn) eq 1) then begin
         mn = '0'+mn
      endif
      name = pref1+'GEOSChem.SpeciesConc.'+year1+mn+'01_0000z.nc4'

      ; Retreive all monthly GEOS-Chem aerosol species
      NH4 = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NH4', lat=lat, lon=lon)
      NO3 = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NIT', lat=lat, lon=lon)
      SO4 = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SO4', lat=lat, lon=lon)
      BCi = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPI', lat=lat, lon=lon)
      OCi = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPI', lat=lat, lon=lon)
      BCo = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPO', lat=lat, lon=lon)
      OCo = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPO', lat=lat, lon=lon)

      ; Loop through stations to find matching indices
      for st=0, max_sta-1 do begin

         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type1, IndLon, IndLat,  $
                    Center=[ Lat_Sta[st], Lon_Sta[st] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1
         
         ; Assign GEOS-Chem concentrations for each site
         ; Add scaling of OC to account for non-carbon mass. Colette Heald
         ;  suggests using a factor of 2.1 to be consistent with SOA code,
         ;  but values of 1.4-2.4 can be assumed (mpayer, 11/27/12)
         gc_mean(0, st, mon, 0) = NH4(indlon, indlat, 0)*ppb_ugm3*MWaer(0)
         gc_mean(0, st, mon, 1) = (BCi(indlon, indlat, 0)+BCo(indlon, indlat, 0))*ppb_ugm3*MWaer(1)
         gc_mean(0, st, mon, 2) = (OCi(indlon, indlat, 0)+OCo(indlon, indlat, 0))*ppb_ugm3*MWaer(2)*2.1
         gc_mean(0, st, mon, 3) = NO3(indlon, indlat, 0)*ppb_ugm3*MWaer(3)
         gc_mean(0, st, mon, 4) = SO4(indlon, indlat, 0)*ppb_ugm3*MWaer(4)

      endfor

   endfor ; mon

   print, 'Done reading Model 1'

   ;------------------------------------------------------------------------
   ; Read data from 2nd model
   ;------------------------------------------------------------------------
   for mon=0, 11 do begin

      ctm_cleanup, /no_gc

      ; Month name & filename
      mn = strtrim(String(fix(mon+1)), 2)
      if(strlen(mn) eq 1) then begin
         mn = '0'+mn
      endif
      name = pref2+'GEOSChem.SpeciesConc.'+year2+mn+'01_0000z.nc4'

      ; Retreive all monthly GEOS-Chem aerosol species
      NH4 = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NH4', lat=lat, lon=lon)
      NO3 = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NIT', lat=lat, lon=lon)
      SO4 = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SO4', lat=lat, lon=lon)
      BCi = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPI', lat=lat, lon=lon)
      OCi = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPI', lat=lat, lon=lon)
      BCo = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPO', lat=lat, lon=lon)
      OCo = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPO', lat=lat, lon=lon)


      ; Loop through stations to find matching indices
      for st=0, max_sta-1 do begin

         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type1, IndLon, IndLat,  $
                    Center=[ Lat_Sta[st], Lon_Sta[st] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1
         
         ; Assign GEOS-Chem concentrations for each site
         ; Add scaling of OC to account for non-carbon mass. Colette Heald
         ;  suggests using a factor of 2.1 to be consistent with SOA code,
         ;  but values of 1.4-2.4 can be assumed (mpayer, 11/27/12)
         gc_mean(1, st, mon, 0) = NH4(indlon, indlat, 0)*ppb_ugm3*MWaer(0)
         gc_mean(1, st, mon, 1) = (BCi(indlon, indlat, 0)+BCo(indlon, indlat, 0))*ppb_ugm3*MWaer(1)
         gc_mean(1, st, mon, 2) = (OCi(indlon, indlat, 0)+OCo(indlon, indlat, 0))*ppb_ugm3*MWaer(2)*2.1
         gc_mean(1, st, mon, 3) = NO3(indlon, indlat, 0)*ppb_ugm3*MWaer(3)
         gc_mean(1, st, mon, 4) = SO4(indlon, indlat, 0)*ppb_ugm3*MWaer(4)

      endfor

   endfor ; mon

   print, 'Done reading Model 2'

   if ( nVersions eq 3 ) then begin
   
   ;------------------------------------------------------------------------
   ; Read data from 3rd model
   ;------------------------------------------------------------------------
   for mon=0, 11 do begin

      ctm_cleanup, /no_gc

      ; Month name & filename
      mn = strtrim(String(fix(mon+1)), 2)
      if(strlen(mn) eq 1) then begin
         mn = '0'+mn
      endif
      name = pref3+'GEOSChem.SpeciesConc.'+year3+mn+'01_0000z.nc4'

      ; Retreive all monthly GEOS-Chem aerosol species
      NH4 = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NH4', lat=lat, lon=lon)
      NO3 = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NIT', lat=lat, lon=lon)
      SO4 = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SO4', lat=lat, lon=lon)
      BCi = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPI', lat=lat, lon=lon)
      OCi = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPI', lat=lat, lon=lon)
      BCo = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPO', lat=lat, lon=lon)
      OCo = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPO', lat=lat, lon=lon)


      ; Loop through stations to find matching indices
      for st=0, max_sta-1 do begin

         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type1, IndLon, IndLat,  $
                    Center=[ Lat_Sta[st], Lon_Sta[st] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1
         
         ; Assign GEOS-Chem concentrations for each site
         ; Add scaling of OC to account for non-carbon mass. Colette Heald
         ;  suggests using a factor of 2.1 to be consistent with SOA code,
         ;  but values of 1.4-2.4 can be assumed (mpayer, 11/27/12)
         gc_mean(2, st, mon, 0) = NH4(indlon, indlat, 0)*ppb_ugm3*MWaer(0)
         gc_mean(2, st, mon, 1) = (BCi(indlon, indlat, 0)+BCo(indlon, indlat, 0))*ppb_ugm3*MWaer(1)
         gc_mean(2, st, mon, 2) = (OCi(indlon, indlat, 0)+OCo(indlon, indlat, 0))*ppb_ugm3*MWaer(2)*2.1
         gc_mean(2, st, mon, 3) = NO3(indlon, indlat, 0)*ppb_ugm3*MWaer(3)
         gc_mean(2, st, mon, 4) = SO4(indlon, indlat, 0)*ppb_ugm3*MWaer(4)


      endfor

   endfor ; mon

   print, 'Done reading Model 3'

   endif
   
   ;======================================================================
   ; Create the plot
   ;======================================================================
   for sp=nsp-1, 0, -1 do begin

      nrow = 4
      ncol = 4
      !P.Multi = [0, nrow, ncol, 1, 0]

      for st=0, max_sta-1 do begin

         ; specify arrays
         data = reform(aer_mean(st, *, sp))
         datastd = reform(aer_std(st, *, sp))
         gc1 = reform(gc_mean(0, st, *, sp))
         gc2 = reform(gc_mean(1, st, *, sp))
         if ( nVersions eq 3 ) then begin
         gc3 = reform(gc_mean(2, st, *, sp))
         endif
         
      ; set maximum plot value for each species
         if ( nVersions eq 2 ) then begin
            maxd = max([gc1, gc2])
         endif else begin
            maxd = max([gc1, gc2, gc3])
         endelse
         maxd2 = max(data)
         if maxd2 gt maxd then maxd = maxd2


         bddata = where(data le 0)

         if bddata(0) eq -1 then begin
 
            ; plot observations
            plot, findgen(12)+1, data, linestyle=0, color=1, psym=-5, symsize=0.6, charsize=1.5, $
                  /xstyle, /ystyle, xrange=[0, 13], yrange=[0, maxd], $
                  title=id_sta(st)+': '+name_sta(st), xmargin=[3, 2], ymargin=[3, 1], $
                  xticks=13, xtickname=[' ','J','F','M','A','M','J','J','A','S','O','N','D',' ']

            ; plot standard deviations
            for w=0, 11 do begin
               errbar = [data[w]-datastd[w], data[w]+datastd[w]]
               oplot,  [w+1, w+1], errbar, $
                       linestyle=0, color=1
            endfor

            ; overplot 3 model results
            oplot, findgen(12)+1, gc1, linestyle=0, color=2
            oplot, findgen(12)+1, gc1, psym=2, symsize=0.4, color=2   
            oplot, findgen(12)+1, gc2, linestyle=0, color=3
            oplot, findgen(12)+1, gc2, psym=3, symsize=0.4, color=2   
            if ( nVersions eq 3 ) then begin
            oplot, findgen(12)+1, gc3, linestyle=0, color=4
            oplot, findgen(12)+1, gc3, psym=4, symsize=0.4, color=2   
            endif
            
         xyouts, 0.5, 0.03, 'IMPROVE '+species(sp)+' [!7l!3gm!U-3!N]', color=1, charsize=1, /normal, align=0.5
         if species(sp) eq 'OCf' then xyouts, 0.03, 0.5, 'GEOS-Chem '+species(sp)+' (w/o SOA) [!7l!3gm!U-3!N]', orientation=90, color=1, charsize=1, /normal, align=0.5 $
         else xyouts, 0.03, 0.5, 'GEOS-Chem '+species(sp)+' [!7l!3gm!U-3!N]', orientation=90, color=1, charsize=1, /normal, align=0.5
         xyouts, 0.5, 0.96, title, color=1, charsize=1.2, /normal, align=0.5
         
         endif

      endfor

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
