; $Id: map_pm25_geos_3_models.pro,v 1.2 2010/03/02 18:12:34 bmy Exp $
pro map_pm25_geos_3_models, pref1,  label1, dlat1,   dlon1, model1, year1, $
                            pref2,  label2, dlat2,   dlon2, model2, year2, $
                            pref3,  label3, dlat3,   dlon3, model3, year3, $
                            title,  psname1, psname2, max_sta, filest

; PURPOSE:
;        Compares PM2.5 global surface concentrations with GEOS-Chem
;        monthly mean concentrations from 3 models on a map
; 
;        Written by: Colette L. Heald (clh, 7/7/2009)
;
;        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;        % NOTE: This routine can be combined with routine     %
;        %  plot_scatter_pm25_geos_3_models.pro when migrating %
;        %  to Python (mps, 9/21/17)                           %
;        %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;
; MODIFICATION HISTORY:
;        clh, 07 Jul 2009: Initial version
;        mps, 27 Jan 2017: Update to allow for comparison of 2 versions,
;                          intead of the default 3 versions
;        mps, 21 Sep 2017: Create two sets of plots:
;                           1. PM2.5 with complex SOA species
;                           2. PM2.5 with simple  SOA species

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
   !X.OMARGIN=[12,4] 
   !Y.OMARGIN=[4,8]
   !X.THICK=4
   !Y.THICK=4
   !P.CHARTHICK=2.5
   !P.THICK=2.5

   ; Use Postscript font
   !P.FONT=0

   myct_define
   ncolors = !myct.ncolors
   bottom = !myct.bottom
   margin = [0.001, 0.001, 0.001, 0.001]
   
   mmonth = strarr(12)
   mmonth=['Jan','Feb','Mar','Apr','May','Jun',$
           'Jul','Aug','Sep','Oct','Nov','Dec']
   nmon = n_elements(mmonth)

   ; Define a variable for symbol size to make it easier to change
   ; globally.  Reduced symsize from 1 to 0.8 to make plot symbols
   ; easier to read. (bmy, 3/2/10)
   symsize = 0.8

   ; conversion from ppb to ug/m3 (must also multiply by MWaer)
   ; ug/m3 = vmr x na / Avg x MWx10^6
   ; ug/sm3 = ug/m3 x (T/STP_T) x (STP_P/P) = vmr x MWe6 / R x 100 x (STP_P/STP_T)
   STP_P = 1013.25
   STP_T = 298.
   ppb_ugm3 = 1e6 / 8.314 * 100. * STP_P/STP_T *1e-9 ; 4.1e-2 * MWaer

   ; Define molecular weights (g/mol)
   MW_NH4    = 18.0
   MW_NIT    = 62.0
   MW_SO4    = 96.0
   MW_BC     = 12.0
   MW_OC     = 12.0
   MW_DUST   = 29.0
   MW_SALA   = 31.4
   MW_SOA    = 150.0
   MW_SOAGX  = 58.0
   MW_SOAMG  = 72.0
   MW_SOAIE  = 118.0
   MW_SOAME  = 102.0
   MW_INDIOL = 102.0
   MW_LVOCOA = 154.0
   MW_ISN1OA = 226.0

   ; model names
   if ( nVersions eq 2 ) then begin
      modlabels = [label1, label2]
   endif else begin
      modlabels = [label1, label2, label3]
   endelse 

   ;========================================================================
   ; --- read PM2.5 data ---
   ;========================================================================
   id_sta       = strarr(max_sta)
   network_sta  = strarr(max_sta)
   lon_sta      = fltarr(max_sta)
   lat_sta      = fltarr(max_sta)
   pm25_mean    = fltarr(max_sta, nmon)
   pm25_std     = fltarr(max_sta, nmon)
   pm25_ct      = fltarr(max_sta, nmon)
   line =  ' '

   ; open file & header 
   openr, ilun_pos, filest, /get_lun
   readf, ilun_pos, line
   vars = strbreak(line, ',')

   ; loop for stations
   for i=0, max_sta-1 do begin
         readf, ilun_pos, line
         junk = strbreak(line, ',')
         
         id_sta(i) = junk(0)
         network_sta(i) = junk(1)
         lat_sta(i) = float(junk(2))
         lon_sta(i) = float(junk(3))
         pm25_mean(i, *) = float(junk(4:15))
         pm25_std(i, *) = float(junk(16:27))
         pm25_ct(i, *) = float(junk(28:39))
         
   endfor ; i

   close,  ilun_pos
   free_lun, ilun_pos

   print, 'Done reading PM2.5 data'

   ;========================================================================
   ; --- open model output and match data ---
   ;========================================================================
   gc_mean_c = fltarr(nVersions, max_sta, nmon) ; PM2.5 w/ complex SOA
   gc_mean_s = fltarr(nVersions, max_sta, nmon) ; PM2.5 w/ simple  SOA

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
      NH4    = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NH4',    lat=lat, lon=lon)
      NIT    = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NIT',    lat=lat, lon=lon)
      SO4    = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SO4',    lat=lat, lon=lon)
      BCPI   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPI',   lat=lat, lon=lon)
      OCPI   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPI',   lat=lat, lon=lon)
      BCPO   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPO',   lat=lat, lon=lon)
      OCPO   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPO',   lat=lat, lon=lon)
      DST1   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_DST1',   lat=lat, lon=lon)
      DST2   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_DST2',   lat=lat, lon=lon)
      SALA   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SALA',   lat=lat, lon=lon)
      TSOA0  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA0',  lat=lat, lon=lon)
      TSOA1  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA1',  lat=lat, lon=lon)
      TSOA2  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA2',  lat=lat, lon=lon)
      TSOA3  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA3',  lat=lat, lon=lon)
      ASOAN  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOAN',  lat=lat, lon=lon)
      ASOA1  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOA1',  lat=lat, lon=lon)
      ASOA2  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOA2',  lat=lat, lon=lon)
      ASOA3  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOA3',  lat=lat, lon=lon)
      SOAGX  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SOAGX',  lat=lat, lon=lon)
      SOAIE  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SOAIE',  lat=lat, lon=lon)
      INDIOL = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_INDIOL', lat=lat, lon=lon)
      LVOCOA = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_LVOCOA', lat=lat, lon=lon)
      SOAS   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SOAS',   lat=lat, lon=lon)

      ; Sum up complex SOA species (Pye et al. 2010)
      SOA    = TSOA0 + TSOA1 + TSOA2 + TSOA3 + $
               ASOAN + ASOA1 + ASOA2 + ASOA3

      ; Loop through stations to find matching indices
      for st=0, max_sta-1 do begin

         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type1, IndLon, IndLat,  $
                    Center=[ Lat_Sta[st], Lon_Sta[st] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1

         ; Convert ppb to ug/m3
         NH4_ugm3    = NH4(    indlon, indlat, 0) * ppb_ugm3 * MW_NH4
         NIT_ugm3    = NIT(    indlon, indlat, 0) * ppb_ugm3 * MW_NIT
         SO4_ugm3    = SO4(    indlon, indlat, 0) * ppb_ugm3 * MW_SO4
         BCPI_ugm3   = BCPI(   indlon, indlat, 0) * ppb_ugm3 * MW_BC
         OCPI_ugm3   = OCPI(   indlon, indlat, 0) * ppb_ugm3 * MW_OC
         BCPO_ugm3   = BCPO(   indlon, indlat, 0) * ppb_ugm3 * MW_BC
         OCPO_ugm3   = OCPO(   indlon, indlat, 0) * ppb_ugm3 * MW_OC
         DST1_ugm3   = DST1(   indlon, indlat, 0) * ppb_ugm3 * MW_DUST
         DST2_ugm3   = DST2(   indlon, indlat, 0) * ppb_ugm3 * MW_DUST
         SALA_ugm3   = SALA(   indlon, indlat, 0) * ppb_ugm3 * MW_SALA
         SOA_ugm3    = SOA(    indlon, indlat, 0) * ppb_ugm3 * MW_SOA
         SOAGX_ugm3  = SOAGX(  indlon, indlat, 0) * ppb_ugm3 * MW_SOAGX
         SOAIE_ugm3  = SOAIE(  indlon, indlat, 0) * ppb_ugm3 * MW_SOAIE
         INDIOL_ugm3 = INDIOL( indlon, indlat, 0) * ppb_ugm3 * MW_INDIOL
         LVOCOA_ugm3 = LVOCOA( indlon, indlat, 0) * ppb_ugm3 * MW_LVOCOA
         SOAS_ugm3   = SOAS(   indlon, indlat, 0) * ppb_ugm3 * MW_SOA
         SOAS_ugm3 = 0.0

         ; Sum up isoprene SOA species (Marais et al. 2016)
         ISOAAQ_ugm3 = SOAGX_ugm3  + SOAIE_ugm3  + $
                       INDIOL_ugm3 + LVOCOA_ugm3

         ; Assign GEOS-Chem concentrations for each site
         ; Add scaling of OC to account for non-carbon mass. Colette Heald
         ;  suggests using a factor of 2.1 to be consistent with SOA code,
         ;  but values of 1.4-2.4 can be assumed (mpayer, 11/27/12)

         ; Compute PM2.5 with complex SOA (Pye et al. 2010, Marais et al. 2016)
         gc_mean_c(0, st, mon) = 1.33*( NH4_ugm3+ NIT_ugm3 + SO4_ugm3  ) + $
                                 BCPI_ugm3 + BCPO_ugm3 +                   $
                                 2.1*( OCPO_ugm3 + 1.16*OCPI_ugm3 ) +      $
                                 DST1_ugm3 + 0.38*DST2_ugm3 +              $
                                 1.86*SALA_ugm3 +                          $
                                 1.16*SOA_ugm3 + 1.16*ISOAAQ_ugm3

         ; Compute PM2.5 with simple SOA (Sal Farina, Aerosols WG Chairs)
         gc_mean_s(0, st, mon) = 1.33*( NH4_ugm3+ NIT_ugm3 + SO4_ugm3  ) + $
                                 BCPI_ugm3 + BCPO_ugm3 +                   $
                                 2.1*( OCPO_ugm3 + 1.16*OCPI_ugm3 ) +      $
                                 DST1_ugm3 + 0.38*DST2_ugm3 +              $
                                 1.86*SALA_ugm3 +                          $
                                 1.16*SOAS_ugm3

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
      NH4    = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NH4',    lat=lat, lon=lon)
      NIT    = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NIT',    lat=lat, lon=lon)
      SO4    = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SO4',    lat=lat, lon=lon)
      BCPI   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPI',   lat=lat, lon=lon)
      OCPI   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPI',   lat=lat, lon=lon)
      BCPO   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPO',   lat=lat, lon=lon)
      OCPO   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPO',   lat=lat, lon=lon)
      DST1   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_DST1',   lat=lat, lon=lon)
      DST2   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_DST2',   lat=lat, lon=lon)
      SALA   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SALA',   lat=lat, lon=lon)
      TSOA0  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA0',  lat=lat, lon=lon)
      TSOA1  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA1',  lat=lat, lon=lon)
      TSOA2  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA2',  lat=lat, lon=lon)
      TSOA3  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA3',  lat=lat, lon=lon)
      ASOAN  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOAN',  lat=lat, lon=lon)
      ASOA1  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOA1',  lat=lat, lon=lon)
      ASOA2  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOA2',  lat=lat, lon=lon)
      ASOA3  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOA3',  lat=lat, lon=lon)
      SOAGX  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SOAGX',  lat=lat, lon=lon)
      SOAIE  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SOAIE',  lat=lat, lon=lon)
      INDIOL = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_INDIOL', lat=lat, lon=lon)
      LVOCOA = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_LVOCOA', lat=lat, lon=lon)
      SOAS   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SOAS',   lat=lat, lon=lon)

      ; Sum up complex SOA species (Pye et al. 2010)
      SOA    = TSOA0 + TSOA1 + TSOA2 + TSOA3 + $
               ASOAN + ASOA1 + ASOA2 + ASOA3

      ; Loop through stations to find matching indices
      for st=0, max_sta-1 do begin

         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type1, IndLon, IndLat,  $
                    Center=[ Lat_Sta[st], Lon_Sta[st] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1

         ; Convert ppb to ug/m3
         NH4_ugm3    = NH4(    indlon, indlat, 0) * ppb_ugm3 * MW_NH4
         NIT_ugm3    = NIT(    indlon, indlat, 0) * ppb_ugm3 * MW_NIT
         SO4_ugm3    = SO4(    indlon, indlat, 0) * ppb_ugm3 * MW_SO4
         BCPI_ugm3   = BCPI(   indlon, indlat, 0) * ppb_ugm3 * MW_BC
         OCPI_ugm3   = OCPI(   indlon, indlat, 0) * ppb_ugm3 * MW_OC
         BCPO_ugm3   = BCPO(   indlon, indlat, 0) * ppb_ugm3 * MW_BC
         OCPO_ugm3   = OCPO(   indlon, indlat, 0) * ppb_ugm3 * MW_OC
         DST1_ugm3   = DST1(   indlon, indlat, 0) * ppb_ugm3 * MW_DUST
         DST2_ugm3   = DST2(   indlon, indlat, 0) * ppb_ugm3 * MW_DUST
         SALA_ugm3   = SALA(   indlon, indlat, 0) * ppb_ugm3 * MW_SALA
         SOA_ugm3    = SOA(    indlon, indlat, 0) * ppb_ugm3 * MW_SOA
         SOAGX_ugm3  = SOAGX(  indlon, indlat, 0) * ppb_ugm3 * MW_SOAGX
         SOAIE_ugm3  = SOAIE(  indlon, indlat, 0) * ppb_ugm3 * MW_SOAIE
         INDIOL_ugm3 = INDIOL( indlon, indlat, 0) * ppb_ugm3 * MW_INDIOL
         LVOCOA_ugm3 = LVOCOA( indlon, indlat, 0) * ppb_ugm3 * MW_LVOCOA
         SOAS_ugm3   = SOAS(   indlon, indlat, 0) * ppb_ugm3 * MW_SOA
         SOAS_ugm3 = 0.0

         ; Sum up isoprene SOA species (Marais et al. 2016)
         ISOAAQ_ugm3 = SOAGX_ugm3  + SOAIE_ugm3  + $
                       INDIOL_ugm3 + LVOCOA_ugm3

         ; Assign GEOS-Chem concentrations for each site
         ; Add scaling of OC to account for non-carbon mass. Colette Heald
         ;  suggests using a factor of 2.1 to be consistent with SOA code,
         ;  but values of 1.4-2.4 can be assumed (mpayer, 11/27/12)

         ; Compute PM2.5 with complex SOA (Pye et al. 2010, Marais et al. 2016)
         gc_mean_c(1, st, mon) = 1.33*( NH4_ugm3+ NIT_ugm3 + SO4_ugm3  ) + $
                                 BCPI_ugm3 + BCPO_ugm3 +                   $
                                 2.1*( OCPO_ugm3 + 1.16*OCPI_ugm3 ) +      $
                                 DST1_ugm3 + 0.38*DST2_ugm3 +              $
                                 1.86*SALA_ugm3 +                          $
                                 1.16*SOA_ugm3 + 1.16*ISOAAQ_ugm3

         ; Compute PM2.5 with simple SOA (Sal Farina, Aerosols WG Chairs)
         gc_mean_s(1, st, mon) = 1.33*( NH4_ugm3+ NIT_ugm3 + SO4_ugm3  ) + $
                                 BCPI_ugm3 + BCPO_ugm3 +                   $
                                 2.1*( OCPO_ugm3 + 1.16*OCPI_ugm3 ) +      $
                                 DST1_ugm3 + 0.38*DST2_ugm3 +              $
                                 1.86*SALA_ugm3 +                          $
                                 1.16*SOAS_ugm3

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
      NH4    = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NH4',    lat=lat, lon=lon)
      NIT    = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_NIT',    lat=lat, lon=lon)
      SO4    = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SO4',    lat=lat, lon=lon)
      BCPI   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPI',   lat=lat, lon=lon)
      OCPI   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPI',   lat=lat, lon=lon)
      BCPO   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_BCPO',   lat=lat, lon=lon)
      OCPO   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_OCPO',   lat=lat, lon=lon)
      DST1   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_DST1',   lat=lat, lon=lon)
      DST2   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_DST2',   lat=lat, lon=lon)
      SALA   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SALA',   lat=lat, lon=lon)
      TSOA0  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA0',  lat=lat, lon=lon)
      TSOA1  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA1',  lat=lat, lon=lon)
      TSOA2  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA2',  lat=lat, lon=lon)
      TSOA3  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_TSOA3',  lat=lat, lon=lon)
      ASOAN  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOAN',  lat=lat, lon=lon)
      ASOA1  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOA1',  lat=lat, lon=lon)
      ASOA2  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOA2',  lat=lat, lon=lon)
      ASOA3  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_ASOA3',  lat=lat, lon=lon)
      SOAGX  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SOAGX',  lat=lat, lon=lon)
      SOAIE  = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SOAIE',  lat=lat, lon=lon)
      INDIOL = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_INDIOL', lat=lat, lon=lon)
      LVOCOA = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_LVOCOA', lat=lat, lon=lon)
      SOAS   = get_species_geos(name, data=data, $
                            species='SpeciesConcVV_SOAS',   lat=lat, lon=lon)

      ; Sum up complex SOA species (Pye et al. 2010)
      SOA    = TSOA0 + TSOA1 + TSOA2 + TSOA3 + $
               ASOAN + ASOA1 + ASOA2 + ASOA3

      ; Loop through stations to find matching indices
      for st=0, max_sta-1 do begin

         ; Get the lon & lat indices corresponding to this station
         CTM_Index, Type1, IndLon, IndLat,  $
                    Center=[ Lat_Sta[st], Lon_Sta[st] ], /Non

         ; Convert from F90 to IDL notation
         IndLon = IndLon - 1
         IndLat = IndLat - 1

         ; Convert ppb to ug/m3
         NH4_ugm3    = NH4(    indlon, indlat, 0) * ppb_ugm3 * MW_NH4
         NIT_ugm3    = NIT(    indlon, indlat, 0) * ppb_ugm3 * MW_NIT
         SO4_ugm3    = SO4(    indlon, indlat, 0) * ppb_ugm3 * MW_SO4
         BCPI_ugm3   = BCPI(   indlon, indlat, 0) * ppb_ugm3 * MW_BC
         OCPI_ugm3   = OCPI(   indlon, indlat, 0) * ppb_ugm3 * MW_OC
         BCPO_ugm3   = BCPO(   indlon, indlat, 0) * ppb_ugm3 * MW_BC
         OCPO_ugm3   = OCPO(   indlon, indlat, 0) * ppb_ugm3 * MW_OC
         DST1_ugm3   = DST1(   indlon, indlat, 0) * ppb_ugm3 * MW_DUST
         DST2_ugm3   = DST2(   indlon, indlat, 0) * ppb_ugm3 * MW_DUST
         SALA_ugm3   = SALA(   indlon, indlat, 0) * ppb_ugm3 * MW_SALA
         SOA_ugm3    = SOA(    indlon, indlat, 0) * ppb_ugm3 * MW_SOA
         SOAGX_ugm3  = SOAGX(  indlon, indlat, 0) * ppb_ugm3 * MW_SOAGX
         SOAIE_ugm3  = SOAIE(  indlon, indlat, 0) * ppb_ugm3 * MW_SOAIE
         INDIOL_ugm3 = INDIOL( indlon, indlat, 0) * ppb_ugm3 * MW_INDIOL
         LVOCOA_ugm3 = LVOCOA( indlon, indlat, 0) * ppb_ugm3 * MW_LVOCOA
         SOAS_ugm3   = SOAS(   indlon, indlat, 0) * ppb_ugm3 * MW_SOA

         ; Sum up isoprene SOA species (Marais et al. 2016)
         ISOAAQ_ugm3 = SOAGX_ugm3  + SOAIE_ugm3  + $
                       INDIOL_ugm3 + LVOCOA_ugm3
         
         ; Assign GEOS-Chem concentrations for each site
         ; Add scaling of OC to account for non-carbon mass. Colette Heald
         ;  suggests using a factor of 2.1 to be consistent with SOA code,
         ;  but values of 1.4-2.4 can be assumed (mpayer, 11/27/12)

         ; Compute PM2.5 with complex SOA (Pye et al. 2010, Marais et al. 2016)
         gc_mean_c(2, st, mon) = 1.33*( NH4_ugm3+ NIT_ugm3 + SO4_ugm3  ) + $
                                 BCPI_ugm3 + BCPO_ugm3 +                   $
                                 2.1*( OCPO_ugm3 + 1.16*OCPI_ugm3 ) +      $
                                 DST1_ugm3 + 0.38*DST2_ugm3 +              $
                                 1.86*SALA_ugm3 +                          $
                                 1.16*SOA_ugm3 + 1.16*ISOAAQ_ugm3

         ; Compute PM2.5 with simple SOA (Sal Farina, Aerosols WG Chairs)
         gc_mean_s(2, st, mon) = 1.33*( NH4_ugm3+ NIT_ugm3 + SO4_ugm3  ) + $
                                 BCPI_ugm3 + BCPO_ugm3 +                   $
                                 2.1*( OCPO_ugm3 + 1.16*OCPI_ugm3 ) +      $
                                 DST1_ugm3 + 0.38*DST2_ugm3 +              $
                                 1.86*SALA_ugm3 +                          $
                                 1.16*SOAS_ugm3

      endfor

   endfor ; mon

   print, 'Done reading Model 3'

   endif
   
   ;======================================================================
   ; Create the plot (PM2.5 w/ complex SOA)
   ;======================================================================

   ;color plot
   open_device, olddevice,/ps,/color,filename=psname1
   multipanel, rows=4, cols=3

   ; set a blank map background
   modeltype = ctm_type('GEOSFP', res=4)
   modelgrid = ctm_grid(modeltype)
   xmid = modelgrid.xmid
   ymid = modelgrid.ymid
   temp = fltarr(72, 46)
   temp(*, *) = 0.0

   for imod=0, nVersions-1 do begin
      ; Get PM2.5 w/ complex SOA
      gchem = reform(gc_mean_c(imod, *, *))

      maxd = 50
      offset = 0

      for mon=0, 11 do begin

         ; specify arrays
         data = pm25_mean(*, mon)
         gd = where(data gt 0)
         data = reform(data(gd))
         model = reform(gchem(gd, mon))
         diff = data-model

         ; map model, obs and difference
         myct, /WhGrYlRd, ncolors=ncolors
         ctm_overlay, temp(3:46, 25:42), xmid(3:46), ymid(25:42), $
                      model, lon_sta(gd), lat_sta(gd), $
                      t_symbol=1, symsize=symsize, $
                      csfac=0.8, margin=margin, $
                      /nogxlabels, /nogylabels, /sample, $
                      /continents, /coasts, $
                      mindata=0, maxdata=maxd

         ctm_overlay, temp(3:46, 25:42), xmid(3:46), ymid(25:42), $
                      data, lon_sta(gd), lat_sta(gd), $
                      t_symbol=1, symsize=symsize, $
                      csfac=0.8, margin=margin, $
                      /nogxlabels, /nogylabels, /sample, $
                      /continents, /coasts,  $
                      mindata=0, maxdata=maxd

         colorbar, min=0, max=maxd, divisions=6, charsize=1, $
                   position=[0.14, 0.05, 0.5, 0.07], format='(i2)', unit='[!7l!3gm!U-3!N]'

         myct, /BuWhWhRd, ncolors=ncolors

         ctm_overlay, temp(3:46, 25:42), xmid(3:46), ymid(25:42), $
                      diff, lon_sta(gd), lat_sta(gd), $
                      t_symbol=1, symsize=symsize, $
                      csfac=0.8, margin=margin, $
                      /nogxlabels, /nogylabels, /sample, $
                      /continents, /coasts, $
                      mindata=-maxd/2, maxdata=maxd/2

         colorbar, min=-maxd/2, max=maxd/2, divisions=5, charsize=1, $
                   position=[0.7, 0.05, 0.89, 0.07], format='(f5.1)', unit='[!7l!3gm!U-3!N]'


         xyouts, 0.2, 0.93, 'GEOS-Chem PM2.5 w/ complex SOA', color=1, charsize=1.2, /normal, align=0.5
         xyouts, 0.51, 0.93, 'PM2.5 observations', color=1, charsize=1.2, /normal, align=0.5
         xyouts, 0.82, 0.93, 'Obs-Model', color=1, charsize=1.2, /normal, align=0.5

         xyouts, 0.02, 0.8-offset, mmonth(mon), color=1, charsize=1.2, /normal
         xyouts, 0.5, 0.97, modlabels(imod), color=1, charsize=1.2, /normal, align=0.5
       
         offset = offset+0.21
         if mon eq 3 or mon eq 7 or mon eq 11 then offset = 0

      endfor

   endfor

   ; Cleanup & quit 
   close_device, /TIMESTAMP
   
   ;======================================================================
   ; Create the plot (PM2.5 w/ simple SOA)
   ;======================================================================

   ;color plot
   open_device, olddevice,/ps,/color,filename=psname2
   multipanel, rows=4, cols=3

   ; set a blank map background
   modeltype = ctm_type('GEOSFP', res=4)
   modelgrid = ctm_grid(modeltype)
   xmid = modelgrid.xmid
   ymid = modelgrid.ymid
   temp = fltarr(72, 46)
   temp(*, *) = 0.0

   for imod=0, nVersions-1 do begin
      ; Get PM2.5 w/ simple SOA
      gchem = reform(gc_mean_s(imod, *, *))

      maxd = 50
      offset = 0

      for mon=0, 11 do begin

         ; specify arrays
         data = pm25_mean(*, mon)
         gd = where(data gt 0)
         data = reform(data(gd))
         model = reform(gchem(gd, mon))
         diff = data-model

         ; map model, obs and difference
         myct, /WhGrYlRd, ncolors=ncolors
         ctm_overlay, temp(3:46, 25:42), xmid(3:46), ymid(25:42), $
                      model, lon_sta(gd), lat_sta(gd), $
                      t_symbol=1, symsize=symsize, $
                      csfac=0.8, margin=margin, $
                      /nogxlabels, /nogylabels, /sample, $
                      /continents, /coasts, $
                      mindata=0, maxdata=maxd

         ctm_overlay, temp(3:46, 25:42), xmid(3:46), ymid(25:42), $
                      data, lon_sta(gd), lat_sta(gd), $
                      t_symbol=1, symsize=symsize, $
                      csfac=0.8, margin=margin, $
                      /nogxlabels, /nogylabels, /sample, $
                      /continents, /coasts,  $
                      mindata=0, maxdata=maxd

         colorbar, min=0, max=maxd, divisions=6, charsize=1, $
                   position=[0.14, 0.05, 0.5, 0.07], format='(i2)', unit='[!7l!3gm!U-3!N]'

         myct, /BuWhWhRd, ncolors=ncolors

         ctm_overlay, temp(3:46, 25:42), xmid(3:46), ymid(25:42), $
                      diff, lon_sta(gd), lat_sta(gd), $
                      t_symbol=1, symsize=symsize, $
                      csfac=0.8, margin=margin, $
                      /nogxlabels, /nogylabels, /sample, $
                      /continents, /coasts, $
                      mindata=-maxd/2, maxdata=maxd/2

         colorbar, min=-maxd/2, max=maxd/2, divisions=5, charsize=1, $
                   position=[0.7, 0.05, 0.89, 0.07], format='(f5.1)', unit='[!7l!3gm!U-3!N]'


         xyouts, 0.2, 0.93, 'GEOS-Chem PM2.5 w/ simple SOA', color=1, charsize=1.2, /normal, align=0.5
         xyouts, 0.51, 0.93, 'PM2.5 observations', color=1, charsize=1.2, /normal, align=0.5
         xyouts, 0.82, 0.93, 'Obs-Model', color=1, charsize=1.2, /normal, align=0.5

         xyouts, 0.02, 0.8-offset, mmonth(mon), color=1, charsize=1.2, /normal
         xyouts, 0.5, 0.97, modlabels(imod), color=1, charsize=1.2, /normal, align=0.5
       
         offset = offset+0.21
         if mon eq 3 or mon eq 7 or mon eq 11 then offset = 0

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
