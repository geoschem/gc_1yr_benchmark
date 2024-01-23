;-----------------------------------------------------------------------
; NAME:
;        SATELLITE_BRO_CLOUD
;
; PURPOSE:
;        Generate BrO vertical column densities for different latitudinal
;        bands and plot with GOME-2 observations.
;
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR.
;        (2) Theses plots can be compared to Figure 5 of Parrella et al. (2012)
;        (3) Notes from Peter Zoogman on applying Ak in IDL:
;            ````````````````````````````````````````````````
;            ap_temp = alog(apriori)
;            temp3   = alog( transpose(ctm_vertical_profile) )
;            temp3   = ap_temp + AK ## ( transpose(temp3 - ap_temp) )
;            retrieval = exp(temp3)
;
; MODIFICATION HISTORY:
;  19 May 2010 - J. Parrella -  Initial version
;  11 Jul 2012 - M. Payer    -  Modified for use with benchmark_1yr.pro
;
;-----------------------------------------------------------------------
;
; Resolve external routines
@ ./subroutines/list_with_path.pro
@ ./subroutines/break_path.pro
@ ./subroutines/concat_dir.pro

;-----------------------------------------------------------------------

PRO satellite_bro_cloud, BrDir2, Dlat2, Dlon2, Model2, $
                         BrDir3, Dlat3, Dlon3, Model3, $
                         Title,  PSName

;Uncomment later when have 3 runs of bromine output
;PRO satellite_bro_cloud, BrDir1, Dlat1, Dlon1, Model1, $
;                         BrDir2, Dlat2, Dlon2, Model2, $
;                         BrDir3, Dlat3, Dlon3, Model3, $
;                         Title,  PSName

   ;--------------------------
   ; Declare constants
   ;--------------------------
   ; Av : avagadro's number
   Av = 6.0221415d23 ; molecules/mol

   ; Conversion factors
   ppb2vv = 1e-9     ; convert ppbv to the actual mole/mole
   cm2m   = 1e-2     ; centemeters to meters
   
   ; Molecular weight of BrO
   MWbro = 0.096     ; kg/mol

   ;-------------------------------------------------------
   ; Get info on GEOS-Chem grid depending on type
   ;-------------------------------------------------------   
   ; Get MODELINFO structure
   ; Now hardwire Model, so the routine can read the offline data
   ;ModelInfo = CTM_Type( Model2, Res=[ DLon3, DLat3 ] )
   ModelInfo = CTM_Type( 'GEOSFP_47L', Res=[ DLon3, DLat3 ] )

   ; Get CTM grid information
   ymid  = CTM_Grid(ModelInfo, /YMID)
   xmid  = CTM_Grid(ModelInfo, /XMID)
   zmid  = CTM_Grid(ModelInfo, /ZMID)
   zedge = CTM_Grid(ModelInfo, /Zedge)

   nlat  = n_elements(ymid)
   nlon  = n_elements(xmid)
   nalt  = n_elements(zmid)

   ; ---------------------------------------
   ; Get the ctm box sizes [m3]
   ; ---------------------------------------
   GridInfo = ctm_grid(ModelInfo)
   vol = ctm_boxsize(GridInfo, /volume, /m3)

   ; ------------------------------
   ; Read GOME-2 AK from N. Theys
   ; 
   ; Columns:
   ; 1. altitude (km)
   ; 2. AK1 = SZA45, albedo=6%  ; apply everywhere else
   ; 3. AK2 = SZA80, albedo=6%
   ; 4. AK3 = SZA45, albedo=80%
   ; 5. AK4 = SZA80, albedo=80% ; apply to Arctic and Antarctic
   ; ------------------------------
   readdata, 'data/BrO/BrO_AK.dat', ak, header, delim=' '
   ak_alt           = reform( ak[0, *] ) ; km
   ak_pol1          = reform( ak[4, *] )
;  ak_pol2          = reform( ak[2, *] ) ; summertime
   ak_arctic_summer = reform( ak[2, *] ) ; apply to arctic summer
   ak_other         = reform( ak[1, *] ) ; reform( ak[1, *] )

   ; ------------------------------
   ; Read cloudy AKs as well
   ; ------------------------------
   cakd = 'data/BrO/BrO_AK_cloud/'
   ; 0km cth
   readdata, cakd+'BrO_AK_cloud0.dat', ak, header, delim=' '
   akc0_alt = reform( ak[0, *] )
   akc0_45  = reform( ak[1, *] )
   akc0_80  = reform( ak[2, *] )
   ; 1km cth
   readdata, cakd+'BrO_AK_cloud1.dat', ak, header, delim=' '
   akc1_alt = reform( ak[0, *] )
   akc1_45  = reform( ak[1, *] )
   akc1_80  = reform( ak[2, *] )
   ; 2km cth
   readdata, cakd+'BrO_AK_cloud2.dat', ak, header, delim=' '
   akc2_alt = reform( ak[0, *] )
   akc2_45  = reform( ak[1, *] )
   akc2_80  = reform( ak[2, *] )
   ; 3km cth
   readdata, cakd+'BrO_AK_cloud3.dat', ak, header, delim=' '
   akc3_alt = reform( ak[0, *] )
   akc3_45  = reform( ak[1, *] )
   akc3_80  = reform( ak[2, *] )
   ; 4km cth
   readdata, cakd+'BrO_AK_cloud4.dat', ak, header, delim=' '
   akc4_alt = reform( ak[0, *] )
   akc4_45  = reform( ak[1, *] )
   akc4_80  = reform( ak[2, *] )
   ; 5km cth
   readdata, cakd+'BrO_AK_cloud5.dat', ak, header, delim=' '
   akc5_alt = reform( ak[0, *] )
   akc5_45  = reform( ak[1, *] )
   akc5_80  = reform( ak[2, *] )

   ; ---------------------------------------
   ; Select bpch files
   ; ---------------------------------------
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Hardwire file path to point to Justin's v8-02-02 run for now
; Uncomment these lines when more runs with bromine output are available
;   ; Model 1
;   dir1 = BrDir1
;   gc_file1 = list_with_path('ts_satellite.2005*', dir1)
;
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;   ; Model 1: v8-02-02 run from Parella et al. (2012)
;   dir1 = '/home/jpp/bromine/v.8-02-02/present-day/satellite_comps/v8-02-02.Br.present_satellite_allHet_final/'
;   gc_file1 = list_with_path('ts_satellite.2007*', dir1)

;   dir1b = '/home/jpp/bromine/v.8-02-02/present-day/satellite_comps/rime_50p_hobr_br2_satellite/'
;   gc_file1b = list_with_path('ts_satellite.2007*', dir1b)

   ; Model 2
   dir2 = BrDir2
   gc_file2 = list_with_path('ts_satellite.*', dir2)

   ; Model 3
   dir3 = BrDir3
   gc_file3 = list_with_path('ts_satellite.*', dir3)

   ; -------------------------------
   ; Number of days for each month
   ; -------------------------------
   ; Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
   ndays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

   ; ---------------------------------------
   ; Restore some variables from GC output
   ; b/c they aren't stored in the 9am - 5pm
   ; output.
   ; ---------------------------------------
   ; Find the air number density [#/m3]
   restore, 'data/BrO/nair_2007.sav'

   ; Find the mass of air per box [kg/box]
   restore, 'data/BrO/airmass_2007.sav'

   ; Tropopause heights [km]
   restore, 'data/BrO/tph_2007.sav'

   ; Restore the box heights [m]
   restore, 'data/BrO/boxhgt_2007.sav'

   ; Restore the pressure edge [hPa]
   restore, 'data/BrO/pedge_2007.sav'

   ; Restore the fraction of time each box spends in the troposphere
   ; to make sure we are only integrating over tropospheric boxes.
   ; Reduce contamination... because ND45 (tracer conc's) does not
   ; exclude stratospheric air and there's no chemistry there.
   restore, 'data/BrO/timetrop_2007.sav'

   ; --------------------------------------
   ; Read in the p-TOMCAT BrO columns for
   ; same spatial averaging
   ; --------------------------------------
   readdata, 'data/BrO/tomcat_trop_column_BrO_latmean_new.dat', $
             tomcat, /noheader, cols=6
   tomcat = temporary(tomcat * 1.d13)

   ; --------------------------------------
   ; Read in the GOME-2 Tropospheric
   ; columns.
   ; --------------------------------------
   nskip = 7
   readdata, 'data/BrO/theys2010_BrO.dat', g2_bro, skp1=nskip, $
             /noheader, cols=12, lines=6
   nskip = nskip+6+3
   readdata, 'data/BrO/theys2010_BrO.dat', g2_bro_sd, skp1=nskip, $
             /noheader, cols=12, lines=6
   nskip = nskip+6+3
   readdata, 'data/BrO/theys2010_BrO.dat', g2_bro_er, skp1=nskip, $
             /noheader, cols=12, lines=6

   g2_bro    = temporary(g2_bro    * 1.d13)
   g2_bro_sd = temporary(g2_bro_sd * 1.d13)
   g2_bro_er = temporary(g2_bro_er * 1.d13)

   ; Declare column arrays
;   bro1_column  = make_array( nlon, nlat, 12, /double, value=0.d0)
  bro2_column  = make_array( nlon, nlat, 12, /double, value=0.d0)
   bro3_column  = make_array( nlon, nlat, 12, /double, value=0.d0)
   column_count = make_array( nlon, nlat, 12, /double, value=0.d0)

;   bro1_col_60_90N = make_array( 12, /double, value=0.d0)
;   bro1_col_30_60N = make_array( 12, /double, value=0.d0)
;   bro1_col_0_30N  = make_array( 12, /double, value=0.d0)
;   bro1_col_60_90S = make_array( 12, /double, value=0.d0)
;   bro1_col_30_60S = make_array( 12, /double, value=0.d0)
;   bro1_col_0_30S  = make_array( 12, /double, value=0.d0)

   bro2_col_60_90N = make_array( 12, /double, value=0.d0)
   bro2_col_30_60N = make_array( 12, /double, value=0.d0)
   bro2_col_0_30N  = make_array( 12, /double, value=0.d0)
   bro2_col_60_90S = make_array( 12, /double, value=0.d0)
   bro2_col_30_60S = make_array( 12, /double, value=0.d0)
   bro2_col_0_30S  = make_array( 12, /double, value=0.d0)

   bro3_col_60_90N = make_array( 12, /double, value=0.d0)
   bro3_col_30_60N = make_array( 12, /double, value=0.d0)
   bro3_col_0_30N  = make_array( 12, /double, value=0.d0)
   bro3_col_60_90S = make_array( 12, /double, value=0.d0)
   bro3_col_30_60S = make_array( 12, /double, value=0.d0)
   bro3_col_0_30S  = make_array( 12, /double, value=0.d0)

   ; And locations
   i60_90N = where(   ymid ge  60.0 )
   i30_60N = where( ( ymid ge  30.0 ) and ( ymid lt  60.0 ) )
   i0_30N  = where( ( ymid ge   0.0 ) and ( ymid lt  30.0 ) )
   i0_30S  = where( ( ymid gt -30.0 ) and ( ymid le   0.0 ) )
   i30_60S = where( ( ymid gt -60.0 ) and ( ymid le -30.0 ) )
   i60_90S = where(   ymid le -60.0 )

   ; Cloud fractions for each day
   cf_day    = make_array( nlon, nlat, nalt, 12, 31, /double, value=0.d0)
   bro_day   = make_array( nlon, nlat, nalt, 12, 31, /double, value=0.d0)
   count_day = make_array( nlon, nlat, nalt, /double, value=0.d0)
   count2    = make_array( nlon, nlat, nalt, /double, value=0.d0)

;   cf1_temp  = make_array( nlon, nlat, nalt, /double, value=0.d0)
   cf2_temp  = make_array( nlon, nlat, nalt, /double, value=0.d0)
   cf3_temp  = make_array( nlon, nlat, nalt, /double, value=0.d0)

;   cth1_temp = make_array( nlon, nlat, 1, /double, value=0.d0)
   cth2_temp = make_array( nlon, nlat, 1, /double, value=0.d0)
   cth3_temp = make_array( nlon, nlat, 1, /double, value=0.d0)

;   cth1_km   = make_array( nlon, nlat, 12, 31, /double, value=0.d0)
   cth2_km   = make_array( nlon, nlat, 12, 31, /double, value=0.d0)
   cth3_km   = make_array( nlon, nlat, 12, 31, /double, value=0.d0)

;   bro1_temp = make_array( nlon, nlat, nalt, /double, value=0.d0)
   bro2_temp = make_array( nlon, nlat, nalt, /double, value=0.d0)
   bro3_temp = make_array( nlon, nlat, nalt, /double, value=0.d0)

;   ak1_cloud  = make_array( nalt, /double, value=0.d0)
   ak2_cloud  = make_array( nalt, /double, value=0.d0)
   ak3_cloud  = make_array( nalt, /double, value=0.d0)

   ; ---------------------------------------
   ; Now loop over each month and get the
   ; average columns for specific regions
   ; following regions chosen by
   ; Theys et al. 2010 in ACPD (figure 14)
   ; ---------------------------------------
   ifile = -1
   for imon=0, 11 do begin

      pedge = reform(pedge_2007[*, *, *, imon])

      for iday=0, ndays[imon]-1 do begin

         ; Avoid running out of file units
         ctm_cleanup, /no_gc

         ; Count the files
         ifile =  ifile + 1

         ;-----------------------------------------------------
         ; 1. Restore bromine file - Model 1
         ;-----------------------------------------------------
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
; Use v8-02-02 run from from Parrella et al. (2012) for now
; Uncomment these lines when more runs with bromine output are available
;         print,  gc_file1[ifile]
;         ctm_get_data, temp, file=gc_file1[ifile]
;
;         ; extract BrO in ppb
;         ptr      = temp.data
;         category = temp.category
;         tnames   = temp.tracername
;         units    = temp.unit
;
;         ; find BrO concentrations
;         ibro1 = where( (category eq 'IJ-AVG-$') and $
;                        (tnames   eq 'BrO')            )
;         icf1  = where( (category eq 'TIME-SER') and $
;                        (tnames   eq 'CF')           ) 
;         icth1 = where( (category eq 'TIME-SER') and $
;                        (tnames   eq 'CThgt')         ) 
;
;         ; store daytime BrO in ppb
;         Print, 'ibro=', ibro1
;         Print, 'nalt=', nalt
;         bro1_temp[*, *, 0:39] = *(( ptr[ibro1])[0])
;         cf1_temp [*, *, 0:39] = *(( ptr[icf1 ])[0])
;         cth1_temp[*, *, 0   ] = *(( ptr[icth1])[0]) ; in hPa
;
;         ; free up space
;         undefine, temp
;         undefine, ptr
;         undefine, category
;         undefine, tnames
;         undefine, units
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;         print,  gc_file1[ifile]
;         ctm_get_data, temp, file=gc_file1[ifile]
;
;         ; Extract BrO in ppb
;         ptr      = temp.data
;         category = temp.category
;         tnames   = temp.tracername
;         units    = temp.unit
;
;         ; Find BrO concentrations
;         ibro1 = where( (category eq 'IJ-AVG-$') and $
;                        (tnames   eq 'BrO')            )
;
;         ; Store daytime BrO in ppb
;         Print, 'ibro=', ibro1
;         Print, 'nalt=', nalt
;         bro1_temp[*, *, 0:39] = *(( ptr[ibro1])[0])
;
;         ; Free up space
;         undefine, temp
;         undefine, ptr
;         undefine, category
;         undefine, tnames
;         undefine, units
;
;         ;-----------------------------------------------------
;         ; 1b. Get cloud information (v8-02-02)
;         ;-----------------------------------------------------
;         print,  gc_file1b[ifile]
;         ctm_get_data, temp, file=gc_file1b[ifile]
;         
;         ; Extract BrO in ppb
;         ptr      = temp.data
;         category = temp.category
;         tnames   = temp.tracername
;         units    = temp.unit
;
;         ; Find BrO cloud info
;         icf1  = where( (category eq 'TIME-SER') and $
;                        (tnames   eq 'CF')           ) 
;         icth1 = where( (category eq 'TIME-SER') and $
;                        (tnames   eq 'CThgt')         ) 
;
;         cf1_temp [*, *, 0:39] = *(( ptr[icf1])[0])
;         cth1_temp[*, *, 0]    = *(( ptr[icth1])[0]) ; in hPa
;
;         ; Free up space
;         undefine, temp
;         undefine, ptr
;         undefine, category
;         undefine, tnames
;         undefine, units 

         ;-----------------------------------------------------
         ; 2. Restore bromine file - Model 2
         ;-----------------------------------------------------
         print, 'FILENAME=', gc_file2[ifile]
         ctm_get_data, temp, file=gc_file2[ifile]

         ; Extract BrO in ppb
         ptr      = temp.data
         category = temp.category
         tnames   = temp.tracername
         units    = temp.unit

         ; Find BrO concentrations
         ibro2 = where( (category eq 'IJ-AVG-$') and $
                        (tnames   eq 'BrO')            )
         icf2  = where( (category eq 'TIME-SER') and $
                        (tnames   eq 'CF')           ) 
         icth2 = where( (category eq 'TIME-SER') and $
                        (tnames   eq 'CThgt')         ) 

         ; Store daytime BrO in ppb
         ;Print, 'ibro=', ibro2
         ;Print, 'nalt=', nalt
         bro2_temp[*, *, 0:39] = *(( ptr[ibro2])[0])
         cf2_temp [*, *, 0:39] = *(( ptr[icf2])[0])
         cth2_temp[*, *, 0]    = *(( ptr[icth2])[0]) ; in hPa

         ; Free up space
         undefine, temp
         undefine, ptr
         undefine, category
         undefine, tnames
         undefine, units

         ;-----------------------------------------------------
         ; 3. Restore bromine file - Model 3
         ;-----------------------------------------------------
         print, 'FILENAME=', gc_file3[ifile]
         ctm_get_data, temp, file=gc_file3[ifile]

         ; Extract BrO in ppb
         ptr      = temp.data
         category = temp.category
         tnames   = temp.tracername
         units    = temp.unit

         ; Find BrO concentrations
         ibro3 = where( (category eq 'IJ-AVG-$') and $
                        (tnames   eq 'BrO')            )
         icf3  = where( (category eq 'TIME-SER') and $
                        (tnames   eq 'CF')           ) 
         icth3 = where( (category eq 'TIME-SER') and $
                        (tnames   eq 'CThgt')         ) 

         ; Store daytime BrO in ppb
         Print, 'ibro=', ibro3
         Print, 'nalt=', nalt
         bro3_temp[*, *, 0:39] = *(( ptr[ibro3])[0])
         cf3_temp [*, *, 0:39] = *(( ptr[icf3])[0])
         cth3_temp[*, *, 0]    = *(( ptr[icth3])[0]) ; in hPa

         ; Free up space
         undefine, temp
         undefine, ptr
         undefine, category
         undefine, tnames
         undefine, units

         ;-----------------------------------------------------

         ; -------------------------------
         ; Box heights this month in (km)
         ; -------------------------------
         bxhgt = total( reform(boxhgt_2007[*, *, *, imon]), 3, /cumulative) / 1.d3

         ; 5. now calculate the number densities for BrO
         nair     = nair_2007[*, *, *, imon] ; #/m3
;         bro1_nd  = double(bro1_temp * 1.e-9 * nair * vol)  ; # molecules/box
;         bro1_nd2 = double(bro1_temp * 1.e-9 * nair)        ; # molecules/m3

         bro2_nd  = double(bro2_temp * 1.e-9 * nair * vol)  ; # molecules/box
         bro2_nd2 = double(bro2_temp * 1.e-9 * nair)        ; # molecules/m3

         bro3_nd  = double(bro3_temp * 1.e-9 * nair * vol)  ; # molecules/box
         bro3_nd2 = double(bro3_temp * 1.e-9 * nair)        ; # molecules/m3

         ; ------------------------------
         ; Loop over the lat and lon
         ; ------------------------------
         for ilat=0, nlat-1 do begin
            for ilon=0, nlon-1 do begin 

               ; Model altitude grid (km) (in cumulative heights)
               mod_alt = total( boxhgt_2007[ilon, ilat, *, imon], /cumulative) $
                         / 1.d3

               alt_mid = total( boxhgt_2007[ilon, ilat, *, imon]/2.d0, /cumulative) $
                         / 1.d3

               ; Model box edges
               bxh_tmp = [0, reform(bxhgt[ilon, ilat, *])]   ; km
               p_tmp   = reform(pedge[ilon, ilat, *])        ; hPa

               ; Cloud top pressure
;               ctp1_tmp = reform( cth1_temp[ilon, ilat, 0] ) ; hPa
               ctp2_tmp = reform( cth2_temp[ilon, ilat, 0] ) ; hPa
               ctp3_tmp = reform( cth3_temp[ilon, ilat, 0] ) ; hPa

               ; ---------------------------------------------
               ; interpolate for the cloud top altitudes (km)
               ; ---------------------------------------------
;               cth1_tmp = interpol(bxh_tmp, alog(p_tmp), alog(ctp1_tmp))
               cth2_tmp = interpol(bxh_tmp, alog(p_tmp), alog(ctp2_tmp))
               cth3_tmp = interpol(bxh_tmp, alog(p_tmp), alog(ctp3_tmp))

;               cth1_km[ilon, ilat, imon, iday] = cth1_tmp[0] ; km
               cth2_km[ilon, ilat, imon, iday] = cth2_tmp[0] ; km
               cth3_km[ilon, ilat, imon, iday] = cth3_tmp[0] ; km

               ; Find altitude of the 400 hpa point
               km_400hpa = interpol(bxh_tmp, alog(p_tmp), alog(400.0d0))

               ; Get the model box midpoints (km)
               alt_mid = reform(boxhgt_2007[ilon, ilat, *, imon]/2.d0)/1.d3 $
                         + bxh_tmp[0:nalt-1]

               ; Find the column Cf as the maximum CF of these boxes
               ; below 400 hPa
               ialt_cf = where( bxh_tmp[1:nalt] le km_400hpa, it)
               if (it < 0) then continue

               ; Now calculate the max CF
;               column1_cf = max(cf1_temp[ilon, ilat, ialt_cf])
               column2_cf = max(cf2_temp[ilon, ilat, ialt_cf])
               column3_cf = max(cf3_temp[ilon, ilat, ialt_cf])

;               ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;               ; MODEL 1
;               ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;               ; ---------------------------------------------
;               ; Cycle if the Cloud Top Height < 400 hPa
;               ;    or if the Cloud fraction is > 0.4 *
;               ;     * need to figure out conversion of box cf to column
;               ; ---------------------------------------------
;               if (ctp1_tmp    lt 400.d0) then continue
;               if (column1_cf  ge 0.4   ) then continue
;               if (cth1_tmp[0] gt 5.0   ) then continue ; no cloudy AK above 5km
;
;               ; ------------------------------------------
;               ; Deal with the GOME2 averaging kernel
;               ; ------------------------------------------
;               ; Select clear-sky AK
;               if ( abs(ymid[ilat]) gt 60.0 ) then begin
;                  ak_clear = ak_pol1
;               endif else begin
;                  ak_clear = ak_other
;               endelse
;           
;               ; Reduce albedo in arctic if it's summertime
;               if ( (ymid[ilat] gt 60.0) and (imon ge 5) and $
;                    (imon le 7) ) then begin
;                  ak_clear = ak_arctic_summer
;               endif
;
;               ; ---------------------------------------------
;               ; Check to make sure that the cloud top is not
;               ; at the surface, or lower than the ground
;               ; ---------------------------------------------
;               if ( (ctp1_tmp[0]      ge p_tmp[0]         ) or $
;                    (abs(cth1_tmp[0]) le 1.e-3            ) or $
;                    (fix(p_tmp[0])    eq fix(ctp1_tmp[0]) ) or $
;                    (cth1_tmp[0]      le 0.0 )            ) then begin
;
;                  ; ------------------------------------------
;                  ; Just use the clear AK if no cloud present
;                  ; ------------------------------------------
;                  ak1_model = reform(interpol(ak_clear, ak_alt, alt_mid))
;
;               endif else begin ; deal with clouds if present
;                  
;                  ; --------------------------------------
;                  ; 1. Select the cloudy AKs below and
;                  ;    above the model cloud top height
;                  ; --------------------------------------
;                  if ( (cth1_tmp[0] gt 0.d0)   and $
;                       (cth1_tmp[0] le 1.d0) ) then begin
;                     cld_lo_km = 0.d0
;                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
;                        akc_lo = akc0_80
;                        akc_la = akc0_alt ; km
;                        akc_hi = akc1_80
;                        akc_ha = akc1_alt ; km
;                     endif else begin
;                        akc_lo = akc0_45
;                        akc_la = akc0_alt ; km
;                        akc_hi = akc1_45
;                        akc_ha = akc1_alt ; km
;                     endelse
;                  endif else if ( (cth1_tmp[0] gt 1.d0)   and $
;                                  (cth1_tmp[0] le 2.d0) ) then begin
;                     cld_lo_km = 1.d0
;                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
;                        akc_lo = akc1_80
;                        akc_la = akc1_alt ; km
;                        akc_hi = akc2_80
;                        akc_ha = akc2_alt ; km
;                     endif else begin
;                        akc_lo = akc1_45
;                        akc_la = akc1_alt ; km
;                        akc_hi = akc2_45
;                        akc_ha = akc2_alt ; km
;                     endelse
;                  endif else if ( (cth1_tmp[0] gt 2.d0)   and $
;                                  (cth1_tmp[0] le 3.d0) ) then begin
;                     cld_lo_km = 2.d0
;                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
;                        akc_lo = akc2_80
;                        akc_la = akc2_alt ; km
;                        akc_hi = akc3_80
;                        akc_ha = akc3_alt ; km
;                     endif else begin
;                        akc_lo = akc2_45
;                        akc_la = akc2_alt ; km
;                        akc_hi = akc3_45
;                        akc_ha = akc3_alt ; km
;                     endelse
;                  endif else if ( (cth1_tmp[0] gt 3.d0)   and $
;                                  (cth1_tmp[0] le 4.d0) ) then begin
;                     cld_lo_km = 3.d0
;                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
;                        akc_lo = akc3_80
;                        akc_la = akc3_alt ; km
;                        akc_hi = akc4_80
;                        akc_ha = akc4_alt ; km
;                     endif else begin
;                        akc_lo = akc3_45
;                        akc_la = akc3_alt ; km
;                        akc_hi = akc4_45
;                        akc_ha = akc4_alt ; km
;                     endelse
;                  endif else if ( (cth1_tmp[0] gt 4.d0)   and $
;                                  (cth1_tmp[0] le 5.d0) ) then begin
;                     cld_lo_km = 4.d0
;                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
;                        akc_lo = akc4_80
;                        akc_la = akc4_alt ; km
;                        akc_hi = akc5_80
;                        akc_ha = akc5_alt ; km
;                     endif else begin
;                        akc_lo = akc4_45
;                        akc_la = akc4_alt ; km
;                        akc_hi = akc5_45
;                        akc_ha = akc5_alt ; km
;                     endelse
;                  endif 
;
;                  ; --------------------------------------
;                  ; 2. Interpolate both AKs to a regular
;                  ;    altitude grid
;                  ; --------------------------------------
;                  akc_lo_mod = reform(interpol(akc_lo, akc_la, alt_mid))
;                  akc_hi_mod = reform(interpol(akc_hi, akc_ha, alt_mid))
;
;                  ; --------------------------------------
;                  ; 2. Interpolate both upper and lower
;                  ;    AKs to the model altitude grid
;                  ; --------------------------------------
;                  dadz = (akc_hi_mod - akc_lo_mod)        ; / 1.0 km
;                  dz   = reform(cth1_tmp[0]  - cld_lo_km) ; km difference
;                  ak1_cloud = akc_lo_mod + dz[0] * dadz
;                  ; Set all AKs below the cloud to zero
;                  ilo = where(mod_alt lt cth1_tmp[0], it)
;                  if (it gt 0) then ak1_cloud[ilo] = 0.d0
;
;                  ; Test
;                  if ( dz gt 0.5 ) then begin 
;                     ak1_cloud = akc_hi_mod
;                  endif else begin
;                     ak1_cloud = akc_lo_mod
;                  endelse
;                  
;                  ; ------------------------------------------
;                  ; Interpolate clear AK and then calc the
;                  ; weighted average for a mixed scene
;                  ; ------------------------------------------
;                  ak_clear_model = reform(interpol(ak_clear, ak_alt, alt_mid))
;                  
;                  ak1_model = column1_cf * ak1_cloud + $
;                             (1.d0 - column1_cf) * ak_clear_model
;
;               endelse 

               ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               ; MODEL 2
               ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               ; ---------------------------------------------
               ; Cycle if the Cloud Top Height < 400 hPa
               ;    or if the Cloud fraction is > 0.4 *
               ;     * need to figure out conversion of box cf to column
               ; ---------------------------------------------
               if (ctp2_tmp    lt 400.d0) then continue
               if (column2_cf  ge 0.4   ) then continue
               if (cth2_tmp[0] gt 5.0   ) then continue ; no cloudy AK above 5km

               ; ------------------------------------------
               ; Deal with the GOME2 averaging kernel
               ; ------------------------------------------
               ; Select clear-sky AK
               if ( abs(ymid[ilat]) gt 60.0 ) then begin
                  ak_clear = ak_pol1
               endif else begin
                  ak_clear = ak_other
               endelse
           
               ; Reduce albedo in arctic if it's summertime
               if ( (ymid[ilat] gt 60.0) and (imon ge 5) and $
                    (imon le 7) ) then begin
                  ak_clear = ak_arctic_summer
               endif

               ; ---------------------------------------------
               ; Check to make sure that the cloud top is not
               ; at the surface, or lower than the ground
               ; ---------------------------------------------
               if ( (ctp2_tmp[0]      ge p_tmp[0]         ) or $
                    (abs(cth2_tmp[0]) le 1.e-3            ) or $
                    (fix(p_tmp[0])    eq fix(ctp2_tmp[0]) ) or $
                    (cth2_tmp[0]      le 0.0 )            ) then begin

                  ; ------------------------------------------
                  ; Just use the clear AK if no cloud present
                  ; ------------------------------------------
                  ak2_model = reform(interpol(ak_clear, ak_alt, alt_mid))

               endif else begin ; deal with clouds if present
                  
                  ; --------------------------------------
                  ; 1. Select the cloudy AKs below and
                  ;    above the model cloud top height
                  ; --------------------------------------
                  if ( (cth2_tmp[0] gt 0.d0)   and $
                       (cth2_tmp[0] le 1.d0) ) then begin
                     cld_lo_km = 0.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc0_80
                        akc_la = akc0_alt ; km
                        akc_hi = akc1_80
                        akc_ha = akc1_alt ; km
                     endif else begin
                        akc_lo = akc0_45
                        akc_la = akc0_alt ; km
                        akc_hi = akc1_45
                        akc_ha = akc1_alt ; km
                     endelse
                  endif else if ( (cth2_tmp[0] gt 1.d0)   and $
                                  (cth2_tmp[0] le 2.d0) ) then begin
                     cld_lo_km = 1.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc1_80
                        akc_la = akc1_alt ; km
                        akc_hi = akc2_80
                        akc_ha = akc2_alt ; km
                     endif else begin
                        akc_lo = akc1_45
                        akc_la = akc1_alt ; km
                        akc_hi = akc2_45
                        akc_ha = akc2_alt ; km
                     endelse
                  endif else if ( (cth2_tmp[0] gt 2.d0)   and $
                                  (cth2_tmp[0] le 3.d0) ) then begin
                     cld_lo_km = 2.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc2_80
                        akc_la = akc2_alt ; km
                        akc_hi = akc3_80
                        akc_ha = akc3_alt ; km
                     endif else begin
                        akc_lo = akc2_45
                        akc_la = akc2_alt ; km
                        akc_hi = akc3_45
                        akc_ha = akc3_alt ; km
                     endelse
                  endif else if ( (cth2_tmp[0] gt 3.d0)   and $
                                  (cth2_tmp[0] le 4.d0) ) then begin
                     cld_lo_km = 3.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc3_80
                        akc_la = akc3_alt ; km
                        akc_hi = akc4_80
                        akc_ha = akc4_alt ; km
                     endif else begin
                        akc_lo = akc3_45
                        akc_la = akc3_alt ; km
                        akc_hi = akc4_45
                        akc_ha = akc4_alt ; km
                     endelse
                  endif else if ( (cth2_tmp[0] gt 4.d0)   and $
                                  (cth2_tmp[0] le 5.d0) ) then begin
                     cld_lo_km = 4.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc4_80
                        akc_la = akc4_alt ; km
                        akc_hi = akc5_80
                        akc_ha = akc5_alt ; km
                     endif else begin
                        akc_lo = akc4_45
                        akc_la = akc4_alt ; km
                        akc_hi = akc5_45
                        akc_ha = akc5_alt ; km
                     endelse
                  endif 

                  ; --------------------------------------
                  ; 2. Interpolate both AKs to a regular
                  ;    altitude grid
                  ; --------------------------------------
                  akc_lo_mod = reform(interpol(akc_lo, akc_la, alt_mid))
                  akc_hi_mod = reform(interpol(akc_hi, akc_ha, alt_mid))

                  ; --------------------------------------
                  ; 2. Interpolate both upper and lower
                  ;    AKs to the model altitude grid
                  ; --------------------------------------
                  dadz = (akc_hi_mod - akc_lo_mod)        ; / 1.0 km
                  dz   = reform(cth2_tmp[0]  - cld_lo_km) ; km difference
                  ak2_cloud = akc_lo_mod + dz[0] * dadz
                  ; Set all AKs below the cloud to zero
                  ilo = where(mod_alt lt cth2_tmp[0], it)
                  if (it gt 0) then ak2_cloud[ilo] = 0.d0

                  ; Test
                  if ( dz gt 0.5 ) then begin 
                     ak2_cloud = akc_hi_mod
                  endif else begin
                     ak2_cloud = akc_lo_mod
                  endelse
                  
                  ; ------------------------------------------
                  ; Interpolate clear AK and then calc the
                  ; weighted average for a mixed scene
                  ; ------------------------------------------
                  ak_clear_model = reform(interpol(ak_clear, ak_alt, alt_mid))
                  
                  ak2_model = column2_cf * ak2_cloud + $
                             (1.d0 - column2_cf) * ak_clear_model

               endelse 

               ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               ; MODEL 3
               ;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
               ; ---------------------------------------------
               ; Cycle if the Cloud Top Height < 400 hPa
               ;    or if the Cloud fraction is > 0.4 *
               ;     * need to figure out conversion of box cf to column
               ; ---------------------------------------------
               if (ctp3_tmp    lt 400.d0) then continue
               if (column3_cf  ge 0.4   ) then continue
               if (cth3_tmp[0] gt 5.0   ) then continue ; no cloudy AK above 5km

               ; ------------------------------------------
               ; Deal with the GOME2 averaging kernel
               ; ------------------------------------------
               ; Select clear-sky AK
               if ( abs(ymid[ilat]) gt 60.0 ) then begin
                  ak_clear = ak_pol1
               endif else begin
                  ak_clear = ak_other
               endelse
          
               ; Reduce albedo in arctic if it's summertime
               if ( (ymid[ilat] gt 60.0) and (imon ge 5) and $
                    (imon le 7) ) then begin
                  ak_clear = ak_arctic_summer
               endif

               ; ---------------------------------------------
               ; Check to make sure that the cloud top is not
               ; at the surface, or lower than the ground
               ; ---------------------------------------------
               if ( (ctp3_tmp[0]      ge p_tmp[0]         ) or $
                    (abs(cth3_tmp[0]) le 1.e-3            ) or $
                    (fix(p_tmp[0])    eq fix(ctp3_tmp[0]) ) or $
                    (cth3_tmp[0]      le 0.0 )            ) then begin

                  ; ------------------------------------------
                  ; Just use the clear AK if no cloud present
                  ; ------------------------------------------
                  ak3_model = reform(interpol(ak_clear, ak_alt, alt_mid))

               endif else begin ; deal with clouds if present
                  
                  ; --------------------------------------
                  ; 1. Select the cloudy AKs below and
                  ;    above the model cloud top height
                  ; --------------------------------------
                  if ( (cth3_tmp[0] gt 0.d0)   and $
                       (cth3_tmp[0] le 1.d0) ) then begin
                     cld_lo_km = 0.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc0_80
                        akc_la = akc0_alt ; km
                        akc_hi = akc1_80
                        akc_ha = akc1_alt ; km
                     endif else begin
                        akc_lo = akc0_45
                        akc_la = akc0_alt ; km
                        akc_hi = akc1_45
                        akc_ha = akc1_alt ; km
                     endelse
                  endif else if ( (cth3_tmp[0] gt 1.d0)   and $
                                  (cth3_tmp[0] le 2.d0) ) then begin
                     cld_lo_km = 1.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc1_80
                        akc_la = akc1_alt ; km
                        akc_hi = akc2_80
                        akc_ha = akc2_alt ; km
                     endif else begin
                        akc_lo = akc1_45
                        akc_la = akc1_alt ; km
                        akc_hi = akc2_45
                        akc_ha = akc2_alt ; km
                     endelse
                  endif else if ( (cth3_tmp[0] gt 2.d0)  and $
                                 (cth3_tmp[0] le 3.d0) ) then begin
                     cld_lo_km = 2.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc2_80
                        akc_la = akc2_alt ; km
                        akc_hi = akc3_80
                        akc_ha = akc3_alt ; km
                     endif else begin
                        akc_lo = akc2_45
                        akc_la = akc2_alt ; km
                        akc_hi = akc3_45
                        akc_ha = akc3_alt ; km
                     endelse
                  endif else if ( (cth3_tmp[0] gt 3.d0)   and $
                                  (cth3_tmp[0] le 4.d0) ) then begin
                     cld_lo_km = 3.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc3_80
                        akc_la = akc3_alt ; km
                        akc_hi = akc4_80
                        akc_ha = akc4_alt ; km
                     endif else begin
                        akc_lo = akc3_45
                        akc_la = akc3_alt ; km
                        akc_hi = akc4_45
                        akc_ha = akc4_alt ; km
                     endelse
                  endif else if ( (cth3_tmp[0] gt 4.d0)   and $
                                  (cth3_tmp[0] le 5.d0) ) then begin
                     cld_lo_km = 4.d0
                     if ( abs(ymid[ilat]) gt 60.0 ) then begin
                        akc_lo = akc4_80
                        akc_la = akc4_alt ; km
                        akc_hi = akc5_80
                        akc_ha = akc5_alt ; km
                     endif else begin
                        akc_lo = akc4_45
                        akc_la = akc4_alt ; km
                        akc_hi = akc5_45
                        akc_ha = akc5_alt ; km
                     endelse
                  endif 

                  ; --------------------------------------
                  ; 2. Interpolate both AKs to a regular
                  ;    altitude grid
                  ; --------------------------------------
                  akc_lo_mod = reform(interpol(akc_lo, akc_la, alt_mid))
                  akc_hi_mod = reform(interpol(akc_hi, akc_ha, alt_mid))

                  ; --------------------------------------
                  ; 2. Interpolate both upper and lower
                  ;    AKs to the model altitude grid
                  ; --------------------------------------
                  dadz = (akc_hi_mod - akc_lo_mod)        ; / 1.0 km
                  dz   = reform(cth3_tmp[0]  - cld_lo_km) ; km difference
                  ak3_cloud = akc_lo_mod + dz[0] * dadz
                  ; Set all AKs below the cloud to zero
                  ilo = where(mod_alt lt cth3_tmp[0], it)
                  if (it gt 0) then ak3_cloud[ilo] = 0.d0

                  ; Test
                  if ( dz gt 0.5 ) then begin 
                     ak3_cloud = akc_hi_mod
                  endif else begin
                     ak3_cloud = akc_lo_mod
                  endelse
                  
                  ; ------------------------------------------
                  ; Interpolate clear AK and then calc the
                  ; weighted average for a mixed scene
                  ; ------------------------------------------
                  ak_clear_model = reform(interpol(ak_clear, ak_alt, alt_mid))
                  
                  ak3_model = column3_cf * ak3_cloud + $
                             (1.d0 - column3_cf) * ak_clear_model

               endelse 

               ; ------------------------------------------
               ; Interpolate the model onto the
               ; AK grid
               ; ------------------------------------------
               ; Now deal with the integration of columns
               bro1_col = 0.0
               bro2_col = 0.0
               bro3_col = 0.0
               bh_total = 0.0

               for ialt=0, nalt-1 do begin

                  ; Get the box height in geopotential height [m]
                  bh_total =  bh_total + $
                              boxhgt_2007[ilon, ilat, ialt, imon]

                  if ( bh_total/1.e3  $
                       ge tph_2007[ilon, ilat, imon]) then continue

;                  bro1_col = bro1_col + $
;                             bro1_nd2[ilon, ilat, ialt] * $
;                             boxhgt_2007[ilon, ilat, ialt, imon] * $
;                             ak1_model[ialt]

                  bro2_col = bro2_col + $
                             bro2_nd2[ilon, ilat, ialt] * $
                             boxhgt_2007[ilon, ilat, ialt, imon] * $
                             ak2_model[ialt]

                  bro3_col = bro3_col + $
                             bro3_nd2[ilon, ilat, ialt] * $
                             boxhgt_2007[ilon, ilat, ialt, imon] * $
                             ak3_model[ialt]

               endfor

               ; Store and convert to #/cm2 from #/m2
;               bro1_column[ilon, ilat, imon]  = bro1_column[ilon, ilat, imon] $
;                                              + bro1_col  / (100.0)^2
               bro2_column[ilon, ilat, imon]  = bro2_column[ilon, ilat, imon] $
                                              + bro2_col  / (100.0)^2
               bro3_column[ilon, ilat, imon]  = bro3_column[ilon, ilat, imon] $
                                              + bro3_col  / (100.0)^2
               column_count[ilon, ilat, imon] = column_count[ilon, ilat, imon] $
                                                + 1.d0
            endfor     ; longitude loop
         endfor        ; latitude loop


      endfor  ; loop over the days in each month

      ; Save space
      ctm_cleanup


      ; Avoid divide by zeros
      cct = reform( column_count[*, *, imon] )
      it = where( cct eq 0.0, cit)
      if (cit gt 0) then cct[it] = 1.0

      ; Divide BrO column by the number of days to complete the mean
;      bro1_column[*, *, imon] = reform(bro1_column[*, *, imon]) / cct
      bro2_column[*, *, imon] = reform(bro2_column[*, *, imon]) / cct
      bro3_column[*, *, imon] = reform(bro3_column[*, *, imon]) / cct


      ; Now store the mean columns for each latitude
;      bro1_col_60_90N[imon] = mean( bro1_column[*, i60_90N, imon], /double)
;      bro1_col_30_60N[imon] = mean( bro1_column[*, i30_60N, imon], /double)
;      bro1_col_0_30N [imon] = mean( bro1_column[*, i0_30N,  imon], /double)
;      bro1_col_60_90S[imon] = mean( bro1_column[*, i60_90S, imon], /double)
;      bro1_col_30_60S[imon] = mean( bro1_column[*, i30_60S, imon], /double)
;      bro1_col_0_30S [imon] = mean( bro1_column[*, i0_30S,  imon], /double)

      bro2_col_60_90N[imon] = mean( bro2_column[*, i60_90N, imon], /double)
      bro2_col_30_60N[imon] = mean( bro2_column[*, i30_60N, imon], /double)
      bro2_col_0_30N [imon] = mean( bro2_column[*, i0_30N,  imon], /double)
      bro2_col_60_90S[imon] = mean( bro2_column[*, i60_90S, imon], /double)
      bro2_col_30_60S[imon] = mean( bro2_column[*, i30_60S, imon], /double)
      bro2_col_0_30S [imon] = mean( bro2_column[*, i0_30S,  imon], /double)

      bro3_col_60_90N[imon] = mean( bro3_column[*, i60_90N, imon], /double)
      bro3_col_30_60N[imon] = mean( bro3_column[*, i30_60N, imon], /double)
      bro3_col_0_30N [imon] = mean( bro3_column[*, i0_30N,  imon], /double)
      bro3_col_60_90S[imon] = mean( bro3_column[*, i60_90S, imon], /double)
      bro3_col_30_60S[imon] = mean( bro3_column[*, i30_60S, imon], /double)
      bro3_col_0_30S [imon] = mean( bro3_column[*, i0_30S,  imon], /double)

   endfor ; end loop over months

   months  = ['J', 'F', 'M', 'A', 'M', 'J', 'J', $
              'A', 'S', 'O', 'N', 'D']
   nmonths = 12
   imonth = findgen(nmonths)

   ; Add custom colors
   ; [Red Green Blue] in IDL
   TVLCT, 213, 94,  0,   201  ; Model 1: Vermillion
   TVLCT, 0,   158, 115, 200  ; Model 2: Bluish Green
   TVLCT, 86,  180, 233, 202  ; Model 3: Sky blue


   !p.font   = 0.25
   !p.multi  = [0, 2, 3, 0, 0]
   !y.margin = [4, 6]
   
   open_device, /portrait, /helvetica, /ps, /color, $
                bits=8, filename=psname

   !p.charthick = 3.0
   !p.thick     = 3.0
;   !p.xthick    = 3.0
;   !p.ythick    = 3.0

   ; -------------------------------------
   ; 1. 60 - 90 N comparison
   ; -------------------------------------
   ; plot them
;   plot, imonth, bro1_col_60_90N, color=1, $
   plot, imonth, bro2_col_60_90N, color=1, $
         xtickname=months, /xstyle, xminor=0, xticks=11, $
         xtitle='Month', title='60 - 90N', $
         yrange=[0.0, 6.0d13], yticks=3, yminor=0, /ystyle, $
         charsize=2.2

   ; a. Add Theys et al. 2010 data
   upb = g2_bro[*, 5] + g2_bro_er[*, 5]
   lob = g2_bro[*, 5] - g2_bro_er[*, 5]

   ; Deal with NaN
   isel = where( finite(lob) eq 1, count)
   polyfill, [imonth[isel], reverse(imonth[isel])], [lob[isel], $
             reverse(upb[isel])], color=15
   oplot, imonth, g2_bro[*, 5], color=1, psym=6
   errorbar, imonth, g2_bro[*, 5], g2_bro_sd[*, 5], color=1

   ; b. Add GEOS-Chem and p-TOMCAT back in
;   oplot, imonth, bro1_col_60_90N, color=201, linestyle=0, thick=3
   oplot, imonth, bro2_col_60_90N, color=200, linestyle=0, thick=3
   oplot, imonth, bro3_col_60_90N, color=202, linestyle=0, thick=3


   ; -------------------------------------
   ; 2. 30 - 60 N comparison
   ; -------------------------------------
;   plot, imonth, bro1_col_30_60N, color=1, $
   plot, imonth, bro2_col_30_60N, color=1, $
         xtickname=months, /xstyle, xminor=0, xticks=11, $
         xtitle='Month', title='30 - 60N', $
         yrange=[0.0, 6.0d13], yticks=3, yminor=0, /ystyle, $
         charsize=2.2

   ; a. Add Theys et al. 2010 data
   upb = g2_bro[*, 4] + g2_bro_er[*, 4]
   lob = g2_bro[*, 4] - g2_bro_er[*, 4]
   polyfill, [imonth, reverse(imonth)], [lob, reverse(upb)], color=15
   oplot, imonth, g2_bro[*, 4], color=1, psym=6, symsize=1.2
   errorbar, imonth, g2_bro[*, 4], g2_bro_sd[*, 4], color=1

   ; b. Add GEOS-Chem and p-TOMCAT back in
;   oplot, imonth, bro1_col_30_60N, color=201, linestyle=0, thick=3
   oplot, imonth, bro2_col_30_60N, color=200, linestyle=0, thick=3
   oplot, imonth, bro3_col_30_60N, color=202, linestyle=0, thick=3


   ; -------------------------------------
   ; 3. 0 - 30 N comparison
   ; -------------------------------------
;   plot, imonth, bro1_col_0_30N, color=1, $
   plot, imonth, bro2_col_0_30N, color=1, $
         xtickname=months, /xstyle, xminor=0, xticks=11, $
         xtitle='Month', title='0 - 30N', $
         yrange=[0.0, 6.0d13], yticks=3, yminor=0, /ystyle, $
         ytitle='GEOS-Chem Tropospheric BrO Columns [#/cm2]', $
         charsize=2.2

   ; a. Add Theys et al. 2010 data
   upb = g2_bro[*, 3] + g2_bro_er[*, 3]
   lob = g2_bro[*, 3] - g2_bro_er[*, 3]
   polyfill, [imonth, reverse(imonth)], [lob, reverse(upb)], color=15
   oplot, imonth, g2_bro[*, 3], color=1, psym=6, symsize=1.2
   errorbar, imonth, g2_bro[*, 3], g2_bro_sd[*, 3], color=1

   ; b. Add GEOS-Chem and p-TOMCAT back in
;   oplot, imonth, bro1_col_0_30N, color=201, linestyle=0, thick=3
   oplot, imonth, bro2_col_0_30N, color=200, linestyle=0, thick=3
   oplot, imonth, bro3_col_0_30N, color=202, linestyle=0, thick=3


   ; -------------------------------------
   ; 4. 0 - 30 S comparison
   ; -------------------------------------
;   plot, imonth, bro1_col_0_30S, color=1, $
   plot, imonth, bro2_col_0_30S, color=1, $
         xtickname=months, /xstyle, xminor=0, xticks=11, $
         xtitle='Month', title='0 - 30S', $
         yrange=[0.0, 6.0d13], yticks=3, yminor=0, /ystyle, $
         charsize=2.2

   ; a. Add Theys et al. 2010 data
   upb = g2_bro[*, 2] + g2_bro_er[*, 2]
   lob = g2_bro[*, 2] - g2_bro_er[*, 2]
   polyfill, [imonth, reverse(imonth)], [lob, reverse(upb)], color=15
   oplot, imonth, g2_bro[*, 2], color=1, psym=6, symsize=1.2
   errorbar, imonth, g2_bro[*, 2], g2_bro_sd[*, 2], color=1

   ; b. Add GEOS-Chem and p-TOMCAT back in
;   oplot, imonth, bro1_col_0_30S, color=201, linestyle=0, thick=3
   oplot, imonth, bro2_col_0_30S, color=200, linestyle=0, thick=3
   oplot, imonth, bro3_col_0_30S, color=202, linestyle=0, thick=3


   ; -------------------------------------
   ; 5. 30 - 60 S comparison
   ; -------------------------------------
;   plot, imonth, bro1_col_30_60S, color=1, $
   plot, imonth, bro2_col_30_60S, color=1, $
         xtickname=months, /xstyle, xminor=0, xticks=11, $
         xtitle='Month', title='30 - 60S', $
         yrange=[0.0, 6.0d13], yticks=3, yminor=0, /ystyle, $
         charsize=2.2

   ; a. Add Theys et al. 2010 data
   upb = g2_bro[*, 1] + g2_bro_er[*, 1]
   lob = g2_bro[*, 1] - g2_bro_er[*, 1]
   polyfill, [imonth, reverse(imonth)], [lob, reverse(upb)], color=15
   oplot, imonth, g2_bro[*, 1], color=1, psym=6, symsize=1.2
   errorbar, imonth, g2_bro[*, 1], g2_bro_sd[*, 1], color=1

   ; b. Add GEOS-Chem and p-TOMCAT back in
;   oplot, imonth, bro1_col_30_60S, color=201, linestyle=0, thick=3
   oplot, imonth, bro2_col_30_60S, color=200, linestyle=0, thick=3
   oplot, imonth, bro3_col_30_60S, color=202, linestyle=0, thick=3

   ; Now print some stats on how much we underestimate the
   ; observations by:

   ; -------------------------------------
   ; 6. 60 - 90 S comparison
   ; -------------------------------------
;   plot, imonth, bro1_col_60_90S, color=1, $
   plot, imonth, bro2_col_60_90S, color=1, $
         xtickname=months, /xstyle, xminor=0, xticks=11, $
         xtitle='Month', title='60 - 90S', $
         yrange=[0.0, 6.0d13], yticks=3, yminor=0, /ystyle, $
         charsize=2.2

   ; a. Add Theys et al. 2010 data
   upb = g2_bro[*, 0] + g2_bro_er[*, 0]
   lob = g2_bro[*, 0] - g2_bro_er[*, 0]

   ; Deal with NaN's for this region
   inan = where( finite(lob) eq 0, count)

   if (count gt 0) then begin
      igroup1 = indgen( inan[0] )
      ng1 = n_elements(igroup1)
      ng2 = 12 - (ng1 + count)
      igroup2 = inan[count-1] + 1 + indgen(ng2)
      ; first part
      polyfill, [imonth[igroup1], reverse(imonth[igroup1])], [lob[igroup1], $
                reverse(upb[igroup1])], color=15
      polyfill, [imonth[igroup2], reverse(imonth[igroup2])], [lob[igroup2], $
                reverse(upb[igroup2])], color=15
   endif else begin
      polyfill, [imonth, reverse(imonth)], [lob, reverse(upb)], color=15
   endelse
   oplot, imonth, g2_bro[*, 0], color=1, psym=6, symsize=1.2
   errorbar, imonth, g2_bro[*, 0], g2_bro_sd[*, 0], color=1

   ; b. Add GEOS-Chem and p-TOMCAT back in
;   oplot, imonth, bro1_col_60_90S, color=201, linestyle=0, thick=3
   oplot, imonth, bro2_col_60_90S, color=200, linestyle=0, thick=3
   oplot, imonth, bro3_col_60_90S, color=202, linestyle=0, thick=3

   xyouts, 0.5, 0.96, title, /normal, align=0.5, charsize=1.0,color=1

   close_device

end
