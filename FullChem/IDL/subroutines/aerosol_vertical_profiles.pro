;-----------------------------------------------------------------------
;+
; NAME:
;        AEROSOL_VERTICAL_PROFILES
;
; PURPOSE:
;        Calculate  and compare mean vertical profiles for several aerosols in 
;        January and July over the US, Europe and North Atlantic for three
;        GEOS-Chem model versions
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        AEROSOL_VERTICAL_PROFILES, PREFS, VERSIONS, MODEL, RES,
;                                   TITLE [, Keywords ]
;
; INPUTS:
;        PREFS -> A 3-element vector with the directory and label names of
;                 models to be plotted
;
;        VERSIONS -> A 3-element vector containing the model version
;             names from the "red", 'green", and "blue" simulations. 
;
;        MODEL -> The model name (e.g. GEOS5, GEOSFP, MERRA2)
;
;        RES -> A 2-element vector containing the model resolution
;
;        TITLE -> A string containing the top title for the plot
;
; KEYWORD PARAMETERS:
;        /PS -> Set this switch to generate PostScript output.
;
;        OUTFILENAME -> If /PS is set, will write PostScript output 
;             to a file whose name is specified by this keyword.
;             Default is "tracer_ratio.pro".
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        External Subroutines Required:
;        =========================================
;        CTM_TYPE              CTM_GRID
;        CTM_GET_DATABLOCK
;     
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR.
;
; EXAMPLE:
;        PREFS       = [ Pref1, Pref2, Pref3 ]
;        VERSIONS    = [ Vers1, Vers2, Vers3 ]
;        MODEL       = MODEL3
;        RES         = RES3
;        PSNAME      = 'Aerosol_Vertical_Profiles.ps'
;
;        AEROSOL_VERTICAL_PROFILES, PREFS, VERSIONS, MODEL, RES, /PS, $
;                                   OUTFILENAME=PSNAME, _EXTRA=e 
;
;             ; Creates profile plots from 3 different model versions
;             ; using netCDF output files from the various GEOS-Chem
;             ; 1-yr benchmark simulations.  (NOTE: this is the actual
;             ; calling sequence from driver routine BENCHMARK_1YR.)
;
; MODIFICATION HISTORY:
;        ewl, 17 Nov 2015: Initial version
;                          - Borrows code from 1-year benchmark routine
;                            plot_station_profiles_geos_3_models.pro and
;                            Hongyu's radon burden scripts.
;        mps, 09 Dec 2015: Modify for the 1-year benchmark plotting routines
;        mps, 27 Jan 2017: Update to allow for comparison of 2 versions,
;                          intead of the default 3 versions
;-----------------------------------------------------------------------

pro aerosol_vertical_profiles, Prefs, Versions, Model, Res, Years, Title, $
                               DO_GCHP, PS=PS, OutFileName=OutFileName, _EXTRA=e

   ; Arguments
   if ( N_Elements( Model    ) ne 1 ) then Message, 'Invalid MODEL!'
   if ( N_Elements( Res      ) ne 2 ) then Message, 'Invalid RES!'
   if ( N_Elements( Title    ) ne 1 ) then Message, 'Invalid TITLE!'
   
   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'aer_profiles.ps'

   ; Number of model versions to compare
   num_versions = n_elements(versions)

   ; Tracer info
   tracer_id   = [ 34,     27,     38,     42,     43   ]  
   tracer_str  = ['BCPI', 'SO4',  'DST1', 'SALA', 'SALC']
   num_tracers = n_elements(tracer_id)

   ; Define time info
   month       = ['01',      '07'  ]
   month_str   = ['January', 'July']
   num_months  = n_elements(month)
   time =  fltarr(num_versions, num_months)

   ; Define region info (use GEOS-Chem tagged Ox bounds)
   region = [ 'USA', 'Europe', 'North Atlantic']
   minlat = [  22.0,  30.0,     30.0           ] 
   maxlat = [  50.0,  70.0,     50.0           ]
   minlon = [-127.5, -30.0,    -50.0           ]
   maxlon = [ -65.0,  50.0,    -30.0           ]
   num_regions = n_elements(region)

   ; Get model and grid info
   modelinfo = CTM_type(Model, Res=Res)
   gridinfo  = CTM_grid(modelinfo, psurf=986) 
   xmid      = gridinfo.xmid
   ymid      = gridinfo.ymid
   zmid      = gridinfo.zmid
   dao_sige  = gridinfo.sigmid
   IM        = gridinfo.IMX
   JM        = gridinfo.JMX
   LM        = gridinfo.LMX 
   DI        = gridinfo.DI
   DJ        = gridinfo.DJ
   PMID      = gridinfo.pmid
   XRange    = [-90,90]   ;Lat
   limit     = [XRange(0), -180, XRange(1), 180]

   ; Initialize arrays to store mean edge pressure per level and
   ; concentration for each level and tracer
   conc_3d   = fltarr(IM, JM, LM, num_tracers)
   totconc   = fltarr(num_tracers)
   avgconc   = fltarr(LM, num_tracers, num_versions, num_months, num_regions)
   avgpres   = fltarr(LM, num_versions, num_months, num_regions)   
   
   ; Loop over versions
   for iv = 0, num_versions-1 do begin

      ; Loop over months
      for imon = 0, num_months-1 do begin

         ctm_cleanup, /no_gc

         mn = strtrim(String(fix(imon+1)), 2)
         if (strlen(mn) eq 1) then begin         
            mn = '0'+mn     
         endif
         
         ; Input filename
         InFile_spc  = Prefs[iv]+'GEOSChem.SpeciesConc.'+years[iv]+mn+'01_0000z.nc4'
         InFile_met  = Prefs[iv]+'GEOSChem.StateMet.'+years[iv]+mn+'01_0000z.nc4'
         InFile_edge = Prefs[iv]+'GEOSChem.LevelEdgeDiags.'+years[iv]+mn+'01_0000z.nc4'

         ; Get pressure edges and species concentrations
         pres_3d = get_3dfield_geos(InFile_edge, data=data, $
                                    field='Met_PEDGE', lat=lat, lon=lon)
         for it = 0, num_tracers-1 do begin
            spcname = 'SpeciesConcVV_' + tracer_str[it]
            conc_3d(*,*,*,it) = get_species_geos(InFile_spc, data=data, $
                                   species=spcname,  lat=lat, lon=lon) 
         endfor
         
         ; Loop over regions
         for ir = 0, num_regions-1 do begin

            ; Initialize
            totpres    = 0
            totconc(*) = 0

            ; Determine number of grid boxes in this region
            ilon = WHERE(lon ge minlon(ir) and lon le maxlon(ir))
            ilat = WHERE(lat ge minlat(ir) and lat le maxlat(ir))
            nlon = n_elements(ilon)
            nlat = n_elements(ilat)
                        
            ; Loop over grid boxes
            for L = 0, LM-1 do begin

               ; Compute total pressure and concentrations for this level
               totpres = TOTAL(pres_3d( ilon, ilat, L))
               for it = 0, num_tracers-1 do begin
                  totconc(it) = TOTAL(conc_3d( ilon, ilat, L, it ))
               endfor

               ; Get avg concentrations and pressure edges for this tracer,
               ; version, month, and region for all levels
               avgpres(L,     iv, imon, ir) = totpres / ( nlon *  nlat )
               for it = 0, num_tracers-1 do begin
               avgconc(L, it, iv, imon, ir) = totconc(it) / ( nlon *  nlat )
               endfor
            
            endfor ; L

            ; clean up memory
            ctm_cleanup, /NO_GC, NO_FILE_CLOSE = 0

         endfor ; regions
      endfor ; months
   endfor ; versions

   ; Show 6 panels in a 3x2 grid: month on horiz axis and region on vertical.
   ; Show different versions as different colors and include key in title.
   ; Specify tracer in title.
   
   ; Open postscript file
   open_device, olddevice, PS=PS, /color, filename=OutFileName, /portrait

   ; Get defaults (bmy, 6/7/11)
   X_OMARGIN   = !X.OMARGIN
   Y_OMARGIN   = !Y.OMARGIN
   X_MARGIN    = !X.MARGIN
   Y_MARGIN    = !Y.MARGIN
   P_CHARTHICK = !P.CHARTHICK
   P_CHARSIZE  = !P.CHARSIZE
   P_THICK     = !P.THICK
   X_THICK     = !X.THICK
   Y_THICK     = !Y.THICK

   ; Plot settings
   !X.OMARGIN=[12,8] 
   !Y.OMARGIN=[10,8]
   !X.MARGIN=[0,0]
   !Y.MARGIN=[0,0]
   !P.CHARTHICK=2.5
   !P.CHARSIZE=1.5
   !P.THICK=2.5
   !X.THICK=4
   !Y.THICK=4

   ; Use Postscript font
   !P.FONT=0

   ; set up plot grid - 3 columns (region) x 2 rows (month) 
   nrow=3
   ncol=2
   !P.Multi = [0,nrow,ncol,0,1]

   ; for indexing reference:
   ;   avgconc = fltarr(LM, num_tracers, num_versions, num_months, num_regions)
   ;   avgpres = fltarr(LM, num_versions, num_months, num_regions)

   ltitle = tracer_str + ' - ' + month[1] + ' region - ' + region[1]
   for it = 0, num_tracers-1 do begin
      for ir = 0, num_regions-1 do begin
         for imon = 0, num_months-1 do begin

            maxconc = max( avgconc(*,it,[0:2],imon,ir) )
            Xmax = 1.1 * maxconc
            
            yrange    = [ 1000,  100]
            ytickv    = [ 1000,  800,  600,  400,  200 ]
            ytickname = ['1000','800','600','400','200']

            ;====================
            ; Version1 in red
            iv = 0

            ; upper left
            if (ir eq 0 ) && (imon eq 0) then begin 
               plot,  avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),   $
               linestyle=1, psym=4, symsize=0.3, color=1,                $
               yrange = [1000, 400],                                     $
               xrange = [0.0, Xmax];,                                     $
            endif

            ; upper middle
            if (ir eq 1 ) && (imon eq 0) then begin 
               plot,  avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),   $
               linestyle=1, psym=4, symsize=0.3, color=1,                $
               yrange = [1000, 400],                                     $
               xrange = [0.0, Xmax],                                     $
               title = Title
            endif

            ; upper right
            if (ir eq 2) && (imon eq 0) then begin 
               plot,  avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),   $
               linestyle=1, psym=4, symsize=0.3, color=1,                $
               yrange = [1000, 400],                                     $
               xrange = [0.0, Xmax];,                                     $
            endif

            ; lower left
            if (ir eq 0 ) && (imon eq 1) then begin 
               plot,  avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),   $
               linestyle=1, psym=4, symsize=0.3, color=1,                $
               yrange = [1000, 400],                                     $
               xrange = [0.0, Xmax];,                                     $
            endif

            ; lower middle and lower right
            if ((ir eq 1) || (ir eq 2)) && (imon eq 1) then begin 
               plot,  avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),   $
               linestyle=1, psym=4, symsize=0.3, color=1,                $
               yrange = [1000, 400],                                     $
               xrange = [0.0, Xmax];,                                     $

            endif

            ; Plot the data again, in red
            oplot, avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),  $
                   linestyle=1, psym = 4,  symsize = 0.3, color = 2
            oplot, avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),  $
                   linestyle=0, color=2
      
            ;====================
            ; Version2 in green
            iv = 1
            oplot, avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),  $
                   linestyle=1, psym=4, symsize=0.3, color=3
            oplot, avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),  $
                   linestyle=0, color=3

            if ( num_versions eq 3 ) then begin
            ;====================
            ; Version3 in blue
            iv = 2
            oplot, avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),  $
                   linestyle=1, psym=4, symsize=0.3, color=4
            oplot, avgconc( * ,it,iv,imon,ir), avgpres( * ,iv,imon,ir),  $
                   linestyle=0, color=4
            endif

            ; Y-axis
            xyouts, 0.05, 0.5, 'Pressure (hPa)', $
                    /normal, align=0.5, orientation=90, charsize=1.2, color=1
      
            ; X-axis
            xyouts, 0.5, 0.05, 'Mean Tracer Concentration (ppbv)', $
                    /normal, align=0.5, charsize=1., color=1
               
            ; top title
            xyouts, 0.5,0.95, tracer_str[it] , $
                    /normal, align=0.5, charsize=1.2, color=1

           ; Write the month on the plot
           xyouts, 0.5*Xmax, 450, region[ir] + '!c' + month_str[imon],      $
           charsize = 1, /data, color=1

         endfor
      endfor
   endfor

   ; Cleanup & quit 
   close_device, /TIMESTAMP
   close,/all

   ; Restore defaults (need this?)
   !X.OMARGIN   = X_OMARGIN   
   !Y.OMARGIN   = Y_OMARGIN   
   !X.MARGIN    = X_MARGIN    
   !Y.OMARGIN   = Y_MARGIN    
   !P.CHARTHICK = P_CHARTHICK 
   !P.THICK     = P_THICK     
   !X.THICK     = X_THICK     
   !Y.THICK     = Y_THICK 

end
