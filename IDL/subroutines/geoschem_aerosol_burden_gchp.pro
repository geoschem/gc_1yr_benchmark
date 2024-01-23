;-----------------------------------------------------------------------
;+
; NAME:
;        GEOSCHEM_AEROSOL_BURDEN_GCHP
;
; PURPOSE:
;        Calculate annual mean global burdens of certain aerosol species.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        GEOSCHEM_AEROSOL_BURDEN, PREF1, VERSION1, MODEL1, RES1,
;                     PREF2, VERSION2, MODEL2, RES2 [, Keywords ] [, Keywords ]
;
; INPUTS:
;        PREF1 -> Directory and label name of model to be plotted
;
;        VERSION1 -> The model version being plotted
;
;        MODEL1 -> The model name (e.g. GEOS5, GEOSFP, MERRA2)
;
;        RES1 -> A 2-element vector containing the model resolution
;
;        PREF2 -> Directory and label name of model source for BXHGHT-$
;                 and TR-PAUSE-$ diagnostics
;
;        VERSION2 -> The model version for BXHGHT-$ and TR-PAUSE-$ diagnostics
;
;        MODEL2 -> The model name (e.g. GEOS5, GEOSFP, MERRA2)
;
;        RES2 -> A 2-element vector containing the model resolution
;
; KEYWORD PARAMETERS:
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
;        (1) Meant to be called from BENCHMARK_1YR if using GCHP since
;            GCHP does not yet have diagnostics.
;
; EXAMPLE:
;        PREF1        = PREF2
;        VERSIONS1    = VERS2
;        MODEL1       = MODEL2
;        RES1         = RES2
;        PREF2        = PREF1
;        VERSIONS2    = VERS1
;        MODEL2       = MODEL1
;        RES2         = RES1
;        OUTFILENAME = 'Aerosol_Burdens.txt'
;
;        GEOSCHEM_MEAN_AOD, PREF1, VERSION1, MODEL1, RES1, $
;                           PREF1, VERSION1, MODEL1, RES1, $
;                           OUTFILENAME=OUTFILENAME, _EXTRA=e 
;
;            ; Creates text output file containing global aerosol burdens
;
; MODIFICATION HISTORY:
;        ewl, 29 Nov 2017: Initial version
;-----------------------------------------------------------------------

pro geoschem_aerosol_burden_gchp, Pref1, Version1, Model1, Res1, $
                                  Pref2, Version2, Model2, Res2, $
                                  OutFileName=OutFileName, _EXTRA=e 

   ; Arguments
   if ( N_Elements( Pref1    ) ne 1 ) then Message, 'Invalid PREF1!'
   if ( N_Elements( Version1 ) ne 1 ) then Message, 'Invalid VERSION1!'
   if ( N_Elements( Model1   ) ne 1 ) then Message, 'Invalid MODEL1!'
   if ( N_Elements( Res1     ) ne 2 ) then Message, 'Invalid RES1!'
   if ( N_Elements( Pref2    ) ne 1 ) then Message, 'Invalid PREF2!'
   if ( N_Elements( Version2 ) ne 1 ) then Message, 'Invalid VERSION2!'
   if ( N_Elements( Model2   ) ne 1 ) then Message, 'Invalid MODEL2!'
   if ( N_Elements( Res2     ) ne 2 ) then Message, 'Invalid RES2!'
   
   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'aerosol_burden.txt'

   ; Configurable parameters
   mw_air = 28.9644*1e-3   ;kg/mole 
   conv_factor = 1.E-9     ; ppbv * conv_factor --> v/v
   tracer_str =  ['BCPI', 'SO4', 'DST1', 'SALA', 'SALC' ]
   tracer_id =  [34, 27, 38, 42, 43]
   tracer_MW =  [12., 96., 29., 31.4, 31.4]
   num_tracers = n_elements(tracer_id)

   ; Get model and grid info
   modelinfo1 = CTM_type(Model1, Res=Res1)
   gridinfo1  = CTM_grid(modelinfo1, psurf=986) 
   xmid1      = gridinfo1.xmid
   ymid1      = gridinfo1.ymid
   zmid1      = gridinfo1.zmid
   IM1        = gridinfo1.IMX
   JM1        = gridinfo1.JMX
   LM1        = gridinfo1.LMX
   PMID1      = gridinfo1.pmid
   lev1_1      = 1  
   lev2_1      = LM1
   XRange1    = [-90,90]   ;Lat
   limit1     = [XRange1(0), -180, XRange1(1), 180]

   modelinfo2 = CTM_type(Model2, Res=Res2)
   gridinfo2  = CTM_grid(modelinfo2, psurf=986) 
   xmid2      = gridinfo2.xmid
   ymid2      = gridinfo2.ymid
   zmid2      = gridinfo2.zmid
   IM2        = gridinfo2.IMX
   JM2        = gridinfo2.JMX
   LM2        = gridinfo2.LMX
   PMID2      = gridinfo2.pmid
   lev1_2      = 1  
   lev2_2      = LM2
   XRange2    = [-90,90]   ;Lat
   limit2     = [XRange2(0), -180, XRange2(1), 180]
   
   ;=========================================
   ; Initialize arrays
   trpause = fltarr(IM2, JM2, 12)      ; monthly tropopause (level/height/mb)
   airmass = fltarr(IM2, JM2, LM2, 12) ; monthly grid box masses
   const   = fltarr(IM1, JM1, LM1, num_tracers, 12) ; monthly mean concentrations

   ;=========================================
   ; Loop over months
   for IMONTH = 0, 12-1 do begin

      mn = strtrim(String(fix(IMONTH+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif
      print,  "Getting data for month ",  mn

      ; Input filename
      InFile1 = pref1+mn+'01.nc'
      InFile2 = pref2+mn+'01.nc'
      
      print, "Files to read: "
      print, InFile1
      print, InFile2

      ; Get each tropopause metric (level, height, pressure)
      CTM_Get_Data, trpause_ptr, 'TR-PAUSE', File=Infile2, Tracer=1, /Quiet
      trop_1mo  = *( trpause_ptr[0].Data )
      trpause(*, *, IMONTH) = trop_1mo
      Undefine, trpause_ptr
      Undefine, trop_1mo

      ; clean up memory
      ctm_cleanup, /NO_GC, NO_FILE_CLOSE = 0

      ; Get airmass
      CTM_Get_Data, airmass_ptr, 'BXHGHT-$', File=InFile2, Tracer=2, /Quiet
      air_1mo  = *( airmass_ptr[0].Data )
      airmass(*,*,*,IMONTH) = air_1mo
      Undefine, airmass_ptr
      Undefine, air_1mo

      ; clean up memory
      ctm_cleanup, /NO_GC,  NO_FILE_CLOSE = 0

      ; Get concentrations in ppbv for each tracer
      for itrac = 0,num_tracers-1 do begin
         ntrac =  tracer_id[itrac]
         CTM_Get_Data, trc_conc_ptr, 'IJ-AVG-$', File=InFile1, $
                       Tracer = ntrac, /Quiet
         trc_1mo  = *( trc_conc_ptr[0].Data )
         const(*,*,*,itrac,IMONTH) = trc_1mo
         Undefine, trc_conc_ptr
         Undefine, trc_1mo
      endfor  ;itrac

      ; clean up memory
      ctm_cleanup, /NO_GC, NO_FILE_CLOSE = 0

   endfor  ;IMONTH

   ;;================================================================
   ;; Get grid box areas in m2 and define more grid params
   ;CTM_Get_Data, area_m2_ptr, 'DXYP', File=InFile2, Tracer=1, /Quiet
   ;area_m2  = *( area_m2_ptr[0].Data )
   ;darea   = area_m2
   ;UnDefine,  area_m2_ptr
   ;UnDefine,  area_m2

   ; clean up memory
   ;ctm_cleanup, /NO_GC, NO_FILE_CLOSE = 0

   modlon  = xmid1
   modlat  = ymid1
   modpres = pmid1
   ltpause = intarr (IM1,JM1)
   for i=0,IM1-1 do begin
      for j=0,JM1-1 do begin
         ltpause(i,j) = mean (trpause(i,j,*))
      endfor
   endfor
   UnDefine,  trpause

   ;===========================================================
   ; Open file to create table for tropospheric burden
   openW, unit, OutFileName, /get_lun
   printf, unit, ' '
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   ; kludge for GCHP memory updates
   printf, unit, '  Annual Average Global Burdens of Aerosols in v11-02b-HP'
   ;printf, unit, '  Annual Average Global Burdens of Aerosols in ' + Version1
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

   ; Also print to screen
   print, ' '
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   ; kludge for GCHP memory updates
   print, '  Annual Average Global Burdens of Aerosols in v11-02b-HP'
   ;print, '  Annual Average Global Burdens of Aerosols in ' + Version1
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

   ;===========================================================
   ; Set air mass array
   air_mass_arr = fltarr(n_elements(modlon), n_elements(modlat), $
                                   n_elements(modpres), 12)
   for m=0,12-1 do begin
      for k=0,n_elements(modpres)-1 do begin
         air_mass_arr(*,*,k,m) = airmass(*,*,k,m)  
      endfor
   endfor

   ; Allocate species arrays
   spc_mass_arr = fltarr(n_elements(modlon), n_elements(modlat),     $
                               n_elements(modpres), 12)
   month_mass_trop = fltarr(12)
   month_mass_alllevels = fltarr(12)

   ; Loop over species
   for itrac = 0,num_tracers-1 do begin
      print,  "Calculing mean burden for tracer ", itrac, " of ", num_tracers-1

      ; Create tracer conc array and convert to v/v
      modtrac  = reform (const (*,*,*,itrac,*))
      modtrac  = modtrac * conv_factor 

      ; initialize values
      spc_mass_arr( *,*,*,*) =  0

      ; Loop over all months
      for m=0,12-1 do begin
      
         for k=0,n_elements(modpres)-1 do begin
            spc_mass_arr(*,*,k,m) = air_mass_arr(*,*,k,m) / mw_air           $
                                 * modtrac(*, *, k, m) * tracer_MW[itrac]
         endfor
   
         month_mass_trop(m)   = 0.
         for i=0,n_elements(modlon)-1 do begin
            for j=0,n_elements(modlat)-1 do begin 
               for k=1-1, ltpause(i,j)  -1  do begin
                  month_mass_trop(m)   = month_mass_trop(m)  +      $
                                      spc_mass_arr(i, j, k, m)
               endfor
            endfor
         endfor  

      endfor  ;m

      ; Calculate mean annual burdens for this tracer [Tg]
      mass_alllevels   = total (spc_mass_arr(*,*,*,*),  /DOUBLE) / 12. * 1.E-12
      mass_trop = total (month_mass_trop(*),  /DOUBLE) / 12. * 1.E-12

      ; Print burdens to file
      printf, unit, ' '
      printf, unit, tracer_str[itrac], ' burden [Tg]' 
      printf, unit, '   Strat + Trop:  ', mass_alllevels
      printf, unit, '   Troposphere:   ', mass_trop

      ; Also print to screen
      print, ' '
      print, tracer_str[itrac], ' burden [Tg]'
      print, '   Strat + Trop:  ',  mass_alllevels
      print, '   Troposphere:   ',  mass_trop

   endfor  ;itrac

   ; Close output file
   free_lun, unit

end
