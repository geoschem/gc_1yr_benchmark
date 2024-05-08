;-----------------------------------------------------------------------
;+
; NAME:
;        GEOSCHEM_AEROSOL_BURDEN
;
; PURPOSE:
;        Calculate annual mean global burdens of certain aerosol species
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        GEOSCHEM_AEROSOL_BURDEN, PREF, VERSION, MODEL, RES [, Keywords ]
;
; INPUTS:
;        PREF -> Directory and label name of model to be plotted
;
;        VERSION -> The model version being plotted
;
;        MODEL -> The model name (e.g. GEOS5, GEOSFP, MERRA2)
;
;        RES -> A 2-element vector containing the model resolution
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
;        (1) Meant to be called from BENCHMARK_1YR.
;
; EXAMPLE:
;        PREF        = PREF3
;        VERSIONS    = VERS3
;        MODEL       = MODEL3
;        RES         = RES3
;        OUTFILENAME = 'Aerosol_Burdens.txt'
;
;        GEOSCHEM_MEAN_AOD, PREF, VERSION, MODEL, RES, $
;                           OUTFILENAME=OUTFILENAME, _EXTRA=e 
;
;            ; Creates text output file containing global aerosol burdens
;
; MODIFICATION HISTORY:
;        ewl, 06 Nov 2015: Initial version
;                          - Adapted from script to calculate the global
;                            budgets of 210Pb and 7Be using written by
;                            hyl,1/20/12, hongyu.liu-1@nasa.gov
;        mps, 09 Dec 2015: Modify for the 1-year benchmark plotting routines
;-----------------------------------------------------------------------

pro geoschem_aerosol_burden, Pref, Version, Model, Res, Year, $
                             OutFileName=OutFileName, _EXTRA=e 

   ; Arguments
   if ( N_Elements( Pref    ) ne 1 ) then Message, 'Invalid PREF!'
   if ( N_Elements( Version ) ne 1 ) then Message, 'Invalid VERSION!'
   if ( N_Elements( Model   ) ne 1 ) then Message, 'Invalid MODEL!'
   if ( N_Elements( Res     ) ne 2 ) then Message, 'Invalid RES!'
   
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
   modelinfo = CTM_type(Model, Res=Res)
   gridinfo  = CTM_grid(modelinfo, psurf=986) 
   xmid      = gridinfo.xmid
   ymid      = gridinfo.ymid
   zmid      = gridinfo.zmid
   dao_sige  = gridinfo.sigmid
   IM        = gridinfo.IMX
   JM        = gridinfo.JMX
   LM        = gridinfo.LMX
   PMID      = gridinfo.pmid
   lev1      = 1  
   lev2      = LM
   XRange    = [-90,90]   ;Lat
   limit     = [XRange(0), -180, XRange(1), 180]

   ;=========================================
   ; Initialize arrays
   trpause = fltarr(72, 46, 12)           ; monthly tropopause (level/height/mb)
   airmass = fltarr(72, 46, LM, 12)       ; monthly grid box masses
   const   = fltarr(IM, JM, LM, num_tracers, 12) ; monthly mean concentrations
  
   ;=========================================
   ; Loop over months
   for IMONTH = 0, 12-1 do begin

      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(IMONTH+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Input filenames
      name     = pref+'GEOSChem.SpeciesConc.'+year+mn+'01_0000z.nc4'
      name_Met = pref+'GEOSChem.StateMet.'+year+mn+'01_0000z.nc4'

      ; Get tropopause level
      trpause(*, *, IMONTH) = get_2dfield_geos(name_Met, data=data, $
                              field='Met_TropLev', lat=lat, lon=lon)
      
      ; Get airmass
      airmass(*,*,*,IMONTH) = get_3dfield_geos(name_Met, data=data, $
                              field='Met_AD', lat=lat, lon=lon)

      ; Get concentrations in ppbv for each tracer
      for itrac = 0, num_tracers-1 do begin
         spcname = 'SpeciesConcVV_' + tracer_str[itrac]
         const(*,*,*,itrac,IMONTH) = get_species_geos(name, data=data, $
                            species=spcname,  lat=lat, lon=lon) 
      endfor  ;itrac

   endfor  ;IMONTH

   ; Define more grid params
   modlon  = xmid
   modlat  = ymid
   modpres = pmid

   ltpause = intarr (IM,JM)
   for i=0,IM-1 do begin
      for j=0,JM-1 do begin
         ltpause(i,j) = mean (trpause(i,j,*))
      endfor
   endfor

   ;===========================================================
   ; Open file to create table for tropospheric burden
   openW, unit, OutFileName, /get_lun
   printf, unit, ' '
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   printf, unit, '  Annual Average Global Burdens of Aerosols in ' + Version
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

   ; Also print to screen
   print, ' '
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   print, '  Annual Average Global Burdens of Aerosols in ' + Version
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

   ;===========================================================
   ; Loop over tracers to calculate mean burden
   for itrac = 0,num_tracers-1 do begin

      ; Create tracer conc array and convert to v/v
      modtrac  = reform (const (*,*,*,itrac,*))
      modtrac  = modtrac * conv_factor 

      ; Loop over all months
      for m=0,12-1 do begin
      
         ctm_cleanup, /no_gc

         if (m eq 0) then begin
            temp_mass_air = fltarr(n_elements(modlon), n_elements(modlat), $
                                   n_elements(modpres), 12)
            temp_mass = fltarr(n_elements(modlon), n_elements(modlat),     $
                               n_elements(modpres), 12)
            month_mass_t = fltarr(12)
         endif
   
         for k=0,n_elements(modpres)-1 do begin
            temp_mass_air(*,*,k,m) = airmass(*,*,k,m)  
            temp_mass(*,*,k,m) = temp_mass_air(*,*,k,m) / mw_air           $
                                 * modtrac(*, *, k, m) * tracer_MW[itrac]
         endfor
   
         month_mass_t(m)   = 0.
         for i=0,n_elements(modlon)-1 do begin
            for j=0,n_elements(modlat)-1 do begin 
               for k=1-1, ltpause(i,j)  -1  do begin
                  month_mass_t(m)   = month_mass_t(m)  +      $
                                      temp_mass(i, j, k, m)
               endfor
            endfor
         endfor  

      endfor  ;m

      ; Calculate mean annual burdens for this tracer [Tg]
      mass   = total (temp_mass   (*,*,*,*),  /DOUBLE) / 12. * 1.E-12
      mass_t = total (month_mass_t(*),  /DOUBLE) / 12. *  1.E-12

      ; Print burdens to file
      printf, unit, ' '
      printf, unit, tracer_str[itrac], ' burden [Tg]' 
      printf, unit, '   Strat + Trop:  ', mass
      printf, unit, '   Troposphere:   ', mass_t

      ; Also print to screen
      print, ' '
      print, tracer_str[itrac], ' burden [Tg]'
      print, '   Strat + Trop:  ',  mass
      print, '   Troposphere:   ',  mass_t

   endfor  ;itrac

   ; Close output file
   free_lun, unit

end
