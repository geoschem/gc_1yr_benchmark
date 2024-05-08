;-----------------------------------------------------------------------
;+
; NAME:
;        GEOSCHEM_MEAN_AOD
;
; PURPOSE:
;        Calculate annual global mean AOD (strat+trop and trop only) of certain 
;        aerosol species
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        GEOSCHEM_MEAN_AOD, PREF, VERSION, MODEL, RES [, Keywords ]
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
;        OUTFILENAME = 'Meam_AOD.txt'
;
;        GEOSCHEM_MEAN_AOD, PREF, VERSION, MODEL, RES, $
;                           OUTFILENAME=OUTFILENAME, _EXTRA=e 
;
;            ; Creates text output file containing global mean AOD totals
;
; MODIFICATION HISTORY:
;        ewl, 12 Nov 2015: Initial version
;                          - Adapted from script to calculate the global
;                            budgets of 210Pb and 7Be using written by
;                            hyl,1/20/12, hongyu.liu-1@nasa.gov
;        mps, 09 Dec 2015: Modify for the 1-year benchmark plotting routines
;-----------------------------------------------------------------------

pro geoschem_mean_aod, Pref, Version, Model, Res, $
                       OutFileName=OutFileName, _EXTRA=e 

   ; Arguments
   if ( N_Elements( Pref    ) ne 1 ) then Message, 'Invalid PREF!'
   if ( N_Elements( Version ) ne 1 ) then Message, 'Invalid VERSION!'
   if ( N_Elements( Model   ) ne 1 ) then Message, 'Invalid MODEL!'
   if ( N_Elements( Res     ) ne 2 ) then Message, 'Invalid RES!'
   
   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'mean_aod.txt'

   ; Tracer info
   tracer_str =  ['Dust (OPD)',                   $
                  'Sulfate (OPSO4550)',           $
                  'Black Carbon (OPBC550)',       $
                  'Organic Carbon (OPOC550)',     $
                  'Sea Salt (accum) (OPSSa550)',  $
                  'Sea salt (coarse) (OPSSc550)' ]
   tracer = [ 'AODDust',           $
              'AODHyg550nm_SO4',   $
              'AODHyg550nm_BCPI',  $
              'AODHyg550nm_OCPI',  $
              'AODHyg550nm_SALA',  $
              'AODHyg550nm_SALC' ]
   num_tracers = n_elements(tracer)   

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
   trpause = fltarr(72, 46, 12)                  ; monthly tropopause level
   aod     = fltarr(IM, JM, LM, num_tracers, 12) ; monthly mean aod per grid box
  
   ;=========================================
   ; Loop over months
   for IMONTH = 0, 12-1 do begin

      mn = strtrim(String(fix(IMONTH+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Input filenames
      InFile_met = pref+'GEOSChem.StateMet.2016'+mn+'01_0000z.nc4'
      InFile_aod = pref+'GEOSChem.Aerosols.2016'+mn+'01_0000z.nc4'

      ; Get tropopause level
      trpause(*, *, IMONTH) = get_2dfield_geos(InFile_Met, $
                              field='Met_TropLev', lat=lat, lon=lon)
      
      ; Get aod for each tracer
      for itrac = 0,num_tracers-1 do begin
         field = tracer[itrac]
         aod(*,*,*,itrac,IMONTH) = get_3dfield_geos(InFile_aod, $
                              field=field, lat=lat, lon=lon)
      endfor  ;itrac

   endfor  ;IMONTH

   ; Get grid box areas in m2 
   darea = get_area_geos(InFile_met, field='AREA', lat=lat, lon=lon)

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

   ; Open file to create table for annual mean AOD
   openW, unit, outfilename, /get_lun
   printf, unit, ' '
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   printf, unit, '  Annual Mean Global Aerosol Optical Depths in ' + Version
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

   ; Also print to screen
   print, ' '
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   print, '  Annual Mean Global Aerosol Optical Depths in ' + Version
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

   ; Loop over tracers to calculate mean AOD
   for itrac = 0,num_tracers-1 do begin

      ; Create tracer aod array
      modaod  = reform (aod (*, *, *, itrac, *))

      ; Loop over all months
      for m=0,12-1 do begin

         ; initialize arrays if first month
         if (m eq 0) then begin
            all_aod = fltarr(n_elements(modlon), n_elements(modlat),  $
                               n_elements(modpres), 12)
            month_aod_t_sum = fltarr(12)
         endif

         ; Weight AODs for this month by grid box area
         for k=0,n_elements(modpres)-1 do begin
            all_aod(*,*,k,m) = modaod(*,*,k,m) * darea(*,*)
         endfor

         ; Sum area-weighted AODs for troposphere
         month_aod_t_sum(m)   = 0.
         for i=0,n_elements(modlon)-1 do begin
            for j=0,n_elements(modlat)-1 do begin 
               for k=1-1, ltpause(i,j)  -1  do begin
                  month_aod_t_sum(m)   = month_aod_t_sum(m)  +        $
                                      all_aod(i, j, k, m)
               endfor
            endfor
         endfor  

      endfor  ;m

      ; Calculate mean AOD for this tracer [unitless]
      mean_aod = total (all_aod(*,*,*,*)) / 12. / total( darea(*,*) ) 
      mean_aod_t = total (month_aod_t_sum(*)) / 12. / total( darea(*,*) )

      ; Print mean AOD to file
      printf, unit, ' '
      printf, unit, tracer_str[itrac], ' mean AOD [unitless]' 
      printf, unit, '   Strat + Trop:  ', mean_aod
      printf, unit, '   Troposphere:   ', mean_aod_t

      ; Also print to screen
      print, ' '
      print, tracer_str[itrac], ' mean AOD [unitless]'
      print, '   Strat + Trop:  ',  mean_aod
      print, '   Troposphere:   ',  mean_aod_t

   endfor  ;itrac

   ; Close output file
   free_lun, unit

end
