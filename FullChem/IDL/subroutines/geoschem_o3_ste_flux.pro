;-----------------------------------------------------------------------
;+
; NAME:
;        GEOSCHEM_O3_STE_FLUX
;
; PURPOSE:
;        Calculate annual strat-trop exchange flux for ozone
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        GEOSCHEM_O3_STE_FLUX, PREF, VERSION, MODEL, RES [, Keywords ]
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

pro geoschem_o3_ste_flux, Pref, Version, Model, Res, Year, $
                          OutFileName=OutFileName, _EXTRA=e 

   ; Arguments
   if ( N_Elements( Pref    ) ne 1 ) then Message, 'Invalid PREF!'
   if ( N_Elements( Version ) ne 1 ) then Message, 'Invalid VERSION!'
   if ( N_Elements( Model   ) ne 1 ) then Message, 'Invalid MODEL!'
   if ( N_Elements( Res     ) ne 2 ) then Message, 'Invalid RES!'
   
   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'o3_ste_flux.txt'

   ; Configurable parameters
   tracer_str  = [ 'O3' ]
   tracer_id   = [ 2 ]
   num_tracers = n_elements(tracer_id)

   ; Months
   month = [ 'Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
             'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'  ]

   ; Conversion factor for sec -> year
   secinyear = 3600. * 24. * 365.

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
   trpause = fltarr(72, 46, 12)          ; monthly tropopause (level/height/mb)
   upflux  = fltarr(IM, JM, LM, num_tracers, 12) ; monthly mean up/down flux
  
   ;=========================================
   ; Loop over months
   for IMONTH = 0, 12-1 do begin

      mn = strtrim(String(fix(IMONTH+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Input filename
      InFile_met  = pref+'GEOSChem.StateMet.'+year+mn+'01_0000z.nc4'
      InFile_flux = pref+'GEOSChem.AdvFluxVert.'+year+mn+'01_0000z.nc4'
      
      ; Get tropopause level
      trpause(*, *, IMONTH) = get_2dfield_geos(InFile_met, data=data, $
                              field='Met_TropLev', lat=lat, lon=lon)

      ; Get flux for each tracer
      for itrac = 0, num_tracers-1 do begin
         field = 'AdvFluxVert_' + tracer_str(itrac)
         upflux(*,*,*,itrac,IMONTH) = get_3dfield_geos(InFile_flux, data=data, $
                                field=field, lat=lat, lon=lon)
      endfor  ;itrac

   endfor  ;IMONTH

   ;================================================================
   ; Define grid parameters
   modlon  = xmid
   modlat  = ymid
   modpres = pmid

;   ltpause = intarr (IM,JM)
;   for i=0,IM-1 do begin
;      for j=0,JM-1 do begin
;         ltpause(i,j) = mean (trpause(i,j,*))
;      endfor
;   endfor

   ;===========================================================
   ; Open file to create table for tropospheric burden
   openW, unit, OutFileName, /get_lun
   printf, unit, ' '
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   printf, unit, '  Annual Global O3 Strat-Trop Exchange in ' + Version
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   printf, unit, ' '

   ; Also print to screen
   print, ' '
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   print, '  Annual Global O3 Strat-Trop Exchange in ' + Version
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   print, ' '

   ;===========================================================
   ; Loop over tracers to calculate mean burden
   for itrac = 0,num_tracers-1 do begin

      ; Initialize
      flux       = 0.0
      month_flux = fltarr(12)
      modflux    = reform(upflux(*,*,*,itrac,*))

      ; Loop over all months
      for m=0,12-1 do begin

         ; Compute monthly flux
         month_flux(m) = 0.0
         for i=0,n_elements(modlon)-1 do begin
         for j=0,n_elements(modlat)-1 do begin 

            ; Obtain flux for 100 hPa (~ level 35)
            ;lev = fix(trpause(i,j,m))
            lev = 35 
            month_flux(m) = month_flux(m) + modflux(i,j,lev-1,m)


         endfor
         endfor
       
         ; Convert kg/s -> Tg/yr
         month_flux(m) = month_flux(m) * 1.E-9 * secinyear

         ; Print monthly flux to file
         printf, unit, month[m], '   ', month_flux(m), ' Tg/yr'
         print, month[m], '   ', month_flux(m), ' Tg/yr'

      endfor  ;m

      ; Compute anual flux
      flux = flux + total(month_flux)/12

      ; Print burdens to file
      printf, unit, ' '
      printf, unit, 'Annual', flux, ' Tg/yr'
      printf, unit, ' '
      printf, unit, 'NOTE: O3 STE flux is computed using the up/down mass ', $
                    '      flux diagnostic (UP-FLX-$) at 100 hPa.'

      ; Also print to screen
      print, ' '
      print,  'Annual', flux, ' Tg/yr'
      print, ' '
      print, 'NOTE: O3 STE flux is computed using the up/down mass ', $
             '      flux diagnostic (UP-FLX-$) at 100 hPa.'


   endfor  ;itrac

   ; Close output file
   free_lun, unit

end
