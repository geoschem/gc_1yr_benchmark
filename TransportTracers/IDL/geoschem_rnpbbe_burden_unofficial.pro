;-----------------------------------------------------------------------
;+
; NAME:
;        GEOSCHEM_RNPBBE_BURDEN_UNOFFICIAL
;
; PURPOSE:
;        Calculate annual mean global burdens of RnPbBe species. This is
;        an unofficial benchmark routine for use with GCHP prior to the
;        availability of diagnostics.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        GEOSCHEM_RNPBBE_BURDEN_UNOFFICIAL, PREF1, VERSION1, MODEL1, RES1,
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
;        (1) Meant to be called from driver script gchp_rnpbbe_mass.pro
;        (2) Based on standard 1-year benchmark script for aerosol
;            burdens when using GCHP without diagnostic output
;        (3) Uses GCC diagnostics for converting mol/mol to mass
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
;        OUTFILENAME = 'RnPbBe_Burdens.txt'
;
;        GEOSCHEM_RNPBBE_BURDEN_UNOFFICIAL, PREF1, VERSION1, MODEL1, RES1, $
;                           PREF1, VERSION1, MODEL1, RES1, $
;                           OUTFILENAME=OUTFILENAME, _EXTRA=e 
;
;            ; Creates text output file containing global aerosol burdens
;
; MODIFICATION HISTORY:
;        ewl, 27 Jul 2017: Initial version
;-----------------------------------------------------------------------

pro geoschem_rnpbbe_burden_unofficial, Pref1, Version1, Model1, Res1, $
                                  Pref2, Version2, Model2, Res2, $
                                  Pref3, Version3, Model3, Res3, $
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
   if ( N_Elements( Pref3    ) ne 1 ) then Message, 'Invalid PREF3!'
   if ( N_Elements( Version3 ) ne 1 ) then Message, 'Invalid VERSION3!'
   if ( N_Elements( Model3   ) ne 1 ) then Message, 'Invalid MODEL3!'
   if ( N_Elements( Res3     ) ne 2 ) then Message, 'Invalid RES3!'
   
   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'rnpbbe_burden.txt'

   ; Configurable parameters
   mw_air = 28.9644*1e-3   ;kg/mol
   ;conv_Rn_mBqSCM_to_vv = 5.6397E22 ; mBq/SCM * conv_factor --> v/v for Rn
   ;conv_Pb_mBqSCM_to_vv = 2.6141E19 ; mBq/SCM * conv_factor --> v/v for Pb
   ;conv_Be_mBqSCM_to_vv = 4.0513E21 ; mBq/SCM * conv_factor --> v/v for Be7
   conv_ppbv_to_vv =  1.E-9       ; ppbv    * conv_factor --> v/v 
   tracer_str =  ['Rn', 'Pb', 'Be7', 'PASV' ]
   tracer_id =  [1, 2, 3, 4]
   tracer_MW =  [222., 210., 7.0, 1.0] ; g/mol
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
   ; rest of filename after prefix: MM01.nc

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
   ; rest of filename after prefix: MM010000_Regrid.nc

   modelinfo3 = CTM_type(Model3, Res=Res3)
   gridinfo3  = CTM_grid(modelinfo3, psurf=986) 
   xmid3      = gridinfo3.xmid
   ymid3      = gridinfo3.ymid
   zmid3      = gridinfo3.zmid
   IM3        = gridinfo3.IMX
   JM3        = gridinfo3.JMX
   LM3        = gridinfo3.LMX
   PMID3      = gridinfo3.pmid
   lev1_3      = 1  
   lev3_3      = LM3
   XRange3    = [-90,90]   ;Lat
   limit3     = [XRange3(0), -180, XRange3(1), 180] 
   ; rest of filename after prefix: MM01_Regrid.nc
  
   ;=========================================
   ; Initialize arrays
   trpause = fltarr(IM3, JM3, 12)      ; monthly tropopause (level/height/mb)
   airmass = fltarr(IM3, JM3, LM3, 12) ; monthly grid box masses
   const_gchp   = fltarr(IM1, JM1, LM1, num_tracers, 12) ; monthly conc
   const_gcc   = fltarr(IM1, JM1, LM1, num_tracers, 12) ; monthly conc

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
      InFile2 = pref2+mn+'010000_Regrid.nc'
      InFile3 = pref3+mn+'01_Regrid.nc'
      
      print, "Files to read: "
      print, InFile1
      print, InFile2
      print, InFile3

      ; Get each tropopause metric (level, height, pressure)
      Print,  InFile1
      CTM_Get_Data, trpause_ptr, 'TR-PAUSE', File=Infile3, Tracer=1, /Quiet
      trop_1mo  = *( trpause_ptr[0].Data )
      trpause(*, *, IMONTH) = trop_1mo
      Undefine, trpause_ptr
      Undefine, trop_1mo

      ; clean up memory
      ctm_cleanup, /NO_GC, NO_FILE_CLOSE = 0

      ; Get airmass
      Print,  InFile3
      CTM_Get_Data, airmass_ptr, 'BXHGHT-$', File=InFile3, Tracer=2, /Quiet
      air_1mo  = *( airmass_ptr[0].Data )
      airmass(*,*,*,IMONTH) = air_1mo
      Undefine, airmass_ptr
      Undefine, air_1mo

      ; clean up memory
      ctm_cleanup, /NO_GC,  NO_FILE_CLOSE = 0

      ; Get concentrations in ppbv for each tracer (gchp)
      Print,  InFile1
      for itrac = 0,num_tracers-1 do begin
         ntrac =  tracer_id[itrac]
         CTM_Get_Data, trc_conc_ptr, 'IJ-AVG-$', File=InFile1, $
                       Tracer = ntrac, /Quiet
         trc_1mo  = *( trc_conc_ptr[0].Data )
         const_gchp(*,*,*,itrac,IMONTH) = trc_1mo
         Undefine, trc_conc_ptr
         Undefine, trc_1mo
      endfor  ;itrac

      ; clean up memory
      ctm_cleanup, /NO_GC,  NO_FILE_CLOSE = 0

      ; Get concentrations in ppbv for each tracer (gcc)
      Print,  InFile2
      for itrac = 0,num_tracers-1 do begin
         ntrac =  tracer_id[itrac]
         CTM_Get_Data, trc_conc_ptr, 'IJ-AVG-$', File=InFile2, $
                       Tracer = ntrac, /Quiet
         trc_1mo  = *( trc_conc_ptr[0].Data )
         const_gcc(*,*,*,itrac,IMONTH) = trc_1mo
         Undefine, trc_conc_ptr
         Undefine, trc_1mo
      endfor  ;itrac

      ; clean up memory
      ctm_cleanup, /NO_GC, NO_FILE_CLOSE = 0

   endfor  ;IMONTH

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
   print,  OutFileName
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
   ; Loop over tracers to calculate burdens for gchp
   for itrac = 0,num_tracers-1 do begin

      print,  "Calculating burden for tracer ",  itrac,  " of ",  num_tracers-1

      ; Create tracer conc array and convert to v/v if needed
      ; GCHP output was converted from mol/mol to ppbv during the
      ; regridding of 4x5 to 1x1.25 and therefore must be converted back.
      modtrac_gchp  = reform (const_gchp (*,*,*,itrac,*))
      modtrac_gchp  = modtrac_gchp * conv_ppbv_to_vv
      modtrac_gcc  = reform (const_gcc (*,*,*,itrac,*))
      modtrac_gcc  = modtrac_gcc

      ; Create arrays for storing global mass per month
      trop_mass_gchp_t = fltarr(12)
      full_mass_gchp_t = fltarr(12)
      trop_mass_gcc_t = fltarr(12)
      full_mass_gcc_t = fltarr(12)
      all_mass_gchp = fltarr(n_elements(modlon), n_elements(modlat),     $
                               n_elements(modpres), 12)
      all_mass_gcc = fltarr(n_elements(modlon), n_elements(modlat),     $
                               n_elements(modpres), 12)

      ; Loop over all months
      for m=0,12-1 do begin
      
         if (m eq 0) then begin
         endif
   
         for k=0,n_elements(modpres)-1 do begin
            all_mass_gchp(*,*,k,m) = airmass(*,*,k,m) / mw_air           $
                                 * modtrac_gchp(*, *, k, m) * tracer_MW[itrac]
            all_mass_gcc(*,*,k,m) = airmass(*,*,k,m) / mw_air           $
                                 * modtrac_gcc(*, *, k, m) * tracer_MW[itrac]
         endfor
   
         trop_mass_gchp_t(m)   = 0.
         trop_mass_gcc_t(m)   = 0.
         for i=0,n_elements(modlon)-1 do begin
            for j=0,n_elements(modlat)-1 do begin 
               for k=1-1, ltpause(i,j)  -1  do begin
                  trop_mass_gchp_t(m)   = trop_mass_gchp_t(m)  +      $
                                      all_mass_gchp(i, j, k, m)
                  trop_mass_gcc_t(m)   = trop_mass_gcc_t(m)  +      $
                                      all_mass_gcc(i, j, k, m)
               endfor
            endfor
         endfor  

         full_mass_gchp_t(m)   = 0.
         full_mass_gcc_t(m)   = 0.
         for i=0,n_elements(modlon)-1 do begin
            for j=0,n_elements(modlat)-1 do begin 
               for k=0,n_elements(modpres)-1 do begin
                  full_mass_gchp_t(m)   = full_mass_gchp_t(m)  +      $
                                      all_mass_gchp(i, j, k, m)
                  full_mass_gcc_t(m)   = full_mass_gcc_t(m)  +      $
                                      all_mass_gcc(i, j, k, m)
               endfor
            endfor
         endfor  

      endfor  ;m
      Undefine, all_mass_gchp
      Undefine, all_mass_gcc
      Undefine, modtrac

      ;; Calculate mean annual burdens for this tracer [Tg]
      ;mass   = total (all_mass   (*,*,*,*)) / 12. * 1.E-12
      ;mass_t = total (trop_mass_t(*)) / 12. *  1.E-12

      ; Convert g to Tg
      full_mass_gchp_t =  full_mass_gchp_t *  1.E-12
      full_mass_gcc_t =  full_mass_gcc_t *  1.E-12
      trop_mass_gchp_t =  trop_mass_gchp_t *  1.E-12
      trop_mass_gcc_t =  trop_mass_gcc_t *  1.E-12

      ; Print burdens to file
      printf, unit, ' '
      printf, unit, tracer_str[itrac], ' burden [Tg]' 
      printf, unit, '   Strat + Trop: GCHP         GCC'
      printf, unit, '      Jan: ', full_mass_gchp_t(0),  full_mass_gcc_t(0) 
      printf, unit, '      Feb: ', full_mass_gchp_t(1),  full_mass_gcc_t(1) 
      printf, unit, '      Mar: ', full_mass_gchp_t(2),  full_mass_gcc_t(2) 
      printf, unit, '      Apr: ', full_mass_gchp_t(3),  full_mass_gcc_t(3) 
      printf, unit, '      May: ', full_mass_gchp_t(4),  full_mass_gcc_t(4) 
      printf, unit, '      Jun: ', full_mass_gchp_t(5),  full_mass_gcc_t(5) 
      printf, unit, '      Jul: ', full_mass_gchp_t(6),  full_mass_gcc_t(6) 
      printf, unit, '      Aug: ', full_mass_gchp_t(7),  full_mass_gcc_t(7) 
      printf, unit, '      Sep: ', full_mass_gchp_t(8),  full_mass_gcc_t(8) 
      printf, unit, '      Oct: ', full_mass_gchp_t(9),  full_mass_gcc_t(9) 
      printf, unit, '      Nov: ', full_mass_gchp_t(10), full_mass_gcc_t(10)
      printf, unit, '      Dec: ', full_mass_gchp_t(11), full_mass_gcc_t(11)
      printf, unit, '   Troposphere: GCHP        GCC'
      printf, unit, '      Jan: ', trop_mass_gchp_t(0),  trop_mass_gcc_t(0) 
      printf, unit, '      Feb: ', trop_mass_gchp_t(1),  trop_mass_gcc_t(1) 
      printf, unit, '      Mar: ', trop_mass_gchp_t(2),  trop_mass_gcc_t(2) 
      printf, unit, '      Apr: ', trop_mass_gchp_t(3),  trop_mass_gcc_t(3) 
      printf, unit, '      May: ', trop_mass_gchp_t(4),  trop_mass_gcc_t(4) 
      printf, unit, '      Jun: ', trop_mass_gchp_t(5),  trop_mass_gcc_t(5) 
      printf, unit, '      Jul: ', trop_mass_gchp_t(6),  trop_mass_gcc_t(6) 
      printf, unit, '      Aug: ', trop_mass_gchp_t(7),  trop_mass_gcc_t(7) 
      printf, unit, '      Sep: ', trop_mass_gchp_t(8),  trop_mass_gcc_t(8) 
      printf, unit, '      Oct: ', trop_mass_gchp_t(9),  trop_mass_gcc_t(9) 
      printf, unit, '      Nov: ', trop_mass_gchp_t(10), trop_mass_gcc_t(10)
      printf, unit, '      Dec: ', trop_mass_gchp_t(11), trop_mass_gcc_t(11)

      ; Also print to screen
      print, ' '
      print, tracer_str[itrac], ' burden [Tg]'
      print, '   Strat + Trop: GCHP        GCC'
      print, '      Jan: ', full_mass_gchp_t(0),  full_mass_gcc_t(0) 
      print, '      Feb: ', full_mass_gchp_t(1),  full_mass_gcc_t(1) 
      print, '      Mar: ', full_mass_gchp_t(2),  full_mass_gcc_t(2) 
      print, '      Apr: ', full_mass_gchp_t(3),  full_mass_gcc_t(3) 
      print, '      May: ', full_mass_gchp_t(4),  full_mass_gcc_t(4) 
      print, '      Jun: ', full_mass_gchp_t(5),  full_mass_gcc_t(5) 
      print, '      Jul: ', full_mass_gchp_t(6),  full_mass_gcc_t(6) 
      print, '      Aug: ', full_mass_gchp_t(7),  full_mass_gcc_t(7) 
      print, '      Sep: ', full_mass_gchp_t(8),  full_mass_gcc_t(8) 
      print, '      Oct: ', full_mass_gchp_t(9),  full_mass_gcc_t(9) 
      print, '      Nov: ', full_mass_gchp_t(10), full_mass_gcc_t(10)
      print, '      Dec: ', full_mass_gchp_t(11), full_mass_gcc_t(11)
      print, '   Troposphere: GCHP        GCC'
      print, '      Jan: ', trop_mass_gchp_t(0),  trop_mass_gcc_t(0) 
      print, '      Feb: ', trop_mass_gchp_t(1),  trop_mass_gcc_t(1) 
      print, '      Mar: ', trop_mass_gchp_t(2),  trop_mass_gcc_t(2) 
      print, '      Apr: ', trop_mass_gchp_t(3),  trop_mass_gcc_t(3) 
      print, '      May: ', trop_mass_gchp_t(4),  trop_mass_gcc_t(4) 
      print, '      Jun: ', trop_mass_gchp_t(5),  trop_mass_gcc_t(5) 
      print, '      Jul: ', trop_mass_gchp_t(6),  trop_mass_gcc_t(6) 
      print, '      Aug: ', trop_mass_gchp_t(7),  trop_mass_gcc_t(7) 
      print, '      Sep: ', trop_mass_gchp_t(8),  trop_mass_gcc_t(8) 
      print, '      Oct: ', trop_mass_gchp_t(9),  trop_mass_gcc_t(9) 
      print, '      Nov: ', trop_mass_gchp_t(10), trop_mass_gcc_t(10)
      print, '      Dec: ', trop_mass_gchp_t(11), trop_mass_gcc_t(11)

      ; Also create time series plot

   endfor  ;itrac

   ; Close output file
   free_lun, unit

end


