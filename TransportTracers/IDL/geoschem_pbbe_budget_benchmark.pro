;calculate the global budgets of 210Pb and 7Be using v8-03-02, v9-01-01, v9-01-02, v9-01-03e
;hyl,1/20/12, hongyu.liu-1@nasa.gov

pro geoschem_pbbe_budget_benchmark, Year,  Met,    Res,    Version, $
                                    InDir, InFile, OutDir, _EXTRA=e

   ; Output filenames
   OutFile1 = OutDir + Version + '.Pb-Be_budget_trop_strat.txt'
   OutFile2 = OutDir + Version + '.Pb-Be_budget_troposphere.txt'

   ; Determine if it is a leap year
   if ( year eq '1984' or year eq '1988' or year eq '1992' or $
        year eq '1996' or year eq '2000' or year eq '2004' or $
        year eq '2008' or year eq '2012' or '2016' ) then begin
      ndays = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
   endif else begin
      ndays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
   endelse

   ; Get model and grid info
   modelinfo = CTM_type(Met, res=Res)
   gridinfo  = CTM_grid(modelinfo, psurf=986) 
   xmid      = gridinfo.xmid
   ymid      = gridinfo.ymid
   zmid      = gridinfo.zmid
   dao_sige  = gridinfo.sigmid
   IM        = gridinfo.IMX
   JM        = gridinfo.JMX
   LM        = gridinfo.LMX
   PMID      = gridinfo.pmid

   ;------------------------------------------------
   ; Read tropopause height
   ;------------------------------------------------
   read_geoschem_trpause_geos5_2005, Year, InFile, Met, trpause,$
                                     xmid_out, ymid_out, zmid_out
   ltpause = intarr (IM,JM)
   for i=0,IM-1 do begin
   for j=0,JM-1 do begin
     ltpause(i,j) = mean (trpause(i,j,0,*))
   endfor
   endfor

   ; Debug
   ;help, trpause
   ;print, 'trpause in level: ', min(trpause(*,*,0,*)), max(trpause(*,*,0,*))
   ;print, 'trpause in height:', min(trpause(*,*,1,*)), max(trpause(*,*,1,*))
   ;print, 'trpause in mbar: ',  min(trpause(*,*,2,*)), max(trpause(*,*,2,*))
   ;help, ltpause
   ;print, min(ltpause), max(ltpause)    ;27, 35
   ;print, min(pmid (ltpause)), max(pmid (ltpause)), $
   ;       median(pmid (ltpause)), $
   ;       mean (pmid (ltpause))         ;85.4390 311.616 226.280 189.63
   ;print, min(pmid (ltpause-1)), max(pmid (ltpause-1)), $
   ;       median(pmid (ltpause-1)), $
   ;       mean (pmid (ltpause-1))       ;100.514 354.463 265.870 222.518
   ;stop

   ;------------------------------------------------
   ; Read grid box surface area
   ;------------------------------------------------
   XRange=[-90,90]   ;Lat
   limit =  [XRange(0), -180, XRange(1), 180]
   yyyymmdd = YEAR + '0101'
   yymmdd2tau0, yyyymmdd, TAU1
   ; for Jan only
   success = ctm_get_datablock(DXY_M2, 'DXYP',                             $
                              XMid=XMid_out, YMid=YMid_out, ZMid=ZMid_out, $
                              Tracer=25001, Tau0=TAU1,                     $
                              Lat = [limit(0), limit(2)],                  $
                              Lon = [limit(1), limit(3)],                  $
                              FileName=InFile)
   darea = DXY_M2   ;m2

   ;------------------------------------------------
   ; Read air mass
   ;------------------------------------------------
   read_geoschem_airmass_geos5_2005, Year, InFile, Met, airmass, $
                                     xmid, ymid, zmid

   ; Debug
   ;help, airmass
   ;print, 'airmass= ', min(airmass), max(airmass)
   ;stop


   ; Debug
   ;help, zsurf, dz3d, darea, ltpause, htpause
   ;print, darea(143,89,0), darea(72,45,0), min(ltpause), max(ltpause), fix(min(ltpause)+0.5), fix(max(ltpause)+0.5)
   ;dz3d=array[144,90,24,12], darea=Array[144,90,12], rho_air=array[144,90,24,12], ltpause,htpause=Array[144,90,12]

   ;------------------------------------------------
   ; Read RnPbBe emissions and loss
   ;------------------------------------------------
   read_geoschem_source_decay_pbbe_geos5_2005, year, InFile, Met, $
      source, decay, xmid_out, ymid_out, zmid

   ; Debug
   ;help, source, decay, xmid_out, ymid_out, zmid
   ;print, 'Rn source: ', min(source(*,*,*,0,*)), max(source(*,*,*,0,*))    ;kg/m2/month  
   ;print, 'Pb source: ', min(source(*,*,*,1,*)), max(source(*,*,*,1,*))    
   ;print, 'Be source: ', min(source(*,*,*,2,*)), max(source(*,*,*,2,*))
   ;print, 'Rn/Pb/Be decay: ', min(decay), max(decay)    

   radon222_source = reform (source(*,*,*,0,*))
   lead210_source  = reform (source(*,*,*,1,*))
   be7_source      = reform (source(*,*,*,2,*))
   radon222_decay  = reform (decay (*,*,*,0,*))
   lead210_decay   = reform (decay (*,*,*,1,*))
   be7_decay       = reform (decay (*,*,*,2,*))

   ;------------------------------------------------
   ; Read loss of tracer to wetdep and drydep
   ;------------------------------------------------
   modpbwdep = fltarr (IM,JM,12)
   modbewdep = fltarr (IM,JM,12)
   modpbddep = fltarr (IM,JM,12)
   modbeddep = fltarr (IM,JM,12)
   read_geoschem_depflx_pbbe_geos5_2005, Year, InFile, Met, wetdep_ls, $
      wetdep_cv, drydep, xmid, ymid, zmid_geos5
   LM_gc_geos5 = n_elements(zmid_geos5)

   ; Debug
   ;help, wetdep_ls, wetdep_cv, drydep, xmid, ymid, zmid_geos5
   ;print, min(wetdep_ls), max(wetdep_ls)   ;Array[72, 46, 2, 12]
   ;print, min(drydep), max(drydep)

   modpbwdep_ls = reform (wetdep_ls (*,*,0,*))
   modbewdep_ls = reform (wetdep_ls (*,*,1,*))

   modpbwdep_cv = reform (wetdep_cv (*,*,0,*))
   modbewdep_cv = reform (wetdep_cv (*,*,1,*))

   modpbddep = reform (drydep (*,*,0,*))
   modbeddep = reform (drydep (*,*,1,*))

   ;------------------------------------------------
   ; Read tracer concentrations in v/v
   ;------------------------------------------------
   read_geoschem_rnpbbe_geos5_2005, Year, InFile, Met, const, $
      xmid, ymid, zmid   
 ;const = fltarr(IM, JM, LM, 3, 12)
   modrn  = reform (const (*,*,*,0,*))
   modpb  = reform (const (*,*,*,1,*))
   modbe7 = reform (const (*,*,*,2,*))

   ; This assumes units for RnPbBe concentrations are mBq/SCM in tracerinfo.dat
   ; Need to convert to v/v for this routine (mps, 6/15/15)
   modrn  = modrn  / 5.6397E22   ;Rn  mBq/SCM --> v/v
   modpb  = modpb  / 2.6141E19   ;Pb  mBq/SCM --> v/v
   modbe7 = modbe7 / 4.0513E21   ;Be7 mBq/SCM --> v/v

   mw_air = 28.9644*1e-3   ;kg/mole
   modlon = xmid
   modlat = ymid
   modpres = pmid

   tau_lead210 = 0.
   tau_be7     = 0.

   ;------------------------------------------------
   ; Calculate budget
   ;------------------------------------------------
   ; Loop over 12 months
   for m=0,12-1 do begin
   
      if (m eq 0) then begin
         temp_radon222_source = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12) 
         temp_radon222_decay  = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12) 
         temp_lead210_source  = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12) 
         temp_lead210_decay   = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12)
         temp_be7_source      = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12)   
         temp_be7_decay       = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12)
         temp_modpbwdep_ls    = fltarr(n_elements(modlon), $
                                n_elements(modlat), 12)
         temp_modpbwdep_cv    = fltarr(n_elements(modlon), $
                                n_elements(modlat), 12)
         temp_modpbddep       = fltarr(n_elements(modlon), $
                                n_elements(modlat), 12)
         temp_modbewdep_ls    = fltarr(n_elements(modlon), $
                                n_elements(modlat), 12)
         temp_modbewdep_cv    = fltarr(n_elements(modlon), $
                                n_elements(modlat), 12)
         temp_modbeddep       = fltarr(n_elements(modlon), $
                                n_elements(modlat), 12)
         temp_mass_air        = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12)
         temp_mass_radon222   = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12)
         temp_mass_lead210    = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12)
         temp_mass_be7        = fltarr(n_elements(modlon), $
                                n_elements(modlat), n_elements(modpres), 12)
         month_mass_radon222t = fltarr(12) 
         month_mass_lead210t  = fltarr(12)
         month_mass_be7t      = fltarr(12)
         month_source_lead210t= fltarr(12)
         month_source_be7t    = fltarr(12)
         month_decay_radon222t= fltarr(12)
         month_decay_lead210t = fltarr(12)
         month_decay_be7t     = fltarr(12)
      endif

      ;kg/m2/month --> g/month
      ;help, radon222_source, modpbwdep_ls, modpbwdep_cv, modpbddep, $
      ;      modbewdep_ls, modbewdep_cv, modbeddep

      temp_radon222_source(*,*,m) = radon222_source (*,*,0,m) * $
                                    darea(*, *) * 1000. 
      temp_modpbwdep_ls   (*,*,m) = modpbwdep_ls    (*,*,m)   * $
                                    darea(*, *) * 1000. 
      temp_modpbwdep_cv   (*,*,m) = modpbwdep_cv    (*,*,m)   * $
                                    darea(*, *) * 1000. 
      temp_modpbddep      (*,*,m) = modpbddep       (*,*,m)   * $
                                    darea(*, *) * 1000. 
      temp_modbewdep_ls   (*,*,m) = modbewdep_ls    (*,*,m)   * $
                                    darea(*, *) * 1000. 
      temp_modbewdep_cv   (*,*,m) = modbewdep_cv    (*,*,m)   * $
                                    darea(*, *) * 1000. 
      temp_modbeddep      (*,*,m) = modbeddep       (*,*,m)   * $
                                    darea(*, *) * 1000. 

      ;help, radon222_decay

      for k=0,n_elements(modpres)-1 do begin
         temp_radon222_decay(*,*,k,m) = radon222_decay (*,*,k,m) * $
                                        darea (*, *) * 1000. 
         temp_lead210_source(*,*,k,m) = lead210_source (*,*,k,m) * $
                                        darea (*, *) * 1000. 
         temp_lead210_decay(*,*,k,m)  = lead210_decay  (*,*,k,m) * $
                                        darea (*, *) * 1000. 
         temp_be7_source(*,*,k,m)     = be7_source (*,*,k,m) *     $
                                        darea (*, *) * 1000.
         temp_be7_decay(*,*,k,m)      = be7_decay  (*,*,k,m) *     $
                                        darea (*, *) * 1000. 
         temp_mass_air(*,*,k,m)       = airmass(*,*,k,m)  
         temp_mass_radon222(*,*,k,m)  = temp_mass_air(*,*,k,m) /   $
                                        mw_air * modrn(*, *, k, m) * 222.
         temp_mass_lead210(*,*,k,m)   = temp_mass_air(*,*,k,m) /   $
                                        mw_air * modpb(*, *, k, m) * 210.
         temp_mass_be7(*,*,k,m)       = temp_mass_air(*,*,k,m) /   $
                                        mw_air * modbe7(*, *, k, m) * 7.  
      endfor

      month_mass_radon222t(m)  = 0.
      month_mass_lead210t(m)   = 0.
      month_mass_be7t(m)       = 0.
      month_source_lead210t(m) = 0.
      month_source_be7t(m)     = 0.
      month_decay_radon222t(m) = 0.
      month_decay_lead210t(m)  = 0.
      month_decay_be7t(m)      = 0.

      for i=0,n_elements(modlon)-1 do begin
      for j=0,n_elements(modlat)-1 do begin 
      for k=1-1, ltpause(i,j)  -1  do begin
         month_mass_radon222t(m)  = month_mass_radon222t(m) +      $
                                    temp_mass_radon222(i, j, k, m)
         month_mass_lead210t(m)   = month_mass_lead210t(m)  +      $
                                    temp_mass_lead210(i, j, k, m)
         month_mass_be7t(m)       = month_mass_be7t(m) +           $
                                    temp_mass_be7(i, j, k, m)
         month_source_lead210t(m) = month_source_lead210t(m) +     $
                                    temp_lead210_source(i, j, k, m)
         month_source_be7t(m)     = month_source_be7t(m) +         $
                                    temp_be7_source(i, j, k, m)
         month_decay_radon222t(m) = month_decay_radon222t(m) +     $
                                    temp_radon222_decay(i, j, k, m)
         month_decay_lead210t(m)  = month_decay_lead210t(m) +      $
                                    temp_lead210_decay(i, j, k, m)
         month_decay_be7t(m)      = month_decay_be7t(m) +          $
                                    temp_be7_decay(i, j, k, m)
      endfor
      endfor
      endfor

      tau_lead210 = tau_lead210 + 1. / $
                     (  month_mass_lead210t(m)  $
                     / total(temp_modpbwdep_ls(*,*,m)+ $
                     temp_modpbwdep_cv(*,*,m)+ $
                     temp_modpbddep(*,*,m)) *ndays(m) $
                     ) 
      tau_be7     = tau_be7 + 1. / $ 
                     (  month_mass_be7t(m)  $
                     / total(temp_modbewdep_ls(*,*,m)+ $
                     temp_modbewdep_cv(*,*,m)+ $
                     temp_modbeddep(*,*,m)) *ndays(m) $
                     ) 

   endfor  ;m

   ;print, 'Radon source per year (g): ', total ( temp_radon222_source(*,*,*) )

   aa_radon222_source= total (temp_radon222_source(*,*,  *)) / $
                       float(total(ndays(*)))  
   aa_radon222_decay = total (temp_radon222_decay (*,*,*,*)) / $
                       float(total(ndays(*)))  

   aa_lead210_source = total (temp_lead210_source (*,*,*,*)) / $
                       float(total(ndays(*)))  
   aa_lead210_decay  = total (temp_lead210_decay  (*,*,*,*)) / $
                       float(total(ndays(*))) 

   aa_be7_source     = total (temp_be7_source     (*,*,*,*)) / $
                       float(total(ndays(*))) 
   aa_be7_decay      = total (temp_be7_decay      (*,*,*,*)) / $
                       float(total(ndays(*))) 

   aa_modpbwdep_ls   = total (temp_modpbwdep_ls   (*,*,  *)) / $
                       float(total(ndays(*)))  
   aa_modpbwdep_cv   = total (temp_modpbwdep_cv   (*,*,  *)) / $
                       float(total(ndays(*)))  
   aa_modpbddep      = total (temp_modpbddep      (*,*,  *)) / $
                       float(total(ndays(*))) 

   aa_modbewdep_ls   = total (temp_modbewdep_ls   (*,*,  *)) / $
                       float(total(ndays(*)))  
   aa_modbewdep_cv   = total (temp_modbewdep_cv   (*,*,  *)) / $
                       float(total(ndays(*)))  
   aa_modbeddep      = total (temp_modbeddep      (*,*,  *)) / $
                       float(total(ndays(*)))  

   mass_radon222     = total (temp_mass_radon222  (*,*,*,*)) / 12.  
   mass_lead210      = total (temp_mass_lead210   (*,*,*,*)) / 12.
   mass_be7          = total (temp_mass_be7       (*,*,*,*)) / 12.

   ;------------------------------------------------
   ; Compute accumulation term (dm/dt)
   ;------------------------------------------------

   ; Start and end restart files
   if ( Year eq '2013' ) then begin
      Rst1 = InDir + 'restarts/GEOSChem.Restart.20130101_0000z.nc4'
      Rst2 = InDir + 'restarts/GEOSChem.Restart.20140101_0000z.nc4'
   endif else if ( Year eq '2016' ) then begin
      Rst1 = InDir + 'restarts/GEOSChem.Restart.20160101_0000z.nc4'
      Rst2 = InDir + 'restarts/GEOSChem.Restart.20170101_0000z.nc4'
   endif

   ; Read start concentrations (v/v)
   ncdf_read, input1, filename=Rst1, /all,                 $
              varnames=vars, vardimid=varid, attribute=att,    $
              dimnames=dims, dims=dimlen
   PbConc1 = input1.SpeciesRst_Pb210
   BeConc1 = input1.SpeciesRst_Be7

   ; Read end concentrations (v/v)
   ncdf_read, input2, filename=Rst2, /all,                 $
              varnames=vars, vardimid=varid, attribute=att,    $
              dimnames=dims, dims=dimlen
   PbConc2 = input2.SpeciesRst_Pb210
   BeConc2 = input2.SpeciesRst_Be7

   ; Set dimensions
   ; Dimensions in file saved out from Restart collection in HISTORY.rc
   ; are time, lev, ilev, lat, lon (mps, 1/23/19)
   nlon = dimlen[4]
   nlat = dimlen[3]
   nlev = dimlen[1]
   ntime= dimlen[0] 

   airmass1 = fltarr(nlon,nlat,nlev)
   airmass2 = fltarr(nlon,nlat,nlev)
   PbMass1  = fltarr(nlon,nlat,nlev)
   BeMass1  = fltarr(nlon,nlat,nlev)
   PbMass2  = fltarr(nlon,nlat,nlev)
   BeMass2  = fltarr(nlon,nlat,nlev)
   PbMass1t = fltarr(nlon,nlat,nlev)
   BeMass1t = fltarr(nlon,nlat,nlev)
   PbMass2t = fltarr(nlon,nlat,nlev)
   BeMass2t = fltarr(nlon,nlat,nlev)

   ; Loop over all levels and convert v/v --> kg
   for k = 0, nlev-1 do begin
      airmass1(*,*,k) = airmass(*,*,k,0) 
      PbMass1(*,*,k) = PbConc1(*,*,k) * airmass1(*,*,k) / ( mw_air / 210. )
      BeMass1(*,*,k) = BeConc1(*,*,k) * airmass1(*,*,k) / ( mw_air / 7.   )

      airmass2(*,*,k) = airmass(*,*,k,11)
      PbMass2(*,*,k) = PbConc2(*,*,k) * airmass2(*,*,k) / ( mw_air / 210. )
      BeMass2(*,*,k) = BeConc2(*,*,k) * airmass2(*,*,k) / ( mw_air / 7.   )
   endfor

   ; Loop over the tropopause and convert v/v -->kg
   for i=0,nlon-1 do begin
   for j=0,nlat-1 do begin 
   for k=0,ltpause(i,j)  -1  do begin
      airmass1(*,*,k) = airmass(*,*,k,0) 
      PbMass1t(*,*,k) = PbConc1(*,*,k) * airmass1(*,*,k) / ( mw_air / 210. )
      BeMass1t(*,*,k) = BeConc1(*,*,k) * airmass1(*,*,k) / ( mw_air / 7.   )

      airmass2(*,*,k) = airmass(*,*,k,11)
      PbMass2t(*,*,k) = PbConc2(*,*,k) * airmass2(*,*,k) / ( mw_air / 210. )
      BeMass2t(*,*,k) = BeConc2(*,*,k) * airmass2(*,*,k) / ( mw_air / 7.   )
   endfor
   endfor
   endfor

   ; Compute change in mass
   PbAccumTerm     = total(PbMass2 (*,*,*)) - total(PbMass1 (*,*,*))
   BeAccumTerm     = total(BeMass2 (*,*,*)) - total(BeMass1 (*,*,*))

   PbAccumTermTrop = total(PbMass2t(*,*,*)) - total(PbMass1t(*,*,*))
   BeAccumTermTrop = total(BeMass2t(*,*,*)) - total(BeMass1t(*,*,*))

   ;------------------------------------------------
   ; Print summary to screen
   ;------------------------------------------------
   print,'---------------- Annual Average Global Budgets of 210Pb and 7Be -----------------' 
   print,'aa_radon222_source (g/day,kg/yr) = ', $
          aa_radon222_source, aa_radon222_source * 365./1000.
   print,'aa_radon222_decay  (g/day,kg/yr) = ', $
          aa_radon222_decay,  aa_radon222_decay  * 365./1000.
   print,'aa_lead210_source  (g/day,kg/yr) = ', $
          aa_lead210_source,  aa_lead210_source  * 365./1000.
   print,'aa_lead210_decay   (g/day,kg/yr) = ', $
          aa_lead210_decay,   aa_lead210_decay   * 365./1000.
   print,'aa_be7_source      (g/day,kg/yr) = ', $
          aa_be7_source,      aa_be7_source      * 365./1000.
   print,'aa_be7_decay       (g/day,kg/yr) = ', $
          aa_be7_decay,       aa_be7_decay       * 365./1000.

   print,'aa_modpbwdep_ls    (g/day,kg/yr) = ', $
          aa_modpbwdep_ls,    aa_modpbwdep_ls    * 365./1000.
   print,'aa_modpbwdep_cv    (g/day,kg/yr) = ', $
          aa_modpbwdep_cv,    aa_modpbwdep_cv    * 365./1000.
   print,'aa_modpbddep       (g/day,kg/yr) = ', $
          aa_modpbddep,       aa_modpbddep       * 365./1000.
   print,'aa_modbewdep_ls    (g/day,kg/yr) = ', $
          aa_modbewdep_ls,    aa_modbewdep_ls    * 365./1000.
   print,'aa_modbewdep_cv    (g/day,kg/yr) = ', $
          aa_modbewdep_cv,    aa_modbewdep_cv    * 365./1000.
   print,'aa_modbeddep       (g/day,kg/yr) = ', $
          aa_modbeddep,       aa_modbeddep       * 365./1000.

   print,'mass_radon222 (burden, g) = ', mass_radon222
   print,'mass_lead210  (burden, g) = ', mass_lead210
   print,'mass_be7      (burden, g) = ', mass_be7

   print,'mass_radon222t(burden, g) = ', total (month_mass_radon222t(*))/12.
   print,'mass_lead210t (burden, g) = ', total (month_mass_lead210t(*))/12.
   print,'mass_be7t     (burgen, g) = ', total (month_mass_be7t(*))/12.

   print,'----------------- check if source/sink balances ------------'
   print, 'radon222 source, sink (g/day)= ',      $
           aa_radon222_source, aa_radon222_decay
   print, 'lead210 source, sink (g/day) = ',      $
           aa_lead210_source,  aa_lead210_decay + $
           aa_modpbwdep_ls + aa_modpbwdep_cv + aa_modpbddep

   print, 'be7 source, sink (g/day)     = ',      $
           aa_be7_source, aa_be7_decay +          $
           aa_modbewdep_ls + aa_modbewdep_cv + aa_modbeddep

   print, 'trop tau_lead210 (day)       = ', 1/(tau_lead210/12.)
   print, 'trop tau_be7(day)            = ', 1/(tau_be7/12.)
   print, 'Pb  dm/dt (g)                = ', PbAccumTerm
   print, 'Be7 dm/dt (g)                = ', BeAccumTerm

   print,'----------------- for trop budget -------------'
   print,'source_lead210t (g/day) = ', $
          total (month_source_lead210t(*)) / float(total(ndays(*)))
   print,'source_be7t (g/day)     = ', $
          total (month_source_be7t(*))     / float(total(ndays(*)))
   print,'decay_radon222t (g/day) = ', $
          total (month_decay_radon222t(*)) / float(total(ndays(*)))
   print,'decay_lead210t (g/day)  = ', $
          total (month_decay_lead210t(*))  / float(total(ndays(*)))
   print,'decay_be7t (g/day)      = ', $
          total (month_decay_be7t(*))      / float(total(ndays(*)))

   ;------------------------------------------------
   ; Produce a table for trop+strat budget 
   ;------------------------------------------------
   openW, unit, OutFile1, /get_lun
      printf, unit, 'Table 1. Annual Average Global Budgets of 210Pb and 7Be in the Troposphere+Stratosphere for ' + year
      printf, unit, '                               210Pb         7Be'
      printf, unit, '  Burden, g             ',   mass_lead210,         $
                                                  mass_be7 
      printf, unit, '  Sources, g d-1        ',   aa_lead210_source,    $
                                                  aa_be7_source
      printf, unit, '    stratosphere        ',   $
         aa_lead210_source - total (month_source_lead210t(*)) /         $
            float(total(ndays(*))), $
         aa_be7_source     - total (month_source_be7t(*))     /         $
            float(total(ndays(*)))
      printf, unit, '    troposphere         ',   $
         total (month_source_lead210t(*)) / float(total(ndays(*))),     $
         total (month_source_be7t(*))     / float(total(ndays(*)))
      printf, unit, '  Sinks, g d-1          ',   $
         aa_lead210_decay + aa_modpbwdep_ls + aa_modpbddep + aa_modpbwdep_cv, $
         aa_be7_decay     + aa_modbewdep_ls + aa_modbeddep + aa_modbewdep_cv
      printf, unit, '    dry deposition      ',   aa_modpbddep,         $
                                                  aa_modbeddep
      printf, unit, '    wet deposition      '
      printf, unit, '      stratiform        ',   aa_modpbwdep_ls,      $
                                                  aa_modbewdep_ls
      printf, unit, '      convective        ',   aa_modpbwdep_cv,      $
                                                  aa_modbewdep_cv
      printf, unit, '    radioactive decay   ',   aa_lead210_decay,     $
                                                  aa_be7_decay
      printf, unit, ' '
      printf, unit, '  Accumulation Term     '
      printf, unit, '    Initial mass, g     ', total(PbMass1(*,*,*)),  $
                                                total(BeMass1(*,*,*))
      printf, unit, '    Final   mass, g     ', total(PbMass2(*,*,*)),  $
                                                total(BeMass2(*,*,*))
      printf, unit, '    Difference, g       ', PbAccumTerm, BeAccumTerm
   free_lun, unit

   ;------------------------------------------------
   ; Produce a table for trop budget 
   ;------------------------------------------------
   openW, unit, OutFile2, /get_lun
      printf, unit, 'Table 2. Annual Average Global Budgets of 210Pb and 7Be in the Troposphere for ' + year
      printf, unit, '                               210Pb         7Be'
      printf, unit, '  Burden, g             ',                         $
         total (month_mass_lead210t(*))/ 12.,                           $
         total (month_mass_be7t(*))    / 12.
      printf, unit, '  Residence time, days  ',   1/(tau_lead210/12.),  $
                                                  1/(tau_be7/12.)
      printf, unit, '  Sources, g d-1        '
      printf, unit, '    from stratosphere   ',                         $
         total (month_decay_lead210t(*))  / float(total(ndays(*))) +    $
            aa_modpbwdep_ls + aa_modpbddep + aa_modpbwdep_cv -          $
            total (month_source_lead210t(*)) / float(total(ndays(*))),  $
         total (month_decay_be7t(*))      / float(total(ndays(*))) +    $
            aa_modbewdep_ls + aa_modbeddep + aa_modbewdep_cv -          $
            total (month_source_be7t(*))  / float(total(ndays(*)))
      printf, unit, '    within troposphere  ',                         $
         total (month_source_lead210t(*)) / float(total(ndays(*))),     $
         total (month_source_be7t(*))     / float(total(ndays(*)))
      printf, unit, '  Sinks, g d-1          '
      printf, unit, '    dry deposition      ',   aa_modpbddep,         $
                                                  aa_modbeddep
      printf, unit, '    wet deposition      ',                         $
         aa_modpbwdep_ls + aa_modpbwdep_cv,                             $
         aa_modbewdep_ls + aa_modbewdep_cv
      printf, unit, '      stratiform        ',   aa_modpbwdep_ls,      $
                                                  aa_modbewdep_ls
      printf, unit, '      convective        ',   aa_modpbwdep_cv,      $
                                                  aa_modbewdep_cv
      printf, unit, '    radioactive decay   ',                         $
         total (month_decay_lead210t(*)) / float(total(ndays(*))),      $
         total (month_decay_be7t(*)) / float(total(ndays(*)))
      printf, unit, ' '
      printf, unit, 'Accumulation Term       '
      printf, unit, '  Initial mass, g       ', total(PbMass1t(*,*,*)),  $
                                                total(BeMass1t(*,*,*))
      printf, unit, '  Final   mass, g       ', total(PbMass2t(*,*,*)),  $
                                                total(BeMass2t(*,*,*))
      printf, unit, '  Difference, g         ', PbAccumTermTrop, BeAccumTermTrop
   free_lun, unit

end
