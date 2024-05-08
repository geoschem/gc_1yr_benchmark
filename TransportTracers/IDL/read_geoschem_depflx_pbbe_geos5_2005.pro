;hyl, 1/19/2012 (hongyu.liu-1@nasa.gov)

pro read_geoschem_depflx_pbbe_geos5_2005, year,  filename, code, $
                                          wetdepflx_ls, wetdepflx_cv, $
                                          drydepflx, xmid_out, ymid_out, zmid

 wetdepflx_ls = fltarr(72, 46, 2, 12)   ;geos5-das
 wetdepflx_cv = fltarr(72, 46, 2, 12)   ;geos5-das
 drydepflx = fltarr(72, 46, 2, 12)

 for itrac = 1,2 do begin

;choose tracer #
 case ( itrac ) of 
;0: begin & ntrac=1     &  CharTrac='Rn'  &  SubTrac='Rn'  & end      
 1: begin & ntrac=2  &  CharTrac='Pb'  &  SubTrac='Pb'  & end     
 2: begin & ntrac=4  &  CharTrac='Be7' &  SubTrac='Be7' & end  
 endcase

 XRange=[-90,90]   ;Lat
 limit =  [XRange(0), -180, XRange(1), 180]

;--------------------- loop over 1 year
 YEARS = ['01' ]
 
if ( Year eq '2013' ) then begin
   YYYY  = ['2013']
   yymmdd2tau0, '130101', TAU1
   yymmdd2tau0, '130201', TAU2
   yymmdd2tau0, '130301', TAU3
   yymmdd2tau0, '130401', TAU4
   yymmdd2tau0, '130501', TAU5
   yymmdd2tau0, '130601', TAU6
   yymmdd2tau0, '130701', TAU7
   yymmdd2tau0, '130801', TAU8
   yymmdd2tau0, '130901', TAU9
   yymmdd2tau0, '131001', TAU10
   yymmdd2tau0, '131101', TAU11
   yymmdd2tau0, '131201', TAU12
endif else if ( Year eq '2016' ) then begin
   YYYY  = ['2016']
   yymmdd2tau0, '160101', TAU1
   yymmdd2tau0, '160201', TAU2
   yymmdd2tau0, '160301', TAU3
   yymmdd2tau0, '160401', TAU4
   yymmdd2tau0, '160501', TAU5
   yymmdd2tau0, '160601', TAU6
   yymmdd2tau0, '160701', TAU7
   yymmdd2tau0, '160801', TAU8
   yymmdd2tau0, '160901', TAU9
   yymmdd2tau0, '161001', TAU10
   yymmdd2tau0, '161101', TAU11
   yymmdd2tau0, '161201', TAU12
endif

 time = [ long(tau1), long(tau2), long(tau3), long(tau4), long(tau5), long(tau6),   $
          long(tau7), long(tau8), long(tau9), long(tau10),long(tau11), long(tau12)]

 for IYEAR = 0, n_elements( YEARS ) -1 do begin
   IYR = YEARS ( IYEAR )

;--------------------- loop over 4 months
 for IMONTH = 0, n_elements(time) -1 do begin

modelinfo = CTM_type(code, res=4)
gridinfo  = CTM_grid(modelinfo, psurf=986) 
xmid = gridinfo.xmid
ymid = gridinfo.ymid
zmid = gridinfo.zmid
dao_sige = gridinfo.sigmid
IM = gridinfo.IMX
JM = gridinfo.JMX
LM = gridinfo.LMX
PMID = gridinfo.pmid
;print, 'IM,JM,LM= ', IM,JM,LM
;print, 'pmid= ', pmid

limit = [-90,-180,90,180]

if (IMONTH eq 0 ) then begin
  success = ctm_get_datablock(DXY_M2, 'DXYP',                              $   ;for Jan only
                              XMid=XMid_out, YMid=YMid_out, ZMid=ZMid_out, $
                              Tracer=25001, Tau0=TAU1,                     $
                              Lat = [limit(0), limit(2)],                  $
                              Lon = [limit(1), limit(3)],                  $
                              FileName=filename)
  DXY = DXY_M2 * 1e+4  ;cm2
endif

success = ctm_get_datablock(std_wetdls, 'WETDLS-$',                       $
                             XMid=XMid_out, YMid=YMid_out, ZMid=ZMid_out, $
                             Tracer=ntrac, Tau0=time(imonth),             $
                             Lat = [limit(0), limit(2)],                  $
                             Lon = [limit(1), limit(3)],                  $
                            ;Lev = [lev1, lev2],                          $
                             FileName=filename )
success = ctm_get_datablock(std_wetdcv, 'WETDCV-$',                       $
                             XMid=XMid_out, YMid=YMid_out, ZMid=ZMid_out, $
                             Tracer=ntrac, Tau0=time(imonth),             $
                             Lat = [limit(0), limit(2)],      $
                             Lon = [limit(1), limit(3)],      $
                            ;Lev = [lev1, lev2],              $
                             FileName=filename )
success = ctm_get_datablock(std_dryd, 'DRYD-FLX',                         $
                             XMid=XMid_out, YMid=YMid_out, ZMid=ZMid_out, $
                             Tracer=ntrac, Tau0=time(imonth),             $
                             Lat = [limit(0), limit(2)],      $
                             Lon = [limit(1), limit(3)],      $
                            ;Lev = [lev1, lev2],              $
                             FileName=filename )

;totdepflx(*,*,itrac-1,IMONTH) = std_wetdls + std_wetdcv + std_dryd
 for i=0,IM-1 do begin
 for j=0,JM-1 do begin
   wetdepflx_ls(i,j,itrac-1,IMONTH) = total ( std_wetdls (i,j,*) )
   wetdepflx_cv(i,j,itrac-1,IMONTH) = total ( std_wetdcv (i,j,*) )
   if ( wetdepflx_cv (i,j,itrac-1,IMONTH) lt 0.) then begin
      print, 'wetdepflx_cv (',i,j,itrac-1,IMONTH,')= ',wetdepflx_cv (i,j,itrac-1,IMONTH)
;      stop
    endif

 endfor
 endfor
 drydepflx(*,*,itrac-1,IMONTH) = std_dryd

;kg/s --> kg/m2/month
 wetdepflx_ls(*,*,itrac-1,IMONTH) = wetdepflx_ls(*,*,itrac-1,IMONTH) / (DXY*1.e-4) * 3600.*24.*365./12. 
 wetdepflx_cv(*,*,itrac-1,IMONTH) = wetdepflx_cv(*,*,itrac-1,IMONTH) / (DXY*1.e-4) * 3600.*24.*365./12.

;-----------------------------------------------------------------------------
; Prior to 6/15/15:
; drydepflx(*,*,itrac-1,IMONTH) = drydepflx(*,*,itrac-1,IMONTH) / (DXY*1.e-4) * 3600.*24.*365./12. 
;-----------------------------------------------------------------------------
; Drydep diagnostic is now in units molec/cm3/s (mps, 6/15/15)
; molec/cm3/s --> kg/m2/month
 if ( itrac eq 1 ) then begin
 drydepflx(*,*,itrac-1,IMONTH) = drydepflx(*,*,itrac-1,IMONTH) / 6.022e23 * 210e-3 *1.e4 * 3600.*24.*365./12. 
 endif 
 if ( itrac eq 2 ) then begin
 drydepflx(*,*,itrac-1,IMONTH) = drydepflx(*,*,itrac-1,IMONTH) / 6.022e23 * 7e-3 *1.e4 * 3600.*24.*365./12. 
 endif

;print, 'max(std_zm), min(std_zm)= ', max(std_zm), min(std_zm)

endfor  ;IMONTH

endfor  ;IYEAR

endfor  ;itrac

end
