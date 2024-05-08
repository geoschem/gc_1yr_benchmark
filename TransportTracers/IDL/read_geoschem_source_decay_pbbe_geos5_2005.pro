;hyl,1/20/2012, hongyu.liu-1@nasa.gov

pro read_geoschem_source_decay_pbbe_geos5_2005, year, filename, code, source, decay, wetdepflx, drydepflx, xmid_out, ymid_out, zmid_out

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

 source = fltarr(IM, JM, LM, 3, 12)   ;geos5-das
 decay  = fltarr(IM, JM, LM, 3, 12)

 for itrac = 0,2 do begin

;choose tracer #
 case ( itrac ) of 
 0: begin & ntrac=1  &  CharTrac='Rn'  &  SubTrac='Rn'  & end      
 1: begin & ntrac=2  &  CharTrac='Pb'  &  SubTrac='Pb'  & end     
 2: begin & ntrac=4  &  CharTrac='Be7' &  SubTrac='Be7' & end  
 endcase

;layers saved & to be plotted
 lev1=1  & lev2=LM ;29.7km
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

if (IMONTH eq 0 ) then begin
  success = ctm_get_datablock(DXY_M2, 'DXYP',                              $   ;for Jan only
                              XMid=XMid_out, YMid=YMid_out, ZMid=ZMid_out, $
                              Tracer=25001, Tau0=time(imonth),              $
                              Lat = [limit(0), limit(2)],                  $
                              Lon = [limit(1), limit(3)],                  $
                              Lev = [lev1, lev2],                          $
                              FileName=filename)
  DXY = DXY_M2 * 1e+4  ;cm2
endif

;get_field_3d_4x5, code, ntrac, 'IJ-AVG-$', lev1, lev2, time(imonth), limit, filename, std_3d, xmid, ymid, zmid
;help, std_3d

success = ctm_get_datablock(std_source, 'RN--SRCE',                       $
                             XMid=XMid_out, YMid=YMid_out, ZMid=ZMid_out, $
                             Tracer=ntrac, Tau0=time(imonth),                      $
                             Lat = [limit(0), limit(2)],                  $
                             Lon = [limit(1), limit(3)],                  $
                             Lev = [lev1, lev2],                          $
                             FileName=filename )
success = ctm_get_datablock(std_decay, 'RN-DECAY',                       $
                             XMid=XMid_out, YMid=YMid_out, ZMid=ZMid_out, $
                             Tracer=ntrac, Tau0=time(imonth),          $
                             Lat = [limit(0), limit(2)],      $
                             Lon = [limit(1), limit(3)],      $
                             Lev = [lev1, lev2],              $
                             FileName=filename )

;totdepflx(*,*,itrac-1,IMONTH) = std_wetdls + std_wetdcv + std_dryd
 for i=0,IM-1 do begin
 for j=0,JM-1 do begin
 for k=0,LM-1 do begin
   source(i,j,k,itrac,IMONTH) =  std_source (i,j,k)  
   decay (i,j,k,itrac,IMONTH) =  std_decay  (i,j,k) 
   if ( source(i,j,k,itrac,IMONTH) lt 0.) then stop 
   if ( decay (i,j,k,itrac,IMONTH) lt 0.) then stop
 endfor
 endfor
 endfor

;kg/s --> kg/m2/month
 source(*,*,*,itrac,IMONTH) = source(*,*,*,itrac,IMONTH) / (DXY*1.e-4) * 3600.*24.*365./12. 
 decay (*,*,*,itrac,IMONTH) = decay (*,*,*,itrac,IMONTH) / (DXY*1.e-4) * 3600.*24.*365./12. 

endfor  ;IMONTH

endfor  ;IYEAR

endfor  ;itrac

end

