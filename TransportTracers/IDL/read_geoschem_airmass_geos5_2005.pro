;hyl,1/20/2011, hongyu.liu-1@nasa.gov

pro read_geoschem_airmass_geos5_2005, year, filename, code, AD, $
                                      xmid, ymid, zmid

modelinfo = CTM_type(code, res=4)
gridinfo  = CTM_grid(modelinfo, psurf=986)  
;xmid = gridinfo.xmid
;ymid = gridinfo.ymid
;zmid = gridinfo.zmid
dao_sige = gridinfo.sigmid
IM = gridinfo.IMX
JM = gridinfo.JMX
LM = gridinfo.LMX
PMID = gridinfo.pmid
;print, 'IM,JM,LM= ', IM,JM,LM
;print, 'pmid= ', pmid

 AD = fltarr(72, 46, LM,12)   ;geos5-das

 for itrac = 0,0 do begin

;layers saved & to be plotted
 lev1=1  & lev2=LM ;47L - 29.7km
 XRange=[-90,90]   ;Lat
 limit =  [XRange(0), -180, XRange(1), 180]

;--------------------- loop over 1 year
YEARS = ['01' ]
;2005
;MONTHS= ['01', '04', '07','10']
;CharMON = ['January','April','July','October']

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

 time = [ long(tau1), long(tau2), long(tau3), long(tau4), long(tau5), long(tau6), $
          long(tau7), long(tau8), long(tau9), long(tau10),long(tau11), long(tau12)]

 for IYEAR = 0, n_elements( YEARS ) -1 do begin
   IYR = YEARS ( IYEAR )

;--------------------- loop over 4 months
 for IMONTH = 0, n_elements(time) -1 do begin

;get_field_3d_4x5, 24002, 'BXHGHT-$', lev1, lev2, time(imonth), limit,
;filename, std_3d, xmid, ymid, zmid
get_field_3d_4x5, code, 24002, 'BXHGHT-$', lev1, lev2, time(imonth), limit, filename, std_3d, xmid, ymid, zmid
help, std_3d

 AD(*,*,*,IMONTH) = std_3d

;print, 'max(std_zm), min(std_zm)= ', max(std_zm), min(std_zm)

endfor  ;IMONTH

endfor  ;IYEAR

endfor  ;itrac

end
