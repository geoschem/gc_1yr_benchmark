;hyl, 1/19/2012 (hongyu.liu-1@nasa.gov)

pro read_geoschem_rnpbbe_geos5_2005, Year, filename, code, constd, $
                                     xmid, ymid, zmid

 ModelInfo = CTM_type(code, res=res)  ;Psurf can be reset
 GridInfo  = CTM_grid(ModelInfo)
 xmid = GridInfo.xmid
 ymid = GridInfo.ymid
 zmid = GridInfo.zmid
 IM = gridinfo.IMX
 JM = gridinfo.JMX
 LM = gridinfo.LMX

 constd = fltarr(IM, JM, LM, 3, 12)   ;geos5-das

 for itrac = 0,2 do begin

;choose tracer #
 case ( itrac ) of 
0: begin & ntrac=1  &  CharTrac='Rn'  &  SubTrac='Rn'  & end      
1: begin & ntrac=2  &  CharTrac='Pb'  &  SubTrac='Pb'  & end     
2: begin & ntrac=4  &  CharTrac='Be7' &  SubTrac='Be7' & end  
endcase

;layers saved & to be plotted
 lev1=1  & lev2=LM ;47L - 29.7km
 XRange=[-90,90]   ;Lat
 limit =  [XRange(0), -180, XRange(1), 180]

;--------------------- loop over 1 year
 YEARS = ['01' ]

;MONTHS= ['01', '03', '06', '10']
;CharMON = ['Jan', 'March', 'June', 'October']
;time = [140256L, 141672L, 143880L, 146808L]

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

modelinfo = CTM_type(code, res=4)
gridinfo  = CTM_grid(modelinfo, psurf=986)    ;986mb ?
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

;get_field_3d_4x5, ntrac, 'IJ-AVG-$', lev1, lev2, time(imonth), limit,
;filename, std_3d, xmid, ymid, zmid
get_field_3d_4x5, code, ntrac, 'IJ-AVG-$', lev1, lev2, time(imonth), limit, filename, std_3d, xmid, ymid, zmid
help, std_3d
 constd(*,*,*,itrac,IMONTH) = std_3d

;print, 'max(std_zm), min(std_zm)= ', max(std_zm), min(std_zm)

endfor  ;IMONTH

endfor  ;IYEAR

endfor  ;itrac

end
