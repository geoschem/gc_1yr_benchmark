;hyl, 1/19/2012, hongyu.liu-1@nasa.gov

pro read_geoschem_trpause_geos5_2005, year, filename, code, trpause, xmid_out, ymid_out, zmid_out

 trpause = fltarr(72, 46, 3, 12)   ;geos5-das tropopause in level/height/mb

 for itrac = 0,2 do begin

;choose tracer #
 case ( itrac ) of 
0: begin & ntrac=26001 & end      
1: begin & ntrac=26002 & end     
2: begin & ntrac=26003 & end  
endcase

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

modelinfo = CTM_type(code, res=4)
gridinfo  = CTM_grid(modelinfo, psurf=986)    ;986mb ?
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

success = ctm_get_datablock(std_2d, 'TR-PAUSE',                 $
                            ;XMid=XMid_out, YMid=YMid_out, ZMid=ZMid_out,    $
                             Tracer=ntrac, Tau0=time(IMONTH),    $
                             Lat = [limit(0), limit(2)],         $
                             Lon = [limit(1), limit(3)],         $
                            ;Lev = [lev1, lev2],      $
                             FileName=filename )

;help, xmid_out, ymid_out, zmid_out
;stop

if ( not success ) then begin
     Message, 'Could not find field_3d_4x5!'
     stop
endif

 trpause(*,*,itrac,IMONTH) = std_2d


endfor  ;IMONTH

endfor  ;IYEAR

endfor  ;itrac

end
