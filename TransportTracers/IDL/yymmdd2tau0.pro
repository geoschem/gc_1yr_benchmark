;call sequence: yymmdd2tau0, '960821', tau0
;input: YYMMDD, e.g. '960821', '000201'
;output: TAU0
;hyl,1/19/2012, hongyu.liu-1@nasa.gov

pro yymmdd2tau0, YYMMDD, TAU0

if ( strlen(string(YYMMDD)) eq 8 ) then begin
 YYMMDD = StrMid (string(YYMMDD),2,6)  ;
endif else begin
 YYMMDD = StrMid (string(YYMMDD),0,6)
endelse

 IYR = StrMid (YYMMDD,0,2)    ;e.g.'96'
 IMONTH = StrMid (YYMMDD,2,2) ;'08'
 IDAY = StrMid (YYMMDD,4,2)   ;'21'

 DAYS_MON_NOLEAP    = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
 DAYS_MON_LEAP      = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]

;taking account of LEAP years!
 if ( IYR eq '84' or IYR eq '88' or IYR eq '92' or IYR eq '96' $
   or IYR eq '00' or IYR eq '04' or IYR eq '08' or IYR eq '12' or IYR eq '16') then begin
    DAYS_MON = DAYS_MON_LEAP
 endif else begin
    DAYS_MON = DAYS_MON_NOLEAP
 endelse

 if ( IMONTH eq '01' ) then begin
    JDAY= fix (IDAY)
 endif else begin
    JDAY= total ( DAYS_MON( 0:(fix(IMONTH)-2) ) ) + fix(IDAY)
 endelse

tau0 = yyyyjday2tau ( IYR, JDAY)

end
