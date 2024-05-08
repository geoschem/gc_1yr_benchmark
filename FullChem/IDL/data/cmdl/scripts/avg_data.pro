
; This script reads data for one site in the NOAA GMD (formerly CMDL)
; network, extracts the appropriate year, averages the data, and 
; writes it out in the format expected by the GEOS-Chem benchmarking
; routines.
; 
; It was created by attempting to back-out what was done for earlier
; versions of the data files by Inna.
; 
; The list of stations with no data (and years they ended) is valid
; only for 2009-2013 as I haven't tested for other years.
; 
; To use, specify site as argument. Year can also be set, or defaults
; to 2013.
;
; J. Fisher, 4 Jul 2017

PRO AVG_DATA, SITE, YEAR=YEAR

; Default to 2013 (year of GC benchmark)
if ~keyword_set(year) then year=2013L

; dead stations
if (strlowcase(site) eq 'kzd' and year ge 2010) then begin
   print,'KZD data ends in 2009'
   return
endif else if (strlowcase(site) eq 'kzm' and year ge 2010) then begin
   print,'KZM data ends in 2009'
   return
endif else if (strlowcase(site) eq 'stm' and year ge 2010) then begin
   print,'STM data ends in 2009'
   return
endif else if (strlowcase(site) eq 'bme' and year ge 2011) then begin
   print,'BME data ends in 2010'
   return
endif else if (strlowcase(site) eq 'bal' and year ge 2012) then begin
   print,'BAL data ends in 2011'
   return
endif else if (strlowcase(site) eq 'bsc' and year ge 2012) then begin
   print,'BSC data ends in 2011'
   return
endif else if (year eq 2013 and strmid(strlowcase(site),0,3) eq 'poc') then begin
   print,'No POC data in 2013'
   return
endif

; Open data file for the site
openr,lun,'co_'+strlowcase(site)+'_surface-flask_1_ccgg_event.txt',/get_lun

; Set up variables to be used in reading
line=''
yyyy=0L
mm=0L
dd=0L
COval=0.
QC1=''
QC2=''
QC3=''
mmdd=0L
mon=0L
COdata=0.

; Read # of header lines
readf,lun,line
result=strbreak(line,':')
nhdr=long(result[1])

; Read header
for i=1,nhdr-1 do readf,lun,line

; Format statement for reading data
fmt = '(4x,i4,x,i2,x,i2,41x,f7.3,11x,3(a1),79x)'

; Start reading data
while ~eof (lun) do begin
   readf,lun,yyyy,mm,dd,COval,QC1,QC2,QC3,format=fmt
   
   ; Look for specified year and good data (QC1=.)
   if yyyy lt year then continue else $
   if yyyy gt year then break    else $
   if QC1 eq '.' then begin
   
      ; Make month-day combo for later use
      mmdd = [mmdd,mm*100L+dd]
      ; Also store month and data
      mon  = [mon, mm]
      COdata = [COdata,COval]

   endif
endwhile

; Close file
close,lun
free_lun,lun

; Average flasks from same day first
;   GMD samples are often done in sets of replicates
;   separated by a few min. These aren't really 
;   separate data points so we average them first.
COdata=tapply(COdata[1:*],mmdd[1:*],'mean')
mon=tapply(mon[1:*],mmdd[1:*],'mean')
mmdd=tapply(mmdd[1:*],mmdd[1:*],'mean')

; Now calculate monthly statistics
COmean = tapply(COdata,mon,'mean',countval=COcnt,group=months)
COstd  = tapply(COdata,mon,'stddev')
COmin  = tapply(COdata,mon,'min')
COmax  = tapply(COdata,mon,'max')
COmed  = tapply(COdata,mon,'median')


; Write to file
openw,lun,strlowcase(site)+'.mn.'+string(year,'(i4.4)'),/get_lun
fmt2='(f0.2,x,f0.2,x,i0,x,3(f0.2,x))'

; Loop over months
for m=0,11 do begin
    
    ; Check there is data for this month
    ind = where(months eq m+1,ct)
    if ct eq 0 then $
       printf,lun,'-999 -999 -999 -999 -999 -999' $
    else $
       printf,lun,COmean[ind],COstd[ind],COcnt[ind],COmin[ind],COmax[ind],COmed[ind],format=fmt2

endfor
       
; Close file
close,lun
free_lun,lun
 
end
