
year = 2009 ;desired year for comparison

imon = 5   ; desired month for comparison

;-----------------------------------------------------
nmonths= 12
ModelInfo = CTM_TYPE('GEOS5', res=4)
GridInfo  = CTM_Grid( ModelInfo )
imax = n_elements(gridinfo.xmid)
jmax = n_elements(gridinfo.ymid)
lmax = n_elements(gridinfo.zmid)

jmaxs = 36
lmaxs = 28

GC_3Ddat=fltarr(imax,jmax,lmax)
GC_2Ddat=fltarr(jmax,lmax,nmonths)

;-----------------------------------------------------
; get file with GEOS-Chem monthly mean fields
;-----------------------------------------------------
year_str = strtrim(string(year,format='(i4)'),2)
bpch_file = '/Users/storage/data/GEOS/climatology/bpch/trac_avg.geos5_4x5.'+year_str+'01010000'

;-----------------------------------------------------
; read in OSIRIS O3 data
;-----------------------------------------------------
sat_file = '/Users/storage/data/SPARC_DI/OSIRIS_O3/SPARC_DI_T2Mz_O3_'+year_str+'_OSIRIS_v5-0_p01.nc'
ncdf_read,file=sat_file,satdat,/all

;-----------------------------------------------------
; read in GEOS-Chem data and average
;-----------------------------------------------------
for n=imon,imon do begin

   date = ymd2date(year,(n+1),01)
   tau = nymd2tau(long(date),000000L)

   ctm_get_data,datainfo,'IJ-AVG-$',file=bpch_file,tau0=tau,tracer=2
   GC_3Ddat = *(DataInfo[0].data)
   for k=0,lmax-1 do begin
     for j=0,jmax-1 do begin
        icount = 0
        zonalval=0.0
        for i=0,imax-1 do begin
          zonalval = zonalval + GC_3Ddat[i,j,k]
          icount = icount + 1
        endfor  
        if (icount gt 0) then begin
          GC_2Ddat[j,k,n] = zonalval/icount
        endif
     endfor
   endfor

endfor

yyyymm_str = strtrim(string(year),2) + strtrim(string((imon+1),format='(i02)'),2)

OPEN_Device, /inches, Bits=8, /Color, _EXTRA=e,/portrait
;OPEN_Device, /inches, Bits=8, /Color, _EXTRA=e,/portrait,ysize=6,/ps

A = FINDGEN(17) * (!PI*2/16.)
USERSYM, COS(A), SIN(A), /FILL

os_levs = [11,15,21]   ; OSIRIS 50, 10, and 1 hPa
gc_levs = [38,47,58]   ; GEOS-5 levels
pcols = [1,2,3]

plot,gridinfo.ymid,1.0e-3*GC_2Ddat[*,gc_levs[0],imon],$ ;/nodata,$
xrange=[-90.,90.],xticks=6,xminor=3,xstyle=1,$
xtitle='Latitude',ytitle='O3 Mixing Ratio (ppm)',$
yrange=[0,12],yticks=6,$
title='O3 in '+yyyymm_str+' at 50 hPa (black), 10 hPa (red), 1 hPa (green)',col=1

for it=0,2 do begin
  ilev = gc_levs[it]
  satlev=os_levs[it]

  print,"level = ",gridinfo.pmid[ilev],satdat.plev[satlev]

  oplot,gridinfo.ymid,1.0e-3*GC_2Ddat[*,ilev,imon],line=0,col=pcols[it]

  indx = where(satdat.O3[*,satlev,imon] gt 0.0)
  oplot,satdat.lat(indx),1.0e+6*satdat.o3[indx,satlev,imon],psym=8,symsize=1.25,col=pcols[it]

  errplot,satdat.lat(indx),1.0e+6*satdat.O3[indx,satlev,imon]+1.0e+6*satdat.O3_STD[indx,satlev,imon],$
  1.0e+6*satdat.O3[indx,satlev,imon]-1.0e+6*satdat.O3_STD[indx,satlev,imon],col=pcols[it]

endfor

close_device

end
