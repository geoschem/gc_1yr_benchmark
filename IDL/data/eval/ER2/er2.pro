pro er2

;*****************************************************
; plot observed CO from ER-2 with model monthly mean data
;*****************************************************
close,/all

myct, 27, ncol=120, range=[0.1,0.7], bottom=18, saturation= 0.8

TVLCT, [0,255,0,0], [0,0,255,0], [0,0,0,255]
  ERASE
  SET_PLOT,'ps'
;  DEVICE,FILENAME='er2.ps',/Inches,Ysize=9.0,Xsize=6.0,Yoffset=1.
  DEVICE,FILENAME='er2.ps',/Landscape,/Inches,Ysize=7.0,Xsize=9.5,Yoffset=10.,/color


;!P.Multi=[0,2,2]

;**********************************************
; STEP 1:  read in ER-2 data
;**********************************************

goto, jump3

        InFileName = 'ER2.dat'
        Open_File, InFileName, 1,/read

        InFileName = 'ER2no9999.dat'
        Open_File, InFileName, 2,/write

    i=0L
 WHILE NOT EOF(1) DO BEGIN
   i=i+1
   READF, 1, date, time, lat, lon, press, co,$
          format='(7x,i6,4x,i8,4x,f9.4,4x,f9.3,3x,f10.4,3x,f10.4)'

   if co ne 9999 then begin
   if lat ne 9999 then begin
   if lon ne 9999 then begin
   if press ne 9999 then begin
     PRINTF, 2, date, time, lat, lon, press, co,$
          format='(7x,i6,4x,i8,4x,f9.4,4x,f9.3,3x,f10.4,3x,f10.4)'
   endif
   endif
   endif
   endif

 ENDWHILE

   PRINT,'Number of Lines = ',i

      close, 1 
      close, 2

jump3:

;**********************************************
; STEP 2:  read in ER-2 data with 9999
;**********************************************
;goto, jump4

        InFileName = 'ER2no9999.dat'
        Open_File, InFileName, 2,/read

nlats=46
nlons=72

; latp=fltarr(nlats)
; lonp=fltarr(nlons)

 for k=0,nlats-1 do begin
;    latp(k)=k
 endfor
 for k=0,nlons-1 do begin
;    lonp(k)=k
 endfor


nlines=9852L
 co=fltarr(nlines)
 press=fltarr(nlines)
 lat=fltarr(nlines)
 lon=fltarr(nlines)

 A=fltarr(1)
 B=fltarr(1)
 C=fltarr(1)
 D=fltarr(1)

ncount=intarr(nlons,nlats)

 i=-1L

 WHILE NOT EOF(2) DO BEGIN
;   i=i+1 
;   READF, 2, date, time, lat, lon, press, co,$
   READF, 2, date, time, A, B, D, C,$
          format='(7x,i6,4x,i8,4x,f9.4,4x,f9.3,3x,f10.4,3x,f10.4)'

   if date gt 960128 and date lt 960301 then begin
      i=i+1
      lat(i)=A(0)
      lon(i)=B(0)
      co(i)=C(0)
      press(i)=D(0)
   endif

lon1=0.
lon2=0.
lat1=0.
lat2=0.

;alon=lon
;alat=lat

;findbox, alon, alat, lon1, lon2, lat1, lat2

;print,alon,alat,lon1,lat1
;ncount(lon1-1,lat1-1-nmlats)=ncount(lon1-1,lat1-1-nmlats)+1
;

 ENDWHILE

      close, 2

print,'MAX LAT=',max(lat)
print,'MIN LAT=',min(lat)
print,'MAX LON=',max(lon)
print,'MIN LON=',min(lon)

;**********************************************
; STEP 3: Create  
;**********************************************

;levels=[0,10,100,250,500,1000,5000,8000]
;contour,ncount,lonp,latp,levels=levels,xstyle=1,ystyle=1,thick=1


;**********************************************
; STEP 4:  Plot ER-2 data 
;**********************************************

plot,co,press,linestyle=0,psym=1,symsize=0.1,yrange=[1000,0]


jump4:

;**********************************************
close_device
return
end




