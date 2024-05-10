pro plot_test_profile, Species, Species_geos, max_sta,$
               ext1, ext2, ext3, title, psname, nalt1, nalt2, nalt3

; For selected regions from aircraft compains plot data profiles (black
; solid line and profiles from 2 models - geos and geos_2  
; winds, plotted with linestyles 1 to 2 and colors red and green
; correspondently
;
; NOTE: Cleaned up and removed extraneous stuff.  Also now use drawing
;       colors from the !MYCT variable (bmy, 3/25/08)

   ; Get defaults (bmy, 6/7/11)
   X_OMARGIN   = !X.OMARGIN
   Y_OMARGIN   = !Y.OMARGIN
   X_MARGIN    = !X.MARGIN
   Y_MARGIN    = !Y.MARGIN
   P_CHARTHICK = !P.CHARTHICK
   P_THICK     = !P.THICK
   X_THICK     = !X.THICK
   Y_THICK     = !Y.THICK

   ; Plot settings
   !X.OMARGIN = [10, 8] 
   !Y.OMARGIN = [8, 8]
   !P.CHARTHICK = 2.5
   !P.THICK = 2.5
   !X.THICK = 4
   !Y.THICK = 4

   ; Use Postscript font
   !P.FONT=0

   ; File with information about stations
   filest = 'data/netCDF/'+Species+'.stations'

   openr, usta, filest, /get_lun
   iname_sta = ''
   ititle_sta = ''

   mmonth = strarr(12)
   mmonth = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', $
             'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']

   scales = [1.5, 2, 1.5, 1.5, 2, 3, 1.5, 1.5, 1.5, 1, 1.5, 1, 1, 1, 1, 1, $
             2, 2, 2, 4, 2.5, 2.5, 0.5, 0.5, 1, 1, 1, 1, 2, 2, 1, 0.5, $
         0.5,0.5,0.5,2,2,2,1]

   open_device, /ps, /color, filename=psname, /portrait

   ; Specify directory with the data (vertical profiles) 
   pre = 'data/eval/aircraft/data/'+species+'/'
   xtitle = 'HNO3 (ppb)' 

   ;---------------------------------
   ; --- read station & indices ---
   ;---------------------------------   

   name_sta = strarr(max_sta)
   month    = fltarr(max_sta)
   lol      = fltarr(max_sta)
   lor      = fltarr(max_sta)
   lad      = fltarr(max_sta)
   lau      = fltarr(max_sta)
   H        = fltarr(max_sta)
   year     = intarr(max_sta)
   title_sta = strarr(max_sta)

   ;------------------------------------------------------
   ; Read in information about stations from input file
   ;------------------------------------------------------
   for i=0, max_sta-1 do begin
      readf, usta, iname_sta,                  $
             ilol, ilor, ilad, ilau,          $
             imonth, iH, iyear, ititle_sta,         $
             format='(a36,1x,i4,1x,i4,1x,i4,1x,i4,1x,i4,1x,i4,1x,i2,1x,a25)'
      name_sta(i) = iname_sta
      month(i)    = imonth
      lol(i)      = ilol
      lor(i)      = ilor
      lad(i)      = ilad
      lau(i)      = ilau
      H(i)        = iH
      year(i)     = iyear
      title_sta(i) = ititle_sta
   endfor

   ; Set number of rows and columns
   nrow = 4
   ncol = 4
   !P.Multi = [0, nrow, ncol, 1, 0]

   ;------------------------------------------------------
   ; --- open files ---
   ;------------------------------------------------------
   ncount = 0

   ; Loop through the stations
   for k=1, max_sta do begin

      ncount = ncount+1
      ;;print, 'STATION : ', k
      kk = k-1 
      ix = k
      file = ''

      file = pre+strtrim(name_sta(kk), 2)

; Replace NA with -999.

;    cmd = "sed 's/NA/-999./g' "+file+" > tmpfile"
;    spawn, cmd
;    cmd = "sed 's/NaN/-999./g' tmpfile > tmpfile2"
;    spawn, cmd

      ilun = 99
      openr, ilun, file

      dummy = ''
      readf, ilun, dummy
      
      maxd = 50
      pressure  = fltarr(maxd)
      gtemean   = fltarr(maxd)
      gtemedian = fltarr(maxd)
      gtestd    = fltarr(maxd)
      gtenum    = fltarr(maxd)

      ; Read data profile
      i = 0
      while (not eof(ilun)) do begin
         i = i + 1
         readf, ilun,                                             $
                ipressure, inum, igtemean, igtemedian, igtestd,   $
                ip10, ip25, ip75, ip90, imin, imax
         pressure(i-1)  = ipressure 
         gtenum(i-1)    = inum
         gtemean(i-1)   = igtemean
         gtemedian(i-1) = igtemedian
         gtestd(i-1)    = igtestd
      endwhile

      Hk = i

      close, ilun

      ; Station title
      ltitle = strtrim(title_sta(kk), 2)

      ; If median value < 0, replace with -999
      tmp = pressure(0:Hk-1) 
      ind = where(gtemedian(0:Hk-1) lt 0.)    
      if ind(0) ge 0 then $
         tmp(ind) = -999.

      ind2 = where(gtestd(0:Hk-1) lt 0.)

      ;-----------------------------------
      ; -- plot observed data --
      ;-----------------------------------
      if (pressure(0) lt 10) $
         then tmp(0:Hk-1) = 1013*(1.-2.257e-5*(pressure(0:Hk-1)*1.e3))^5.2533  
      yrange = [1000, 100]
      height = 0
      mmm = 230

      highval = scales(k-1)
      loval = -0.01

      ;%%% Kludge %%%%%%%%%%%%%
      case ( species_geos ) of
         'NH3': highval = 0.5
         'NH4': highval = 2
         'NIT': highval = 2
      endcase
      ;%%%%%%%%%%%%%%%%%%%%%%%%

      ; Plot medians -- nodata!
      plot, gtemedian(0:Hk-1)/1000, -alog10(tmp), xstyle=1, ystyle=5, $
            title=ltitle, linestyle=0, psym=-5, symsize=0.6, $
            xticks=4, min_val=-900, yrange=[-3, -alog10(200)], $
            xrange=[loval, highval], $
            charsize=1.2, xmargin=[4, 3], $
            ymargin=[3, 2], color=!MYCT.BLACK, /NoData


      howfarover = 0.8*highval

      howfarover2 = highval*0.5

      tempo3 = 1900+year(kk)
      if year(kk) lt 50 then begin
         tempo3 = 2000+year(kk)
      endif 
      tempo = string(format='(a3,",",i4)', mmonth(month(kk)-1), tempo3)
      xyouts, howfarover2, -alog10(mmm), tempo, charsize=0.7, /data, color=1

;      ; Plot means
;      oplot, gtemean(0:Hk-1)/1000, -alog10(tmp), $
;             linestyle=0, psym=6, symsize=0.2, color=!MYCT.BLACK
;
;      for w=0, Hk-1 do begin
;         if gtestd(w) gt 0. then begin
;            errbar = [gtemean(w)-gtestd(w), gtemean(w)+gtestd(w)]
;
;            ; Plot error bars
;            oplot, errbar/1000, [-alog10(tmp(w)), -alog10(tmp(w))], color=1
;            nnum = ''
;            nnum = strtrim(string(gtenum(w), format='(i4)'), 2)
;            xyouts, howfarover, -alog10(tmp(w)), nnum, $
;                    charsize=0.6, /data, color=!MYCT.BLACK
;        endif
;      endfor

      ;-----------------------------------
      ; -- plot data from 1st model --
      ;-----------------------------------

      ; Look for file in temporary directory
      filef  = 'temp/' + strtrim(name_sta(kk), 2) + ext1
      ;%%% Kludge %%%
      FileF = Replace_Token( FileF, 'HNO3', Species_Geos, Delim='' )
      ;%%%%%%%%%%%%%%
      result = findfile(filef, count=toto)
     
      pressure = fltarr(nalt1) 
      co = fltarr(nalt1)       

      openr, ix, filef
      for j=1, nalt1 do begin  ;19 levels 
         jx = j - 1
         readf, ix, fpres, fco, format='(f13.3,f13.4)'
         pressure[jx] = fpres
         co[jx] = fco
      endfor

      close, ix

      ; Plot profile 
      oplot, co, -alog10(pressure), $
             psym=-sym(1), symsize=0.2, linestyle=1, color=!MYCT.RED

      ;-----------------------------------
      ; -- plot data from 2nd model --
      ;-----------------------------------

      ; Look for file in temporary directory
      filef  =  'temp/' + strtrim(name_sta(kk), 2) + ext2
      ;%%% Kludge %%%
      FileF = Replace_Token( FileF, 'HNO3', Species_Geos, Delim='' )
      ;%%%%%%%%%%%%%%
      result = findfile(filef, count=toto)

      pressure = fltarr(nalt2)
      co = fltarr(nalt2)      

      openr, ix, filef
      for j=1, nalt2 do begin 
         jx = j - 1
         readf, ix, fpres, fco, format='(f13.3,f13.4)'
         pressure[jx] = fpres
         co[jx] = fco
      endfor
      
      close, ix

      ; Plot profile from geos_2 model
      oplot, co, -alog10(pressure), $
             psym=-sym(1), symsize=0.2, linestyle=2, color=!MYCT.GREEN

      ;-----------------------------------
      ; -- plot data from 3rd model --
      ;-----------------------------------

      ; Look for file in temporary directory
      filef  = 'temp/' + strtrim(name_sta(kk), 2) + ext3
      ;%%% Kludge %%%%
      FileF = Replace_Token( FileF, 'HNO3', Species_Geos, Delim='' )
      ;%%%%%%%%%%%%%%%
      result = findfile(filef, count=toto)

      pressure = fltarr(nalt3) 
      co = fltarr(nalt3)       

      openr, ix, filef
      for j=1, nalt3 do begin 
         jx = j - 1
         readf, ix, fpres, fco, format='(f13.3,f13.4)'
         pressure[jx] = fpres
         co[jx] = fco
      endfor

      close, ix

      ; Plot profile from geos_3 model

      oplot, co, -alog10(pressure),$
             psym=-sym(1), symsize=0.2, linestyle=3, color=!MYCT.BLUE

      ;-----------------------------------
      ; -- Put labels on the axes --
      ;-----------------------------------

;if ncount eq nrow*ncol then begin
      xyouts, 0.05, 0.5, 'Pressure (hPa)', $
              /normal, align=0.5, orientation=90, charsize=1.2, $
              color=!MYCT.BLACK

      xyouts, 0.5, 0.05, Species_geos + ' (ppb)', $
              /normal, align=0.5, charsize=1., color=!MYCT.BLACK
;endif

      pres = [1000, 800, 600, 400, 200]
      logpres = -alog10(pres)
      pres1 = strtrim(string(pres), 2)
      pres1[1] = " "
      pres2 = replicate(' ', n_elements(pres1))

      pres1 = strtrim(string(pres), 2)
      axis, loval, yaxis=0, yticks=12, yrange=[-3, -alog10(200)], $
            ytickv=logpres, ytickname=pres1, /ystyle, color=!MYCT.BLACK

      axis, highval, yaxis=1, yticks=12, yrange=[-3, -alog10(200)], $
            ytickv=logpres, ytickname=pres2, /ystyle, color=!MYCT.BLACK

      xyouts, 0.5, 0.96, title, $
              /normal, align=0.5, charsize=1.2, color=!MYCT.BLACK
   endfor 

   close_device, /TIMESTAMP

   close, /all

   ; Restore defaults (bmy, 6/7/11)
   !X.OMARGIN   = X_OMARGIN   
   !Y.OMARGIN   = Y_OMARGIN   
   !X.MARGIN    = X_MARGIN    
   !Y.OMARGIN   = Y_MARGIN    
   !P.CHARTHICK = P_CHARTHICK 
   !P.THICK     = P_THICK     
   !X.THICK     = X_THICK     
   !Y.THICK     = Y_THICK 

end


