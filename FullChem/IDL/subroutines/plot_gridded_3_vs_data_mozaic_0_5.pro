; $Id: plot_gridded_3_vs_data_mozaic_0_5.pro,v 1.2 2008/03/11 17:47:25 bmy Exp $
pro plot_gridded_3_vs_data_mozaic_0_5, Species, max_sta, ext1,   ext2,  $
                                       ext3,    title,   psname, filest

   ; For selected regions from aircraft compains plot data profiles (black
   ; solid line and profiles from 3 models - with maccm3, dao and giss 
   ; winds, plotted with linestyles 1 to 3 and colors red, green and blue 
   ; correspondently
   ;
   ; Modified to read station files from the temp/ subdirectory (bmy, 3/7/05)
   ;
   ; Also now reference colors with !MYCT.RED, etc. (bmy, 3/11/08)
   
   ;=======================================================================
   ; Initialization 
   ;=======================================================================

   ; Get defaults (bmy, 6/7/11)
   X_OMARGIN   = !X.OMARGIN
   Y_OMARGIN   = !Y.OMARGIN
   X_MARGIN    = !X.MARGIN
   Y_MARGIN    = !Y.MARGIN
   P_CHARTHICK = !P.CHARTHICK
   P_THICK     = !P.THICK
   X_THICK     = !X.THICK
   Y_THICK     = !Y.THICK

   ; Set some defaults
   !X.OMARGIN   = [10,8] 
   !Y.OMARGIN   = [8,8]
   !P.CHARTHICK = 3
   !P.THICK     = 4.5
   !X.THICK     = 4
   !Y.THICK     = 4

   ; Use Postscript font
   !P.FONT=0

   ; Max # of model levels in station files (bmy, 3/11/08)
   MAXLEV       = 100

   ; Open file with information about stations
   openr, usta, filest, /get_lun
   iname_sta=''
   ititle_sta=''

   mmonth = strarr(12)
   mmonth=['Jan','Feb','Mar','Apr','May','Jun',$
           'Jul','Aug','Sep','Oct','Nov','Dec']

   pressure_sonde=fltarr(70)
   pressure_sonde=[962.35  ,891.25  ,825.41  ,764.42  ,707.94  ,$
                   655.64  ,607.20  ,562.34  ,520.80  ,482.32  ,$
                   446.69  ,413.69  ,383.12  ,354.81  ,328.60  ,$
                   304.32  ,281.84  ,261.02  ,241.73  ,223.87  ,$
                   207.33  ,192.01  ,177.82  ,164.69  ,152.52  ,$
                   141.26  ,130.82  ,121.15  ,112.20  ,103.91  ,$
                    96.23  , 89.12  , 82.54  , 76.44  , 70.79  ,$
                    65.57  , 60.72  , 56.23  , 52.08  , 48.23  ,$
                    44.67  , 41.37  , 38.31  , 35.48  , 32.86  ,$
                    30.43  , 28.18  , 26.10  , 24.17  , 22.38  ,$
                    20.73  , 19.20  , 17.78  , 16.47  , 15.25  ,$
                    14.12  , 13.08  , 12.12  , 11.22  , 10.39  ,$
                     9.62  ,  8.91  ,  8.25  ,  7.64  ,  7.08  ,$
                     6.56  ,  6.07  ,  5.62  ,  5.21  ,  4.82  ]

   scales=[320,320,320,320,320,320,320,320]

   ; Open the PS file
   Open_Device, /PS, /Color, Bits=8, Filename=PsName, /Portrait

   ; Specify directory with the data (vertical profiles) 

   pre = '/data/eval/aircraft/data/'+species+'/'
   xtitle = 'CO (ppb)' 

   ;=======================================================================
   ; --- read station & indice ---
   ;=======================================================================

   name_sta = strarr(max_sta)
   month    = fltarr(max_sta)
   lol      = fltarr(max_sta)
   lor      = fltarr(max_sta)
   lad      = fltarr(max_sta)
   lau      = fltarr(max_sta)
   H        = fltarr(max_sta)
   year     = intarr(max_sta)
   title_sta = strarr(max_sta)
   num_sta  =strarr(max_sta)

   ; Read in information about stations from input file

   for i=0,max_sta-1 do begin
      readf,usta, iname_sta,                  $
         ilol, ilor, ilad, ilau,          $
         imonth , iH, iyear, ititle_sta,inum_sta,         $
         format='(a36,1x,i4,1x,i4,1x,i4,1x,i4,1x,i4,1x,i4,1x,i2,1x,a24,1x,a3)'
      name_sta(i) = iname_sta
      month(i)    = imonth
      lol(i)      = ilol
      lor(i)      = ilor
      lad(i)      = ilad
      lau(i)      = ilau
      H(i)        = iH
      year(i)     = iyear
      title_sta(i) = ititle_sta
      num_sta(i) = inum_sta
   endfor

   ; Set number of rows and columns
   nrow=3
   ncol=3
   !P.Multi = [0,nrow,ncol,1,0]

   ;=======================================================================
   ; ---  open files ---
   ;=======================================================================
   ncount=0

   ; Loop through the stations
   for k = 1, max_sta do begin
      
      ncount=ncount+1
      kk = k-1 
      ix = k
      file=''

      ;====================================================================
      ; Get name of CO data profile
      ;====================================================================
      name_sonde='data/co.prof.for.gmi/co.prof.'+$
                 strtrim(String(fix(num_sta(kk))), 2)+'.0.5'

      read_sondes_co_0_5, name_sonde, month(kk), month(kk), month(kk), $
                          month(kk),  sonde1,    std1,      sonde2,    $
                          std2,       sonde3,    std3,      sonde4, std4
      
      inds1 = Where(pressure_sonde ge 200 and sonde1>0)
      sonde1_p=sonde1[inds1]
      std1_p=std1[inds1]
      pressure_sonde1_p=pressure_sonde[inds1]

      ; Station title
      ltitle=''
      ltitle = strtrim(title_sta(kk),2)

      ;====================================================================
      ; -- plot observed data --
      ;====================================================================
      yrange = [1000, 100]
      height = 0
      mmm = 230

      highval=scales(k-1)
      loval=0
      if highval ge 300 then loval = 60

      ytickv = [1000,800,600,400,200]
      ytickname = ['1000','800','600','400','200']

      ; Plot medians
      plot,sonde1_p, pressure_sonde1_p, xstyle=1,ystyle=1,/ylog,$
         title=ltitle,linestyle=0,psym=-5,symsize=0.6,/nodata,$
         yticks=n_elements(ytickv)-1,ytickv=ytickv,ytickname=ytickname,$
         xticks=4, min_val=-900, yrange=[1000,200], xrange=[loval,highval],$
         charsize=1.8, xmargin=[4,3], ymargin=[3,2], color=!MYCT.BLACK

      oplot, sonde1_p, pressure_sonde1_p,$
             psym=0, symsize=0.2, linestyle=0, color=!MYCT.BLACK

      ; Put error bars (one standard deviation) for sonde data
      levs=n_elements(sonde1_p)
      for w = 0, levs-1 do begin
         if std1_p(w) gt 0 then begin
            errbar = [sonde1_p(w)-std1_p(w), sonde1_p(w)+std1_p(w)]
            oplot, errbar, [pressure_sonde1_p(w),pressure_sonde1_p(w)],$
               linestyle=0,color=1
         endif
      endfor
     
      howfarover  = 0.8*highval
      howfarover2 = highval*0.5

      ;====================================================================
      ; -- read results from the first model --
      ;====================================================================

      ; NOTE: increase the # of model levels for GEOS-5 (bmy, 3/18/08)
      Pressure = FltArr( MAXLEV )       
      CO       = FltArr( MAXLEV )            

      ; Read data from MOZAIC file
      FileF    = 'temp/' + StrTrim( Name_Sta[KK], 2 ) + Ext1
      Result   = FindFile( FileF )

      OpenR, Ix, FileF
      N = 0
      while ( not EOF( ix ) ) do begin
         Readf, Ix, Fpres, Fco, format='(f13.3,f13.4)'
         Pressure[N] = Fpres
         CO[N]       = Fco
         N           = N + 1
      endwhile
      Close, Ix

      ; Resize data arrays
      Pressure = Pressure[0:N-1]
      CO       = CO[0:N-1]

      ; PLOT profile from the first model
      Oplot, CO, Pressure, Psym=0, SymSize=0.2, LineStyle=0, Color=!MYCT.RED

      ;====================================================================
      ; -- read results from the second model --
      ;====================================================================

      ; NOTE: increase the # of model levels for GEOS-5 (bmy, 3/18/08)
      Pressure = FltArr( MAXLEV )       
      CO       = FltArr( MAXLEV )            

      ; Read data from MOZAIC file
      FileF    = 'temp/' + StrTrim( Name_Sta[KK], 2 ) + Ext2
      Result   = FindFile( FileF )

      OpenR, Ix, FileF
      N = 0
      while ( not EOF( ix ) ) do begin
         Readf, Ix, Fpres, Fco, format='(f13.3,f13.4)'
         Pressure[N] = Fpres
         CO[N]       = Fco
         N           = N + 1
      endwhile
      Close, Ix

      ; Resize data arrays
      Pressure = Pressure[0:N-1]
      CO       = CO[0:N-1]

      ; Plot profile from the 2nd model
      Oplot, CO, Pressure, Psym=0, Symsize=0.2, Linestyle=0, Color=!MYCT.GREEN

      ;====================================================================
      ; -- read results from the third model --
      ;====================================================================
 
      ; NOTE: increase the # of model levels for GEOS-5 (bmy, 3/18/08)
      Pressure = FltArr( MAXLEV )       
      CO       = FltArr( MAXLEV )            

      ; Read data from MOZAIC file
      FileF    = 'temp/' + StrTrim( Name_Sta[KK], 2 ) + Ext3
      Result   = FindFile( FileF )

      OpenR, Ix, FileF
      N = 0
      while ( not EOF( Ix ) ) do begin
         Readf, Ix, Fpres, Fco, format='(f13.3,f13.4)'
         Pressure[N] = Fpres
         CO[N]       = Fco
         N           = N + 1
      endwhile
      Close, Ix

      ; Resize data arrays
      Pressure = Pressure[0:N-1]
      CO       = CO[0:N-1]

      ; Plot profile from 3rd model
      Oplot, Co, Pressure, Psym=0, SymSize=0.2, LineStyle=0, color=!MYCT.BLUE

      ;====================================================================
      ; Axis labels
      ;====================================================================
      
      ; Y-axis
      xyouts, 0.05, 0.65, 'Pressure (hPa)', $
              /normal, align=0.5, orientation=90, charsize=1.2, color=1

      ; X-axis
      xyouts, 0.5, 0.33, 'CO (ppb)', $
              /normal, align=0.5, charsize=1., color=1

      ; top title
      xyouts, 0.5,0.95, title, $
              /normal, align=0.5, charsize=1.2, color=1

   endfor 

   ; Close device
   Close_Device, /TIMESTAMP

   ; Close all files
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


