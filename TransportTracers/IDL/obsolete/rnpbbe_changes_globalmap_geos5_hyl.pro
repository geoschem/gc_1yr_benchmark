;to plot percentage changes in 222Rn, 210Pb, and 7Be concentrations
;at vertical levels (hyl,1/19/12; hongyu.liu-1@nasa.gov)

pro rnpbbe_changes_globalmap_geos5_hyl, ps=ps

   ; Save existing color table
   TvLct, R, G, B, /Get

   ; Difference colortable
   MyCt, /BuYlRd
   Bottom  = !MYCT.BOTTOM
   NColors = !MYCT.NCOLORS

   ;loop over vertical levels
   for ilev=1, 2 do begin

      case ilev of
         1: begin & lev_plot = 0  & charlev = 'Surface' & end ;Surface (0.06km)
         2: begin & lev_plot = 23 & charlev = '500hPa'  & end ;500 hPa (5.41km)
      endcase

      BW = 0   ;1-B/W ;0-color
      
      ; Input files
      new_version =  'GEOS_FP'
      old_version =  'GEOS_5'
      ; Current version:
      file1 = '/home/mpayer/SR1/v9-02/v9-02k/RnPbBe/' + new_version + '/bpch/ctm.bpch.20130301'
      ; Previous version:
      file2 = '/home/mpayer/SR1/v9-02/v9-02k/RnPbBe/' + old_version + '/bpch/ctm.bpch.20130301'
      ; Output postscript filename:
      fps   = '/home/mpayer/SR1/v9-02/v9-02k/RnPbBe/' + new_version + '/output/' + new_version + '.Rn-Pb-Be_changes.' + charlev + '.ps'
      
      ;positions of the maps
      pos1 = [0.30, 0.70, 0.70, 0.85]
      pos3 = [0.30, 0.42, 0.70, 0.57]
      pos5 = [0.30, 0.14, 0.70, 0.29]
      
      XTicks = 6  &  XMinor = 2  &  XRange = [-180, 180]  ;Lon
      YTicks = 4  &  YMinor = 4  &  YRange = [-90, 90]    ;Lat
      Limit = [-90, -180, 90, 180]
           
      open_device, OLD_DEVICE, ps=PS,                  $
             ;winparam=winparam, /color, /landscape,   $
              winparam=winparam, /color, /portrait,    $
              filename=fps

      code = 'GEOS5_47L'
      CharCode = 'GEOS-FP'
      modelinfo = CTM_type(code, res=4)
      gridinfo  = CTM_grid(modelinfo, psurf=1013.25) 
      xmid = gridinfo.xmid
      ymid = gridinfo.ymid
      zzmid = gridinfo.zmid
      dao_sige = gridinfo.sigmid
      IM = gridinfo.IMX
      JM = gridinfo.JMX
      LM = gridinfo.LMX
      ;print, 'IM,JM,LM= ', IM, JM, LM

      ;layers saved & to be plotted
      lev1=1  & lev2=LM ;47L - 29.7km
      XRange=[-90,90]   ;Lat
      limit =  [XRange(0), -180, XRange(1), 180]

;      MONTHS  = ['01',  '07' ]
;      CharMON = ['Jan', 'Jul']
      MONTHS  = [ '03' ]
      CharMON = ['Mar' ]
  
      ;--------------------- loop over months
      for IMONTH=0, n_elements( MONTHS ) -1 do begin
         IMN = MONTHS ( IMONTH )
         print, 'Month = ',  IMN

;----------------------------------------------------------------------------
; Changed date for GEOS-FP simulations
;         YEARS = ['05' ]
;         YYYY  = ['2005']
;         yymmdd2tau0, '050101', tau1
;         yymmdd2tau0, '050701', tau2
;         time = [ long(tau1), long(tau2) ]
;----------------------------------------------------------------------------
         YEARS = ['13' ]
         YYYY  = ['2013']
         yymmdd2tau0, '130301', tau1
         time = [ long(tau1) ]
      
         ;---- loop over tracers -----
         for itrac=1, 3 do begin
         
            ;choose tracer #
            case ( itrac ) of
               1: begin & ntrac = 1  & CharTrac = '222Rn'  & SubTrac = '[222Rn]' & End ;Rn
               2: begin & ntrac = 2  &  CharTrac = '210Pb' & SubTrac = '[210Pb]' & end ;Pb
               3: begin & ntrac = 3  &  CharTrac = '7Be'   & SubTrac = '[7Be]'   & end ;Be
            endcase

            category = 'IJ-AVG-$'
         
            if ( itrac eq 1) then begin
               position = pos1
            endif
            if ( itrac eq 2) then begin
               position = pos3
            endif
            if ( itrac eq 3) then begin
               position = pos5
            endif
         
            xtitle = 'Latitude'  &  ytitle = 'Altitude (km)'
         
;            read_geoschem_rnpbbe_geos5_2005, file1, code, const, $
;                                             xmid, ymid, zmid    $
;                                             ;const = fltarr(IM, JM,
;                                             ;LM, 3, 12)
;            std_3d = const(*, *, *, ntrac-1, 6-1)

            get_field_3d_4x5, code, ntrac, 'IJ-AVG-$',   lev1,   lev2,   $
                              time(imonth), limit,       file1,  std_3d, $
                              xmid,         ymid,        zmid
            help, std_3d
         
;            read_geoschem_rnpbbe_geos5_2005, file2, code, const, $
;                                             xmid, ymid, zmid    $
;                                             ;const = fltarr(IM, JM,
;                                             ;LM, 3, 12)
;            clear_3d = const(*, *, *, ntrac-1, 6-1)

            get_field_3d_4x5, code, ntrac, 'IJ-AVG-$',   lev1,   lev2,     $
                              time(imonth), limit,       file2,  clear_3d, $
                              xmid,         ymid,        zmid
            help, clear_3d

            print, 'Current version:  max(std_3d),   min(std_3d)  = ', $
                   max(std_3d),   min(std_3d)
            print, 'Previous version: max(clear_3d), min(clear_3d)= ', $
                   max(clear_3d), min(clear_3d) 

            ;find delta
            ;delta_xy = FltArr (IM,JM,1)

            delta_xy = std_3d(*, *, lev_plot) - clear_3d(*, *, lev_plot) 
            help, delta_xy  ;Array[72,46]
         
            perc_xy  = FltArr (IM, JM  )
         
            for I=0, IM-1 do begin 
            for J=0, JM-1 do begin  
               if ( clear_3d(I, J, lev_plot) ne 0 ) then begin                
                  perc_xy (I, J)  =   delta_xy(I, J) / clear_3d(I, J, lev_plot)    
               endif else begin
                  perc_xy (J, L)  = -999
               endelse
            endfor
            endfor

            ;print, 'max, min(delta_xy)= ', max(delta_xy), min(delta_xy)
            ;print, 'perc_xy(*,0:6-1)= ', perc_xy(*, 0:6-1)   ;examples 
            ;print, 'max, min, median, mean(perc_xy)= ', max(perc_xy), min(perc_xy), median(perc_xy), mean(perc_xy)
            DataArr = perc_xy(*, *)*100
         
            print, fps


            ; NOTE: Eyeballed the color scheme using cindex.pro (bmy, 1/20/12)
            c_colors = [ 19, 22, 24, 26, 27, 28, 30, 32, 33, 35]

            c_levels = [-25, -15, -10, -5, -1, 1, 5, 10, 15, 25] 
            ;c_levels = [-40, -30, -20, -10, 0, 10, 20, 30, 40] 
            c_annotation =  strtrim(string(format='(i3)', c_levels), 2)
            c_linestyle = ( c_levels lt 0 )
            mindata = min(c_levels)
            maxdata = max(c_levels)
            ;print, 'mindata, maxdata= ', mindata, maxdata
         
            tvmap, DataArr(*, *), xmid(*), ymid(*), /continents,     $      
                limit=limit, c_thick=4, /grid, mparam=mparam,        $
                mindata=mindata, maxdata=maxdata, c_levels=c_levels, $ ;cbunit='ppbv', $
                NColors=NColors,  Bottom=Bottom,                     $
                title=SubTrac,  $
                /FCONTOUR,                   $
                /cbar, divisions=n_elements(c_levels)-1, $
                cbposition=[position(0), position(1)-0.05, $
                            position(2), position(1)-0.03], $
                cbmin=mindata, cbmax=maxdata, $
                cbformat='(I12)',                               $
                charsize=0.85, csfac=1.0, tcsfac=1.2, min_val=0, $
                Position=position, /noerase, $
                c_colors=C_Colors,  $ ; bmy added C_COLORS (1/20/12)
                dlon=60
         
         endfor  ;itrac
      
         ;xyouts, (pos1(0)+pos1(2))/2, pos1(3)+0.02, '210Pb', color=1, /normal, charsize= 1.8, align=0.5, CharThick=3

         fig_title  = 'GEOS-Chem ' + new_version + ' % changes at '      + $
                      charlev +', ' + CharMon (IMONTH) + ' ' + YYYY

         fig_title2 = '( ' + new_version + ' - ' + old_version + ' ) / ' + $
                      old_version

         XYoutS, 0.5, 0.92, fig_title,  $
            /Normal, Color=!MYCT.BLACK, CharSize=1.5, Align=0.5

         XYoutS, 0.5, 0.88, fig_title2,  $
            /Normal, Color=!MYCT.BLACK, CharSize=1.5, Align=0.5


         ERASE   ;advance one page

      endfor  ;IMONTH

   close_device

   endfor  ;ilev

   ; Restore existing color table
   TvLct, R, G, B

end

