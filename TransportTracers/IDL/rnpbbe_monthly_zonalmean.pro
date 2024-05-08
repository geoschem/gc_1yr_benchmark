;to plot monthly zonal mean 222Rn,210Pb and 7Be concentrations
;hyl,1/19/2012 (hongyu.liu-1@nasa.gov)

pro rnpbbe_monthly_zonalmean, Year, Met, Res, Version, InDir, InFile, OutDir, $
                              PS=PS, _EXTRA=e

   ; Output filename
   OutFile = OutDir + Version + '.zonal_mean_monthly.ps'

   ; Get model and grid info
   modelinfo = CTM_type(Met, res=Res)
   gridinfo  = CTM_grid(modelinfo, psurf=986) 
   dao_sige  = gridinfo.sigmid
   IM        = gridinfo.IMX
   JM        = gridinfo.JMX
   LM        = gridinfo.LMX
   PMID      = gridinfo.pmid

   ; Debug
   ;print, 'IM,JM,LM= ', IM, JM, LM
   ;print, 'pmid= ', pmid

   ;layers saved & to be plotted
   lev1 = 1  & lev2 = LM

   XTicks = 6  &  XMinor = 2  &  XRange = [-90, 90]  ;Lat
   YRange = [1000, 50]    ;mb

   ; Change colorbar modified blue->red spectrum (mps, 6/15/15)
   ;myct, 27
   myct, 127

   !p.color = 0
   !p.font = 0
   !x.ticks = 6
   !p.charsize = 1.6
   !p.thick = 3
   !p.charthick = 3
   !x.style = 1
   !y.style = 1
   !X.Thick = 3
   !Y.Thick = 3

   if keyword_set(PS) then begin
      set_plot, 'PS'
   endif

   open_device, OLD_DEVICE, ps=PS,                       $
                winparam=winparam, /color, /portrait,    $
                filename=OutFile

   limit =  [XRange(0), -180, XRange(1), 180]

   multipanel, col=2, row=2, $
               margin=[0.055, 0.085, 0.055, 0.085], $
               omargin=[0.05, 0.08, 0.05, 0.08]

   multipanel, Position=PositionN
   print, 'Position= ', PositionN

   ;------------------------------------------------
   ; Loop over tracers
   ;------------------------------------------------
   for itrac=0, 2 do begin

      ;choose tracer #
      case ( itrac ) of 
      0: begin & ntrac = 1  &  CharTrac = 'Rn'  &  SubTrac = 'Rn'  & end      
      1: begin & ntrac = 2  &  CharTrac = 'Pb'  &  SubTrac = 'Pb'  & end     
      2: begin & ntrac = 4  &  CharTrac = 'Be7' &  SubTrac = 'Be7' & end  
      endcase

      case itrac of
         0: begin
            levs = [0.01, 0.1, 1,   10,  20,  $
                    50,   100, 200, 500, 1000 ]
            c_annotation =  strtrim(string(format='(f10.2)', levs), 2)
            fig_title = ' !U222!NRn [mBq/SCM]'
         end
         1: begin
            ; Use same contour intervals as Fig. 3 in Liu et al., JGR 2001
            levs  = [ 0.02, 0.05, 0.1,  0.15, 0.20, 0.30, $
                      0.40,  0.50, 0.60, 0.7,  0.8 ]
            c_annotation =  strtrim(string(format='(f10.2)', levs), 2)
            fig_title = ' !U210!NPb [mBq/SCM]'
         end
         2: begin
            ; Use same contour intervals as Fig. 3 in Liu et al., JGR 2001
            levs  = [ 1, 2.5, 5, 7.5, 10, 20, 50, 100, 200, 500, 1000, 5000 ]
            c_annotation =  strtrim(string(format='(f10.2)', levs), 2)
            fig_title = ' !U7!NBe [mBq/SCM]'
         end
      endcase

      colors = 30+256.*findgen(n_elements(levs))/(1.*n_elements(levs))

      c_linestyle = ( levs lt 0 )

      ;------------------------------------------------
      ; Loop over 4 months
      ;------------------------------------------------
      MONTHS  = ['01',      '04',    '07',   '10']
      CharMON = ['January', 'April', 'July', 'October']
 
      for IMONTH = 0, n_elements( MONTHS ) -1 do begin

         IMN = MONTHS ( IMONTH )

         yyyymmdd = YEAR + IMN + '01'
         yymmdd2tau0, yyyymmdd, tau1
         time = long( tau1)

         ; Get tracer concentration [v/v]
         get_field_3d_4x5, Met, ntrac, 'IJ-AVG-$', lev1, lev2, time, $
                           limit, InFile, std_3d, xmid, ymid, zmid

         ; Debug
         ;help, std_3d
      
;------------------------------------------------------------------------------
; Now tracers are in mBq/SCM in tracerinfo.dat (mpayer, 1/24/12)
;      ;This assumes units for RnPbBe concentrations are v/v in tracerinfo.dat
;      case itrac of
;         0: std_3d = std_3d * 5.6397E22   ;Rn  v/v-->mBq/SCM
;         1: std_3d = std_3d * 2.6141E19   ;Pb  v/v-->mBq/SCM
;         2: std_3d = std_3d * 4.0513E21   ;Be7 v/v-->mBq/SCM
;      endcase
;------------------------------------------------------------------------------

         ;------------------------------------------------
         ; Calculate zonal average
         ;------------------------------------------------
         std_zm = FltArr (JM,lev2)
         for J = 0, JM-1 do begin 
         for L = 0, lev2-1 do begin  
            std_zm (J,L) = total ( std_3d(*,J,L) ) / n_elements(xmid)
         endfor
         endfor

         print, 'max(std_zm), min(std_zm)= ', max(std_zm), min(std_zm)


         ;------------------------------------------------
         ; Create zonal mean plots
         ;------------------------------------------------
         case (IMONTH) of
            0: begin
               ;xtitle=' '
               ;ytitle='!5Pressure (hPa)'
            end
            1: begin
               ;xtitle=' '
               ;ytitle='!5Pressure (hPa)'
            end
            2: begin
               ;xtitle=' '
               ;ytitle='!5Pressure (hPa)'
            end
            3: begin
               ;xtitle=' '
               ;ytitle='!5Pressure (hPa)'
            end
         endcase

         title = CharMON ( IMONTH ) + ' ' +  YEAR

         ;tvplot_hyl, std_zm, ymid(*), pmid(0:lev2-1), /contour, c_levels = levs, c_thick=4, c_linestyle=c_linestyle, $
         ;XRange=XRange, YRange=YRange, XTicks=XTicks, XMinor=XMinor, XStyle=1, $
         ;YTicks=YTicks, YMinor=YMinor, YStyle=1, $
         ;title=title, ytitle=ytitle, Xtitle=xtitle, c_colors=1, c_charsize=1.5, $
         ;c_annotation = c_annotation, Position=PositionN, CSFAC=1.4, /ylog

         contour, std_zm, ymid(*), pmid(0:lev2-1),levels=levs,             $
                  /follow,xrange=[-90,90],yrange=[1000,50],ytype=1,/fill,  $
                  title=title, xstyle=1, ystyle=1,xticks=6,                $
                  xtitle='!5', ytitle='!5Pressure (hPa)',                  $
                  c_colors=colors, color=1, position=PositionN,            $
                  charsize=1.1 

         contour, std_zm, ymid(*), pmid(0:lev2-1),levels=levs,             $
                  /follow,xrange=[-90,90],yrange=[1000,50],xticks=6,       $
                  xstyle=1, ystyle=1, ytype=1,/noerase,                    $
                  c_labels=(levs gt -100.), color=1, position=PositionN,   $
                  charsize=1.1

         top_title =  'GEOS-Chem ' + Version + ' Zonal Mean, ' + year

         xyouts, 0.5, 0.95, top_title,   $
                /Normal, Color=!MYCT.BLACK, CharSize=1.5, Align=0.5

         xyouts, 0.5, 0.91, fig_title,   $
                /Normal, Color=!MYCT.BLACK, CharSize=1.5, Align=0.5

         multipanel, /advance, Position = PositionN

      endfor  ;IMONTH

   ERASE  ;advance one page 

   endfor  ;itrac

   close_device

end
