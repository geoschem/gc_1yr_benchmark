;to plot monthly zonal mean 222Rn,210Pb and 7Be concentrations
;hyl,1/19/2012 (hongyu.liu-1@nasa.gov)
; Modified by Karen Yu to plot zonal mean ratios

pro zonal_mean_ratios

   code = 'GEOS5_47L'
   NA = 0

   version = 'v9-02k'
   file1 = '/home/mpayer/GC/stdrun_1yr/runs/v9-02/v9-02k/RnPbBe/GEOS_FP/bpch/ctm.bpch.20130301'
   file2 =  '/home/mpayer/GC/stdrun_1yr/runs/v9-02/v9-02k/RnPbBe/GEOS_5/bpch/ctm.bpch.20130301'
   fps   =  './output/' + version + '_zonal_mean_ratios_4x5.ps'

   modelinfo = CTM_type(code, res=4)
   gridinfo  = CTM_grid(modelinfo, psurf=986) 
   dao_sige = gridinfo.sigmid
   IM = gridinfo.IMX
   JM = gridinfo.JMX
   LM = gridinfo.LMX
   PMID = gridinfo.pmid
   print, 'IM,JM,LM= ', IM, JM, LM
   print, 'pmid= ', pmid

   if ( NA ) then begin 
     IM = 225
     JM = 202
   endif

   ;layers saved & to be plotted
   lev1 = 1  & lev2 = LM

   XTicks = 6  &  XMinor = 2  &  XRange = [-90, 90]  ;Lat
   YRange = [1000, 50]    ;mb

   limit =  [XRange(0), -180, XRange(1), 180]

   if ( NA ) then begin
     limit = [9.75, -130.0, 60.0, -60.0]
   endif

   myct, 63, ncolors=11

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

   open_device, /Color, Bits=8, /portrait, ps=1, filename=fps, _Extra=e

   multipanel, col=2, row=2, $
               margin=[0.055, 0.085, 0.055, 0.085], $
               omargin=[0.05, 0.08, 0.05, 0.08]

   multipanel, Position=PositionN
   ;print, 'Position= ', PositionN

   for itrac=0, 2 do begin

   ;choose tracer #
   case ( itrac ) of 
   0: begin & ntrac = 1  &  CharTrac = 'Rn'  &  SubTrac = 'Rn'  & end      
   1: begin & ntrac = 2  &  CharTrac = 'Pb'  &  SubTrac = 'Pb'  & end     
   2: begin & ntrac = 3  &  CharTrac = 'Be7' &  SubTrac = 'Be7' & end  
   endcase

   case itrac of
      0: begin
         levs = [0.2, 0.4, 0.6, 0.8, 0.9, 1, $
                 1.1, 1.2, 1.3, 1.4, 1.5 ]
         c_annotation =  strtrim(string(format='(f10.2)', levs), 2)
         fig_title = ' !U222!NRn [mBq/SCM]'
      end
      1: begin
         ; Use same contour intervals as Fig. 3 in Liu et al., JGR 2001
         levs = [0.2, 0.4, 0.6, 0.8, 0.9, 1, $
                 1.1, 1.2, 1.3, 1.4, 1.5 ]
         c_annotation =  strtrim(string(format='(f10.2)', levs), 2)
         fig_title = ' !U210!NPb [mBq/SCM]'
      end
      2: begin
         ; Use same contour intervals as Fig. 3 in Liu et al., JGR 2001
         levs  = [0.2,0.4, 0.6, 0.8, 0.9, 1.0, 1.1, 1.2, 1.3, 1.4, 1.5 ]
         c_annotation =  strtrim(string(format='(f10.2)', levs), 2)
         fig_title = ' !U7!NBe [mBq/SCM]'
      end
   endcase

   ;colors = 30+256.*findgen(n_elements(levs))/(1.*n_elements(levs))
   ;colors = [50., 60., 70., 80., 90., 107, 120, 140, 160, 180, 200]
   colors = [28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18 ]
   c_linestyle = ( levs lt 0 )

   YEARS = ['01' ]
   YYYY  = ['2013']

   MONTHS  = '03'
   CharMON = 'March'
   yymmdd2tau0, '130301', tau1
   time = long(tau1)

   for IYEAR=0, n_elements( YEARS ) -1 do begin
      IYR = YEARS ( IYEAR )

      get_field_3d_4x5, code, ntrac, 'IJ-AVG-$', lev1, lev2, time, $
                        limit, file1, std_3d_1, xmid, ymid, zmid
      help, std_3d_1
 	 
      get_field_3d_4x5, code, ntrac, 'IJ-AVG-$', lev1, lev2, time,  $
                        limit, file2, std_3d_2, xmid, ymid, zmid
      help, std_3d_2

;------------------------------------------------------------------------------
; Now tracers are in mBq/SCM in tracerinfo.dat (mpayer, 1/24/12)
;      ;This assumes units for RnPbBe concentrations are v/v in tracerinfo.dat
;      case itrac of
;         0: std_3d = std_3d * 5.6397E22   ;Rn v/v-->mBq/SCM
;         1: std_3d = std_3d * 2.6141E19   ;Pb v/v-->mBq/SCM
;         2: std_3d = std_3d * 4.0513E21   ;Be7 v/v-->mBq/SCM
;      endcase
;------------------------------------------------------------------------------

      ;find zonal average
      std_zm1 = FltArr (JM,lev2)
      std_zm2 = FltArr (JM,lev2)
      for J = 0, JM-1 do begin 
      for L = 0, lev2-1 do begin  
         std_zm1 (J,L) = total ( std_3d_1(*,J,L) ) / n_elements(xmid)
         std_zm2 (J,L) = total ( std_3d_2(*,J,L) ) / n_elements(xmid)
      endfor
      endfor

      print, 'max(std_zm1), min(std_zm1)= ', max(std_zm1), min(std_zm1)
      print, 'max(std_zm2), min(std_zm2)= ', max(std_zm2), min(std_zm2)

      ; compute ratios
      zm_ratio = std_zm1/std_zm2

	help, zm_ratio
	help, ymid
      case (0) of
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

      if ( NA ) then begin
        xr = [9.75, 60.0]
      endif else begin 
        xr = [-90,90]
      endelse

      contour, zm_ratio, ymid(*), pmid(0:lev2-1),levels=levs,             $
               /follow,xrange=xr,yrange=[1000,50],ytype=1,/fill,  $
               title=fig_title, xstyle=1, ystyle=1,xticks=6,                $
               xtitle='!5', ytitle='!5Pressure (hPa)',                  $
               c_colors=colors, color=1, position=PositionN,            $
               charsize=1.1 

      contour, zm_ratio, ymid(*), pmid(0:lev2-1),levels=levs,             $
               /follow,xrange=xr,yrange=[1000,50],xticks=6,       $
               xstyle=1, ystyle=1, ytype=1,/noerase,                    $
               c_labels=(levs gt -100.), color=1, position=PositionN,   $
               charsize=1.1

      top_title =  'March 2013 GEOS57/GEOS5 zonal mean'

      xyouts, 0.5, 0.95, top_title,   $
            /Normal, Color=!MYCT.BLACK, CharSize=1.5, Align=0.5

      multipanel, /advance, Position = PositionN

   endfor  ;IYEAR

   endfor  ;itrac
   MultiPanel, /off
   close_device

end
