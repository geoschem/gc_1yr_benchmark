;to plot monthly zonal mean 222Rn,210Pb and 7Be concentrations
;hyl,1/19/2012 (hongyu.liu-1@nasa.gov)

pro zonal_mean, ps=ps

   code = 'GEOS5_47L'

   version = 'GEOS_5'
   file1   = '/home/mpayer/SR1/v10-01/v10-01-release/RnPbBe/bpch/ctm.bpch.20130101'
   fps     = './output/' + version + '.Rn-Pb-Be_zonal_mean.ps'

   modelinfo = CTM_type(code, res=4)
   gridinfo  = CTM_grid(modelinfo, psurf=986) 
   dao_sige = gridinfo.sigmid
   IM = gridinfo.IMX
   JM = gridinfo.JMX
   LM = gridinfo.LMX
   PMID = gridinfo.pmid
   print, 'IM,JM,LM= ', IM, JM, LM
   print, 'pmid= ', pmid

   ;layers saved & to be plotted
   lev1 = 1  & lev2 = LM

   XTicks = 6  &  XMinor = 2  &  XRange = [-90, 90]  ;Lat
   YRange = [1000, 50]    ;mb

   myct, 57

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

   if keyword_set(ps) then begin
      set_plot, 'PS'
   endif

   open_device, OLD_DEVICE, ps=PS,                       $
                winparam=winparam, /color, /portrait,    $
                filename=fps

   limit =  [XRange(0), -180, XRange(1), 180]

   multipanel, col=2, row=2, $
               margin=[0.055, 0.085, 0.055, 0.085], $
               omargin=[0.05, 0.08, 0.05, 0.08]

   multipanel, Position=PositionN
   print, 'Position= ', PositionN

   for itrac=0, 2 do begin

   ;choose tracer #
   case ( itrac ) of 
   0: begin & ntrac = 1  &  CharTrac = 'Rn'  &  SubTrac = 'Rn'  & end      
   1: begin & ntrac = 2  &  CharTrac = 'Pb'  &  SubTrac = 'Pb'  & end     
   2: begin & ntrac = 3  &  CharTrac = 'Be7' &  SubTrac = 'Be7' & end  
   endcase

   case itrac of
      0: begin
         ;c_levels=[0.005, 0.01, 0.1, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000]
         ;c_levels=[0.005, 0.01, 0.1, 0.5, 1, 2, 5, 10, 20, 50, 100, 200, 500, 1000]
         levs = [0.01, 0.1, 1,   10,  20,  $
                 50,   100, 200, 500, 1000 ]
         c_annotation =  strtrim(string(format='(f10.2)', levs), 2)
         title = '!U222!NRn [mBq/SCM]'
      end
      1: begin
         ; Use same contour intervals as Fig. 3 in Liu et al., JGR 2001
         levs  = [ 0.02, 0.05, 0.1,  0.15, 0.20, 0.30, $
                   0.40, 0.50, 0.60, 0.70, 0.80 ]
         c_annotation =  strtrim(string(format='(f10.2)', levs), 2)
         title = '!U210!NPb [mBq/SCM]'
      end
      2: begin
         ; Use same contour intervals as Fig. 3 in Liu et al., JGR 2001
         levs  = [ 1, 2.5, 5, 7.5, 10, 20, 50, 100, 200, 500, 1000, 5000 ]
         c_annotation =  strtrim(string(format='(f10.2)', levs), 2)
         title = '!U7!NBe [mBq/SCM]'
      end
   endcase

   colors = 30+256.*findgen(n_elements(levs))/(1.*n_elements(levs))

   c_linestyle = ( levs lt 0 )

   ;-------------------
   ;loop over 1 year
   ;-------------------

   YEARS = ['01' ]
   YYYY  = ['2013']

   ;2013
   MONTHS  = ['01']
   yymmdd2tau0, '130101', tau3
   time = [ long(tau3) ]

   for IYEAR=0, n_elements( YEARS ) -1 do begin
      IYR = YEARS ( IYEAR )

   ;-------------------
   ; loop over 12 months
   ;-------------------
   N_Mon = n_elements( MONTHS )
   print,  'Number of months = ', N_Mon

   std_zm = FltArr (JM,lev2,N_Mon+1)

   for IMONTH = 0, n_elements( MONTHS ) -1 do begin
      IMN = MONTHS ( IMONTH )

      get_field_3d_4x5, code, ntrac, 'IJ-AVG-$', lev1, lev2, $
                        time(imonth), limit, file1, std_3d, xmid, ymid, zmid
      help, std_3d
      
;------------------------------------------------------------------------------
; Now tracers are in mBq/SCM in tracerinfo.dat (mpayer, 1/24/12)
;      ;This assumes units for RnPbBe concentrations are v/v in tracerinfo.dat
;      case itrac of
;         0: std_3d = std_3d * 5.6397E22   ;Rn  v/v-->mBq/SCM
;         1: std_3d = std_3d * 2.6141E19   ;Pb  v/v-->mBq/SCM
;         2: std_3d = std_3d * 4.0513E21   ;Be7 v/v-->mBq/SCM
;      endcase
;------------------------------------------------------------------------------

      ;find zonal average
      ;std_zm = FltArr (JM,lev2)
      for J = 0, JM-1 do begin 
      for L = 0, lev2-1 do begin  
         std_zm (J,L,IMONTH) = total ( std_3d(*,J,L) ) / n_elements(xmid)
      endfor
      endfor

      print, 'Month =',  MONTHS ( IMONTH )
      print, 'max(std_zm), min(std_zm)= ', max(std_zm), min(std_zm)

   endfor  ;IMONTH

;   ; Find annual zonal average
;   annual_zm = FltArr (JM,lev2)
;   for J = 0, JM-1 do begin 
;   for L = 0, lev2-1 do begin  
;      annual_zm (J,L) = total ( std_zm(J,L,*) ) / n_elements(MONTHS)
;   endfor
;   endfor

      ;tvplot_hyl, std_zm, ymid(*), pmid(0:lev2-1), /contour, c_levels = levs, c_thick=4, c_linestyle=c_linestyle, $
      ;XRange=XRange, YRange=YRange, XTicks=XTicks, XMinor=XMinor, XStyle=1, $
      ;YTicks=YTicks, YMinor=YMinor, YStyle=1, $
      ;title=title, ytitle=ytitle, Xtitle=xtitle, c_colors=1, c_charsize=1.5, $
      ;c_annotation = c_annotation, Position=PositionN, CSFAC=1.4, /ylog

      contour, std_zm(*,*,0), ymid(*), pmid(0:lev2-1),levels=levs,          $
               /follow,xrange=[-90,90],yrange=[1000,50],ytype=1,/fill,  $
               title=title, xstyle=1, ystyle=1,xticks=6,                $
               xtitle='!5', ytitle='!5Pressure (hPa)',                  $
               c_colors=colors, color=1, position=PositionN,            $
               charsize=1.1

      contour, std_zm(*,*,0), ymid(*), pmid(0:lev2-1),levels=levs,          $
               /follow,xrange=[-90,90],yrange=[1000,50],xticks=6,       $
               xstyle=1, ystyle=1, ytype=1,/noerase,                    $
               c_labels=(levs gt -100.), color=1, position=PositionN,   $
               charsize=1.1


      fig_title =  'GEOS-Chem ' + version + ' Zonal Mean, March 2013'

      xyouts, 0.5, 0.95, fig_title,   $
            /Normal, Color=!MYCT.BLACK, CharSize=1.5, Align=0.5

      multipanel, /advance, Position = PositionN

   endfor  ;IYEAR

   endfor  ;itrac

   close_device

end
