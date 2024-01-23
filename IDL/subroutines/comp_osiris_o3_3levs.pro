;-----------------------------------------------------------------------
; NAME:
;        COMP_OSIRIS_O3_3LEVS
;
; PURPOSE:
;        Generate plots comparing OSIRIS O3 data with model output for
;        Jan, Apr, Jul, and Oct.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        COMP_OSIRIS_O3_3LEVS, FILES, VERSION, MODEL, RES [, Keywords ]
;
; INPUTS:
;        FILES -> A 4-element vector containing the names of files
;             containing GEOS-Chem model output for Jan, Apr, Jul, and Oct.
;
;        VERSION -> The model version being plotted
;
;        MODEL -> The model name (e.g. GEOS5, GEOSFP, MERRA2)
;
;        RES -> A 2-element vector containing the model resolution
;
; KEYWORD PARAMETERS:
;        /PS -> Set this switch to generate PostScript output.
;
;        OUTFILENAME -> If /PS is set, will write PostScript output 
;             to a file whose name is specified by this keyword.
;             Default is "tracer_ratio.pro".
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        External Subroutines Required:
;        =========================================
;        CTM_TYPE              CTM_GRID
;        CTM_GET_DATA
;     
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;
; NOTES:
;        Meant to be called from BENCHMARK_1YR.
;
; EXAMPLE:
;        FILES       = [ PREF3+'0101.nc', PREF3+'0401.nc', $
;                        PREF3+'0701.nc', PREF3+'1001.nc'  ]
;        VERSION     = VERS3
;        MODEL       = MODEL3
;        RES         = RES3
;        OUTFILENAME = 'Strat.O3.OSIRIS.3lev.ps'
;
;        COMP_OSIRIS_O3_3LEVS, FILES, VERSION, MODEL, RES, /PS, $
;                           OUTFILENAME=OUTFILENAME, _EXTRA=e 
;
;            ; Creates plots comparing GEOS-Chem output with ORISIR O3
;            ; for Jan, Apr, Jul, and Oct
;
; MODIFICATION HISTORY:
;  04 Dec 2015 - M. Sulprizio- Modify from original script provided by Dylan
;                              Jones for use in the 1-year benchmark plotting
;                              routines
;-----------------------------------------------------------------------

pro comp_osiris_o3_3levs, Files, Version, Model, Res, PS=PS, $
                          OutFileName=OutFileName, _EXTRA=e

   ; Arguments
   if ( N_Elements( Files    ) ne 4 ) then Message, 'Invalid FILES!'
   if ( N_Elements( Model    ) ne 1 ) then Message, 'Invalid MODEL!'

   ;--------------------------------------------------------
   ; Initialize variables
   ;--------------------------------------------------------
   ; Year for comparison
   year = 2009

   ; Months for comparison
   imon = [ 1, 4, 7, 10 ] ; desired months for comparison
   nmonths= n_elements(imon)

   ModelInfo = CTM_TYPE(Model, res=Res)
   GridInfo  = CTM_Grid( ModelInfo )
   imax = n_elements(gridinfo.xmid)
   jmax = n_elements(gridinfo.ymid)
   lmax = n_elements(gridinfo.zmid)
   
   jmaxs = 36
   lmaxs = 28
   
   GC_3Ddat=fltarr(imax,jmax,lmax)
   GC_2Ddat=fltarr(jmax,lmax,nmonths)

   A = FINDGEN(17) * (!PI*2/16.)
   USERSYM, COS(A), SIN(A), /FILL
   
   os_levs = [11,15,21]   ; OSIRIS 50, 10, and 1 hPa
   gc_levs = [38,47,58]   ; GEOS-5 levels
   pcols   = [ 1, 2, 3]   ; Plot colors: black, red, green
  
   ;--------------------------------------------------------
   ; Read in OSIRIS O3 data
   ;--------------------------------------------------------
   year_str = strtrim(string(year,format='(i4)'),2)
   sat_file = 'data/strat/OSIRIS_O3_NOy_data/SPARC_DI_T2Mz_O3_'+year_str+'_OSIRIS_v5-0_p01.nc'
   ncdf_read,file=sat_file,satdat,/all
   
   ;--------------------------------------------------------
   ; Set up plot
   ;--------------------------------------------------------
   ; Store defaults
   X_OMARGIN   = !X.OMARGIN
   Y_OMARGIN   = !Y.OMARGIN
   X_MARGIN    = !X.MARGIN
   Y_MARGIN    = !Y.MARGIN
   P_CHARTHICK = !P.CHARTHICK
   P_THICK     = !P.THICK
   X_THICK     = !X.THICK
   Y_THICK     = !Y.THICK

   ; Plot settings
   !X.OMARGIN=[4,2] 
   !Y.OMARGIN=[2,2]
   !X.MARGIN=[3,3]
   !Y.MARGIN=[3,3]
   !X.THICK=4
   !Y.THICK=4
   !P.CHARTHICK=2.5
   !P.THICK=2.5

   ; Use Postscript font
   !P.FONT=0

   ; Number of rows & colums on the plot
   Rows = 2
   Cols = 2
   !P.Multi = [0,Rows,Cols,0,0]

   ; Load colortable
   MyCt_Orig = !MYCT
   MyCt, /WhGrYlRd
   
   ; Open the plot device and initialize the page
   Open_Device, /Color, Bits=8, PS=PS, File=OutFileName, _EXTRA=e

   ;--------------------------------------------------------
   ; Loop over months
   ;--------------------------------------------------------
   for n=0,n_elements(imon)-1 do begin
      
      yyyymm_str = strtrim(string(year), 2) + $
                   strtrim(string((imon[n]), format='(i02)'),2)
      print, yyyymm_str

      ;-----------------------------------------------------
      ; Read in GEOS-Chem data and average
      ;-----------------------------------------------------
      ctm_get_data,datainfo,'IJ-AVG-$',file=Files[n],tracer=2
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
   
      ;-----------------------------------------------------
      ; Create the plot
      ;-----------------------------------------------------
      plot, gridinfo.ymid, 1.0e-3*GC_2Ddat[*,gc_levs[0],n], $
            xrange=[-90.,90.], xticks=6, xminor=3, xstyle=1,      $
            ;xtitle='Latitude',ytitle='O3 Mixing Ratio (ppm)',$
            yrange=[0,12], yticks=6,                              $
            title='O3 in '+yyyymm_str, col=1

      ; Loop over levels
      for it=0,2 do begin
        ilev   = gc_levs[it]
        satlev = os_levs[it]

        ; Debug
        ;print,"level = ",gridinfo.pmid[ilev],satdat.plev[satlev]
     
        oplot, gridinfo.ymid, 1.0e-3*GC_2Ddat[*,ilev,n], $
               line=0, col=pcols[it]

        indx = where(satdat.O3[*,satlev,n] gt 0.0)
        oplot, satdat.lat(indx), 1.0e+6*satdat.o3[indx,satlev,n], $
               psym=8, symsize=1.25, col=pcols[it]
   
        errplot, satdat.lat(indx), 1.0e+6*satdat.O3[indx,satlev,n]+    $
                                   1.0e+6*satdat.O3_STD[indx,satlev,n],$
                                   1.0e+6*satdat.O3[indx,satlev,n]-    $
                                   1.0e+6*satdat.O3_STD[indx,satlev,n],$
                                   col=pcols[it]
   
      endfor

   endfor

   ; Plot axis titles
   xyouts, 0.04, 0.5, 'O3 Mixing Ratio (ppm)', /normal, align = 0.5, $
           orientation=90, charsize=1.3, color=1
   xyouts, 0.5, 0.04, 'Latitude', /normal, align=0.5, charsize=1.3,color=1
   
   ; Plot top title
   TopTitle = 'GEOS-Chem ' + Version + ' vs OSIRIS O3 at 50 hPa (black), 10 hPa (red), 1 hPa (green)'
   xyouts, 0.5, 0.95, TopTitle, /normal, align=0.5, charsize=1.5,color=1

   ;====================================================================
   ; Cleanup and quit
   ;====================================================================
   
   ; Close plot device
   close_device

   ; Restore !MYCT sysvar to defaults
   if ( ChkStru( Myct_Orig ) ) then !MYCT = Myct_Orig

   ; Restore defaults
   !X.OMARGIN   = X_OMARGIN   
   !Y.OMARGIN   = Y_OMARGIN   
   !X.MARGIN    = X_MARGIN    
   !Y.OMARGIN   = Y_MARGIN    
   !P.CHARTHICK = P_CHARTHICK 
   !P.THICK     = P_THICK     
   !X.THICK     = X_THICK     
   !Y.THICK     = Y_THICK 
   
   ; Quit
   return
end
