;-----------------------------------------------------------------------
;+
; NAME:
;        RnPbBe_Flux
;
; PURPOSE:
;        Calculate fluxes for a 1-year Rn-Pb-Be benchmark simulation
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        RNPBBE_FLUX, PREF, VERSION, MODEL, RES [, Keywords ]
;
; INPUTS:
;        PREF -> Directory and label name of model to be plotted
;
;        VERSION -> The model version being plotted
;
;        MODEL -> The model name (e.g. GEOS5, GEOSFP, MERRA2)
;
;        RES -> A 2-element vector containing the model resolution
;
; KEYWORD PARAMETERS:
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
;        CTM_GET_DATABLOCK
;     
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;        
; NOTES:
;        (1) Meant to be called from BENCHMARK_1YR.
;
; EXAMPLE:
;        PREF        = PREF3
;        VERSIONS    = VERS3
;        MODEL       = MODEL3
;        RES         = RES3
;        OUTFILENAME = 'Aerosol_Burdens.txt'
;
;        GEOSCHEM_MEAN_AOD, PREF, VERSION, MODEL, RES, $
;                           OUTFILENAME=OUTFILENAME, _EXTRA=e 
;
;            ; Creates text output file containing global aerosol burdens
;
; MODIFICATION HISTORY:
;        mps, 21 Mar 2017: Initial version
;-----------------------------------------------------------------------

pro RnPbBe_Flux, Pref, Version, Model, Res, $
                             OutFileName=OutFileName, _EXTRA=e 

   ; For testing
   ; Hardcode InFile bewlow
   Pref    = 'trac_avg.geosfp_4x5_RnPbBe.'
   Version = 'v11-02a'
   Model   = 'GEOSFP_47L'
   Res     = [5.0, 4.0]
   Year    = 2013

   ; Arguments
   if ( N_Elements( Pref    ) ne 1 ) then Message, 'Invalid PREF!'
   if ( N_Elements( Version ) ne 1 ) then Message, 'Invalid VERSION!'
   if ( N_Elements( Model   ) ne 1 ) then Message, 'Invalid MODEL!'
   if ( N_Elements( Res     ) ne 2 ) then Message, 'Invalid RES!'

   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'RnPbBe_Flux.txt'

   ; Configurable parameters
   SpcStr = [      'Rn',      'Pb',      'Be', 'PASV' ]
   SpcID  = [         1,         2,         3,      4 ]
   SpcMW  = [     222.0,     210.0,       7.0,    1.0 ]
   Factor = [ 5.6397E22, 2.6141E19, 4.0513E21,    1.0 ] ; mBq/SCM -> v/v factor
   nSpc   = N_Elements( SpcId )
   mw_air = 28.9644*1e-3   ;kg/mole

   ; Determine if it is a leap year
   if ( year eq '1984' or year eq '1988' or year eq '1992' or $
        year eq '1996' or year eq '2000' or year eq '2004' or $
        year eq '2008' or year eq '2012' or '2016' ) then begin
      ndays = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
   endif else begin
      ndays = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
   endelse
   secinday = 86400.0

   ; Get model and grid info
   modelinfo = CTM_type(Model, Res=Res)
   gridinfo  = CTM_grid(modelinfo, psurf=986) 
   xmid      = gridinfo.xmid
   ymid      = gridinfo.ymid
   zmid      = gridinfo.zmid
   dao_sige  = gridinfo.sigmid
   IM        = gridinfo.IMX
   JM        = gridinfo.JMX
   LM        = gridinfo.LMX
   PMID      = gridinfo.pmid
   ZRange    = [   1, LM]   ;Lev    
   YRange    = [ -90, 90]   ;Lat
   XRange    = [-180,180]   ;Lon

   ; Select coastal eastern US grid box [42 N, 71 W]
   ctm_index, modelinfo, I, J, CENTER=[42.0,-71.0], /NON_INTERACTIVE
   I = I-1
   J = J-1
   L = 0

   ;=========================================
   ; Initialize arrays
   airmass   = fltarr(72,46,47,12)
   SpcConc   = fltarr(72,46,47,12,nSpc)
   EW_Flux   = fltarr(72,46,47,12,nSpc)
   NS_Flux   = fltarr(72,46,47,12,nSpc)
   UP_Flux   = fltarr(72,46,47,12,nSpc)
   CV_Flux   = fltarr(72,46,46,12,nSpc)
   BL_Flux   = fltarr(72,46,47,12,nSpc)
   Conc      = fltarr(nSpc)
   WestEdge  = fltarr(nSpc)
   EastEdge  = fltarr(nSpc)
   SouthEdge = fltarr(nSpc)
   NorthEdge = fltarr(nSpc)
   BotEdgeUP = fltarr(nSpc)
   TopEdgeUP = fltarr(nSpc)
   BotEdgeCV = fltarr(nSpc)
   TopEdgeCV = fltarr(nSpc)
   BotEdgeBL = fltarr(nSpc)
   TopEdgeBL = fltarr(nSpc)

   ;=========================================
   ; Loop over months
;   for M = 0, 12-1 do begin
   for M = 0, 0 do begin

      mn = strtrim(String(fix(M+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Input filename
;      InFile = pref+mn+'01.nc'
      InFile = 'v11-02a_RnPbBe_TURBDAY/trac_avg.geosfp_4x5_RnPbBe.201307010000'

      ; Get airmass
      success =  ctm_get_datablock(std_3d, 'BXHGHT-$',           $  
                           Tracer=24002,                          $
                           Lat = [YRange[0], YRange[1]],          $
                           Lon = [XRange[0], XRange[1]],          $
                           Lev = [ZRange[0], ZRange[1]],                    $
                           FileName=InFile )
      airmass[*,*,*,M] = std_3d

      ; Loop over species
      for N = 0, nSpc-1 do begin

         ; Get species concentration [mBq/SCM]
         success = ctm_get_datablock(std_3d, 'IJ-AVG-$',            $
                             Tracer=SpcID[N],                       $
                             Lat = [YRange[0], YRange[1]],          $
                             Lon = [XRange[0], XRange[1]],          $
                             FileName=InFile )

         SpcConc[*,*,*,M,N] = std_3d 

         ; Get E-W Flux [kg/s]
         success = ctm_get_datablock(std_3d, 'EW-FLX-$',            $
                             Tracer=SpcID[N],                       $
                             Lat = [YRange[0], YRange[1]],          $
                             Lon = [XRange[0], XRange[1]],          $
                             FileName=InFile )

         EW_Flux[*,*,*,M,N] = std_3d 

         ; Get N-S Flux [kg/s]
         success = ctm_get_datablock(std_3d, 'NS-FLX-$',            $
                             Tracer=SpcID[N],                       $
                             Lat = [YRange[0], YRange[1]],          $
                             Lon = [XRange[0], XRange[1]],          $
                             FileName=InFile )
         NS_Flux[*,*,*,M,N] = std_3d 

         ; Get Up-Down Flux [kg/s]
         success = ctm_get_datablock(std_3d, 'UP-FLX-$',            $
                             Tracer=SpcID[N],                       $
                             Lat = [YRange[0], YRange[1]],          $
                             Lon = [XRange[0], XRange[1]],          $
                             FileName=InFile )
         UP_Flux[*,*,*,M,N] = std_3d 

;         ; Get Convective Mass Flux [kg/s]
;         success = ctm_get_datablock(std_3d, 'CV-FLX-$',            $
;                             Tracer=SpcID[N],                       $
;                             Lat = [YRange[0], YRange[1]],          $
;                             Lon = [XRange[0], XRange[1]],          $
;                             FileName=InFile )
;         CV_Flux[*,*,*,M,N] = std_3d 
;
;         ; Get BL Mixing Flux [kg/s]
;         success = ctm_get_datablock(std_3d, 'TURBMC-$',            $
;                             Tracer=SpcID[N],                       $
;                             Lat = [YRange[0], YRange[1]],          $
;                             Lon = [XRange[0], XRange[1]],          $
;                             FileName=InFile )
;         BL_Flux[*,*,*,M,N] = std_3d 

         ; Get concentrations for grid box and convert to kg
         Conc[N] = SpcConc[I, J, L, M, N]
         Conc[N] = Conc[N] * Factor[N] ; mBq/SCM->v/v
         Conc[N] = Conc[N] * airmass[I,J,L,M] / ( mw_air / SpcMW[N] ) ; v/v->kg

         ; Get flux at grid box edges
         WestEdge[N]  = EW_Flux[I,   J,   L,   M, N] ; Remove Total for now
         EastEdge[N]  = EW_Flux[I+1, J,   L,   M, N]
         SouthEdge[N] = NS_Flux[I,   J,   L,   M, N]
         NorthEdge[N] = NS_Flux[I,   J+1, L,   M, N]
         BotEdgeUP[N] = UP_Flux[I,   J,   L,   M, N]
         TopEdgeUP[N] = UP_Flux[I,   J,   L+1, M, N]
;        BotEdgeCV[N] = Total(CV_Flux[I, J:J+1, L,     M, N])
;        TopEdgeCV[N] = Total(CV_Flux[I, J:J+1, L:L+1, M, N])
;        BotEdgeBL[N] = Total(BL_Flux[I, J:J+1, L,     M, N])
;        TopEdgeBL[N] = Total(BL_Flux[I, J:J+1, L:L+1, M, N])

         ; Convert kg/s -> kg
;         WestEdge[N]  = WestEdge[N]  * secinday * ndays[M]
;         EastEdge[N]  = EastEdge[N]  * secinday * ndays[M]
;         SouthEdge[N] = SouthEdge[N] * secinday * ndays[M]
;         NorthEdge[N] = NorthEdge[N] * secinday * ndays[M]
;         BotEdgeUP[N] = BotEdgeUP[N] * secinday * ndays[M]
;         TopEdgeUP[N] = TopEdgeUP[N] * secinday * ndays[M]
;         BotEdgeCV[N] = BotEdgeCV[N] * secinday * ndays[M]
;         TopEdgeCV[N] = TopEdgeCV[N] * secinday * ndays[M]
;         BotEdgeBL[N] = BotEdgeBL[N] * secinday * ndays[M]
;         TopEdgeBL[N] = TopEdgeBL[N] * secinday * ndays[M]

         ; Convert kg/s -> kg for 1 timestep
         WestEdge[N]  = WestEdge[N]  * 60*10
         EastEdge[N]  = EastEdge[N]  * 60*10
         SouthEdge[N] = SouthEdge[N] * 60*10
         NorthEdge[N] = NorthEdge[N] * 60*10
         BotEdgeUP[N] = BotEdgeUP[N] * 60*10
         TopEdgeUP[N] = TopEdgeUP[N] * 60*10

      endfor ; nSpc

   endfor  ; Month

   ;===========================================================
   ; Open file to create table for tropospheric burden
   openW, unit, OutFileName, /get_lun
   printf, unit, ' '
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   printf, unit, '  Annual Average Flux in ' + Version, ' at 42 N, 71 W'
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

   ; Also print to screen
   print, ' '
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   print, ' Annual Average Flux in ' + Version, ' at 42 N, 71 W'
   print, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'

   ; Loop over species
   for N = 0, nSpc-1 do begin

      EWMass = (WestEdge[N]-EastEdge[N])
      NSMass = (SouthEdge[N]-NorthEdge[N])
      UPMass = (BotEdgeUP[N]-TopEdgeUP[N])
;     CVMass = (BotEdgeCV[N]-TopEdgeCV[N])
;     BLMass = (BotEdgeBL[N]-TopEdgeBL[N])

      ; Print fluxes to file
      printf, unit, ''
      printf, unit, '-----------------------------'
      printf, unit, ' ', SpcStr[N], ' flux [kg]' 
      printf, unit, '-----------------------------'
      printf, unit, ''
      printf, unit, '  Mass        : ', Conc[N]
      printf, unit, ''
      printf, unit, '  West   Edge : ', WestEdge[N]
      printf, unit, '  East   Edge : ', EastEdge[N]
      printf, unit, '  South  Edge : ', SouthEdge[N]
      printf, unit, '  North  Edge : ', NorthEdge[N]
      printf, unit, '  Bottom Edge : '
      printf, unit, '   Advection  : ', BotEdgeUP[N]
      printf, unit, '   Convection : ', BotEdgeCV[N]
      printf, unit, '   BL Mixing  : ', BotEdgeBL[N]
      printf, unit, '  Top    Edge : '
      printf, unit, '   Advection  : ', TopEdgeUP[N]
      printf, unit, '   Convection : ', TopEdgeCV[N]
      printf, unit, '   BL Mixing  : ', TopEdgeBL[N]
      printf, unit, ''
      printf, unit, '  Net flux  : ', EWMass+NSMass+UPMass ;+CVMass+BLMass

      ; Also print to screen
      print, ''
      print, '-----------------------------'
      print, ' ', SpcStr[N], ' flux [kg]' 
      print, '-----------------------------'
      print, ''
      print, '  Mass        : ', Conc[N]
      print, ''
      print, '  West   Edge : ', WestEdge[N]
      print, '  East   Edge : ', EastEdge[N]
      print, '  South  Edge : ', SouthEdge[N]
      print, '  North  Edge : ', NorthEdge[N]
      print, '  Bottom Edge : '
      print, '   Advection  : ', BotEdgeUP[N]
      print, '   Convection : ', BotEdgeCV[N]
      print, '   BL Mixing  : ', BotEdgeBL[N]
      print, '  Top    Edge : '
      print, '   Advection  : ', TopEdgeUP[N]
      print, '   Convection : ', TopEdgeCV[N]
      print, '   BL Mixing  : ', TopEdgeBL[N]
      print, ''
      print, '  Net flux  : ', EWMass+NSMass+UPMass ;+CVMass+BLMass

      ; Debug
      print, ''
      print, 'EWMASS = ', EWMass
      print, 'NSMass = ', NSMass
      print, 'UPMass = ', UPMass


   endfor

   ; Close output file
   free_lun, unit

end
