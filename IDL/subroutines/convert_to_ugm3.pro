;-----------------------------------------------------------------------
;+
; NAME:
;        CONVERT_TO_UGM3
;
; PURPOSE:
;        Converts aerosol tracers from mixing ratio to ug/m3.
;
; CATEGORY:
;        GEOS-CHEM Benchmarking
;
; CALLING SEQUENCE:
;        CONVERT_TO_UgM3, DATA, GRIDINFO, TRACERNAME, BOXHEIGHT, AIRMASS
;
; INPUTS:
;        DATA -> Array contaning aerosol tracer concentrations in ppbv.  
;
;        GRIDINFO -> A GRIDINFO structure returned from function
;             CTM_GRID that defines the geographical grid.
;
;        TRACERNAME -> Name of the aerosol tracer.
;
;        BOXHEIGHT -> Array contaning grid box heights [m].
;
;        AIRMASS -> Array containing the air mass in a grid box [kg].
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Requires CTM_TRACERINFO.
;
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;
; NOTES:
;        None
;
; EXAMPLES:
;         CONVERT_TO_UgM3, Data, GridInfo, 'SO2', BoxHeight, AirMass
;
;             ; Converts SO2 data from ppbv to v/v
;
; MODIFICATION HISTORY:
;        bmy, 30 Aug 2012: VERSION 1.01
;
;-
; Copyright (C) 2012, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to yantosca@seas.harvard.edu
; with subject "IDL routine benchmark_1yr"
;-----------------------------------------------------------------------


pro Convert_To_UgM3, Data,      ModelInfo, GridInfo,  Tracername,            $
                     BoxHeight, AirMass,   L15S=L15S, L42N=L42N

   ; Get the molecular weight of the tracer
   CTM_Tracerinfo, TracerName, MolWt = MolWt

   ; Size of the data array
   S = Size( Data, /Dim )

   ; Area [m2]
   Area = CTM_BoxSize( GridInfo, /Geos, /M2 )

   ;--------------------
   ; For 15S profile
   ;--------------------
   if ( Keyword_Set( L15S ) ) then begin

      ; Take proper slice of data
      Area = CTM_Extract( Area, ModelInfo=ModelInfo, GridInfo=GridInfo,      $
                                Lon=[-180,180],      Lat=[-15,-15]      )

      ; Rebin AREA to match the dimensions of Data
      Area = Rebin( Area, S[0], S[1] )

   endif

   ;--------------------
   ; For 42N profile
   ;--------------------
   if( Keyword_Set( L42N ) ) then begin

      ; Take proper slice of data
      Area = CTM_Extract( Area, ModelInfo=ModelInfo, GridInfo=GridInfo,      $
                                Lon=[-180,180],      Lat=[42, 42]       )
      
      ; Rebin AREA to match the dimensions of Data
      Area = Rebin( Area, S[0], S[1] )

   endif

   ;------------------------------------------------
   ; If 3D data, replicate area vertically
   ;------------------------------------------------
   NDimData = Size( Data, /N_DIMENSIONS )
   if ( NDimData eq 3 ) then begin
      Area3D =  fltarr(S[0], S[1], S[2])
      for L = 0, S[2]-1 do begin
         Area3D( * , * , L) = AREA
      endfor
      Area = Area3D
   endif

   ; Volume [m3]
   Volume = BoxHeight * Area

   ; Convert from [ppbv] --> [ug/m3]
   Data   = Data * 1d-9                      ; 1 [ppbv] = 1e-9 [v/v]
   Data   = Data * AirMass                   ; * AD     [kg   ]
   Data   = Data / ( 28.97d-3 / MolWt[0] )   ; / TCVV
   Data   = Data * 1d9                       ; * 1e9    [ug/kg]
   Data   = Data / Volume                    ; / volume [m3   ]  =  [ug/m3]

end
