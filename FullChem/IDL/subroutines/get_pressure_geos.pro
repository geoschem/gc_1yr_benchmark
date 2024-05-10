; $Id: get_pressure_geos.pro,v 1.3 2008/05/23 20:09:52 bmy Exp $
function Get_Pressure_Geos, FileName, PTOP=PTOP,$
                       Verbose=Verbose, $
                       Lat=FileLat, Lon=FileLon, $
                       _EXTRA=e

   ;====================================================================
   ; Keywords / External functions
   ;====================================================================
   FORWARD_FUNCTION NCDF_Get

   if ( N_Elements( FileName ) ne 1 ) then Message, 'FILENAME not passed!'
   Verbose = Keyword_Set( Verbose )

   ;====================================================================
   ; Read Header and Index information from the netCDF file
   ;====================================================================

   ; Define flags
   IsSigma   = 0
   IsEta     = 0

   ; Expand filename to full path name
   FileName  = Expand_Path( FileName )

   ; Open file
   fId       = NCDF_Open( FileName )

   ; Test if we have ETA coordinates or SIGMA coordinates (bmy, 3/29/04)
   NStru = NCDF_Inquire( fId )
 
   ; Loop over netCDF variables
   for N = 0L, NStru.NVars-1L do begin
      VarDesc = NCDF_VarInq( fId, N )
     
      ; We have sigma grid
      if ( StrTrim( VarDesc.Name ) eq 'SIGC' ) then begin
         IsSigma = 1
         goto, Next
      endif

      if ( StrTrim( VarDesc.Name ) eq 'ETAC' ) then begin
         IsEta = 1
         goto, Next
      endif
   endfor

Next:

   ; Read TIME from file
   FileTime = NCDF_Get( fId, 'time' )
   N_Time   = N_Elements( FileTime )

   ; Read LONGITUDE from file
   FileLon   = NCDF_Get( fId, 'lon' )
   N_Lon     = N_Elements( FileLon )

   ; Read LATITUDE from file
   FileLat   = NCDF_Get( fId, 'lat' )
   N_Lat     = N_Elements( FileLat )

   ; Read ALTITUDE from file
   FileSigma = NCDF_Get( fId, 'lev' )
   N_Alt     = N_Elements( FileSigma )

;------------------------------------------------------------------------------
; Prior to 1/9/14
; Don't need these when we convert files using bpch2coards.pro
;  ; Read SIGMA from file
;  if ( IsSigma ) then begin
;     FileSigma = NCDF_Get( fId, 'SIGC' )
;     N_Alt     = N_Elements( FileSigma )
;  endif 
;
;  ; Read ETA from file
;  if ( IsEta ) then begin
;     FileSigma = NCDF_Get( fId, 'ETAC' )
;     N_Alt     = N_Elements( FileSigma )
;  endif
;------------------------------------------------------------------------------

   ; If /VERBOSE is set, then print out quantities
   if ( Verbose ) then begin
      ;print, 'Time:'
      ;print, FileTime
      ;print, 'Longitudes in file '
      ;print, FileLon
      ;print, 'Latitudes in file '
      ;print, FileLat
      ;print, 'Sigma levels in file '
      ;print, FileSigma
   endif

   ;====================================================================
   ; Read surface pressure from the netCDF file
   ; NOTE: dimensions are: [ lon, lat]
   ;====================================================================

   ; INDD = location of DATE w/in the FILEDATES dimension
   IndD = 0

   ; Read the SURFACE PRESSURE for the given DATE from the file
   ; NOTE: for now, pull out all lon, lat
   OffSet = [ 0,     0,     IndD[0], 0      ]
   Count  = [ N_Lon, N_Lat, 1,       N_Time ]

   ;-----------------------------------------------------
   ; First check if the data file has category PEDGE-$ 
   ;-----------------------------------------------------

   ; First try Met_PEDGE
   ncName = 'Met_PEDGE'
   vId2   =  NCDF_VarID( fId, ncName )

   ; Then try PEDGE_S__PSURF
   if ( vId2 lt 0 ) then begin
      ncName = 'PEDGE_S__PSURF'
      vId2   =  NCDF_VarID( fId, ncName )
   endif

   ; Then try PEDGE_S__PEDGE
   if ( vId2 lt 0 ) then begin
      ncName = 'PEDGE_S__PEDGE'
      vId2   =  NCDF_VarID( fId, ncName )
   endif

   ; Then try PEDGE-S__PSURF
   if ( vId2 lt 0 ) then begin
      ncName = 'PEDGE-S__PSURF'
      vId2   =  NCDF_VarID( fId, ncName )
   endif

   ; Then try PEDGE-S__PEDGE
   if ( vId2 lt 0 ) then begin
      ncName = 'PEDGE-S__PEDGE'
      vId2   = NCDF_VarID( fId, ncName )
   endif

   ; If we have matched the var name, then read the data
   if ( vId2 ge 0 ) then begin

      ; Define Pressure array
      Pressure = FltArr( N_Lon, N_Lat, N_Alt )

      ; The PEDGE-$ diagnostic contains 3-D pressure edges
      Psurf = NCDF_Get( fId, ncName )

      ; Compute the pressure centers from the pressure edges 
      for L = 0L, N_Alt-1L do begin
         Pressure[*,*,L] = 0.5 * ( PSurf[*,*,L] + Psurf[*,*,L+1] )
      endfor

      ; Close file
      NCDF_Close, fId

      ; Return 3-D pressure array to calling program
      return, Pressure
      
   endif

   ;-----------------------------------------------------
   ; Then check if data file has category PS-PTOP
   ; (older category, prior to v8-01-01)
   ;-----------------------------------------------------

   ; First try PS_PTOP__PSURF
   ncName = 'PS_PTOP__PSURF'
   vId    = NCDF_VarID( fId, ncName )

   ; Then try PS-PTOP__PSURF
   if ( vId lt 0 ) then begin
      ncName = 'PS-PTOP__PSURF'
      vId    = NCDF_VarID( fId, ncName )
   endif

   ; Then try PS-PTOP::PSURF (old name from IDL 5)
   if ( vId lt 0 ) then begin
      ncName = 'PS-PTOP::PSURF'
      vId    = NCDF_VarID( fId, ncName )
   endif

   ; Then try to read the surface pressure
   if ( vId ge 0 ) then begin
      Psurf  = NCDF_Get( fId, ncName )
   endif else begin
      S = 'Could not find ' + ncName + ' in ' + Extract_FileName( FileName )
      Message, S
   endelse

   ;====================================================================
   ; Compute pressure at each vertical level, using the formula
   ;====================================================================

   ; NOTE: This is a kludge so that we don't have to rewrite everything
   ; in the many locations where GET_PRESSURE_GEOS is called.
   ; (bmy, 7/10/07)

   ; Get the name of the model (global attribute)
   NCDF_AttGet, fId, 'Model', ModelName, /Global
   ModelName = StrUpCase( StrTrim( ModelName, 2 ) )

   ; Close file
   NCDF_Close, fId

   ; Overwrite the sigma edges for GCAP
   if ( ModelName eq 'GCAP' ) then begin

      ;------------------------------------------
      ; GCAP: compute pressures at box centers
      ;------------------------------------------
 
      ; Ap from GEOS-CHEM for GCAP
      Ap = [   0.002000,   4.318492,   9.893948,  17.987375,  29.677859, $
              49.281923,  74.461436, 100.540229, 120.503991, 132.913897, $
             142.446144, 150.000000, 116.999999,  86.199995,  56.200001, $
              31.599995,  17.800001,   9.999999,   4.629997,   1.459998, $
               0.460995,   0.144999,   0.031200,   0.002058 ]

      ; Bp from GEOS-CHEM for GCAP
      Bp = [   1.000000,   0.971223,   0.934053,   0.880096,   0.802158, $
               0.671463,   0.503597,   0.329736,   0.196643,   0.113909, $
               0.050360,   0.000000,   0.000000,   0.000000,   0.000000, $
               0.000000,   0.000000,   0.000000,   0.000000,   0.000000, $
               0.000000,   0.000000,   0.000000,   0.000000 ]

      ; # of altitudes
      N_Alt = N_Elements( Ap ) - 1L

       ; Pressure array
      Pressure = FltArr( N_Lon, N_Lat, N_Alt )

      ; Formula for real pressure     pressure= (psurf-ptop) * sigma + ptop
      for L = 0L, N_Alt-1L do begin
      for j = 0L, N_Lat-1L do begin
      for i = 0L, N_Lon-1L do begin
         P_Bot           = Ap[L]   + ( Bp[L]   * ( Psurf[I,J,0] - PTOP ) )
         P_Top           = Ap[L+1] + ( Bp[L+1] * ( Psurf[I,J,0] - PTOP ) )
         Pressure[I,J,L] = ( P_Bot + P_Top ) * 0.5e0
      endfor
      endfor
      endfor

   endif else begin

      ;------------------------------------------
      ; Other models except GCAP
      ;------------------------------------------

      ; Pressure array
      Pressure = FltArr( N_Lon, N_Lat, N_Alt )

      ; Formula for real pressure     pressure= (psurf-ptop) * sigma + ptop
      for L = 0L, N_Alt-1L do begin
      for j = 0L, N_Lat-1L do begin
      for i = 0L, N_Lon-1L do begin
         Pressure[I,J,L] = FileSigma[L]* psurf[I,J,0]+PTOP 
      endfor
      endfor
      endfor

   endelse

   ; Return to calling program
   return, Pressure

end
