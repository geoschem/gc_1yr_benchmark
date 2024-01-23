;-----------------------------------------------------------------------
;+
; NAME:
;        IS_AEROSOL
;
; PURPOSE:
;        Determines if a tracer is an aerosol tracer or not
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        RESULT = IS_AEROSOL( TRACERNAME )
;
; INPUTS:
;        TRACERNAME -> Name of a GEOS-Chem tracer.
;
; KEYWORD PARAMETERS:
;        None
;
; OUTPUTS:
;        RESULT -> 1 denotes an aerosol tracer.
;
;
; SUBROUTINES:
;        None
;     
; REQUIREMENTS:
;        References routines from the GAMAP packages.
;        
; NOTES:
;        None
;
; EXAMPLE:
;        PRINT*, IS_AEROSOL( 'BCPO' )
;               1
;
; MODIFICATION HISTORY:
;        bmy, 29 Aug 2012: VERSION 1.01
;
;-.
; Copyright (C) 2012, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine differences"
;-----------------------------------------------------------------------


function Is_Aerosol, TracerName

   if ( N_Elements( Tracername ) eq 0 ) then Message, 'Need to pass TRACERNAME!'

   ; Decide if a tracer is an aerosol or not
   case ( StrUpCase( StrTrim( TracerName, 2 ) ) ) of
      'DMS'  : Aer = 1L
      'SO2'  : Aer = 1L
      'SO4'  : Aer = 1L
      'SO4S' : Aer = 1L
      'MSA'  : Aer = 1L
      'NH3'  : Aer = 1L
      'NH4'  : Aer = 1L
      'NIT'  : Aer = 1L
      'NITS' : Aer = 1L
      'BCPI' : Aer = 1L
      'BCPO' : Aer = 1L
      'OCPI' : Aer = 1L
      'OCPO' : Aer = 1L
      'DST1' : Aer = 1L
      'DST2' : Aer = 1L
      'DST3' : Aer = 1L
      'DST4' : Aer = 1L
      'SALA' : Aer = 1L
      'SALC' : Aer = 1L
      'SOAS' : Aer = 1L
      else   : Aer = 0L
   endcase

   ; Return value
   return, Aer
end
