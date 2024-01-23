;-----------------------------------------------------------------------
;+
; NAME:
;        GET_CONC_RANGE
;
; PURPOSE:
;        Returns a default plotting range for given GEOS-Chem tracers 
;        This will be used to create concentration plots for GEOS-Chem 
;        benchmarking.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        GET_CONC_RANGE, TRACERNAME, THIS_RANGE, THIS_UNIT
;
; INPUTS:
;        TRACERNAME -> Name of the tracer for which a default
;             plotting range will be returned.
;
; KEYWORD PARAMETERS:
;        None
;
; OUTPUTS:
;        THIS_RANGE -> A 2 element vector with the [min,max] values
;             to be used in creating a tracer difference plot.
;
;        THIS_UNIT -> String with the units of the data.
;
; SUBROUTINES:
;        None
;
; REQUIREMENTS:
;        Routine READ_CONC_RANGE must be called before this routine
;        may be used.  This will normally be done at the top of
;        driver routine BENCHMARK_1YR.
;
; NOTES:
;        (1) Meant to be used in conjunction with the GEOS-Chem 
;            benchmark plotting codes.
; 
;        (2) Default ranges for each tracer are read from a file by the 
;            complementary routine READ_DIFF_RANGE and stored in the
;            GDR common block.
;
; EXAMPLE:
;        READ_CONC_RANGE, 'conc_range.1mon'
;        GET_CONC_RANGE, 'NOx', THIS_LOBOUND, THIS_HIBOUND, THIS_UNIT
;        PRINT, THIS_LOBOUND, THIS_HIBOUND
;            0.50000  2.00000
;        PRINT, THIS_UNIT
;            ppbv
;
;            ; Prints the default plotting range and unit for NOx
;          
;
; MODIFICATION HISTORY:
;        bmy, 05 Sep 2012: VERSION 1.00
;
;-
; Copyright (C) 2012, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine get_diff_range"
;-----------------------------------------------------------------------


pro Get_Conc_Range, TracerName, This_LoBound, This_HiBound, This_Unit

   ; Common block
   common GCConc, Names, LoBound, HiBound, UnitStr
 
   ; Match the 
   Ind = Where( Names eq StrUpCase( StrTrim( TracerName, 2 ) ) ) 
 
   ; Return the corresponding range for that tracername
   ; Stop w/ an error message if no match is found
   if ( Ind[0] ge 0 ) then begin
      This_LoBound = LoBound[Ind]
      This_HiBound = HiBound[Ind]
      This_Range   = [ LoBound[Ind], HiBound[Ind] ]
      This_Unit    = UnitStr[Ind]
   endif else begin
      S = TracerName + ' is an invalid tracer name!'
      Message, S
   endelse
end
