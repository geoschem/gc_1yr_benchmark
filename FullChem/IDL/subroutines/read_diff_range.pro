;-----------------------------------------------------------------------
;+
; NAME:
;        READ_DIFF_RANGE
;
; PURPOSE:
;        Reads a file containing default plotting range for given 
;        GEOS-Chem tracers.  This will be used to create absolute 
;        difference plots for GEOS-Chem benchmarking.
;
; CATEGORY:
;        GEOS-Chem Benchmarking
;
; CALLING SEQUENCE:
;        READ_DIFF_RANGE, INPUTFILE
;
; INPUTS:
;        INPUTFILE -> Name of the file that contains the default
;             plotting ranges for each GEOS-Chem tracer.  Default
;             is "diff_range.1mon".
;
; KEYWORD PARAMETERS:
;        None
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        External Subroutines Required:
;        ================================
;        OPEN_FILE   STRBREAK (function)
;
; REQUIREMENTS:
;        READ_DIFF_RANGE must be called first to read the file with
;        default plotting ranges.  This is normally done at the top
;        of driver routine BENCHMARK_1MON.  After this has been done,
;        function GET_DIFF_RANGE may be used to return the default
;        plotting range from within another program.
;
; NOTES:
;        (1) Meant to be used in conjunction with the GEOS-Chem 
;            benchmark plotting codes.
; 
;        (2) Default ranges for each tracer are read from a file by 
;            this routine and stored in the GDR common block.
;
; EXAMPLE:
;        READ_DIFF_RANGE, 'diff_range.1mon'
;        PRINT, GET_DIFF_RANGE( 'NOx' )
;            -0.100000  0.100000
;   
;            ; Prints the default plotting range for NOx
;
; MODIFICATION HISTORY:
;        bmy, 14 Nov 2007: VERSION 1.00
;
;-
; Copyright (C) 2007, Bob Yantosca, Harvard University
; This software is provided as is without any warranty whatsoever.
; It may be freely used, copied or distributed for non-commercial
; purposes.  This copyright notice must be kept with any copy of
; this software. If this software shall be used commercially or
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.as.harvard.edu with subject "IDL routine read_diff_range"
;-----------------------------------------------------------------------


pro Read_Diff_Range, InputFile
 
   ; Common block
   common GCDiff, Names, Ranges, UnitStr
 
   ; Define variables
   Names   = StrArr( 200 )
   Ranges  = FltArr( 200 )
   UnitStr = StrArr( 200 )
   Line    = ''
 
   ; Open the input file
   Open_File, InputFile, Ilun, /Get_Lun
 
   ; Line counter
   N = 0L
 
   ; Loop thru file
   while ( not EOF( Ilun ) ) do begin
      
      ; Read a line
      ReadF, Ilun, Line
 
      ; Skip blank lines & comment lines
      if ( Line                eq '' ) then goto, Next
      if ( StrPos( Line, '#' ) ge 0  ) then goto, Next
 
      ; Break line by spaces
      Result     = StrBreak( Line, ' ' )

      ; Save into arrays
      Names  [N] = StrUpCase( StrTrim( Result[0], 2 ) )   ; Tracer name
      Ranges [N] = Float    (          Result[2]      )   ; Range
      UnitStr[N] = StrTrim  (          Result[3], 2   )   ; Units 

      ; Increment counter
      N = N + 1L
Next:
   endwhile
 
   ; Close file
   Close,    Ilun
   Free_Lun, Ilun
 
   ; Cut arrays down to size
   Names   = Names  [0:N-1L]
   Ranges  = Ranges [0:N-1L]
   UnitStr = UnitStr[0:N-1L]
 
end
