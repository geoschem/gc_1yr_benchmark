;-----------------------------------------------------------------------
;+
; NAME:
;        MAKE_PDF
;
; PURPOSE:
;        Creates PDF files from PostScript files located in
;        the output/ subdirectory.
;
; CATEGORY:
;        GEOS-CHEM Benchmarking
;
; CALLING SEQUENCE:
;        MAKE_PDF 
;
; INPUTS:
;        None 
;
; KEYWORD PARAMETERS:
;        None
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        External Subroutines Required:
;        ===============================
;        MFINDFILE     (function)
;        REPLACE_TOKEN (function)
;
; REQUIREMENTS:
;        You need "ps2pdf" installed on your system.  This ships
;        with most versions of Unix or Linux.
;
; NOTES:
;        None
;
; EXAMPLES:
;        MAKE_PDF
;
;             ; Test to see if default values were read in correctly,
;             ; but do not create any benchmark plots.
;
; MODIFICATION HISTORY:
;        bmy, 07 Nov 2007: VERSION 1.01
;
;-
; Copyright (C) 2007-2008, 
; Bob Yantosca and Philippe Le Sager, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.harvard.edu with subject "IDL routine benchmark_1yr"
;-----------------------------------------------------------------------

pro Make_Pdf, Dir

   ; External functions
   FORWARD_FUNCTION MFindFile, Replace_Token

   ; Default directory
   if ( N_Elements( Dir ) ne 1 ) then Dir = 'output/'

   ; Get a file listing of all PostScript files in the output directory
   List = MFindFile( Dir + '*.ps' )

   ; Loop over files
   for F = 0L, N_Elements( List )-1L do begin

      ; PostScript file name
      PsFile  = List[F]

      ; PDF file name
      PdfFile = Replace_Token( PsFile, '.ps', '.pdf', Delim='' )

      ; Call PS2PDF to create a PDF from each PS file
      print, 'Creating ', PdfFile
      Cmd = 'ps2pdf -dAutoRotatePages=/None ' + PsFile + ' ' + PdfFile
      Spawn, Cmd

   endfor
end
