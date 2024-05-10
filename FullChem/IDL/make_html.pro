;-----------------------------------------------------------------------
;+
; NAME:
;        MAKE_HTML
;
; PURPOSE:
;        Creates a HTML file with a clickable list of all the PDF 
;        files from a GEOS-Chem 1-year benchmark simulation.
;
; CATEGORY:
;        GEOS-CHEM Benchmarking
;
; CALLING SEQUENCE:
;        MAKE_HTML, DIR [, Keywords ]
;
; INPUTS:
;        DIR -> Directory where the PDF files reside.
;
; KEYWORD PARAMETERS:
;        OUTFILENAME -> Name of the HTML file that will be
;             generated.  Default is DIR + "index.html".
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        External Subroutines Required:
;        ==============================
;        ADD_SEPARATOR    (function)
;        EXTRACT_FILENAME (function)
;        EXTRACT_PATH     (function)
;        MFINDFILE        (function)
;
; REQUIREMENTS:
;        Uses routines from the GAMAP package.
;
; NOTES:
;        None
;
; EXAMPLES:
;        MAKE_HTML, '/as2/pub/ftp/pub/geos-chem/1yr_benchmarks/' + $
;                   'v8-01-01/geos5/2005/Run0/output/pdf'
;
;             ; Creates a HTML file which lists files
;             ; in the given directory.
;
; MODIFICATION HISTORY:
;        bmy, 21 Apr 2008: VERSION 1.01
;
;-
; Copyright (C) 2008, 
; Bob Yantosca and Philippe Le Sager, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to yantosca@seas.harvard.edu
; or plesager@seas.harvard.edu with subject "IDL routine benchmark_1yr"
;-----------------------------------------------------------------------

pro Make_Html, Dir, OutFileName=OutFileName
   
   ;=====================================================================
   ; Initialization
   ;=====================================================================

   ; External functions
   FORWARD_FUNCTION Add_Separator, Extract_FileName, Extract_Path, MFindFile
   
   ; Input directory
   if ( N_Elements( Dir ) eq 0 ) then Message, 'Must pass DIR!'

   ; Make sure DIR ends with a slash
   Dir = Add_Separator( Dir )

   ; Output file name
   if ( N_Elements( OutFileName ) eq 0 ) $
      then OutFileName = Dir + 'index.html'
   
   ; Get a file listing of all PostScript files in the output directory
   List = MFindFile( Dir + '*.pdf' )

   ; Exit w/ error msg
   if ( N_Elements( List ) eq 0 ) then begin
      Message, 'No PDF files found in ' + StrTrim( Dir, 2 ), /Info
      return
   endif

   ;=====================================================================
   ; Write the HTML file for each of the plots:
   ;=====================================================================

   ; Open output file
   Open_File, OutFileName, Ilun, /Write, /Get_LUN

   ; Print HTML header
   PrintF, Ilun, '<html>'
   PrintF, Ilun, '<head>'
   PrintF, Ilun, '<title></title>'
   PrintF, Ilun, '<body bgcolor="#ffffff">'
   PrintF, Ilun, '<h2>Contents of ' + Extract_Path( List[0] ) + '</h2>'
   PrintF, Ilun, ''
   PrintF, Ilun, '<ul>'
   
   ; Write each file as a list element
   for F = 0L, N_Elements( List )-1L do begin
      
      ; Extract the file name from the path name
      File = Extract_FileName( List[F] )

      ; Create the link as a list item
      PrintF, Ilun, '<li><a href="' + File + '">' + File + '</a></li>'
   endfor

   ; Print HTML footer
   PrintF, Ilun, '</ul>'
   PrintF, Ilun, '</body>'
   PrintF, Ilun, '</html>'

   ; Close the file
   Close, Ilun
   Free_LUN, Ilun
   
end
