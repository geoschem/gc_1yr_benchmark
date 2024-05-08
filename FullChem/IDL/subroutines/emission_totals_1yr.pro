;-----------------------------------------------------------------------
;+
; NAME:
;        EMISSION_TOTALS_1YR
;
; PURPOSE:
;        Prints totals of GEOS-CHEM emission species for three different
;        model versions.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        EMISSION_TOTALS_1YR [ , Keywords ]
;
; INPUTS:
;        None
;
; KEYWORD PARAMETERS:
;        PREF1 -> Name of a directory containing model output from Version 1.
;
;        PREF2 -> Name of a directory containing model output from Version 2.
;
;        PREF3 -> Name of a directory containing model output from Version 3.
;
;        VERSIONS  -> A 3-element vector containing the version
;             numbers for the "old" and "new" GEOS-Chem model
;             versions.
;
;        OUTFILENAME -> Name of the text file where emission totals
;             and differences will be sent.  Default is "emissions.txt".
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        Internal Subroutines:
;        ==================================
;        GetVersionInfo (function)
;        WriteHeader
;        WriteTracers
;
;        External Subroutines Required:
;        ==================================
;        CTM_SUM_EMISSIONS 
;        UNDEFINE
; 
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;
; NOTES:
;        (1) Assumes that model output contains the
;            following GEOS-CHEM diagnostic categories:
;            (a) ND11 ("ACETSRCE")   (e) ND34 ("BIOFSRCE")
;            (b) ND28 ("BIOBSRCE")   (f) ND36 ("ANTHSRCE")
;            (c) ND29 ("CO--SRCE")   (g) ND46 ("BIOGSRCE")
;            (d) ND32 ("NOX-AC-$", "NOX-AN-$", "NOX-BIOB", 
;                      "NOX-FERT", "NOX-LI-$", "NOX-SOIL")
;            (h) ND13 ("DMS-BIOG", "SO2-AC-$", "SO2-AN-$",
;                      "SO2-EV-$", "SO2-NV-$", "SO4-AN-$",
;                      "NH3-ANTH", "NH3-BIOB", "NH3-BIOG",
;                      "NH3-NATU", "SO2-SHIP")
;            (i) ND06 ( )
;            (j) ND07 ("DUSTSRCE")
;            (k) ND08 ("SALTSRCE")
;
; EXAMPLE:
;        EMISSION_TOTALS_1YR, Pref1, Pref2, Pref3, Versions, $
;                                OUTFILENAME='emissions.txt'
;
;             ; Prints emissions totals for versions 1-3
;                           
; MODIFICATION HISTORY:
;        mps, 02 Jan 2014: VERSION 1.01
;                          - Initial version based on fullchem_emission.pro
;                            from GAMAP v2-16
;        mps, 23 Jan 2014: - Now report NH3 emissions in Tg N
;        bmy, 12 Nov 2014: - Now report anthro + biofuel totals because
;                            HEMCO does not separate these sectors
;        mps, 27 Jan 2017: - Update to allow for comparison of 2 versions,
;                            intead of the default 3 versions
;        bmy, 14 May 2018: - Add lots of CTM_CLEANUP calls to avoid memory
;                            leaks, which can kill an interactive session
;
;-
; Copyright (C) 2001-2014, Bob Yantosca, Harvard University
; This software is provided as is without any warranty
; whatsoever. It may be freely used, copied or distributed
; for non-commercial purposes. This copyright notice must be
; kept with any copy of this software. If this software shall
; be used commercially or sold as part of a larger package,
; please contact the author.
; Bugs and comments should be directed to bmy@io.harvard.edu
; with subject "IDL routine fullchem_emissions"
;-----------------------------------------------------------------------


pro WriteHeader, Ilun, Title, Version1, Version2, Version3, Advance=Advance

   ;====================================================================
   ; Internal subroutine WriteHeader writes a header for each emissions
   ; category with a top title and also the version string information.
   ;====================================================================  

   ; Now use wider format string (bmy, 1/10/11)

   ; Write some spacers -- if /ADVANCE is set
   if ( Keyword_Set( Advance ) ) then begin
      PrintF, Ilun
      PrintF, Ilun
   endif

   PrintF, Ilun, Title
   
   if ( Version3 eq 'None' ) then begin
      
      Format = '('' Tracer    '',a11,3x,a11,3x,a11,'' - '',a)'
      
      PrintF, Ilun, Version1, Version2, Version1, Version2, Format=Format
      PrintF, Ilun, '==============================================================='

      Print, Version1, Version2, Format=Format
   endif else begin
      
      Format = '('' Tracer  '',a20,2x,a20,2x,a20)'

      PrintF, Ilun, Version1, Version2, Version3, Format=Format
      PrintF, Ilun, '============================================================================'
      
   endelse
      
   ; Return to main program
   return
end
 
;-----------------------------------------------------------------------------
 
pro WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit

   ;====================================================================
   ; Internal subroutine WriteTracers writes tracer sums and
   ; differences for a given emissions category.
   ;====================================================================  

   if ( Total3 eq -999 ) then begin

      ; Write totals & difference to OUTFILENAME
      PrintF, Ilun, Name, Total1, Total2, Total2-Total1, Unit, $
                    Format='(a8,2(1x,f13.6),3x,f13.6,3x,a6)'

   endif else begin

      ; Write totals to OUTFILENAME
      PrintF, Ilun, Name, Total1, Total2,  Total3, Unit, $
                    Format='(a8,1x,3(f20.6,2x),a6)'
   endelse
   
   ; Return to main program
   return
end
 
;-----------------------------------------------------------------------------
  
pro Emission_Totals_1yr, Pref1, Pref2, Pref3, Versions,   $
                         OutFileName=OutFileName

   ;====================================================================
   ; Initialization
   ;====================================================================
 
   ; Pass external functions
   FORWARD_FUNCTION GetVersionInfo

   ; Number of model versions to compare
   nVersions = N_Elements( Versions )
   
   ; Get version information from input & output files
   Version1  = Versions[0]
   Version2  = Versions[1]
   if ( nVersions eq 3 ) then begin
   Version3  = Versions[2]
   endif else begin
   Version3  = 'None'
   endelse
      
   ; Default output file neame
   if ( N_Elements( OutFileName ) ne 1 ) $
      then OutFileName = 'emissions.txt'
 
   ;====================================================================
   ; Print anthro + biofuel totals from old & new versions
   ;====================================================================

   ; Open output file
   ; BMY update, set ILUN=1 so that it won't be closed by CTM_CLEANUP
   Ilun = 1
   Open_File, OutFileName, Ilun, /Write

   ; Write anthro header to file
   WriteHeader, Ilun, 'ANTHROPOGENIC + BIOFUEL', Version1, Version2, Version3
   Flush, Ilun

   ; ==== Anthro + Biofuel NO ====
   print, 'Anthro + Biofuel NO'
   Tracer = [ 1 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0
   
   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+'HEMCO_diagnostics.2016'+mn+'010000.nc'
      File2=pref2+'HEMCO_diagnostics.2016'+mn+'010000.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+'HEMCO_diagnostics.2016'+mn+'010000.nc'
      endif
      
      CTM_Sum_Emissions, 'NO-AN-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1_A

      CTM_Sum_Emissions, 'NO-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1_B

      CTM_Sum_Emissions, 'NO-AN-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2_A

      CTM_Sum_Emissions, 'NO-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2_B

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NO-AN-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3_A

      CTM_Sum_Emissions, 'NO-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3_B
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + ( Emiss1_A.Sum + Emiss1_B.Sum )
      Total2 = Total2 + ( Emiss2_A.Sum + Emiss2_B.Sum )
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + ( Emiss3_A.Sum + Emiss3_B.Sum )
      endif else begin
      Total3 = -999
      endelse
      
      if ( nVersions eq 2 ) then begin
         Name   = Emiss2_A.Name
         Unit   = Emiss2_A.Unit
      endif else begin
         Name   = Emiss3_A.Name
         Unit   = Emiss3_A.Unit
      endelse
      
      UnDefine, Emiss1_A
      UnDefine, Emiss1_B
      UnDefine, Emiss2_A
      UnDefine, Emiss2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3_A
      UnDefine, Emiss3_B
      endif
      
   endfor

   ; Write anthro+biofuel NO totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun
      
   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ; ==== Anthro + Biofuel CO ====
   print, 'Anthro + biofuel CO'

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn = '0'+mn     
      endif

      ; Filenames
      File1 = pref1+mn+'01.nc'
      File2 = pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3 = pref3+mn+'01.nc'
      endif
      
      ;%%%%% Anthro + Biofuel %%%%%
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File1, Tracer=1, /Cum_Only, Result=Emiss1_A

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File1, Tracer=3, /Cum_Only, Result=Emiss1_B

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File2, Tracer=1, /Cum_Only, Result=Emiss2_A

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File2, Tracer=3, /Cum_Only, Result=Emiss2_B

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File3, Tracer=1, /Cum_Only, Result=Emiss3_A

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File3, Tracer=3, /Cum_Only, Result=Emiss3_B
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + ( Emiss1_A.Sum + Emiss1_B.Sum )
      Total2 = Total2 + ( Emiss2_A.Sum + Emiss2_B.Sum )
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + ( Emiss3_A.Sum + Emiss3_B.Sum )
      endif else begin
      Total3 = -999
      endelse
      
      Name   = 'CO'
      if ( nVersions eq 2 ) then begin
         ;Name  = Emiss2_A.Name
         Unit   = Emiss2_A.Unit
      endif else begin
         ;Name  = Emiss3_A.Name
         Unit   = Emiss3_A.Unit
      endelse
      
      UnDefine, Emiss1_A
      UnDefine, Emiss1_B
      UnDefine, Emiss2_A
      UnDefine, Emiss2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3_A
      UnDefine, Emiss3_B
      endif
      
   endfor

   ; Write anthro+biofuel CO totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ; ==== Regular SMVGEAR tracers ====
   print, 'Most emission species'
   Tracer = [ 5, 9, 10, 11, 18, 19, 20, 21, 67, 68, 69, 94 ]
 
   ; Loop over tracers
   for n = 0, n_elements(Tracer)-1L do begin

      ; Initialize
      Total1 = 0.0d0
      Total2 = 0.0d0
      Total3 = 0.0d0

      ; Loop over months
      for i = 0, 11 do begin

         ; Free memory
         ctm_cleanup, /no_gc

         mn = strtrim(String(fix(i+1)), 2)
         if (strlen(mn) eq 1) then begin         
            mn='0'+mn     
         endif
      
         ; Filenames
         File1=pref1+mn+'01.nc'
         File2=pref2+mn+'01.nc'
         if ( nVersions eq 3 ) then begin
         File3=pref3+mn+'01.nc'
         endif
         
         CTM_Sum_Emissions, 'ANTHSRCE', $
            File=File1, Tracer=Tracer[n], /Cum_Only, Result=Emiss1_A

         CTM_Sum_Emissions, 'BIOFSRCE', $
            File=File1, Tracer=Tracer[n], /Cum_Only, Result=Emiss1_B
      
         CTM_Sum_Emissions, 'ANTHSRCE', $
            File=File2, Tracer=Tracer[n], /Cum_Only, Result=Emiss2_A

         CTM_Sum_Emissions, 'BIOFSRCE', $
            File=File2, Tracer=Tracer[n], /Cum_Only, Result=Emiss2_B

         if ( nVersions eq 3 ) then begin
         CTM_Sum_Emissions, 'ANTHSRCE', $
            File=File3, Tracer=Tracer[n], /Cum_Only, Result=Emiss3_A

         CTM_Sum_Emissions, 'BIOFSRCE', $
            File=File3, Tracer=Tracer[n], /Cum_Only, Result=Emiss3_B
         endif
         
         ; Calculate annual totals
         Total1 = Total1 + ( Emiss1_A.Sum + Emiss1_B.Sum )
         Total2 = Total2 + ( Emiss2_A.Sum + Emiss2_B.Sum )
         if ( nVersions eq 3 ) then begin
         Total3 = Total3 + ( Emiss3_A.Sum + Emiss3_B.Sum )
         endif else begin
         Total3 = -999
         endelse
         
         if ( nVersions eq 2 ) then begin
            Name   = Emiss2_A.Name
            Unit   = Emiss2_A.Unit
         endif else begin
            Name   = Emiss3_A.Name
            Unit   = Emiss3_A.Unit
         endelse
      
         UnDefine, Emiss1_A
         UnDefine, Emiss1_B
         UnDefine, Emiss2_A
         UnDefine, Emiss2_B
         if ( nVersions eq 3 ) then begin
         UnDefine, Emiss3_A
         UnDefine, Emiss3_B
         endif
         
      endfor

      ; Write anthro+biofuel totals to file
      WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
      Flush, Ilun

      UnDefine, Total1
      UnDefine, Total2
      UnDefine, Total3

   endfor

   ; ==== Anthro + Biofuel SO2 ====
StartHere:
   print, 'Anthro + biofuel SO2'
   Tracer = [ 26 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'SO2-AN-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO2-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1_B, /No_Sec, /Kg
   
      CTM_Sum_Emissions, 'SO2-AN-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO2-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2_B, /No_Sec, /Kg
   
      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'SO2-AN-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO2-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3_B, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + ( Emiss1_A.Sum + Emiss1_B.Sum )
      Total2 = Total2 + ( Emiss2_A.Sum + Emiss2_B.Sum )
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + ( Emiss3_A.Sum + Emiss3_B.Sum )
      endif else begin
      Total3 = -999
      endelse
      
      if ( nVersions eq 2 ) then begin
         Name   = Emiss2_A.Name
         Unit   = Emiss2_A.Unit
      endif else begin
         Name   = Emiss3_A.Name
         Unit   = Emiss3_A.Unit
      endelse
      
      UnDefine, Emiss1_A
      UnDefine, Emiss1_B
      UnDefine, Emiss2_A
      UnDefine, Emiss2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3_A
      UnDefine, Emiss3_B
      endif

   endfor

   ; Write anthro+biofuel SO2 totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ; ==== Anthro + Biofuel SO4 ====
   print, 'Anthro + biofuel SO4'
   Tracer = [ 27 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'SO4-AN-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO4-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1_B, /No_Sec, /Kg
    
      CTM_Sum_Emissions, 'SO4-AN-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO4-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2_B, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'SO4-AN-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO4-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3_B, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + ( Emiss1_A.Sum ) ;+ Emiss1_B.Sum )
      Total2 = Total2 + ( Emiss2_A.Sum ) ;+ Emiss2_B.Sum )
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + ( Emiss3_A.Sum ) ;+ Emiss3_B.Sum )
      endif else begin
      Total3 = -999
      endelse
         
      if ( nVersions eq 2 ) then begin
         Name   = Emiss2_A.Name
         Unit   = Emiss2_A.Unit
      endif else begin
         Name   = Emiss3_A.Name
         Unit   = Emiss3_A.Unit
      endelse
      
      UnDefine, Emiss1_A
      UnDefine, Emiss1_B
      UnDefine, Emiss2_A
      UnDefine, Emiss2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3_A
      UnDefine, Emiss3_B
      endif

   endfor

   ; Write anthro+biofuel SO4 totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ; ==== Anthro + Biofuel NH3 ====
   print, 'Anthro + Biofuel NH3'
   Tracer = [ 30 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'NH3-ANTH', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1_A, /No_Sec, /Kg
    
      CTM_Sum_Emissions, 'NH3-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1_B, /No_Sec, /Kg
    
      CTM_Sum_Emissions, 'NH3-ANTH', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'NH3-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2_B, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NH3-ANTH', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'NH3-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3_B, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + ( Emiss1_A.Sum + Emiss1_B.Sum )
      Total2 = Total2 + ( Emiss2_A.Sum + Emiss2_B.Sum )
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + ( Emiss3_A.Sum + Emiss3_B.Sum )
      endif else begin
      Total3 = -999
      endelse
      
      if ( nVersions eq 2 ) then begin
         Name   = Emiss2_A.Name
         Unit   = Emiss2_A.Unit
      endif else begin
         Name   = Emiss3_A.Name
         Unit   = Emiss3_A.Unit
      endelse
      
      UnDefine, Emiss1_A
      UnDefine, Emiss1_B
      UnDefine, Emiss2_A
      UnDefine, Emiss2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3_A
      UnDefine, Emiss3_B
      endif

   endfor

   ; Convert from Tg NH3 to Tg N
   Total1 = Total1 * 14d0/17d0
   Total2 = Total2 * 14d0/17d0
   if ( nVersions eq 3 ) then begin
   Total3 = Total3 * 14d0/17d0
   endif
   Unit   = 'Tg N'

   ; Write anthro+biofuel NH3 totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ;====================================================================
   ; Print biomass burning totals from old & new versions
   ;====================================================================

   ; Write biomass burning header to file
   WriteHeader, Ilun, 'BIOMASS', Version1, Version2, Version3, /Advance
   Flush, Ilun

   ; ==== Biomass NO ====
   print, 'Biomass NO'
   Tracer = [ 1 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'NO-BIOB', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1

      CTM_Sum_Emissions, 'NO-BIOB', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NO-BIOB', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + Emiss1.Sum
      Total2 = Total2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + Emiss3.Sum
      endif else begin
      Total3 = -999
      endelse
      
      if ( nVersions eq 2 ) then begin
         Name   = Emiss2.Name
         Unit   = Emiss2.Unit
      endif else begin
         Name   = Emiss3.Name
         Unit   = Emiss3.Unit
      endelse

      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif
      
   endfor

   ; Write biomass NO totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ; ==== Biomass CO ====
   ;Tracer = [ 4 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File1, Tracer=2, /Cum_Only, Result=Emiss1

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File2, Tracer=2, /Cum_Only, Result=Emiss2

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File3, Tracer=2, /Cum_Only, Result=Emiss3
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + Emiss1.Sum
      Total2 = Total2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + Emiss3.Sum
      endif else begin
      Total3 = -999
      endelse
      
      Name = 'CO'
      if ( nVersions eq 2 ) then begin
      Unit = Emiss2.Unit
      endif else begin
      Unit = Emiss3.Unit
      endelse
      
      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif
      
   endfor

   ; Write biomass CO totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ; ==== Regular SMVGEAR Tracers ====
   Tracer = [ 5, 9, 10, 11, 18, 19, 20, 21, 26, 30, 34, 35, $
              67, 68, 69, 70, 94, 95 ]

   ; Loop over tracers
   for n = 0, n_elements(Tracer)-1L do begin

      ; Initialize
      Total1 = 0.0d0
      Total2 = 0.0d0
      Total3 = 0.0d0

      ; Loop over months
      for i = 0, 11 do begin

         ; Free memory
         ctm_cleanup, /no_gc
      
         mn = strtrim(String(fix(i+1)), 2)
         if (strlen(mn) eq 1) then begin         
            mn='0'+mn     
         endif
      
         ; Filenames
         File1=pref1+mn+'01.nc'
         File2=pref2+mn+'01.nc'
         if ( nVersions eq 3 ) then begin
         File3=pref3+mn+'01.nc'
         endif
         
         CTM_Sum_Emissions, 'BIOBSRCE', $
            File=File1, Tracer=Tracer[n], /Cum_Only, Result=Emiss1
       
         CTM_Sum_Emissions, 'BIOBSRCE', $
            File=File2, Tracer=Tracer[n], /Cum_Only, Result=Emiss2
      
         if ( nVersions eq 3 ) then begin
         CTM_Sum_Emissions, 'BIOBSRCE', $
            File=File3, Tracer=Tracer[n], /Cum_Only, Result=Emiss3
         endif
         
         ; Calculate annual totals
         Total1 = Total1 + Emiss1.Sum
         Total2 = Total2 + Emiss2.Sum
         if ( nVersions eq 3 ) then begin
         Total3 = Total3 + Emiss3.Sum
         endif else begin
         Total3 = -999
         endelse
         
         if ( nVersions eq 2 ) then begin
            Name   = Emiss2.Name
            Unit   = Emiss2.Unit
         endif else begin
            Name   = Emiss3.Name
            Unit   = Emiss3.Unit
         endelse
      
         UnDefine, Emiss1
         UnDefine, Emiss2
         if ( nVersions eq 3 ) then begin
         UnDefine, Emiss3
         endif
         
      endfor

      ; Write biomass totals to file
      WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
      Flush, Ilun

      UnDefine, Total1
      UnDefine, Total2
      UnDefine, Total3

   endfor

   ; ==== Biomass SO2 ==== 
   Tracer = [ 26 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'SO2-BIOB', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1, /No_Sec, /Kg
    
      CTM_Sum_Emissions, 'SO2-BIOB', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'SO2-BIOB', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + Emiss1.Sum
      Total2 = Total2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + Emiss3.Sum
      endif else begin
      Total3 = -999
      endelse
      
      if ( nVersions eq 2 ) then begin
         Name   = Emiss2.Name
         Unit   = Emiss2.Unit
      endif else begin
         Name   = Emiss3.Name
         Unit   = Emiss3.Unit
      endelse

      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif

   endfor

   ; Write biomass SO2 totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ; ==== Biomass NH3 ==== 
   Tracer = [ 30 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'NH3-BIOB', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1, /No_Sec, /Kg
    
      CTM_Sum_Emissions, 'NH3-BIOB', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NH3-BIOB', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + Emiss1.Sum
      Total2 = Total2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + Emiss3.Sum
      endif else begin
      Total3 = -999
      endelse

      if ( nVersions eq 2 ) then begin
         Name   = Emiss2.Name
         Unit   = Emiss2.Unit
      endif else begin
         Name   = Emiss3.Name
         Unit   = Emiss3.Unit
      endelse

      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif

   endfor

   ; Convert from Tg NH3 to Tg N
   Total1 = Total1 * 14d0/17d0
   Total2 = Total2 * 14d0/17d0
   if ( nVersions eq 3 ) then begin
   Total3 = Total3 * 14d0/17d0
   endif
   Unit   = 'Tg N'

   ; Write biomass NH3 totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ;====================================================================
   ; Print biogenic and natural totals from old & new versions
   ;==================================================================== 

   ; Write biogenic header to file
   WriteHeader, Ilun, 'BIOGENIC AND NATURAL SOURCES', Version1, Version2, $
                Version3, /Advance
   Flush, Ilun

   ; ==== Regular tracers ====
   Tracer = [  1,  2,  3,  4,  5,  7,  8,  9, 10, 11, 12, 13, $
              14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, $
              27, 28, 29  ]

   ; Loop over tracers
   for n = 0, n_elements(Tracer)-1L do begin

      ; Initialize
      Total1 = 0.0d0
      Total2 = 0.0d0
      Total3 = 0.0d0

      ; Loop over months
      for i = 0, 11 do begin

         ; Free memory
         ctm_cleanup, /no_gc

         mn = strtrim(String(fix(i+1)), 2)
         if (strlen(mn) eq 1) then begin         
            mn='0'+mn     
         endif
      
         ; Filenames
         File1=pref1+mn+'01.nc'
         File2=pref2+mn+'01.nc'
         if ( nVersions eq 3 ) then begin
         File3=pref3+mn+'01.nc'
         endif
         
         CTM_Sum_Emissions, 'BIOGSRCE', $
            File=File1, Tracer=Tracer[n], /Cum_Only, Result=Emiss1
       
         CTM_Sum_Emissions, 'BIOGSRCE', $
            File=File2, Tracer=Tracer[n], /Cum_Only, Result=Emiss2
      
         if ( nVersions eq 3 ) then begin
         CTM_Sum_Emissions, 'BIOGSRCE', $
            File=File3, Tracer=Tracer[n], /Cum_Only, Result=Emiss3
         endif
         
         ; Calculate annual totals
         Total1 = Total1 + Emiss1.Sum
         Total2 = Total2 + Emiss2.Sum
         if ( nVersions eq 3 ) then begin
         Total3 = Total3 + Emiss3.Sum
         endif else begin
         Total3 = -999
         endelse
         
         if ( nVersions eq 2 ) then begin
            Name   = Emiss2.Name
            Unit   = Emiss2.Unit
         endif else begin
            Name   = Emiss3.Name
            Unit   = Emiss3.Unit
         endelse

         UnDefine, Emiss1
         UnDefine, Emiss2
         if ( nVersions eq 3 ) then begin
         UnDefine, Emiss3
         endif
      
      endfor

      ; Write biogenic totals to file
      WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
      Flush, Ilun

      UnDefine, Total1
      UnDefine, Total2
      UnDefine, Total3

   endfor

   ; ==== Biogenic DMS ====
   Tracer = [ 25 ]
   
   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'DMS-BIOG', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'DMS-BIOG', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2, /No_Sec, /Kg
 
      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'DMS-BIOG', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + Emiss1.Sum
      Total2 = Total2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + Emiss3.Sum
      endif else begin
      Total3 = -999
      endelse
      
      if ( nVersions eq 2 ) then begin
         Name   = Emiss2.Name
         Unit   = Emiss2.Unit
      endif else begin
         Name   = Emiss3.Name
         Unit   = Emiss3.Unit
      endelse

      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif

   endfor

   ; Write biogenic DMS totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ; ==== Natural Source NH3 ==== 
   Tracer = [ 30 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc
   
      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      ; Initialize
      Emiss1 = 0.0d0
      Emiss2 = 0.0d0
      Emiss3 = 0.0d0

      CTM_Sum_Emissions, 'NH3-NATU', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'NH3-NATU', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NH3-NATU', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + Emiss1.Sum
      Total2 = Total2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + Emiss3.Sum
      endif else begin
      Total3 = -999
      endelse
      
      if ( nVersions eq 2 ) then begin
         Name   = Emiss2.Name
         Unit   = Emiss2.Unit
      endif else begin
         Name   = Emiss3.Name
         Unit   = Emiss3.Unit
      endelse

      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif
      
   endfor

   ; Convert from Tg NH3 to Tg N
   Total1 = Total1 * 14d0/17d0
   Total2 = Total2 * 14d0/17d0
   if ( nVersions eq 3 ) then begin
   Total3 = Total3 * 14d0/17d0
   endif
   Unit   = 'Tg N'

   ; Write natural NH3 totals to file
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ;====================================================================
   ; Print ACETONE sources from old & new versions
   ;==================================================================== 

   ; Write acetone sources header to file
   WriteHeader, Ilun, 'ACETONE SOURCES', Version1, Version2, Version3, /Advance
   Flush, Ilun

   ; Tracer numbers
   Tracer = [ 1, 2, 3, 4 ]

   ; Loop over tracers
   for n = 0, n_elements(Tracer)-1L do begin

      ; Initialize
      Total1 = 0.0d0
      Total2 = 0.0d0
      Total3 = 0.0d0

      ; Loop over months
      for i = 0, 11 do begin

         ; Free memory
         ctm_cleanup, /no_gc
      
         mn = strtrim(String(fix(i+1)), 2)
         if (strlen(mn) eq 1) then begin         
            mn='0'+mn     
         endif
      
         ; Filenames
         File1=pref1+mn+'01.nc'
         File2=pref2+mn+'01.nc'
         if ( nVersions eq 3 ) then begin
         File3=pref3+mn+'01.nc'
         endif
         
         CTM_Sum_Emissions, 'ACETSRCE', $
            File=File1, Tracer=Tracer[n], /Cum_Only, Result=Emiss1
         
         CTM_Sum_Emissions, 'ACETSRCE', $
            File=File2, Tracer=Tracer[n], /Cum_Only, Result=Emiss2

         if ( nVersions eq 3 ) then begin
         CTM_Sum_Emissions, 'ACETSRCE', $
            File=File3, Tracer=Tracer[n], /Cum_Only, Result=Emiss3
         endif
         
         ; Calculate annual totals
         Total1 = Total1 + Emiss1.Sum
         Total2 = Total2 + Emiss2.Sum
         if ( nVersions eq 3 ) then begin
         Total3 = Total3 + Emiss3.Sum
         endif else begin
         Total3 = -999
         endelse
      
         if ( nVersions eq 2 ) then begin
            Name   = Emiss2.Name
            Unit   = Emiss2.Unit
         endif else begin
            Name   = Emiss3.Name
            Unit   = Emiss3.Unit
         endelse

         UnDefine, Emiss1
         UnDefine, Emiss2
         if ( nVersions eq 3 ) then begin
         UnDefine, Emiss3
         endif

      endfor

      ; Write acetone source totals to file
      WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
      Flush, Ilun

      UnDefine, Total1
      UnDefine, Total2
      UnDefine, Total3

   endfor

   ;====================================================================
   ; Print ACETONE sinks from old & new versions
   ;
   ;%%%% NOTE: HEMCO NEEDS TO BE MODIFIED TO SAVE THIS %%%%
   ;==================================================================== 

;   ; Write acetone sinks header to file
;   WriteHeader, Ilun, 'ACETONE SINKS', Version1, Version2, Version3, /Advance
;
;   Tracer = [ 5 ]
;
;   ; Initialize
;   Total1 = 0.0d0
;   Total2 = 0.0d0
;   Total3 = 0.0d0
;
;   ; Loop over months
;   for i = 0, 11 do begin
;
;      mn = strtrim(String(fix(i+1)), 2)
;      if (strlen(mn) eq 1) then begin         
;         mn='0'+mn     
;      endif
;
;      ; Filenames
;      File1=pref1+mn+'01.nc'
;      File2=pref2+mn+'01.nc'
;      if ( nVersions eq 3 ) then begin
;      File3=pref3+mn+'01.nc'
;      endif
;
;      CTM_Sum_Emissions, 'ACETSRCE', $
;         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1
;      
;      CTM_Sum_Emissions, 'ACETSRCE', $
;         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2
;
;      if ( nVersions eq 3 ) then begin
;      CTM_Sum_Emissions, 'ACETSRCE', $
;         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3
;      endif
;
;      ; Calculate annual totals
;      Total1 = Total1 + Emiss1.Sum
;      Total2 = Total2 + Emiss2.Sum
;      if ( nVersions eq 3 ) then begin
;      Total3 = Total3 + Emiss3.Sum
;      endif else begin
;      Total3 = -999
;      endelse
;      
;      if ( nVersions eq 2 ) then begin
;         Name   = Emiss2.Name
;         Unit   = Emiss2.Unit
;      endif else begin
;         Name   = Emiss3.Name
;         Unit   = Emiss3.Unit
;      endelse
;
;      UnDefine, Emiss1
;      UnDefine, Emiss2
;      if ( nVersions eq 3 ) then begin
;      UnDefine, Emiss3
;      endif
;
;   endfor
;
;   ; Write acetone sink totals to file
;   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
; 
;   UnDefine, Total1
;   UnDefine, Total2
;   UnDefine, Total3
   
   ;====================================================================
   ; Print CO source totals from old & new versions
   ;==================================================================== 

   ; Write CO source header to file
   WriteHeader, Ilun, 'CO SOURCES', Version1, Version2, Version3, /Advance
   Flush, Ilun

   ; Tracer numbers
   Tracer = [ 1, 2, 3, 4, 5 ]

   ; Initialize
   AnthTotal1 = 0.0d0
   AnthTotal2 = 0.0d0
   AnthTotal3 = 0.0d0
   BiomTotal1 = 0.0d0
   BiomTotal2 = 0.0d0
   BiomTotal3 = 0.0d0
   MonoTotal1 = 0.0d0
   MonoTotal2 = 0.0d0
   MonoTotal3 = 0.0d0
   ShipTotal1 = 0.0d0
   ShipTotal2 = 0.0d0
   ShipTotal3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc
      
      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn = '0'+mn     
      endif
      
      ; Filenames
      File1 = pref1+mn+'01.nc'
      File2 = pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3 = pref3+mn+'01.nc'
      endif
      
      ;%%%%% Anthro + Biofuel %%%%%
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File1, Tracer=1, /Cum_Only, Result=Emiss1_A

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File1, Tracer=3, /Cum_Only, Result=Emiss1_B
         
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File2, Tracer=1, /Cum_Only, Result=Emiss2_A

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File2, Tracer=3, /Cum_Only, Result=Emiss2_B

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File3, Tracer=1, /Cum_Only, Result=Emiss3_A

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File3, Tracer=3, /Cum_Only, Result=Emiss3_B
      endif
      
      ; Calculate annual totals
      AnthTotal1 = AnthTotal1 + ( Emiss1_A.Sum + Emiss1_B.Sum )
      AnthTotal2 = AnthTotal2 + ( Emiss2_A.Sum + Emiss2_B.Sum )
      if ( nVersions eq 3 ) then begin
      AnthTotal3 = AnthTotal3 + ( Emiss3_A.Sum + Emiss3_B.Sum )
      endif else begin
      AnthTotal3 = -999
      endelse
      
      AnthName = 'COan+bf'
      if ( nVersions eq 2 ) then begin
         AnthUnit = Emiss2_A.Unit
      endif else begin
         AnthUnit = Emiss3_A.Unit
      endelse
      
      UnDefine, Emiss1_A
      UnDefine, Emiss1_B
      UnDefine, Emiss2_A
      UnDefine, Emiss2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3_A
      UnDefine, Emiss3_B
      endif
      
      ;%%%%% Biomass %%%%%
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File1, Tracer=2, /Cum_Only, Result=Emiss1

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File2, Tracer=2, /Cum_Only, Result=Emiss2

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File3, Tracer=2, /Cum_Only, Result=Emiss3
      endif
      
      ; Calculate annual totals
      BiomTotal1 = BiomTotal1 + Emiss1.Sum
      BiomTotal2 = BiomTotal2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      BiomTotal3 = BiomTotal3 + Emiss3.Sum
      endif else begin
      BiomTotal3 = -999
      endelse
      
      BiomName = 'CObb'
      if ( nVersions eq 2 ) then begin
         BiomUnit = Emiss2.Unit
      endif else begin
         BiomUnit = Emiss3.Unit
      endelse
      
      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif
      
      ;%%%%% CO from monoterpenes %%%%%
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File1, Tracer=5, /Cum_Only, Result=Emiss1

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File2, Tracer=5, /Cum_Only, Result=Emiss2

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File3, Tracer=5, /Cum_Only, Result=Emiss3
      endif
      
      ; Calculate annual totals
      MonoTotal1 = MonoTotal1 + Emiss1.Sum
      MonoTotal2 = MonoTotal2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      MonoTotal3 = MonoTotal3 + Emiss3.Sum
      endif else begin
      MonoTotal3 = -999
      endelse
      
      MonoName = 'COmono'
      if ( nVersions eq 2 ) then begin
         MonoUnit = Emiss2.Unit
      endif else begin
         MonoUnit = Emiss3.Unit
      endelse
      
      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif

      ;%%%%% CO from ships %%%%%
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File1, Tracer=6, /Cum_Only, Result=Emiss1

      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File2, Tracer=6, /Cum_Only, Result=Emiss2

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'CO--SRCE', $
         File=File3, Tracer=6, /Cum_Only, Result=Emiss3
      endif
      
      ; Calculate annual totals
      ShipTotal1 = ShipTotal1 + Emiss1.Sum
      ShipTotal2 = ShipTotal2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      ShipTotal3 = ShipTotal3 + Emiss3.Sum
      endif else begin
      ShipTotal3 = -999
      endelse
      
      ShipName = 'COship'
      if ( nVersions eq 2 ) then begin
         ShipUnit = Emiss2.Unit
      endif else begin
         ShipUnit = Emiss3.Unit
      endelse
      
      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif

   endfor

   ; Write CO source totals to file
   WriteTracers, Ilun, AnthName, AnthTotal1, AnthTotal2, AnthTotal3, AnthUnit
   WriteTracers, Ilun, BiomName, BiomTotal1, BiomTotal2, BiomTotal3, BiomUnit
   WriteTracers, Ilun, MonoName, MonoTotal1, MonoTotal2, MonoTotal3, MonoUnit
   WriteTracers, Ilun, ShipName, ShipTotal1, ShipTotal2, ShipTotal3, ShipUnit
   Flush, Ilun
   
   UnDefine, AnthTotal1
   UnDefine, AnthTotal2
   UnDefine, AnthTotal3
   UnDefine, BiomTotal1
   UnDefine, BiomTotal2
   UnDefine, BiomTotal3
   UnDefine, MonoTotal1
   UnDefine, MonoTotal2
   UnDefine, MonoTotal3

   ;====================================================================
   ; Print NOx source totals from old & new versions
   ;
   ; NOTE: NOx source (ND32) diagnostic saves different category
   ; names to the punch file, -- call CTM_SUM_EMISSIONS several times
   ;==================================================================== 
   Tracer = [ 1 ]

   ; Initialize
   AirTotal1  = 0.0d0
   AirTotal2  = 0.0d0
   AirTotal3  = 0.0d0
   AnthTotal1 = 0.0d0
   AnthTotal2 = 0.0d0
   AnthTotal3 = 0.0d0
   BiomTotal1 = 0.0d0
   BiomTotal2 = 0.0d0
   BiomTotal3 = 0.0d0
   BiofTotal1 = 0.0d0
   BiofTotal2 = 0.0d0
   BiofTotal3 = 0.0d0
   FertTotal1 = 0.0d0
   FertTotal2 = 0.0d0
   FertTotal3 = 0.0d0
   LtngTotal1 = 0.0d0
   LtngTotal2 = 0.0d0
   LtngTotal3 = 0.0d0
   SoilTotal1 = 0.0d0
   SoilTotal2 = 0.0d0
   SoilTotal3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      ; ==== Aircraft NOx ====
      CTM_Sum_Emissions, 'NO-AC-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Aircraft1
    
      CTM_Sum_Emissions, 'NO-AC-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Aircraft2

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NO-AC-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Aircraft3
      endif
      
      ; Calculate annual totals
      AirTotal1 = AirTotal1 + Aircraft1.Sum
      AirTotal2 = AirTotal2 + Aircraft2.Sum
      if ( nVersions eq 3 ) then begin
      AirTotal3 = AirTotal3 + Aircraft3.Sum
      endif else begin
      AirTotal3 = -999
      endelse
      
      AirName = 'NOac'
      if ( nVersions eq 2 ) then begin
      AirUnit = Aircraft2.Unit
      endif else begin
      AirUnit = Aircraft3.Unit
      endelse
      
      UnDefine, Aircraft1
      UnDefine, Aircraft2
      if ( nVersions eq 3 ) then begin
      UnDefine, Aircraft3
      endif
      
      ; ==== Anthro NOx + Biofuel NOx  ====
      CTM_Sum_Emissions, 'NO-AN-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_A

      CTM_Sum_Emissions, 'NO-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_B
      
      CTM_Sum_Emissions, 'NO-AN-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_A

      CTM_Sum_Emissions, 'NO-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_B

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NO-AN-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_A

      CTM_Sum_Emissions, 'NO-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_B
      endif
      
      ; Calculate annual totals
      AnthTotal1 = AnthTotal1 + ( Anthro1_A.Sum + Anthro1_B.Sum )
      AnthTotal2 = AnthTotal2 + ( Anthro2_A.Sum + Anthro2_B.Sum )
      if ( nVersions eq 3 ) then begin
      AnthTotal3 = AnthTotal3 + ( Anthro3_A.Sum + Anthro3_B.Sum )
      endif else begin
      AnthTotal3 = -999
      endelse
      
      AnthName = 'NOan+bf'
      if ( nVersions eq 2 ) then begin
         AnthUnit = Anthro2_A.Unit
      endif else begin
         AnthUnit = Anthro3_A.Unit
      endelse
      
      UnDefine, Anthro1_A
      UnDefine, Anthro1_B
      UnDefine, Anthro2_A
      UnDefine, Anthro2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Anthro3_A
      UnDefine, Anthro3_B
      endif
      
      ; ==== Biomass NOx ====
      CTM_Sum_Emissions, 'NO-BIOB', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Biomass1
     
      CTM_Sum_Emissions, 'NO-BIOB', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Biomass2
      
      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NO-BIOB', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Biomass3
      endif
      
      ; Calculate annual totals
      BiomTotal1 = BiomTotal1 + Biomass1.Sum
      BiomTotal2 = BiomTotal2 + Biomass2.Sum
      if ( nVersions eq 3 ) then begin
      BiomTotal3 = BiomTotal3 + Biomass3.Sum
      endif else begin
      BiomTotal3 = -999
      endelse

      BiomName = 'NObb'
      if ( nVersions eq 2 ) then begin
         BiomUnit = Biomass2.Unit
      endif else begin
         BiomUnit = Biomass3.Unit
      endelse
      
      UnDefine, Biomass1
      UnDefine, Biomass2
      if ( nVersions eq 3 ) then begin
      UnDefine, Biomass3
      endif
      
      ; ==== Fertilizer NOx ====
      CTM_Sum_Emissions, 'NO-FERT', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Fert1
      
      CTM_Sum_Emissions, 'NO-FERT', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Fert2

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NO-FERT', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Fert3
      endif
      
      ; Calculate annual totals
      FertTotal1 = FertTotal1 + Fert1.Sum
      FertTotal2 = FertTotal2 + Fert2.Sum
      if ( nVersions eq 3 ) then begin
      FertTotal3 = FertTotal3 + Fert3.Sum
      endif else begin
      FertTotal3 = -999
      endelse
      
      FertName = 'NOfe'
      if ( nVersions eq 2 ) then begin
      FertUnit = Fert2.Unit
      endif else begin
      FertUnit = Fert3.Unit
      endelse
      
      UnDefine, Fert1
      UnDefine, Fert2
      if ( nVersions eq 3 ) then begin
      UnDefine, Fert3
      endif
      
      ; ==== Lightning NOx ====
      CTM_Sum_Emissions, 'NO-LI-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Lightning1
      
      CTM_Sum_Emissions, 'NO-LI-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Lightning2

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NO-LI-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Lightning3
      endif
      
      ; Calculate annual totals
      LtngTotal1 = LtngTotal1 + Lightning1.Sum
      LtngTotal2 = LtngTotal2 + Lightning2.Sum
      if ( nVersions eq 3 ) then begin
      LtngTotal3 = LtngTotal3 + Lightning3.Sum
      endif else begin
      LtngTotal3 = -999
      endelse
      
      LtngName = 'NOli'
      if ( nVersions eq 2 ) then begin
         LtngUnit = Lightning2.Unit
      endif else begin
         LtngUnit = Lightning3.Unit
      endelse
         
      UnDefine, Lightning1
      UnDefine, Lightning2
      if ( nVersions eq 3 ) then begin
      UnDefine, Lightning3
      endif 
      
      ; ==== Soil NOx ====
      CTM_Sum_Emissions, 'NO-SOIL', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Soil1
      
      CTM_Sum_Emissions, 'NO-SOIL', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Soil2

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NO-SOIL', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Soil3
      endif
      
      ; Calculate annual totals
      SoilTotal1 = SoilTotal1 + Soil1.Sum
      SoilTotal2 = SoilTotal2 + Soil2.Sum
      if ( nVersions eq 3 ) then begin
      SoilTotal3 = SoilTotal3 + Soil3.Sum
      endif else begin
      SoilTotal3 = -999
      endelse
         
      SoilName = 'NOso'
      if ( nVersions eq 2 ) then begin
         SoilUnit = Soil2.Unit
      endif else begin
         SoilUnit = Soil3.Unit
      endelse
         
      UnDefine, Soil1
      UnDefine, Soil2
      if ( nVersions eq 3 ) then begin
      UnDefine, Soil3
      endif
      
   endfor

   ; Write NOx totals to file
   WriteHeader,  Ilun, 'NOx SOURCES', Version1, Version2, Version3, /Advance
   WriteTracers, Ilun, AirName,  AirTotal1,  AirTotal2,  AirTotal3,  AirUnit
   WriteTracers, Ilun, AnthName, AnthTotal1, AnthTotal2, AnthTotal3, AnthUnit
   WriteTracers, Ilun, BiomName, BiomTotal1, BiomTotal2, BiomTotal3, BiomUnit
   WriteTracers, Ilun, FertName, FertTotal1, FertTotal2, FertTotal3, FertUnit
   WriteTracers, Ilun, LtngName, LtngTotal1, LtngTotal2, LtngTotal3, LtngUnit
   WriteTracers, Ilun, SoilName, SoilTotal1, SoilTotal2, SoilTotal3, SoilUnit
   Flush, Ilun

   UnDefine, AirTotal1
   UnDefine, AirTotal2
   UnDefine, AirTotal3
   UnDefine, AnthTotal1
   UnDefine, AnthTotal2
   UnDefine, AnthTotal3
   UnDefine, BiomTotal1
   UnDefine, BiomTotal2
   UnDefine, BiomTotal3
   UnDefine, BiofTotal1
   UnDefine, BiofTotal2
   UnDefine, BiofTotal3
   UnDefine, FertTotal1
   UnDefine, FertTotal2
   UnDefine, FertTotal3
   UnDefine, LtngTotal1
   UnDefine, LtngTotal2
   UnDefine, LtngTotal3
   UnDefine, SoilTotal1
   UnDefine, SoilTotal2
   UnDefine, SoilTotal3

   ;====================================================================
   ; DMS emissions
   ;====================================================================
   Tracer = [ 25 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'DMS-BIOG', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Emiss1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'DMS-BIOG', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Emiss2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'DMS-BIOG', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Emiss3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + Emiss1.Sum
      Total2 = Total2 + Emiss2.Sum
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + Emiss3.Sum
      endif else begin
      Total3 = -999
      endelse

      Name = 'DMSbg'
      if ( nVersions eq 2 ) then begin
         Unit   = Emiss2.Unit
      endif else begin
         Unit   = Emiss3.Unit
      endelse

      UnDefine, Emiss1
      UnDefine, Emiss2
      if ( nVersions eq 3 ) then begin
      UnDefine, Emiss3
      endif

   endfor

   ; Write DMS totals to file
   WriteHeader,  Ilun, 'DMS SOURCES', Version1,  Version2, Version3, /Advance
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ;====================================================================
   ; SO2 emissions
   ;====================================================================
   Tracer = [ 26 ]

   ; Initialize
   AirTotal1  = 0.0d0
   AirTotal2  = 0.0d0
   AirTotal3  = 0.0d0
   AnthTotal1 = 0.0d0
   AnthTotal2 = 0.0d0
   AnthTotal3 = 0.0d0
   BiomTotal1 = 0.0d0
   BiomTotal2 = 0.0d0
   BiomTotal3 = 0.0d0
   BiofTotal1 = 0.0d0
   BiofTotal2 = 0.0d0
   BiofTotal3 = 0.0d0
   VolTotal1  = 0.0d0
   VolTotal2  = 0.0d0
   VolTotal3  = 0.0d0
   ShipTotal1 = 0.0d0
   ShipTotal2 = 0.0d0
   ShipTotal3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      ; ==== Aircraft SO2 ==== 
      CTM_Sum_Emissions, 'SO2-AC-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Aircraft1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'SO2-AC-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Aircraft2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'SO2-AC-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Aircraft3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      AirTotal1 = AirTotal1 + Aircraft1.Sum
      AirTotal2 = AirTotal2 + Aircraft2.Sum
      if ( nVersions eq 3 ) then begin
      AirTotal3 = AirTotal3 + Aircraft3.Sum
      endif else begin
      AirTotal3 = -999
      endelse
      
      AirName = 'SO2ac'
      if ( nVersions eq 2 ) then begin
         AirUnit = Aircraft2.Unit
      endif else begin
         AirUnit = Aircraft3.Unit
      endelse

      UnDefine, Aircraft1
      UnDefine, Aircraft2
      if ( nVersions eq 3 ) then begin
      UnDefine, Aircraft3
      endif
      
      ; ==== Anthro SO2 + Biofuel SO2  ==== 
      CTM_Sum_Emissions, 'SO2-AN-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO2-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_B, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'SO2-AN-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO2-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_B, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'SO2-AN-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO2-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_B, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      AnthTotal1 = AnthTotal1 + ( Anthro1_A.Sum + Anthro1_B.Sum )
      AnthTotal2 = AnthTotal2 + ( Anthro2_A.Sum + Anthro2_B.Sum )
      if ( nVersions eq 3 ) then begin
      AnthTotal3 = AnthTotal3 + ( Anthro3_A.Sum + Anthro3_B.Sum )
      endif else begin
      AnthTotal3 = -999
      endelse
      
      AnthName = 'SO2an+bf'
      if ( nVersions eq 2 ) then begin
         AnthUnit = Anthro2_A.Unit
      endif else begin
         AnthUnit = Anthro3_A.Unit
      endelse
      
      UnDefine, Anthro1_A
      UnDefine, Anthro1_B
      UnDefine, Anthro2_A
      UnDefine, Anthro2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Anthro3_A
      UnDefine, Anthro3_B
      endif
      
      ; ==== Biomass SO2 ==== 
      CTM_Sum_Emissions, 'SO2-BIOB', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Biomass1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'SO2-BIOB', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Biomass2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'SO2-BIOB', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Biomass3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      BiomTotal1 = BiomTotal1 + Biomass1.Sum
      BiomTotal2 = BiomTotal2 + Biomass2.Sum
      if ( nVersions eq 3 ) then begin
      BiomTotal3 = BiomTotal3 + Biomass3.Sum
      endif else begin
      BiomTotal3 = -999
      endelse
      
      BiomName = 'SO2bb'
      if ( nVersions eq 2 ) then begin
         BiomUnit = Biomass2.Unit
      endif else begin
         BiomUnit = Biomass3.Unit
      endelse
      
      UnDefine, Biomass1
      UnDefine, Biomass2
      if ( nVersions eq 3 ) then begin
      UnDefine, Biomass3
      endif
      ; ==== Volcano SO2 (eruptive + non-eruptive ==== 
      CTM_Sum_Emissions, 'SO2-EV-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Erup1,    /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO2-NV-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=NonErup1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'SO2-EV-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Erup2,    /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO2-NV-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=NonErup2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'SO2-EV-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Erup3,    /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO2-NV-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=NonErup3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      VolTotal1 = VolTotal1 + ( Erup1.Sum + NonErup1.Sum )
      VolTotal2 = VolTotal2 + ( Erup2.Sum + NonErup2.Sum )
      if ( nVersions eq 3 ) then begin
      VolTotal3 = VolTotal3 + ( Erup3.Sum + NonErup3.Sum )
      endif else begin
      VolTotal3 = -999
      endelse
      
      VolName = 'SO2volc'
      if ( nVersions eq 2 ) then begin
         VolUnit = Erup2.Unit
      endif else begin
         VolUnit = Erup3.Unit
      endelse
         
      UnDefine, Erup1
      UnDefine, NonErup1
      UnDefine, Erup2
      UnDefine, NonErup2
      if ( nVersions eq 3 ) then begin
      UnDefine, Erup3
      UnDefine, NonErup3
      endif
      
      ; ==== Ship exhaust SO2 ==== 
      CTM_Sum_Emissions, 'SO2-SHIP', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Ship1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'SO2-SHIP', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Ship2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'SO2-SHIP', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Ship3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      ShipTotal1 = ShipTotal1 + Ship1.Sum
      ShipTotal2 = ShipTotal2 + Ship2.Sum
      if ( nVersions eq 3 ) then begin
      ShipTotal3 = ShipTotal3 + Ship3.Sum
      endif else begin
      ShipTotal3 = -999
      endelse
      
      ShipName = 'SO2sh'
      if ( nVersions eq 2 ) then begin
         ShipUnit = Ship2.Unit
      endif else begin
         ShipUnit = Ship3.Unit
      endelse

      UnDefine, Ship1
      UnDefine, Ship2
      if ( nVersions eq 3 ) then begin
      UnDefine, Ship3
      endif
      
   endfor

   ; Write SO2 totals to file
   WriteHeader,  Ilun, 'SO2 SOURCES', Version1, Version2, Version3, /Advance
   WriteTracers, Ilun, AirName,  AirTotal1,  AirTotal2,  AirTotal3,  AirUnit
   WriteTracers, Ilun, AnthName, AnthTotal1, AnthTotal2, AnthTotal3, AnthUnit
   WriteTracers, Ilun, BiomName, BiomTotal1, BiomTotal2, BiomTotal3, BiomUnit
   WriteTracers, Ilun, VolName,  VolTotal1,  VolTotal2,  VolTotal3,  VolUnit
   WriteTracers, Ilun, ShipName, ShipTotal1, ShipTotal2, ShipTotal3, ShipUnit
   Flush, Ilun

   Undefine, AirTotal1
   Undefine, AirTotal2
   Undefine, AirTotal3
   Undefine, AnthTotal1
   Undefine, AnthTotal2
   Undefine, AnthTotal3
   Undefine, BiomTotal1
   Undefine, BiomTotal2
   Undefine, BiomTotal3
   Undefine, VolTotal1
   Undefine, VolTotal2
   Undefine, VolTotal3
   Undefine, ShipTotal1
   Undefine, ShipTotal2
   Undefine, ShipTotal3

   ;====================================================================
   ; SO4 emissions
   ;====================================================================
   Tracer = [ 27 ]

   ; Initialize
   Total1 = 0.0d0
   Total2 = 0.0d0
   Total3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      CTM_Sum_Emissions, 'SO4-AN-$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_A, /No_Sec, /Kg
    
      CTM_Sum_Emissions, 'SO4-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_B, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO4-AN-$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO4-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_B, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'SO4-AN-$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'SO4-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_B, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      Total1 = Total1 + ( Anthro1_A.Sum + Anthro1_B.Sum )
      Total2 = Total2 + ( Anthro2_A.Sum + Anthro2_B.Sum )
      if ( nVersions eq 3 ) then begin
      Total3 = Total3 + ( Anthro3_A.Sum + Anthro3_B.Sum )
      endif else begin
      Total3 = -999
      endelse

      Name = 'SO4an+bf'
      if ( nVersions eq 2 ) then begin
         Unit = Anthro2_A.Unit
      endif else begin
         Unit = Anthro3_A.Unit
      endelse

      UnDefine, Anthro1_A
      UnDefine, Anthro1_B
      UnDefine, Anthro2_A
      UnDefine, Anthro2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Anthro3_A
      UnDefine, Anthro3_B
      endif
      
   endfor

   ; Write SO4 totals to file
   WriteHeader,  Ilun, 'SO4 SOURCES', Version1,  Version2, Version3, /Advance
   WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
   Flush, Ilun

   UnDefine, Total1
   UnDefine, Total2
   UnDefine, Total3

   ;====================================================================
   ; NH3 emissions
   ;====================================================================
   Tracer = [ 30 ]

   ; Initialize
   AnthTotal1 = 0.0d0
   AnthTotal2 = 0.0d0
   AnthTotal3 = 0.0d0
   BiomTotal1 = 0.0d0
   BiomTotal2 = 0.0d0
   BiomTotal3 = 0.0d0
   NatTotal1 = 0.0d0
   NatTotal2 = 0.0d0
   NatTotal3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      ; ==== Anthro + Biofuel NH3 ==== 
      CTM_Sum_Emissions, 'NH3-ANTH', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_A, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'NH3-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_B, /No_Sec, /Kg

      CTM_Sum_Emissions, 'NH3-ANTH', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'NH3-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_B, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NH3-ANTH', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_A, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'NH3-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_B, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      AnthTotal1 = AnthTotal1 + ( Anthro1_A.Sum + Anthro1_B.Sum )
      AnthTotal2 = AnthTotal2 + ( Anthro2_A.Sum + Anthro2_B.Sum )
      if ( nVersions eq 3 ) then begin
      AnthTotal3 = AnthTotal3 + ( Anthro3_A.Sum + Anthro3_B.Sum )
      endif else begin
      AnthTotal3 = -999
      endelse
      
      AnthName = 'NH3an+bf'

      UnDefine, Anthro1_A
      UnDefine, Anthro1_B
      UnDefine, Anthro2_A
      UnDefine, Anthro2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Anthro3_A
      UnDefine, Anthro3_B
      endif
      
      ; ==== Biomass NH3 ==== 
      CTM_Sum_Emissions, 'NH3-BIOB', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Biomass1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'NH3-BIOB', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Biomass2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NH3-BIOB', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Biomass3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      BiomTotal1 = BiomTotal1 + Biomass1.Sum
      BiomTotal2 = BiomTotal2 + Biomass2.Sum
      if ( nVersions eq 3 ) then begin
      BiomTotal3 = BiomTotal3 + Biomass3.Sum
      endif else begin
      BiomTotal3 = -999
      endelse
      
      BiomName = 'NH3bb'

      UnDefine, Biomass1
      UnDefine, Biomass2
      if ( nVersions eq 3 ) then begin
      UnDefine, Biomass3
      endif
      
      ; ==== Natural Source NH3 ==== 
      CTM_Sum_Emissions, 'NH3-NATU', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Natural1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'NH3-NATU', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Natural2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'NH3-NATU', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Natural3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      NatTotal1 = NatTotal1 + Natural1.Sum
      NatTotal2 = NatTotal2 + Natural2.Sum
      if ( nVersions eq 3 ) then begin
      NatTotal3 = NatTotal3 + Natural3.Sum
      endif else begin
      NatTotal3 = -999
      endelse
      
      NatName = 'NH3na'

      UnDefine, Natural1
      UnDefine, Natural2
      if ( nVersions eq 3 ) then begin
      UnDefine, Natural3
      endif
      
   endfor

   ; Convert from Tg NH3 to Tg N
   AnthTotal1 = AnthTotal1 * 14d0/17d0
   AnthTotal2 = AnthTotal2 * 14d0/17d0
   if ( nVersions eq 3 ) then begin
   AnthTotal3 = AnthTotal3 * 14d0/17d0
   endif
   BiomTotal1 = BiomTotal1 * 14d0/17d0
   BiomTotal2 = BiomTotal2 * 14d0/17d0
   if ( nVersions eq 3 ) then begin
   BiomTotal3 = BiomTotal3 * 14d0/17d0
   endif
   NatTotal1  = NatTotal1  * 14d0/17d0
   NatTotal2  = NatTotal2  * 14d0/17d0
   if ( nVersions eq 3 ) then begin
   NatTotal3  = NatTotal3  * 14d0/17d0
   endif

   Unit = 'Tg N'

   ; Write NH3 totals to file
   WriteHeader,  Ilun, 'NH3 SOURCES', Version1, Version2, Version3, /Advance
   WriteTracers, Ilun, AnthName, AnthTotal1, AnthTotal2, AnthTotal3, Unit
   WriteTracers, Ilun, BiomName, BiomTotal1, BiomTotal2, BiomTotal3, Unit
   WriteTracers, Ilun, NatName,  NatTotal1,  NatTotal2,  NatTotal3,  Unit
   Flush, Ilun

   UnDefine, AnthTotal1
   UnDefine, AnthTotal2
   UnDefine, AnthTotal3
   UnDefine, BiomTotal1
   UnDefine, BiomTotal2
   UnDefine, BiomTotal3
   UnDefine, NatTotal1
   UnDefine, NatTotal2
   UnDefine, NatTotal3

   ;====================================================================
   ; BLACK CARBON emissions (sum of Hydrophilic and Hydrophobic)
   ;====================================================================

   Tracer = [ 34 ]

   ; Initialize
   AnthTotal1 = 0.0d0
   AnthTotal2 = 0.0d0
   AnthTotal3 = 0.0d0
   BiomTotal1 = 0.0d0
   BiomTotal2 = 0.0d0
   BiomTotal3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      ; ==== Anthro + Biofuel BC ==== 
      CTM_Sum_Emissions, 'BC-ANTH', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_A, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'BC-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_B, /No_Sec, /Kg

      CTM_Sum_Emissions, 'BC-ANTH', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'BC-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_B, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'BC-ANTH', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_A, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'BC-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_B, /No_Sec, /Kg
      endif
      

      ; Calculate annual totals
      AnthTotal1 = AnthTotal1 + ( Anthro1_A.Sum + Anthro1_B.Sum )
      AnthTotal2 = AnthTotal2 + ( Anthro2_A.Sum + Anthro2_B.Sum )
      if ( nVersions eq 3 ) then begin
      AnthTotal3 = AnthTotal3 + ( Anthro3_A.Sum + Anthro3_B.Sum )
      endif else begin
      AnthTotal3 = -999
      endelse
      
      AnthName = 'BCan+bf'
      if ( nVersions eq 2 ) then begin
         AnthUnit = Anthro2_A.Unit
      endif else begin
         AnthUnit = Anthro3_A.Unit
      endelse
      
      UnDefine, Anthro1_A
      UnDefine, Anthro1_B
      UnDefine, Anthro2_A
      UnDefine, Anthro2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Anthro3_A
      UnDefine, Anthro3_B
      endif
      
      ; ==== Biomass BC ==== 
      CTM_Sum_Emissions, 'BC-BIOB', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Biomass1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'BC-BIOB', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Biomass2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'BC-BIOB', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Biomass3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      BiomTotal1 = BiomTotal1 + Biomass1.Sum
      BiomTotal2 = BiomTotal2 + Biomass2.Sum
      if ( nVersions eq 3 ) then begin
      BiomTotal3 = BiomTotal3 + Biomass3.Sum
      endif else begin
      BiomTotal3 = -999
      endelse

      BiomName = 'BCbb'
      if ( nVersions eq 2 ) then begin
         BiomUnit = Biomass2.Unit
      endif else begin
         BiomUnit = Biomass3.Unit
      endelse
      
      UnDefine, Biomass1
      UnDefine, Biomass2
      if ( nVersions eq 3 ) then begin
      UnDefine, Biomass3
      endif
      
   endfor

   ; Write black carbon totals to file
   WriteHeader,  Ilun, 'BLACK CARBON SOURCES', Version1, Version2, Version3, $
                 /Advance
   WriteTracers, Ilun, AnthName, AnthTotal1, AnthTotal2, AnthTotal3, AnthUnit
   WriteTracers, Ilun, BiomName, BiomTotal1, BiomTotal2, BiomTotal3, BiomUnit
   Flush, Ilun

   UnDefine, AnthTotal1
   UnDefine, AnthTotal2
   UnDefine, AnthTotal3
   UnDefine, BiomTotal1
   UnDefine, BiomTotal2
   UnDefine, BiomTotal3

   ;====================================================================
   ; ORGANIC CARBON emissions (both Hydrophilic and Hydrophobic)
   ;====================================================================
 
   Tracer = [ 35 ]

   ; Initialize
   AnthTotal1 = 0.0d0
   AnthTotal2 = 0.0d0
   AnthTotal3 = 0.0d0
   BiomTotal1 = 0.0d0
   BiomTotal2 = 0.0d0
   BiomTotal3 = 0.0d0
   BiogTotal1 = 0.0d0
   BiogTotal2 = 0.0d0
   BiogTotal3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      ; ==== Anthro ORGC ==== 
      CTM_Sum_Emissions, 'OC-ANTH', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_A, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'OC-BIOF', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Anthro1_B, /No_Sec, /Kg

      CTM_Sum_Emissions, 'OC-ANTH', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_A, /No_Sec, /Kg

      CTM_Sum_Emissions, 'OC-BIOF', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Anthro2_B, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'OC-ANTH', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_A, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'OC-BIOF', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Anthro3_B, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      AnthTotal1 = AnthTotal1 + ( Anthro1_A.Sum + Anthro1_B.Sum )
      AnthTotal2 = AnthTotal2 + ( Anthro2_A.Sum + Anthro2_B.Sum )
      if ( nVersions eq 3 ) then begin
      AnthTotal3 = AnthTotal3 + ( Anthro3_A.Sum + Anthro3_B.Sum )
      endif else begin
      AnthTotal3 = -999
      endelse
      
      AnthName = 'OCan+bf'
      if ( nVersions eq 2 ) then begin
         AnthUnit = Anthro2_A.Unit
      endif else begin
         AnthUnit = Anthro3_A.Unit
      endelse
      
      UnDefine, Anthro1_A
      UnDefine, Anthro1_B
      UnDefine, Anthro2_A
      UnDefine, Anthro2_B
      if ( nVersions eq 3 ) then begin
      UnDefine, Anthro3_A
      UnDefine, Anthro3_B
      endif

      ; ==== Biomass ORGC ==== 
      CTM_Sum_Emissions, 'OC-BIOB', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Biomass1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'OC-BIOB', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Biomass2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'OC-BIOB', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Biomass3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      BiomTotal1 = BiomTotal1 + Biomass1.Sum
      BiomTotal2 = BiomTotal2 + Biomass2.Sum
      if ( nVersions eq 3 ) then begin
      BiomTotal3 = BiomTotal3 + Biomass3.Sum
      endif else begin
      BiomTotal3 = -999
      endelse
      
      BiomName = 'OCbb'
      if ( nVersions eq 2 ) then begin
         BiomUnit = Biomass2.Unit
      endif else begin
         BiomUnit = Biomass3.Unit
      endelse
      
      UnDefine, Biomass1
      UnDefine, Biomass2
      if ( nVersions eq 3 ) then begin
      UnDefine, Biomass3
      endif

      ; ==== Biogenic ORGC ==== 
      CTM_Sum_Emissions, 'OC-BIOG', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=Biogenic1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'OC-BIOG', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=Biogenic2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'OC-BIOG', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=Biogenic3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      BiogTotal1 = BiogTotal1 + Biogenic1.Sum
      BiogTotal2 = BiogTotal2 + Biogenic2.Sum
      if ( nVersions eq 3 ) then begin
      BiogTotal3 = BiogTotal3 + Biogenic3.Sum
      endif else begin
      BiogTotal3 = -999
      endelse
      
      BiogName = 'OCbg'
      if ( nVersions eq 2 ) then begin
         BiogUnit = Biogenic2.Unit
      endif else begin
         BiogUnit = Biogenic3.Unit
      endelse

      UnDefine, Biogenic1
      UnDefine, Biogenic2
      if ( nVersions eq 3 ) then begin
      UnDefine, Biogenic3
      endif
      
   endfor

   ; Write organic carbon totals to file
   WriteHeader,  Ilun, 'ORGANIC CARBON SOURCES', Version1, Version2, Version3, $
                 /Advance
   WriteTracers, Ilun, AnthName, AnthTotal1, AnthTotal2, AnthTotal3, AnthUnit
   WriteTracers, Ilun, BiomName, BiomTotal1, BiomTotal2, BiomTotal3, BiomUnit
   WriteTracers, Ilun, BiogName, BiogTotal1, BiogTotal2, BiogTotal3, BiogUnit
   Flush, Ilun

   UnDefine, AnthTotal1
   UnDefine, AnthTotal2
   UnDefine, AnthTotal3
   UnDefine, BiomTotal1
   UnDefine, BiomTotal2
   UnDefine, BiomTotal3
   UnDefine, BiogTotal1
   UnDefine, BiogTotal2
   UnDefine, BiogTotal3

   ;====================================================================
   ; Hydrophilic from hydrophobic tracer
   ;====================================================================

   PLBCTotal1 = 0.0d0
   PLBCTotal2 = 0.0d0
   PLBCTotal3 = 0.0d0
   PLOCTotal1 = 0.0d0
   PLOCTotal2 = 0.0d0
   PLOCTotal3 = 0.0d0

   ; Loop over months
   for i = 0, 11 do begin

      ; Free memory
      ctm_cleanup, /no_gc

      mn = strtrim(String(fix(i+1)), 2)
      if (strlen(mn) eq 1) then begin         
         mn='0'+mn     
      endif

      ; Filenames
      File1=pref1+mn+'01.nc'
      File2=pref2+mn+'01.nc'
      if ( nVersions eq 3 ) then begin
      File3=pref3+mn+'01.nc'
      endif
      
      ; ==== Converted BLKC ==== 
      Tracer = [ 34 ]

      CTM_Sum_Emissions, 'PL-BC=$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=PLBC1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'PL-BC=$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=PLBC2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'PL-BC=$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=PLBC3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      PLBCTotal1 = PLBCTotal1 + PLBC1.Sum
      PLBCTotal2 = PLBCTotal2 + PLBC2.Sum
      if ( nVersions eq 3 ) then begin
      PLBCTotal3 = PLBCTotal3 + PLBC3.Sum
      endif else begin
      PLBCTotal3 = -999
      endelse
      
      PLBCName = 'BChp'
      if ( nVersions eq 2 ) then begin
         PLBCUnit = PLBC2.Unit
      endif else begin
         PLBCUnit = PLBC3.Unit
      endelse
         
      UnDefine, PLBC1
      UnDefine, PLBC2
      if ( nVersions eq 3 ) then begin
      UnDefine, PLBC3
      endif
      
      ; ==== Converted ORGC  ==== 
      Tracer = [ 35 ]
      
      CTM_Sum_Emissions, 'PL-OC=$', $
         File=File1, Tracer=Tracer, /Cum_Only, Result=PLOC1, /No_Sec, /Kg
      
      CTM_Sum_Emissions, 'PL-OC=$', $
         File=File2, Tracer=Tracer, /Cum_Only, Result=PLOC2, /No_Sec, /Kg

      if ( nVersions eq 3 ) then begin
      CTM_Sum_Emissions, 'PL-OC=$', $
         File=File3, Tracer=Tracer, /Cum_Only, Result=PLOC3, /No_Sec, /Kg
      endif
      
      ; Calculate annual totals
      PLOCTotal1 = PLOCTotal1 + PLOC1.Sum
      PLOCTotal2 = PLOCTotal2 + PLOC2.Sum
      if ( nVersions eq 3 ) then begin
         PLOCTotal3 = PLOCTotal3 + PLOC3.Sum
      endif else begin
         PLOCTotal3 = -999
      endelse

      PLOCName = 'OChp'
      if ( nVersions eq 2 ) then begin
         PLOCUnit = PLOC2.Unit
      endif else begin
         PLOCUnit = PLOC3.Unit
      endelse
         
      UnDefine, PLOC1
      UnDefine, PLOC2
      if ( nVersions eq 3 ) then begin
      UnDefine, PLOC3
      endif
      
   endfor

   ; Write hydrophilic totals to file
   WriteHeader,  Ilun, 'HYDROPHILIC TRACER FROM HYDROPHOBIC TRACER', $
                 Version1, Version2, Version3, /Advance
   WriteTracers, Ilun, PLBCName, PLBCTotal1, PLBCTotal2, PLBCTotal3, PLBCUnit
   WriteTracers, Ilun, PLOCName, PLOCTotal1, PLOCTotal2, PLOCTotal3, PLOCUnit
   Flush, Ilun

   Undefine, PLBCTotal1
   Undefine, PLBCTotal2
   Undefine, PLBCTotal3
   Undefine, PLOCTotal1
   Undefine, PLOCTotal2
   Undefine, PLOCTotal3

   ;====================================================================
   ; DUST emissions
   ;====================================================================

   ; Write dust header to file
   WriteHeader, Ilun, 'DUST EMISSIONS', Version1, Version2, Version3, /Advance

   ; ==== Dust tracers ====
   Tracer = [ 38, 39, 40, 41 ]

   ; Loop over tracers
   for n = 0, n_elements(Tracer)-1L do begin

      ; Initialize
      Total1 = 0.0d0
      Total2 = 0.0d0
      Total3 = 0.0d0

      ; Loop over months
      for i = 0, 11 do begin

         ; Free memory
         ctm_cleanup, /no_gc

         mn = strtrim(String(fix(i+1)), 2)
         if (strlen(mn) eq 1) then begin         
            mn='0'+mn     
         endif
      
         ; Filenames
         File1=pref1+mn+'01.nc'
         File2=pref2+mn+'01.nc'
         if ( nVersions eq 3 ) then begin
         File3=pref3+mn+'01.nc'
         endif
         
         CTM_Sum_Emissions, 'DUSTSRCE', $
            File=File1, Tracer=Tracer[n], /Cum_Only, Result=Emiss1, /Kg, /No_Sec
         
         CTM_Sum_Emissions, 'DUSTSRCE', $
            File=File2, Tracer=Tracer[n], /Cum_Only, Result=Emiss2, /Kg, /No_Sec

         if ( nVersions eq 3 ) then begin
         CTM_Sum_Emissions, 'DUSTSRCE', $
            File=File3, Tracer=Tracer[n], /Cum_Only, Result=Emiss3, /Kg, /No_Sec
         endif
         
         ; Calculate annual totals
         Total1 = Total1 + Emiss1.Sum
         Total2 = Total2 + Emiss2.Sum
         if ( nVersions eq 3 ) then begin
         Total3 = Total3 + Emiss3.Sum
         endif else begin
         Total3 = -999
         endelse
      
         if ( nVersions eq 2 ) then begin
            Name   = Emiss2.Name
            Unit   = Emiss2.Unit
         endif else begin
            Name   = Emiss3.Name
            Unit   = Emiss3.Unit
         endelse

         UnDefine, Emiss1
         UnDefine, Emiss2
         if ( nVersions eq 3 ) then begin
         UnDefine, Emiss3
         endif

      endfor

      ; Write anthro totals to file
      WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
      Flush, Ilun
   
      UnDefine, Total1
      UnDefine, Total2
      UnDefine, Total3

   endfor

   ;====================================================================
   ; SEASALT emissions
   ;====================================================================

   ; Write seasalt header to file
   WriteHeader, Ilun, 'SEA SALT EMISSIONS', Version1, Version2, Version3, $
                /Advance
   Flush, Ilun

   ; ==== Seasalt tracers ====
   Tracer = [ 42, 43 ]

   ; Loop over tracers
   for n = 0, n_elements(Tracer)-1L do begin

      ; Initialize
      Total1 = 0.0d0
      Total2 = 0.0d0
      Total3 = 0.0d0

      ; Loop over months
      for i = 0, 11 do begin

         ; Free memory
         ctm_cleanup, /no_gc

         mn = strtrim(String(fix(i+1)), 2)
         if (strlen(mn) eq 1) then begin         
            mn='0'+mn     
         endif
      
         ; Filenames
         File1=pref1+mn+'01.nc'
         File2=pref2+mn+'01.nc'
         if ( nVersions eq 3 ) then begin
         File3=pref3+mn+'01.nc'
         endif
         
         CTM_Sum_Emissions, 'SALTSRCE', $
            File=File1, Tracer=Tracer[n], /Cum_Only, Result=Emiss1, /Kg, /No_Sec
         
         CTM_Sum_Emissions, 'SALTSRCE', $
            File=File2, Tracer=Tracer[n], /Cum_Only, Result=Emiss2, /Kg, /No_Sec

         if ( nVersions eq 3 ) then begin
         CTM_Sum_Emissions, 'SALTSRCE', $
            File=File3, Tracer=Tracer[n], /Cum_Only, Result=Emiss3, /Kg, /No_Sec
         endif
         
         ; Calculate annual totals
         Total1 = Total1 + Emiss1.Sum
         Total2 = Total2 + Emiss2.Sum
         if ( nVersions eq 3 ) then begin
         Total3 = Total3 + Emiss3.Sum
               endif else begin
         Total3 = -999
         endelse
      
         if ( nVersions eq 2 ) then begin
            Name   = Emiss2.Name
            Unit   = Emiss2.Unit
         endif else begin
            Name   = Emiss3.Name
            Unit   = Emiss3.Unit
         endelse

         UnDefine, Emiss1
         UnDefine, Emiss2
         if ( nVersions eq 3 ) then begin
         UnDefine, Emiss3
         endif

      endfor

      ; Write anthro totals to file
      WriteTracers, Ilun, Name, Total1, Total2, Total3, Unit
      Flush, Ilun
   
      UnDefine, Total1
      UnDefine, Total2
      UnDefine, Total3

   endfor

   ;====================================================================
   ; Close file and quit
   ;====================================================================
quit:
   Close,    Ilun
   Free_LUN, Ilun
 
   return
end
   
   
 
 
   
