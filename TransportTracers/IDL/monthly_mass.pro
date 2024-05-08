;-----------------------------------------------------------------------
;+
; NAME:
;        MONTHLY_MASS
;
; PURPOSE:
;        Obtains tracer mass information from ND49 output and prints
;        monthly tracer mass to a file.
;
; CATEGORY:
;        Benchmarking
;
; CALLING SEQUENCE:
;        MONTHLY_MASS, RUNDIR, MASSFILE, YEAR, VERSION, [, Keywords ]
;
; INPUTS:
;      RUNDIR -> Run directory containing the output files from a 1-year
;                benchmark simulation
;
;        FILE -> The text file output from the ND49 diagnostic containing
;                6-hourly global mass totals
;
;        YEAR -> The YEAR corresponding to the data to be plotted.
;
;        VERSION -> The model version number corresponding to the
;             data to be plotted.
;
; KEYWORD PARAMETERS:
;        OUTFILENAME -> Write monthly mass to this file. Default is
;                       "mass.txt".
;
; OUTPUTS:
;        None
; 
; NOTES:
;        (1) Meant to be called from PLOT_PASSIVE.
;
; EXAMPLES:
;        RUNDIR   = '/home/mpayer/SR1/v11-01/v11-01d/GEOSFP/RnPbBe/'
;        FILE     = 'tracer_mass_kg.txt'
;        YEAR     = 2013
;        VERSION  = 'v11-01d'
;
;        SEASON_MAPS, FILE, LEVELS, YEAR, TRACERS, VERSION, $
;                    /PS, OUTFILENAME='mass.txt'
;
;             ; Creates text file containing monthly mass of passive
;             ; tracer for 1-year RnPbeBe benchmark for GEOS-CHEM v11-01d.
;             ; Output is sent to text file "mass.txt".
;
; MODIFICATION HISTORY:
;        mps, 01 Dec 2015: Initial version
;-----------------------------------------------------------------------

pro Monthly_Mass, RunDir, MassFile, Year, Version, $
                  OutFileName=OutFileName, _EXTRA=e 

   ; Arguments
   if ( N_Elements( RunDir   ) ne 1 ) then Message, 'Invalid RUNDIR!'
   if ( N_Elements( MassFile ) ne 1 ) then Message, 'Invalid MASSFILE'
   if ( N_Elements( Year     ) ne 1 ) then Message, 'Invalid YEAR!'
   if ( N_Elements( Version  ) ne 1 ) then Message, 'Invalid VERSION!'
   
   ; Keywords
   if ( N_Elements( OutFileName ) ne 1 ) then OutFileName = 'mass.txt'
   
   ;====================================================================
   ; Read 6-hr mass from MASSFILE
   ;====================================================================

   InFile = RunDir + '/' + MassFile
   
   ; Open the input file
   Open_File, InFile, Ilun, /Get_Lun

   ; Define string variable
   Line = ''

   ; Loop thru file
   while ( not EOF( Ilun ) ) do begin

      ; Read line and break on colon
      ReadF, Ilun, Line
      Result = StrTrim( StrBreak( Line, ':' ), 2 )

;      Print, Result[0], Result[1], Result[2]
      
      ; Parse value of line into individual variables
      case ( Result[0] ) of 
         '2013/01/01 06' : Mass1  = Result[2]
         '2013/02/01 00' : Mass2  = Result[2]
         '2013/03/01 00' : Mass3  = Result[2]
         '2013/04/01 00' : Mass4  = Result[2]
         '2013/05/01 00' : Mass5  = Result[2]
         '2013/06/01 00' : Mass6  = Result[2]
         '2013/07/01 00' : Mass7  = Result[2]
         '2013/08/01 00' : Mass8  = Result[2]
         '2013/09/01 00' : Mass9  = Result[2]
         '2013/10/01 00' : Mass10 = Result[2]
         '2013/11/01 00' : Mass11 = Result[2]
         '2013/12/01 00' : Mass12 = Result[2]
         '2014/01/01 00' : Mass13 = Result[2]
         '2016/01/01 06' : Mass1  = Result[2]
         '2016/02/01 00' : Mass2  = Result[2]
         '2016/03/01 00' : Mass3  = Result[2]
         '2016/04/01 00' : Mass4  = Result[2]
         '2016/05/01 00' : Mass5  = Result[2]
         '2016/06/01 00' : Mass6  = Result[2]
         '2016/07/01 00' : Mass7  = Result[2]
         '2016/08/01 00' : Mass8  = Result[2]
         '2016/09/01 00' : Mass9  = Result[2]
         '2016/10/01 00' : Mass10 = Result[2]
         '2016/11/01 00' : Mass11 = Result[2]
         '2016/12/01 00' : Mass12 = Result[2]
         '2017/01/01 00' : Mass13 = Result[2]
         else : ; Do nothing
      endcase
   endwhile

   ; Close input file
   Close,    Ilun
   Free_Lun, Ilun
   
   ; Create array of monthly mass
   All_Mass = [ Mass1, Mass2, Mass3,  Mass4,  Mass5,  Mass6, Mass7, $
                Mass8, Mass9, Mass10, Mass11, Mass12, Mass13 ]

   ; Convert mass from kg to Tg
   Num_Mass = Double(All_Mass) * 1E-9
   
   ; Calculate some stats
   Min_Mass = Min(Num_Mass)
   Max_Mass = Max(Num_Mass)
   Abs_Diff = (Max_Mass - Min_Mass )
   Pct_Diff = (Max_Mass - Min_Mass ) / Min_Mass * 100

   if ( Abs_Diff lt 1E-10 ) then begin
      ; Express absolute difference in g
      Abs_Diff =  Abs_Diff * 1E12
      DUnit    = ' g'
      Format   = '(A,F17.1,A)'
   endif else if ( Abs_Diff lt 1E-7 ) then begin
      ; Express absolute difference in kg
      Abs_Diff =  Abs_Diff * 1E9
      DUnit    = ' kg'
      Format   = '(A,F17.4,A)'
   endif else if ( Abs_Diff lt 1E-4 ) then begin
      ; Express absolute difference in Mg
      Abs_Diff =  Abs_Diff * 1E6
      DUnit    = ' Mg'
      Format   = '(A,F17.7,A)'
   endif else if ( Abs_Diff lt 1E-1 ) then begin
      ; Express absolute difference in Gg
      Abs_Diff =  Abs_Diff * 1E3
      DUnit    = ' Gg'
      Format   = '(A,F17.10,A)'
   endif else begin
      ; Express absolute difference in Tg
      DUnit    = ' Tg'
      Format   = '(A,F17.13,A)'
   endelse

   ;===========================================================
   ; Open file to create table for tropospheric burden
   openW,  unit, OutFileName, /get_lun
   printf, unit, ' '
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   printf, unit, '  Global Mass of Passive Tracer in ' + version
   printf, unit, '%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%'
   printf, unit, ' '
   printf, unit, ' Date        Mass [Tg]'
   printf, unit, ' ----------  ----------------'
   if ( Year eq '2013' ) then begin
      printf, unit, format = '(A,F17.13,A)', ' 2013/01/01 ', Num_Mass[0]
      printf, unit, format = '(A,F17.13,A)', ' 2013/02/01 ', Num_Mass[1]
      printf, unit, format = '(A,F17.13,A)', ' 2013/03/01 ', Num_Mass[2]
      printf, unit, format = '(A,F17.13,A)', ' 2013/04/01 ', Num_Mass[3]
      printf, unit, format = '(A,F17.13,A)', ' 2013/05/01 ', Num_Mass[4]
      printf, unit, format = '(A,F17.13,A)', ' 2013/06/01 ', Num_Mass[5]
      printf, unit, format = '(A,F17.13,A)', ' 2013/07/01 ', Num_Mass[6]
      printf, unit, format = '(A,F17.13,A)', ' 2013/08/01 ', Num_Mass[7]
      printf, unit, format = '(A,F17.13,A)', ' 2013/09/01 ', Num_Mass[8]
      printf, unit, format = '(A,F17.13,A)', ' 2013/10/01 ', Num_Mass[9]
      printf, unit, format = '(A,F17.13,A)', ' 2013/11/01 ', Num_Mass[10]
      printf, unit, format = '(A,F17.13,A)', ' 2013/12/01 ', Num_Mass[11]
      printf, unit, format = '(A,F17.13,A)', ' 2014/01/01 ', Num_Mass[12]
   endif else if ( Year eq '2016' ) then begin
      printf, unit, format = '(A,F17.13,A)', ' 2016/01/01 ', Num_Mass[0]
      printf, unit, format = '(A,F17.13,A)', ' 2016/02/01 ', Num_Mass[1]
      printf, unit, format = '(A,F17.13,A)', ' 2016/03/01 ', Num_Mass[2]
      printf, unit, format = '(A,F17.13,A)', ' 2016/04/01 ', Num_Mass[3]
      printf, unit, format = '(A,F17.13,A)', ' 2016/05/01 ', Num_Mass[4]
      printf, unit, format = '(A,F17.13,A)', ' 2016/06/01 ', Num_Mass[5]
      printf, unit, format = '(A,F17.13,A)', ' 2016/07/01 ', Num_Mass[6]
      printf, unit, format = '(A,F17.13,A)', ' 2016/08/01 ', Num_Mass[7]
      printf, unit, format = '(A,F17.13,A)', ' 2016/09/01 ', Num_Mass[8]
      printf, unit, format = '(A,F17.13,A)', ' 2016/10/01 ', Num_Mass[9]
      printf, unit, format = '(A,F17.13,A)', ' 2016/11/01 ', Num_Mass[10]
      printf, unit, format = '(A,F17.13,A)', ' 2016/12/01 ', Num_Mass[11]
      printf, unit, format = '(A,F17.13,A)', ' 2017/01/01 ', Num_Mass[12]
   endif
   printf, unit, ' '
   printf, unit, ' Summary'
   printf, unit, ' ------------------------------'
   printf, unit, format = '(A,F17.13,A)', ' Max mass = ', Max_Mass, ' Tg'
   printf, unit, format = '(A,F17.13,A)', ' Min mass = ', Min_Mass, ' Tg'
   printf, unit, format = Format,         ' Abs diff = ', Abs_Diff, DUnit
   printf, unit, format = '(A,F17.4,A)',  ' Pct diff = ', Pct_Diff, ' %'
   printf, unit, ' '
   
   ; Close output file
   free_lun, unit

end
