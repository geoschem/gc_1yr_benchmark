;------------------------------------------------------------------------------
;+
; NAME:
;        gchp_rnpbbe_mass
;
; PURPOSE:
;        quick script to calculate gchp and gcc rnpbbe global masses
;
; CATEGORY:
;        Benchmarking (unofficial)
;
; CALLING SEQUENCE:
;        from command line
;
; MODIFICATION HISTORY:
;        mps, 07 Mar 2016: Initial Version
;        ewl, 21 Jul 2017: Modifications for GCHP 1-year benchmark
;------------------------------------------------------------------------------

; Resolve external routines
@ ./geoschem_rnpbbe_burden_unofficial

;------------------------------------------------------------------------------

pro gchp_rnpbbe_mass,  InputFile,                                           $
                       RunName = RunName,               PSDir = PSDir,      $
                       Debug=Debug,                     Do_PDF=Do_PDF,      $
                       Do_GCHP=Do_GCHP,                 nVersions=nVersions,$  
                       _EXTRA = e

   ; Define number of tracers
   numTracers = 4

   ; Save original color table
   TvLct, R, G, B, /Get

   ; Load a color table with the old MYCT drawing colors
   MyCt, /WhGrYlRd, /Bright_Colors

   ; Print debug output
   Debug       = Keyword_Set( Debug         )

   ; Keywords for Ox, CO, MOZAIC, Aircraft, Lon Profile, and difference plots
   Do_GCHP        = Keyword_Set( Do_GCHP        )
   DynRange       = Keyword_Set( DynRange       )

   ; Define suffix for plots
   if ( DynRange )                    $
      then PsSuffix = '.dyn_range.ps' $
      else PsSuffix = '.ps'       

   ; Default number of versions to compare
   ; Note that 3 are input since a diagnostics file is needed for the
   ; unit conversions of the gchp output and gcc restart
   nVersions = 2
   
   ; Open the input file
   Open_File, InputFile, Ilun, /Get_Lun

   ; Define string variable
   Line = ''

   Print, '=== COMPARING 2 VERSIONS ==='
   ; Loop thru file
   while ( not EOF( Ilun ) ) do begin
   
      ; Read line and break on colon
      ReadF, Ilun, Line
      Result = StrTrim( StrBreak( Line, ':' ), 2 )
   
      ; Parse value of line into individual variables
      ; Model 1 should be GCHP daily file
      ; Model 2 should be GCC restart file
      ; Model 3 should be GCC diagnostics file
      case ( StrUpCase( Result[0] ) ) of 
         'V1' : Vers1  = Result[1]
         'V2' : Vers2  = Result[1]
         'V3' : Vers3  = Result[1]
         'D1' : Dir1   = Add_Separator( Result[1] )
         'D2' : Dir2   = Add_Separator( Result[1] )
         'D3' : Dir3   = Add_Separator( Result[1] )
         'L1' : Label1 = Result[1]
         'L2' : Label2 = Result[1]
         'L3' : Label3 = Result[1]
         'M1' : Model1 = Result[1]
         'M2' : Model2 = Result[1]
         'M3' : Model3 = Result[1]
         'R1' : Res1   = Long( Result[1] )
         'R2' : Res2   = Long( Result[1] )
         'R3' : Res3   = Long( Result[1] )
         'Y1' : Year1  = Result[1]
         'Y2' : Year2  = Result[1]
         'Y3' : Year3  = Result[1]
         else : ; Do nothing
      endcase
   endwhile

   ; Close input file
   Close,    Ilun
   Free_Lun, Ilun

  ;====================================================================
   ; Define some plotting variables
   ;====================================================================

   ; For 1st model - GCHP daily
   PREF1      = Dir1 + Label1
   ModelInfo1 = CTM_Type( Model1, Res=Res1 )
   GridInfo1  = CTM_Grid( ModelInfo1 )
   Dlat1      = GridInfo1.DJ
   Dlon1      = GridInfo1.DI
   Ptop1      = GridInfo1.Pedge[ GridInfo1.LMX ]
   Nalt1      = GridInfo1.LMX

   ; For 2nd model - GCC restart
   PREF2      = Dir2 + Label2
   ModelInfo2 = CTM_Type( Model2, Res=Res2 )
   GridInfo2  = CTM_Grid( ModelInfo2 )
   Dlat2      = GridInfo2.DJ
   Dlon2      = GridInfo2.DI
   Ptop2      = GridInfo2.Pedge[ GridInfo2.LMX ]
   Nalt2      = GridInfo2.LMX

   ; For 3rd model - GCC diagnostics
   PREF3      = Dir3 + Label3
   ModelInfo3 = CTM_Type( Model3, Res=Res3 )
   GridInfo3  = CTM_Grid( ModelInfo3 )
   Dlat3      = GridInfo3.DJ
   Dlon3      = GridInfo3.DI
   Ptop3      = GridInfo3.Pedge[ GridInfo3.LMX ]
   Nalt3      = GridInfo3.LMX

   ; Create plot title for top of page
   Title1  = 'Red: '   + Strtrim( String( Vers1 ), 2 ) + ' (' $
                       + Strtrim( String( Year1 ), 2 ) + ')'
   Title2  = 'Green: ' + Strtrim( String( Vers2 ), 2 ) + ' (' $
                       + Strtrim( String( Year2 ), 2 ) + ')'
   Title   = Title1 + ';  ' + Title2  

   ; RUNNAME is the tag for the PostScript files
   if ( N_Elements( RunName ) eq 0 ) then begin
      if ( nVersions eq 2 ) then RunName = Vers2 $
                            else RunName = Vers3
   endif
   
   ; Redirect PostScript output (end w/ slash)
   if ( N_Elements( PSDir ) eq 0 ) then PSDir   = './output/'

   ;====================================================================
   ; Set links to the proper diaginfo.dat, tracerinfo.dat files
   ;====================================================================

   ; Remove existing file links
   Cmd = 'rm -f diaginfo.dat tracerinfo.dat'
   if ( Debug ) then begin
      print 
      print, Cmd
   endif
   Spawn, Cmd

   ; Link to the diaginfo.dat file
   Cmd = 'ln -s ' + StrTrim( Dir2, 2 )+ 'diaginfo.dat .'
   if ( Debug ) then print, Cmd
   Spawn, Cmd

   ; Link to the tracerinfo.dat file
   Cmd = 'ln -s ' + StrTrim( Dir2, 2 )+ 'tracerinfo.dat .'
   if ( Debug ) then print, Cmd
   Spawn, Cmd
      
   ;=================================================================
   ; Compute RnPbBe Burdens (mass) without GCHP diagnostics
   ;=================================================================
   Print, 'Computing RnPbBe burdens'
   
   OutFile = PSDir + 'RnPbBe_Burdens.' + RunName + '.txt'
   Print,  OutFile
   geoschem_rnpbbe_burden_unofficial,   $
            Pref1, Vers1, Model1, Res1, $
            Pref2, Vers2, Model2, Res2, $
            Pref3, Vers3, Model3, Res3, $
            OutFileName = OutFile, _EXTRA = e
            ; Pref1 is GCHP file (daily mean on 1st of month)
            ; Pref2 is GCC restart file (1st of month)
            ; Pref3 is GCC diagnostics file

   if ( Do_PDF ) then begin

      ; Create PDF files from the postscript files
      Make_PDF, './output/'
 
      ; Remove PS files and only keep PDF files
      Spawn, 'rm -v ./output/*.ps'

   endif

   ; Restore original color table
   TvLct, R, G, B

end
