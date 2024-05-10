;-----------------------------------------------------------------------
;+
; NAME:
;        BENCHMARK_1YR
;
; PURPOSE:
;        Produces plots of tracers (from 3 different GEOS-Chem 1-year 
;        benchmark simulations) vs. various observations.
;
; CATEGORY:
;        GEOS-CHEM Benchmarking
;
; CALLING SEQUENCE:
;        BENCHMARK_1YR, INPUTFILE, [, Keywords ]
;
; INPUTS:
;        INPUTFILE -> A file containing default values for version 
;             numbers, directories, model names, resolutions, etc.
;             Default is "input/input_bm.1yr"
;
; KEYWORD PARAMETERS:
;        PSDIR -> Specifies the directory path where PostScript output
;             files will be created.  Default is ./output.
;
;        RUNNAME -> A string label that will be appended to each of
;             the PostScritp files, to facilitate identification.
;             Default is to use the 3rd model name.
;
;        DEBUG -> Set this switch to print the values of the various
;             input variables.  Use this to make sure that all values
;             have been created corectly.
;
;        /DO_Ox -> Set this switch to produce PostScript output plots
;             for models vs. various Ozone observations.
;
;        /DO_CO -> Set this switch to produce PostScript output plots
;             for models vs. various CO observations.
;
;        /DO_MOZAIC -> Set this switch to produce PostScript output 
;             plots for models vs. MOZAIC data observations.
;
;        /DO_AIRCRAFT -> Set this switch to produce PostScript output 
;             plots for models vs. observations from various aircraft 
;             missions.
;
;        /DO_AEROSOL -> Set this switch to produce PostScript output
;             plots for model vs. aerosol observations.
;
;        /DO_BrO -> Set this switch to produce PostScript output plots
;             for models vs. bromine observations.
;
;        /DO_PAN -> Set this switch to produce PostScript output plots
;             for models vs. PAN observations.
;
;        /DO_PDF -> Make PDF files from the PS files and remove PS
;
;        /DO_GCHP -> Set this switch if performing a one-year benchmark for GCHP
;                 using output without diagnostics
;
;        /DYNRANGE -> Set this switch to produce difference, ratio,
;             and longitudinal profile plots using the entire dynamic
;             range of the data (instead of the preset ranges).
;
;        NVERSIONS -> Set this switch to compare two or three model versions.
;                     The default is 2.
;
; OUTPUTS:
;        None
;
; SUBROUTINES:
;        See list of included subroutines below.
;
; REQUIREMENTS:
;        References routines from both GAMAP and TOOLS packages.
;
; NOTES:
;        (1) Input files and the file w/ the default plotting
;             ranges are contained in the input/ subdirectory.
;        (2) Default is to write plots to the output/ subdirectory
;        (3) Temporary data files are written to the temp/ subdirectory
;
; EXAMPLES:
;        BENCHMARK_1YR, /DEBUG
;             ; Test to see if default values were read in correctly,
;             ; but do not create any benchmark plots.
; 
;        BENCHMARK_1YR, /DO_Ox
;             ; Create benchmark plots for Ozone only
;
;        BENCHMARK_1YR, /DO_Ox, /DO_CO
;             ; Create benchmark plots for Ozone and CO only
;
; MODIFICATION HISTORY:
;  lzh & bmy, 07 Nov 2007: VERSION 1.01
;                          - Based on older code by Inna Megretskaia
;        bmy, 10 Mar 2008: VERSION 1.02
;                          - Change /users/trop/iam to /home/iam to reflect
;                            a change in the home directory path.  This is
;                            needed for the newer Linux logins.
;        bmy, 24 Mar 2008: VERSION 1.03
;                          - Now plot all 43 tracers in difference and
;                            profile plots
;                          - Also introduce 43-tracer ratio maps
;  lzh & bmy, 30 May 2008: VERSION 1.04
;                          - Add new types of plots for MOZAIC & CO
;                          - Reads updated data files.
;                          - Removed some repeated CO plots
;        clh, 07 Jul 2009: VERSION 1.05
;                          - Add aerosol benchmarking
;        bmy, 13 Jul 2009  - Now look for PM2.5 data in the pm25_data dir
;        ccc, 01 Oct 2010  - Updated station files for CO for MOZAIC and
;                            surface data. Also, extended the yrange for
;                            MOZAIC seasonal plots close to surface.
;        bmy, 05 Sep 2012: VERSION 1.06
;                          - Now use ug/m3 as unit for aerosol plots
;                          - Now create AOD diff plots
;                          - Now create AOD map plots
;     mpayer, 07 Sep 2012: - Add bromine benchmark plots
;     mpayer, 05 Nov 2012: - Now plot all 53 tracers in difference, ratio,
;                            and profile plots
;     mpayer, 01 Jul 2013: - Now plot all 63 tracers in difference, ratio,
;                            and profile plots 
;                          - Add /DO_EMISSIONS keyword to plot emission ratios
;        mps, 02 Jan 2014: - Now print annual emission totals to text file
;                            when /DO_EMISSIONS keyword is used
;                          - Now plot all 66 tracers in difference, ratio,
;                            and profile plots
;        mps, 10 Jun 2014: - The user directory /home/iam no longer exists, so
;                            we copied data files to the data/ subdirectory
;        mps, 16 Jun 2014: - Now create zonal mean difference plots
;        mps, 27 Apr 2015: - Now create emission maps and emission difference
;                            plots
;        mps, 29 Apr 2015: - Now create maps, differences, and ratios of
;                            J-values
;        mps, 04 Dec 2015: - Add stratospheric benchmark plots
;        mps, 09 Dec 2015: - Add aerosol burdens, global mean AOD totals, and
;                            aerosol verfical profile plots from Lizzie Lundgren
;        mps, 08 Apr 2016: - Add plots for cloud optical depth
;        mps, 27 Jan 2017: - Allow for comparison of 2 versions, intead of the
;                            default 3 versions. This can be specified by
;                            passing NVERSIONS=2.
;        mps, 10 May 2017: - Add PAN profile benchmark plots
;        mps, 19 Sep 2017: - Add option to output O3 STE flux
;        mps, 21 Sep 2017: - Add concentration maps at surface and 500hPa and
;                            zonal mean concentration plots
;        bmy, 15 May 2018: - Add lots of calls to CTM_Cleanup, /NO_GC so as 
;                            to prevent running out of file units
;
;-
; Copyright (C) 2007-2012, Inna Megretskaia, Lin Zhang,
; Bob Yantosca and Philippe Le Sager, Harvard University
; This software is provided as is without any warranty whatsoever. 
; It may be freely used, copied or distributed for non-commercial 
; purposes. This copyright notice must be kept with any copy of 
; this software. If this software shall be used commercially or 
; sold as part of a larger package, please contact the author.
; Bugs and comments should be directed to bmy@io.as.harvard.edu
; or phs@io.harvard.edu with subject "IDL routine benchmark_1yr"
;-----------------------------------------------------------------------

; Resolve external routines
@ ./subroutines/get_pressure_geos.pro
@ ./subroutines/get_species_geos.pro
@ ./subroutines/get_2dfield_geos.pro
@ ./subroutines/get_3dfield_geos.pro
@ ./subroutines/get_area_geos.pro
@ ./subroutines/read_sondes.pro
@ ./subroutines/read_sondes_0_5.pro
@ ./subroutines/read_sondes_4lev.pro
@ ./subroutines/read_sondes_4lev_mozaic.pro
@ ./subroutines/read_sondes_4lev_mozaic_0_5.pro
@ ./subroutines/read_sondes_co_0_5.pro
@ ./subroutines/read_sondes_co_4lev_mozaic.pro
@ ./subroutines/read_sondes_co_4lev_mozaic_0_5.pro
@ ./subroutines/read_surface_geos.pro
@ ./subroutines/all_stations_cmdl_geos.pro
@ ./subroutines/all_stations_geos.pro
@ ./subroutines/all_stations_geos_mozaic.pro
@ ./subroutines/all_stations_ships_geos.pro
@ ./subroutines/plot_3lev_o3_geos_3_models.pro
@ ./subroutines/plot_3lev_o3_geos_3_models_mozaic.pro
@ ./subroutines/plot_4lev_co_geos_3_models_mozaic.pro
@ ./subroutines/plot_4lev_o3_geos_3_models.pro
@ ./subroutines/plot_4lev_o3_geos_3_models_mozaic.pro
@ ./subroutines/plot_cmdl_3_models_4_months.pro
@ ./subroutines/plot_gridded_3_vs_data_mozaic_0_5.pro
@ ./subroutines/plot_gridded_C2H6_vs_data_geos_3_models.pro
@ ./subroutines/plot_gridded_C3H8_vs_data_geos_3_models.pro
@ ./subroutines/plot_gridded_CO_vs_data_geos_3_models.pro
@ ./subroutines/plot_gridded_H2O2_vs_data_geos_3_models.pro
@ ./subroutines/plot_gridded_HNO3_vs_data_geos_3_models.pro
@ ./subroutines/plot_gridded_NO_vs_data_geos_3_models.pro
@ ./subroutines/plot_gridded_O3_vs_data_geos_3_models.pro
@ ./subroutines/plot_gridded_PAN_vs_data_geos_3_models.pro
@ ./subroutines/plot_ships_3_models_4_months.pro
@ ./subroutines/plot_ships_3_models_co.pro
@ ./subroutines/plot_station_profiles_geos_3_models.pro
@ ./subroutines/plot_station_profiles_geos_3_models_2_months_selected_0_5.pro
@ ./subroutines/plot_station_profiles_co_3_models_0_5.pro
@ ./subroutines/plot_station_profiles_o3_3_models_0_5.pro
@ ./subroutines/plot_surface_co_geos_3_models.pro
@ ./subroutines/plot_surface_o3_geos_3_models.pro
@ ./subroutines/profiles_1yr.pro
@ ./subroutines/differences_1yr.pro
@ ./subroutines/ratios_1yr.pro
@ ./subroutines/read_diff_range.pro
@ ./subroutines/get_diff_range.pro
@ ./subroutines/plot_seas_improve_geos_3_models.pro
@ ./subroutines/plot_scatter_improve_geos_3_models.pro
@ ./subroutines/plot_scatter_pm25_geos_3_models.pro
@ ./subroutines/map_pm25_geos_3_models.pro
@ ./subroutines/get_ratio_range.pro
@ ./subroutines/get_conc_range.pro
@ ./subroutines/read_ratio_range.pro
@ ./subroutines/read_conc_range.pro
@ ./subroutines/is_aerosol.pro
@ ./subroutines/convert_to_ugm3.pro
@ ./subroutines/differences_aod_1yr.pro
@ ./subroutines/maps_aod_1yr.pro
@ ./subroutines/satellite_bro_cloud.pro
@ ./subroutines/list_with_path.pro
@ ./subroutines/break_path.pro
@ ./subroutines/concat_dir.pro
@ ./subroutines/emission_ratios_1yr.pro
@ ./subroutines/emission_differences_1yr.pro
@ ./subroutines/emission_maps_1yr.pro
@ ./subroutines/emission_totals_1yr.pro
@ ./subroutines/zonal_diff_1yr.pro
@ ./subroutines/jv_differences_1yr.pro
@ ./subroutines/jv_maps_1yr.pro
@ ./subroutines/jv_ratios_1yr.pro
@ ./subroutines/strat_diff_1yr.pro
@ ./subroutines/comp_osiris_o3_3levs.pro
@ ./subroutines/geoschem_aerosol_burden.pro
@ ./subroutines/geoschem_aerosol_burden_gchp.pro
@ ./subroutines/geoschem_mean_aod.pro
@ ./subroutines/aerosol_vertical_profiles.pro
@ ./subroutines/cloud_differences_1yr.pro
@ ./subroutines/pan_profiles.pro
@ ./subroutines/geoschem_o3_ste_flux.pro
@ ./subroutines/maps_1yr.pro
@ ./subroutines/zonal_1yr.pro

;------------------------------------------------------------------------------

pro BenchMark_1yr, InputFile,                                                $
                   RunName=RunName,               PSDir=PSDir,               $
                   Do_Ox=Do_Ox,                   Do_CO=Do_CO,               $
                   Do_MOZAIC=Do_MOZAIC,           Do_Aircraft=Do_Aircraft,   $
                   Debug=Debug,                   Do_Profiles=Do_Profiles,   $
                   Do_Diffs=Do_Diffs,             Do_Ratios=Do_Ratios,       $
                   Do_Emissions=Do_emissions,     Do_Aerosol=Do_Aerosol,     $
                   DynRange=DynRange,             Do_BrO=Do_BrO,             $
                   Do_AOD=Do_AOD,                 Do_Strat=Do_Strat,         $ 
                   Do_Zonal_Diffs=Do_Zonal_Diffs, Do_JValues=Do_JValues,     $
                   Do_Cloud_Diffs=Do_Cloud_Diffs, Do_PAN=Do_PAN,             $
                   Do_PDF=Do_PDF,                 Do_GCHP=Do_GCHP,           $
                   Do_STE=Do_STE,                 Do_Maps=Do_Maps,           $
                   Do_Zonal_Conc=Do_Zonal_Conc,   nVersions=nVersions,       $
                   _EXTRA=e

   ;====================================================================
   ; Initialization -- read default parameters from input file 
   ;====================================================================

   ; Define number of tracers
   numTracers = 157

   ; Save original color table
   TvLct, R, G, B, /Get

   ; Load a color table with the old MYCT drawing colors
   MyCt, /WhGrYlRd, /Bright_Colors

   ; Print debug output
   Debug       = Keyword_Set( Debug         )

   ; Keywords for Ox, CO, MOZAIC, Aircraft, Lon Profile, and difference plots
   Do_Ox          = Keyword_Set( Do_Ox          )
   Do_CO          = Keyword_Set( Do_CO          )
   Do_MOZAIC      = Keyword_Set( Do_MOZAIC      )
   Do_Aircraft    = Keyword_Set( Do_Aircraft    )
   Do_Maps        = keyword_Set( Do_Maps        )
   Do_Diffs       = Keyword_Set( Do_Diffs       )
   Do_Ratios      = Keyword_Set( Do_Ratios      )
   Do_Profiles    = Keyword_Set( Do_Profiles    )
   Do_Zonal_Conc  = keyword_Set( Do_Zonal_Conc  )
   Do_Zonal_Diffs = Keyword_Set( Do_Zonal_Diffs )
   Do_Strat       = Keyword_Set( Do_Strat       )
   Do_Emissions   = Keyword_Set( Do_Emissions   )
   Do_Aerosol     = Keyword_Set( Do_Aerosol     )
   Do_AOD         = Keyword_Set( Do_AOD         )
   Do_JValues     = Keyword_Set( Do_JValues     )
   Do_Cloud_Diffs = Keyword_Set( Do_Cloud_Diffs )
   Do_BrO         = Keyword_Set( Do_BrO         )
   Do_PAN         = Keyword_Set( Do_PAN         )
   ;---------------------------------------------------------------
   ; Now always save output as PDF (Bob Yantosca, 29 Jan 2024)
   ;Do_PDF         = Keyword_Set( Do_PDF         )
   Do_PDF         = 1
   ;---------------------------------------------------------------
   Do_GCHP        = Keyword_Set( Do_GCHP        )
   Do_STE         = Keyword_Set( Do_STE         )
   DynRange       = Keyword_Set( DynRange       )
   
   ; Define suffix for plots
   if ( DynRange )                    $
      then PsSuffix = '.dyn_range.ps' $
      else PsSuffix = '.ps'       

   ; Default number of versions to compare
   if ( N_Elements( nVersions ) eq 0 ) then nVersions = 2
   
   ; Default input file
   if ( N_Elements( InputFile ) eq 0 ) then InputFile = 'input/input.1yr'

   ; Open the input file
   Open_File, InputFile, Ilun, /Get_Lun

   ; Define string variable
   Line = ''

   if ( nVersions eq 2 ) then begin

      Print, '=== COMPARING 2 VERSIONS ==='

      ; Loop thru file
      while ( not EOF( Ilun ) ) do begin
      
         ; Read line and break on colon
         ReadF, Ilun, Line
         Result = StrTrim( StrBreak( Line, ':' ), 2 )
      
         ; Parse value of line into individual variables
         case ( StrUpCase( Result[0] ) ) of 
            'V1' : Vers1  = Result[1]
            'V2' : Vers2  = Result[1]
            'D1' : Dir1   = Add_Separator( Result[1] )
            'D2' : Dir2   = Add_Separator( Result[1] )
            'L1' : Label1 = Result[1]
            'L2' : Label2 = Result[1]
            'M1' : Model1 = Result[1]
            'M2' : Model2 = Result[1]
            'R1' : Res1   = Long( Result[1] )
            'R2' : Res2   = Long( Result[1] )
            'Y1' : Year1  = Result[1]
            'Y2' : Year2  = Result[1]
            else : ; Do nothing
         endcase
      endwhile

   endif else begin
      
      Print, '=== COMPARING 3 VERSIONS ==='

      ; Loop thru file
      while ( not EOF( Ilun ) ) do begin
      
         ; Read line and break on colon
         ReadF, Ilun, Line
         Result = StrTrim( StrBreak( Line, ':' ), 2 )
      
         ; Parse value of line into individual variables
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

   endelse
      
   ; Close input file
   Close,    Ilun
   Free_Lun, Ilun

   ;====================================================================
   ; Define some plotting variables
   ;====================================================================

   ; For 1st model
   PREF1      = Dir1
   ModelInfo1 = CTM_Type( Model1, Res=Res1 )
   GridInfo1  = CTM_Grid( ModelInfo1 )
   Dlat1      = GridInfo1.DJ
   Dlon1      = GridInfo1.DI
   Ptop1      = GridInfo1.Pedge[ GridInfo1.LMX ]
   Nalt1      = GridInfo1.LMX

   ; For 2nd model
   PREF2      = Dir2
   ModelInfo2 = CTM_Type( Model2, Res=Res2 )
   GridInfo2  = CTM_Grid( ModelInfo2 )
   Dlat2      = GridInfo2.DJ
   Dlon2      = GridInfo2.DI
   Ptop2      = GridInfo2.Pedge[ GridInfo2.LMX ]
   Nalt2      = GridInfo2.LMX

   if ( nVersions eq 3 ) then begin
   ; For 3rd model
   PREF3      = Dir3
   ModelInfo3 = CTM_Type( Model3, Res=Res3 )
   GridInfo3  = CTM_Grid( ModelInfo3 )
   Dlat3      = GridInfo3.DJ
   Dlon3      = GridInfo3.DI
   Ptop3      = GridInfo3.Pedge[ GridInfo3.LMX ]
   Nalt3      = 47L;GridInfo3.LMX
   endif else begin
   PREF3      = 'None'
   ; Use info from version 2 to avoid errors
   ModelInfo3 = CTM_Type( Model2, Res=Res2 )
   GridInfo3  = CTM_Grid( ModelInfo2 )
   Dlat3      = GridInfo2.DJ
   Dlon3      = GridInfo2.DI
   Ptop3      = GridInfo2.Pedge[ GridInfo2.LMX ]
   Nalt3      = GridInfo2.LMX
   endelse
   
   ; Create plot title for top of page
   Title1  = 'Red: '   + Strtrim( String( Vers1 ), 2 ) + ' (' $
                       + Strtrim( String( Year1 ), 2 ) + ')'
   Title2  = 'Green: ' + Strtrim( String( Vers2 ), 2 ) + ' (' $
                       + Strtrim( String( Year2 ), 2 ) + ')'
   if ( nVersions eq 2 ) then begin
   Title   = Title1 + ';  ' + Title2  
   endif else begin
   Title3  = 'Blue: '  + Strtrim( String( Vers3 ), 2 ) + ' (' $
                       + Strtrim( String( Year3 ), 2 ) + ')'
   Title   = Title1 + ';  ' + Title2 + ';  ' + Title3
   endelse
   
   ; RUNNAME is the tag for the PostScript files
   if ( N_Elements( RunName ) eq 0 ) then begin
      if ( nVersions eq 2 ) then RunName = Vers2 $
                            else RunName = Vers3
   endif
   
   ; Redirect PostScript output (end w/ slash)
   if ( N_Elements( PSDir ) eq 0 ) then PSDir   = './output/'

   ;=================================================================
   ; Debug section -- print out values
   ;=================================================================
   if ( Debug ) then begin
      
      ; 1st model
      print, '%%% 1st model %%%'
      print, 'Vers1  : ', Vers1
      print, 'Dir1   : ', Dir1
      print, 'Label1 : ', Label1
      print, 'Model1 : ', Model1
      print, 'Res1   : ', Res1
      print, 'Year1  : ', Year1
      print, 'Pref1  : ', Pref1
      print, 'Dlat1  : ', Dlat1
      print, 'Dlon1  : ', Dlon1
      print, 'Ptop1  : ', Ptop1    
      print, 'Nalt1  : ', Nalt1

      ; 2nd model
      print
      print, '%%% 2nd model %%%'
      print, 'Vers2  : ', Vers2
      print, 'Dir2   : ', Dir2
      print, 'Label2 : ', Label2
      print, 'Model2 : ', Model2
      print, 'Res2   : ', Res2
      print, 'Year2  : ', Year2
      print, 'Pref2  : ', Pref2
      print, 'Dlat2  : ', Dlat2
      print, 'Dlon2  : ', Dlon2
      print, 'Ptop2  : ', Ptop2    
      print, 'Nalt2  : ', Nalt2

      if ( nVersions eq 3 ) then begin
      ; 3rd model
      print
      print, '%%% 3rd model %%%'
      print, 'Vers3  : ', Vers3
      print, 'Dir3   : ', Dir3
      print, 'Label3 : ', Label3
      print, 'Model3 : ', Model3
      print, 'Res3   : ', Res3
      print, 'Year3  : ', Year3
      print, 'Pref3  : ', Pref3
      print, 'Dlat3  : ', Dlat3
      print, 'Dlon3  : ', Dlon3
      print, 'Ptop3  : ', Ptop3    
      print, 'Nalt3  : ', Nalt3
      endif
      
   endif

   ;====================================================================
   ; Set links to the proper diaginfo.dat, tracerinfo.dat files
   ;====================================================================

;   ; Remove existing file links
;   Cmd = 'rm -f diaginfo.dat tracerinfo.dat'
;   if ( Debug ) then begin
;      print 
;      print, Cmd
;   endif
;   Spawn, Cmd
;
;   if ( nVersions eq 2 ) then begin
;
;      ; Link to the diaginfo.dat file
;      Cmd = 'ln -s ' + StrTrim( Dir2, 2 )+ '../diaginfo.dat .'
;      if ( Debug ) then print, Cmd
;      Spawn, Cmd
;
;      ; Link to the tracerinfo.dat file
;      Cmd = 'ln -s ' + StrTrim( Dir2, 2 )+ '../tracerinfo.dat .'
;      if ( Debug ) then print, Cmd
;      Spawn, Cmd
;      
;   endif else begin
;   
;      ; Link to the diaginfo.dat file
;      Cmd = 'ln -s ' + StrTrim( Dir3, 2 )+ '../diaginfo.dat .'
;      if ( Debug ) then print, Cmd
;      Spawn, Cmd
;
;      ; Link to the tracerinfo.dat file
;      Cmd = 'ln -s ' + StrTrim( Dir3, 2 )+ '../tracerinfo.dat .'
;      if ( Debug ) then print, Cmd
;      Spawn, Cmd
;
;   endelse
   
   ;====================================================================
   ; Create species concentration maps at surface and 500 hPa
   ;====================================================================

   if ( Do_Maps ) then begin

      ; Echo info
      print, 'MAPS: Creating species concentration maps at sfc and 500 hPa'

      ; Define tracers
      Tracers  = LIndGen(numTracers) + 1L

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName = PSDir + 'Concentrations_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Maps_1yr, Files, Tracers, Versions, Do_GCHP,          $
                Month='Jan', /PS, OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName = PSDir + 'Concentrations_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Maps_1yr, Files, Tracers, Versions, Do_GCHP,          $
                Month='Apr', /PS, OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 


      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName = PSDir + 'Concentrations_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Maps_1yr, Files, Tracers, Versions, Do_GCHP,          $
                Month='Jul', /PS, OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName = PSDir + 'Concentrations_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Maps_1yr, Files, Tracers, Versions, Do_GCHP,          $
                Month='Oct', /PS, OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Create difference maps at surface and 500 hPa
   ;====================================================================

   if ( Do_Diffs ) then begin

      ; Echo info
      print, 'DIFFERENCES: Creating difference maps at sfc and 500 hPa'

      ; Read the default plot ranges 
      Read_Diff_Range, 'input/diff_range_w_ugm3.1yr'

      ; Define tracers
      Tracers  = LIndGen(numTracers) + 1L

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName = PSDir + 'Differences_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_1yr, Files, Tracers, Versions, Do_GCHP,          $
                       Month='Jan', DynRange=DynRange,             $
                       /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName = PSDir + 'Differences_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_1yr, Files, Tracers, Versions, Do_GCHP,          $
                       Month='Apr', DynRange=DynRange,             $
                       /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName = PSDir + 'Differences_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_1yr, Files, Tracers, Versions, Do_GCHP,          $
                       Month='Jul', DynRange=DynRange,             $
                       /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName = PSDir + 'Differences_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_1yr, Files, Tracers, Versions, Do_GCHP,          $
                       Month='Oct', DynRange=DynRange,             $
                       /PS,         OutFileName=PSName, _EXTRA=e


      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Create ratio maps at surface and 500 hPa
   ;====================================================================

   if ( Do_Ratios ) then begin

      ; Read the default plot ranges 
      Read_Ratio_Range, 'input/ratio_range_w_ugm3.1yr'

      ; Echo info
      print, 'RATIOS: Creating ratio maps at sfc and 500 hPa'

      ; Define tracers
      Tracers  = LIndGen(numTracers) + 1L

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName   = PSDir + 'Ratios_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Ratios_1yr, Files, Tracers, Versions, Do_GCHP,               $
                  Month='Jan', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName   = PSDir + 'Ratios_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Ratios_1yr, Files, Tracers, Versions, Do_GCHP,               $
                  Month='Apr', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e


      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName   = PSDir + 'Ratios_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Ratios_1yr, Files, Tracers, Versions, Do_GCHP,               $
                  Month='Jul', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName   = PSDir + 'Ratios_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Ratios_1yr, Files, Tracers, Versions, Do_GCHP,               $
                  Month='Oct', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Create longitudinal profile plots
   ;====================================================================

   if ( Do_Profiles ) then begin

      ; Echo info
      print, 'PROFILES: Creating longitudinal profiles of data'

      ; Read the default plot ranges 
      Read_Diff_Range, 'input/diff_range_w_ugm3.1yr'

      ; Define tracers
      Tracers  = LIndGen(numTracers) + 1L

      ; Altitude range
      AltRange = [ 0, 20 ]

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------
      
      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName = PSDir + 'Lon_Profiles_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Profiles_1yr, Files, AltRange, Tracers, Versions, Do_GCHP, $
                    Month='Jan', DynRange=DynRange, /PS,         $
                    OutFileName = PSName, _EXTRA = e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName = PSDir + 'Lon_Profiles_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Profiles_1yr, Files, AltRange, Tracers, Versions, Do_GCHP, $
                    Month='Apr', DynRange=DynRange, /PS,         $
                    OutFileName = PSName, _EXTRA = e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName = PSDir + 'Lon_Profiles_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Profiles_1yr, Files, AltRange, Tracers, Versions, Do_GCHP, $
                    Month='Jul', DynRange=DynRange, /PS,         $
                    OutFileName = PSName, _EXTRA = e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName = PSDir + 'Lon_Profiles_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Profiles_1yr, Files, AltRange, Tracers, Versions, Do_GCHP, $
                    Month='Oct', DynRange=DynRange, /PS,         $
                    OutFileName = PSName, _EXTRA = e

      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Create zonal mean concentration maps
   ;====================================================================

   if ( Do_Zonal_Conc ) then begin

      ; Echo info
      print, 'ZONAL MAPS: Creating zonal mean concentration maps'

      ; Define tracers
      Tracers  = LIndGen(numTracers) + 1L

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]
      
      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName = PSDir + 'Zonal_Concentrations_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Zonal_1yr, Files, Tracers, Versions, Do_GCHP, Month='Jan', /PS, $
                 OutFileName=PSName, /Pressure, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName   = PSDir + 'Zonal_Concentrations_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Zonal_1yr, Files, Tracers, Versions, Do_GCHP, Month='Apr', /PS, $
                 OutFileName=PSName, /Pressure, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName   = PSDir + 'Zonal_Concentrations_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Zonal_1yr, Files, Tracers, Versions, Do_GCHP, Month='Jul', /PS, $
                 OutFileName=PSName, /Pressure, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName   = PSDir + 'Zonal_Concentrations_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Zonal_1yr, Files, Tracers, Versions, Do_GCHP, Month='Oct', /PS, $
                 OutFileName=PSName, /Pressure, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Create zonal mean difference maps
   ;====================================================================

   if ( Do_Zonal_Diffs ) then begin

      ; Echo info
      print, 'ZONAL DIFFERENCES: Creating zonal mean difference maps'

      ; Read the default plot ranges 
      Read_Diff_Range, 'input/diff_range_w_ugm3.1yr'

      ; Define tracers
      Tracers  = LIndGen(numTracers) + 1L

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]
      
      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName = PSDir + 'Zonal_Differences_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Zonal_Diff_1yr, Files, Tracers, Versions, Do_GCHP,           $
                       Month='Jan',   DynRange=DynRange,           $
                       /PS,           OutFileName=PSName,          $
                       /Pressure,     _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName   = PSDir + 'Zonal_Differences_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Zonal_Diff_1yr, Files, Tracers, Versions, Do_GCHP,           $
                       Month='Apr',   DynRange=DynRange,           $
                       /PS,           OutFileName=PSName,          $
                       /Pressure,     _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName   = PSDir + 'Zonal_Differences_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Zonal_Diff_1yr, Files, Tracers, Versions, Do_GCHP,           $
                       Month='Jul',   DynRange=DynRange,           $
                       /PS,           OutFileName=PSName,          $
                       /Pressure,     _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName   = PSDir + 'Zonal_Differences_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Zonal_Diff_1yr, Files, Tracers, Versions, Do_GCHP,           $
                       Month='Oct',   DynRange=DynRange,           $
                       /PS,           OutFileName=PSName,          $
                       /Pressure,     _EXTRA=e

   endif

   ;====================================================================
   ; Create stratospheric maps
   ;====================================================================

   if ( Do_Strat ) then begin

      ; Read the default plot ranges 
      Read_Diff_Range, 'input/diff_range_w_ugm3.1yr'

      ; Echo info
      print, 'STRAT: Creating stratospheric zonal mean difference maps'

      ; Define tracers
      Tracers  = LIndGen(numTracers) + 1L

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName = PSDir + 'Strat_Differences_Jan.' + RunName + PsSuffix

      ; Create the zonal mean difference plots
      Strat_Diff_1yr, Files, Tracers, Versions, Do_GCHP,           $
                  Month='Jan', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e
      
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName = PSDir + 'Strat_Differences_Apr.' + RunName + PsSuffix

      ; Create the zonal mean difference plots
      Strat_Diff_1yr, Files, Tracers, Versions, Do_GCHP,           $
                  Month='Apr', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e
      
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName = PSDir + 'Strat_Differences_Jul.' + RunName + PsSuffix

      ; Create the zonal mean difference plots
      Strat_Diff_1yr, Files, Tracers, Versions, Do_GCHP,           $
                  Month='Jul', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e
      

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName = PSDir + 'Strat_Differences_Oct.' + RunName + PsSuffix

      ; Create the zonal mean difference plots
      Strat_Diff_1yr, Files, Tracers, Versions, Do_GCHP,           $
                  Month='Oct', DynRange=DynRange,                  $
                  /PS,         OutFileName=PSName, _EXTRA=e


      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; OSRIS plots
      ;---------------
      if ( nVersions ne 2 ) then begin

         ; Echo info
         print, 'STRAT: Plotting model vs OSIRIS O3'

         Files    = [ Pref3+'0101.nc', Pref3+'0401.nc', Pref3+'0701.nc', $
                      Pref3+'1001.nc' ]
         PSName   = PSDir + 'Strat.O3.OSIRIS.3lev.' + RunName + PsSuffix

         comp_osiris_o3_3levs, Files, Vers3, Model3, Res3, /PS, $
                               OutFileName = PSName, _Extra = e

         Close,       /All
         Ctm_Cleanup, /No_GC 

      endif

   endif
   
   ;====================================================================
   ; Create maps of emission totals, ratios, and differences
   ;====================================================================

   if ( Do_Emissions ) then begin

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;-----------------
      ; Emission totals
      ;-----------------

      ; Echo info
      print, 'Emissions: Generating emission totals'

      ; Compute table of emissions sums
      OutFile = PSDir + 'Emission_Totals.' + RunName + '.txt'
      Emission_Totals_1yr, Pref1, Pref2, Pref3, Versions, $
                          OutFileName=OutFile, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; January
      ;---------------

      ; Echo info
      print, 'Emissions: Creating emission maps'

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]

      ; Create the emission maps
      Emission_Maps_1yr,        Files,     Versions, /PS, Month='Jan', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ; Create the emission ratio plots
      Emission_Ratios_1yr,      Files,     Versions, /PS, Month='Jan', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ; Create the emission difference plots
      Emission_Differences_1yr, Files,     Versions, /PS, Month='Jan', $
                                Dir=PSDir, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]

      ; Create the emission maps
      Emission_Maps_1yr,        Files,     Versions, /PS, Month='Apr', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC

      ; Create the emission ratio plots
      Emission_Ratios_1yr,      Files,     Versions, /PS, Month='Apr', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ; Create the emission difference plots
      Emission_Differences_1yr, Files,     Versions, /PS, Month='Apr', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]

      ; Create the emission maps
      Emission_Maps_1yr,        Files,     Versions, /PS, Month='Jul', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ; Create the emission ratio plots
      Emission_Ratios_1yr,      Files,     Versions, /PS, Month='Jul', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ; Create the emission difference plots
      Emission_Differences_1yr, Files,     Versions, /PS, Month='Jul', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]

      ; Create the emission maps
      Emission_Maps_1yr,        Files,     Versions, /PS, Month='Oct', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ; Create the emission ratio plots
      Emission_Ratios_1yr,      Files,     Versions, /PS, Month='Oct', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ; Create the emission ratio plots
      Emission_Differences_1yr, Files,     Versions, /PS, Month='Oct', $
                                Dir=PSDir, _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif

   if ( Do_AOD ) then begin

      ;=================================================================
      ; Create AOD difference maps at surface and 500 hPa
      ;=================================================================

      ; Echo info
      print, 'AOD DIFFERENCES: Creating AOD difference maps'

      ; Read the default plot ranges 
      Read_Diff_Range, 'input/aod_diff_range.1yr'

      ; Define tracers
      Tracers  = [ 4, 6, 9, 12, 15, 18 ]

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName = PSDir + 'AOD_Differences_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_AOD_1yr, Files, Tracers, Versions,                   $
                           Month='Jan', DynRange=DynRange,             $
                           /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName = PSDir + 'AOD_Differences_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_AOD_1yr, Files, Tracers, Versions,                   $
                           Month='Apr', DynRange=DynRange,             $
                           /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName = PSDir + 'AOD_Differences_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_AOD_1yr, Files, Tracers, Versions,                   $
                           Month='Jul', DynRange=DynRange,             $
                           /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName = PSDir + 'AOD_Differences_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Differences_AOD_1yr, Files, Tracers, Versions,                   $
                           Month='Oct', DynRange=DynRange,             $
                           /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;=================================================================
      ; Create AOD difference maps at surface and 500 hPa
      ;=================================================================

      ; Echo info
      print, 'AOD MAPS: Creating AOD concentration maps'

      ; Read the default plot ranges 
      Read_Conc_Range, 'input/aod_conc_range.1yr'

      ; Define tracers
      Tracers  = [ 4, 6, 9, 12, 15, 18 ]

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName = PSDir + 'AOD_Maps_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Maps_AOD_1yr, Files, Tracers, Versions,                   $
                    Month='Jan', DynRange=DynRange,             $
                    /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName = PSDir + 'AOD_Maps_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Maps_AOD_1yr, Files, Tracers, Versions,                   $
                    Month='Apr', DynRange=DynRange,             $
                    /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName = PSDir + 'AOD_Maps_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Maps_AOD_1yr, Files, Tracers, Versions,                   $
                    Month='Jul', DynRange=DynRange,             $
                    /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName = PSDir + 'AOD_Maps_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Maps_AOD_1yr, Files, Tracers, Versions,                   $
                    Month='Oct', DynRange=DynRange,             $
                    /PS,         OutFileName=PSName, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;=================================================================
      ; Global mean AOD
      ;=================================================================
      Print, 'AOD: Computing global mean AOD'
      if ( nVersions eq 3 ) then begin
      
         OutFile = PSDir + 'Global_Mean_AOD.' + RunName + '.txt'
         geoschem_mean_aod, Pref3, Vers3, Model3, Res3, $
                            OutFileName=OutFile, _EXTRA=e

         Close,       /All
         Ctm_Cleanup, /No_GC 

      endif else begin
      
         OutFile = PSDir + 'Global_Mean_AOD.' + RunName + '.txt'
         geoschem_mean_aod, Pref2, Vers2, Model2, Res2, $
                            OutFileName=OutFile, _EXTRA=e

      endelse

   endif

   ;====================================================================
   ; Create J-value maps at surface and 500 hPa
   ;====================================================================

   if ( Do_JValues ) then begin

      ; Echo info
      print, 'J-values: Creating J-value maps'

;      ; Read the default plot ranges 
;      Read_Diff_Range, 'input/aod_diff_range.1yr'

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]

      ; Create the J-value plots
      PSName   = PSDir + 'JValue_Maps_Jan.' + RunName + PsSuffix
      JV_Maps_1yr,        Files, Versions, Month='Jan',                  $
                          OutFileName=PSName, /PS, _EXTRA=e
      PSName   = PSDir + 'JValue_Ratios_Jan.' + RunName + PsSuffix
      JV_Ratios_1yr,      Files, Versions, Month='Jan',                  $
                          OutFileName=PSName, /PS, _EXTRA=e
      PSName   = PSDir + 'JValue_Differences_Jan.' + RunName + PsSuffix
      JV_Differences_1yr, Files, Versions, Month='Jan',                  $
                          OutFileName=PSName, /PS, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]

      ; Create the J-value plots
      PSName   = PSDir + 'JValue_Maps_Apr.' + RunName + PsSuffix
      JV_Maps_1yr,        Files, Versions, Month='Apr',                  $
                          OutFileName=PSName, /PS, _EXTRA=e
      PSName   = PSDir + 'JValue_Ratios_Apr.' + RunName + PsSuffix
      JV_Ratios_1yr,      Files, Versions, Month='Apr',                  $
                          OutFileName=PSName, /PS, _EXTRA=e
      PSName   = PSDir + 'JValue_Differences_Apr.' + RunName + PsSuffix
      JV_Differences_1yr, Files, Versions, Month='Apr',                  $
                          OutFileName=PSName, /PS, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]

      ; Create the J-value plots
      PSName   = PSDir + 'JValue_Maps_Jul.' + RunName + PsSuffix
      JV_Maps_1yr,        Files, Versions, Month='Jul',                  $
                          OutFileName=PSName, /PS, _EXTRA=e
      PSName   = PSDir + 'JValue_Ratios_Jul.' + RunName + PsSuffix
      JV_Ratios_1yr,      Files, Versions, Month='Jul',                  $
                          OutFileName=PSName, /PS, _EXTRA=e
      PSName   = PSDir + 'JValue_Differences_Jul.' + RunName + PsSuffix
      JV_Differences_1yr, Files, Versions, Month='Jul',                  $
                          OutFileName=PSName, /PS, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]

      ; Create the J-value plots
      PSName   = PSDir + 'JValue_Maps_Oct.' + RunName + PsSuffix
      JV_Maps_1yr,        Files, Versions, Month='Oct',                  $
                          OutFileName=PSName, /PS, _EXTRA=e
      PSName   = PSDir + 'JValue_Ratios_Oct.' + RunName + PsSuffix
      JV_Ratios_1yr,      Files, Versions, Month='Oct',                  $
                          OutFileName=PSName, /PS, _EXTRA=e
      PSName   = PSDir + 'JValue_Differences_Oct.' + RunName + PsSuffix
      JV_Differences_1yr, Files, Versions, Month='Oct',                  $
                          OutFileName=PSName, /PS, _EXTRA=e

      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Create cloud difference maps
   ;====================================================================

   if ( Do_Cloud_Diffs ) then begin

      ; Echo info
      print, 'CLOUD DIFFERENCES: Creating cloud difference maps'

      ; Define tracers
      Tracers  = [ 1, 2 ]

      ; Version numbers
      if ( nVersions eq 2 ) then Versions = [ Vers1, Vers2 ] $
                            else Versions = [ Vers1, Vers2, Vers3 ]

      ;---------------
      ; January
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0101.nc', Pref2+'0101.nc' ] $
         else Files = [ Pref1+'0101.nc', Pref2+'0101.nc', Pref3+'0101.nc' ]
      PSName   = PSDir + 'Cloud_Differences_Jan.' + RunName + PsSuffix

      ; Create the profile plots
      Cloud_Differences_1yr, Files, Tracers, Versions,               $
                             Month='Jan',    DynRange=DynRange,      $
                             /PS,            OutFileName=PSName,     $
                             /Pressure,      _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; April
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0401.nc', Pref2+'0401.nc' ] $
         else Files = [ Pref1+'0401.nc', Pref2+'0401.nc', Pref3+'0401.nc' ]
      PSName   = PSDir + 'Cloud_Differences_Apr.' + RunName + PsSuffix

      ; Create the profile plots
      Cloud_Differences_1yr, Files, Tracers, Versions,               $
                             Month='Apr',    DynRange=DynRange,      $
                             /PS,            OutFileName=PSName,     $
                             /Pressure,      _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 


      ;---------------
      ; July
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'0701.nc', Pref2+'0701.nc' ] $
         else Files = [ Pref1+'0701.nc', Pref2+'0701.nc', Pref3+'0701.nc' ]
      PSName   = PSDir + 'Cloud_Differences_Jul.' + RunName + PsSuffix

      ; Create the profile plots
      Cloud_Differences_1yr, Files, Tracers, Versions,               $
                             Month='Jul',    DynRange=DynRange,      $
                             /PS,            OutFileName=PSName,     $
                             /Pressure,      _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;---------------
      ; October
      ;---------------

      ; Input & output files
      if ( nVersions eq 2 ) $
         then Files = [ Pref1+'1001.nc', Pref2+'1001.nc' ] $
         else Files = [ Pref1+'1001.nc', Pref2+'1001.nc', Pref3+'1001.nc' ]
      PSName   = PSDir + 'Cloud_Differences_Oct.' + RunName + PsSuffix

      ; Create the profile plots
      Cloud_Differences_1yr, Files, Tracers, Versions,               $
                             Month='Oct',    DynRange=DynRange,      $
                             /PS,            OutFileName=PSName,     $
                             /Pressure,      _EXTRA=e
      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif
   
   ;====================================================================
   ; Plot models vs. ozonesonde data
   ;====================================================================

   if ( Do_Ox ) then begin
      
      ;-----------------------------------------------------------------
      ; %%%%% SEASONAL CYCLE PLOTS %%%%%
      ;-----------------------------------------------------------------
      print, 'IDL_OZONE: Plot models vs. ozonesonde data - seasonal cycle'

      ;-----------------------------------------------------------------
      Print, '-----Seasonal cycle at 3 pressure levels'
      ;-----------------------------------------------------------------
      FilEst       = 'data/netCDF/Sites.O3.prof.1'
      PSName       = PSDir + 'O3.seascycle.3lev.ps'
      Max_Stations = 35

      Plot_3lev_o3_geos_3_models, Pref1, Ptop1,  Dlat1, Dlon1, Model1, Year1, $
                                  Pref2, Ptop2,  Dlat2, Dlon2, Model2, Year2, $
                                  Pref3, Ptop3,  Dlat3, Dlon3, Model3, Year3, $
                                  Title, PSName, Max_Stations, FilEst, $
                                  Do_GCHP

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;-----------------------------------------------------------------
      print, '-----Seasonal cycle at 3 levels at res.kag.sam'
      ;-----------------------------------------------------------------
      FilEst       = 'data/netCDF/Sites.O3.prof.res.kag.sam'
      PSName       = PSDir + 'O3.seascycle.3lev.3page.ps'
      Max_Stations = 12

      Plot_3lev_o3_geos_3_models, Pref1, Ptop1,  Dlat1, Dlon1, Model1, Year1, $
                                  Pref2, Ptop2,  Dlat2, Dlon2, Model2, Year2, $
                                  Pref3, Ptop3,  Dlat3, Dlon3, Model3, Year3, $
                                  Title, PSName, Max_Stations, FilEst, $
                                  Do_GCHP

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;-----------------------------------------------------------------
      print, '-----Seasonal cycle at 4 levels'
      ;-----------------------------------------------------------------
      FilEst       = 'data/netCDF/Sites.O3.prof.trop'
      PSName       = PSDir + 'O3.seascycle.4lev.ps'
      Max_Stations = 8
      Plot_4lev_o3_geos_3_models, Pref1, Ptop1,  Dlat1, Dlon1, Model1, Year1, $
                                  Pref2, Ptop2,  Dlat2, Dlon2, Model2, Year2, $
                                  Pref3, Ptop3,  Dlat3, Dlon3, Model3, Year3, $
                                  Title, PSName, Max_Stations, FilEst, $
                                  Do_GCHP

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;-----------------------------------------------------------------
      print, '-----Seasonal cycle at 4 levels at kag'
      ;-----------------------------------------------------------------
      FilEst       = 'data/netCDF/Sites.O3.prof.kag'
      PSName       = PsDir + 'O3.seascycle.4lev.kag.ps'
      Max_Stations = 4
      Plot_4lev_o3_geos_3_models, Pref1, Ptop1,  Dlat1, Dlon1, Model1, Year1, $
                                  Pref2, Ptop2,  Dlat2, Dlon2, Model2, Year2, $
                                  Pref3, Ptop3,  Dlat3, Dlon3, Model3, Year3, $
                                  Title, PSName, Max_Stations, FilEst, $
                                  Do_GCHP

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;-----------------------------------------------------------------
      Print, '-----Ozonesonde profile'
      ;-----------------------------------------------------------------
      FilEst       = 'data/netCDF/Sites.O3.prof.1'
      PSName       = PSDir + 'O3.profiles.geos.ps'
      Max_Station  = 35
      Plot_station_profiles_geos_3_models, 1,4,7,10,$
                                  Pref1, Ptop1, Dlat1, Dlon1, Model1, Year1, $
                                  Pref2, Ptop2, Dlat2, Dlon2, Model2, Year2, $
                                  Pref3, Ptop3, Dlat3, Dlon3, Model3, Year3, $
                                  Title, PSName, Max_Station, FilEst, $
                                  Do_GCHP
      
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;-----------------------------------------------------------------------
      ; We normally don't plot this, leave the call here though (bmy, 10/20/03)
      ; 6th plot
      ;FilEst='/home/iam/netCDF/Sites.O3.prof.res.kag.sam'
      ;PSName = PSDir + 'O3.profiles.3page.geos.ps'
      ;Max_Station=12
      ;plot_station_profiles_geos_3_models, 1,4,7,10,$
      ;                                pref1, ptop1, dlat1, dlon1, nalt1, $
      ;                                pref2, ptop2, dlat2, dlon2, nalt2, $
      ;                                pref3, ptop3, dlat3, dlon3, nalt3, $
      ;                                title, PSName, Max_Station, FilEst
      ;------------------------------------------------------------------------

      ;-----------------------------------------------------------------
      ; %%%%% PLOTS OF MODELS vs. STATION & SURFACE DATA %%%%%
      ;-----------------------------------------------------------------
      print, 'IDL_OZONE_2: Plot models vs. station and surface data'

      ;-----------------------------------------------------------------
      print, '-----Station profiles'
      ;-----------------------------------------------------------------
      FilEst       = 'data/netCDF/Sites.O3.prof.res.kag.sam'
      PSName       = PSDir + 'O3.profiles.3page.geos.ps'
      Max_Stations = 12 
      Plot_station_profiles_geos_3_models, 1,4,7,10,                   $
                                  Pref1, Ptop1,  Dlat1, Dlon1, Model1, Year1, $
                                  Pref2, Ptop2,  Dlat2, Dlon2, Model2, Year2, $
                                  Pref3, Ptop3,  Dlat3, Dlon3, Model3, Year3, $
                                  Title, PSName, Max_Stations, FilEst, $
                                  Do_GCHP

      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;-----------------------------------------------------------------
      print, '-----Surface ozone'
      ;-----------------------------------------------------------------
      PSName      = PSDir + 'surface.O3.geos.ps'
      Plot_surface_o3_geos_3_models, Pref1, Ptop1, Dlat1, Dlon1, Model1, Year1,$
                                     Pref2, Ptop2, Dlat2, Dlon2, Model2, Year2,$
                                     Pref3, Ptop3, Dlat3, Dlon3, Model3, Year3,$
	                             Title, PSName, Do_GCHP

      ; Close files & cleanup
      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Plot models vs. surface CO data
   ;====================================================================
   if ( Do_CO ) then begin

      ;-----------------------------------------------------------------
      ; %%%%% PLOTS OF MODELS vs. SURFACE CO DATA %%%%%
      ;-----------------------------------------------------------------
      print, 'IDL_CO: Plot models vs. surface CO data'

      ;-----------------------------------------------------------------
      Print, '-----Ground CO short'
      ;-----------------------------------------------------------------
      FilEst    = 'data/netCDF/Sites.ground.CO.short'
      Max_Sta   = 16
      PSName    = PSDir + 'surface.CO.geos.1page.ps'
      Plot_surface_co_geos_3_models, Pref1, Ptop1,  Dlat1,   Dlon1, Model1, Year1, $
                                     Pref2, Ptop2,  Dlat2,   Dlon2, Model2, Year2, $
                                     Pref3, Ptop3,  Dlat3,   Dlon3, Model3, Year3, $
                                     Title, PSName, Max_Sta, FilEst,  Do_GCHP

      ;-----------------------------------------------------------------
      Print, '-----Ground CO 1'
      ;-----------------------------------------------------------------
      FilEst    = 'data/netCDF/Sites.ground.CO.2005'
      Max_Sta   = 42
      PSName    = PSDir + 'surface.CO.geos.ps'
      Plot_surface_co_geos_3_models, Pref1, Ptop1,  Dlat1,   Dlon1, Model1, Year1, $
                                     Pref2, Ptop2,  Dlat2,   Dlon2, Model2, Year2, $
                                     Pref3, Ptop3,  Dlat3,   Dlon3, Model3, Year3, $
                                     Title, PSName, Max_Sta, FilEst,  Do_GCHP

      ;-----------------------------------------------------------------
      ; %%%%% PLOTS OF MODELS vs. CMDL and SHIP CO DATA %%%%%
      ;-----------------------------------------------------------------

      print, 'IDL_CO_2: Plot models vs. CMDL and ship CO data'

      ;-----------------------------------------------------------------
      print, '-----Reading CMDL data'
      ;-----------------------------------------------------------------
      all_stations_cmdl_geos, 'CO', 'SpeciesConcVV_CO', 39, Pref1, Pref1, Year1, $
                                 Ptop1, Dlat1, Dlon1, Model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_cmdl_geos, 'CO', 'SpeciesConcVV_CO', 39, Pref2, Pref1, Year2, $
                                 Ptop2, Dlat2, Dlon2, Model2, '.2'
      endif else begin
      all_stations_cmdl_geos, 'CO', 'SpeciesConcVV_CO', 39, Pref2, Pref2, Year2, $
                                 Ptop2, Dlat2, Dlon2, Model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
      all_stations_cmdl_geos, 'CO', 'SpeciesConcVV_CO', 39, Pref3, Pref3, Year3, $
                                 Ptop3, Dlat3, Dlon3, Model3, '.3'
      endif
      
      ;-----------------------------------------------------------------
      print, '-----CMDL latitudinal distribution'
      ;-----------------------------------------------------------------
      PSName = PSDir + 'cmdl.latdist.ps'
      if ( nVersions eq 2 ) then begin
         Plot_cmdl_3_models_4_months, '.1', '.2', 'None', title, PSName
      endif else begin
         Plot_cmdl_3_models_4_months, '.1', '.2', '.3', title, PSName
      endelse

      ;-----------------------------------------------------------------
      print, '-----Reading ship data'
      ;-----------------------------------------------------------------
      all_stations_ships_geos, $
         'CO','SpeciesConcVV_CO', 13, Pref1, Year1, Ptop1, Dlon1, Dlat1, Model1, '.1'
      all_stations_ships_geos, $
         'CO','SpeciesConcVV_CO', 13, Pref2, Year2, Ptop2, Dlon2, Dlat2, Model2, '.2'
      if ( nVersions eq 3 ) then begin
      all_stations_ships_geos, $
         'CO','SpeciesConcVV_CO', 13, Pref3, Year3, Ptop3, Dlon3, Dlat3, Model3, '.3'
      endif
      
      ;-----------------------------------------------------------------
      print, '-----Ship CO'
      ;-----------------------------------------------------------------
      PSName = PSDir + 'CO.ships.geos.ps'
      if ( nVersions eq 2 ) then begin
         Plot_ships_3_models_co, '.1', '.2', 'None', title, PSName
      endif else begin
         Plot_ships_3_models_co, '.1', '.2', '.3', title, PSName
      endelse
      
      ;-----------------------------------------------------------------
      print, '-----Ship tracks 4 months'
      ;-----------------------------------------------------------------
      PSName = PSDir + 'CO.ships.geos.4.months.ps'
      if ( nVersions eq 2 ) then begin
         Plot_ships_3_models_4_months, '.1', '.2', 'None', title, PSName
      endif else begin
         Plot_ships_3_models_4_months, '.1', '.2', '.3', title, PSName
      endelse
      
      ; Close files & clean up
      Close, /all
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Plot models vs. MOZAIC data
   ;====================================================================
   if ( Do_MOZAIC ) then begin

      print, 'IDL_MOZAIC: Plot models vs. MOZAIC data'

      ;-----------------------------------------------------------------
      print, '-----Reading MOZAIC stations'
      ;-----------------------------------------------------------------

      all_stations_geos_mozaic, 'CO', 'SpeciesConcVV_CO', 36, Pref1, Pref1, $
                                Year1, Ptop1, Dlon1, Dlat1, Model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_geos_mozaic, 'CO', 'SpeciesConcVV_CO', 36, Pref2, Pref1, $
                                Year2, Ptop2, Dlon2, Dlat2, Model2, '.2'
      endif else begin
      all_stations_geos_mozaic, 'CO', 'SpeciesConcVV_CO', 36, Pref2, Pref2, $
                                Year2, Ptop2, Dlon2, Dlat2, Model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
      all_stations_geos_mozaic, 'CO', 'SpeciesConcVV_CO', 36, Pref3, Pref3, $
                                Year3, Ptop3, Dlon3, Dlat3, Model3, '.3'
      endif
      
      ;--------------------------------------------------------
      ; lzh add new locations for MOZAIC CO   04/27/2008
      ;--------------------------------------------------------
      print, '------MOZAIC CO new locations'

      ; (1)  CO profiles
      FilEst  = 'data/netCDF/Sites.CO.prof.mozaic.monthly.new'
      PSName  = PsDir + 'CO.profiles.mozaic.months.15sta.ps'
      Max_Station = 15
      Plot_station_profiles_co_3_models_0_5, 1,     4,     7,     10,      $ 
                                       Pref1, Ptop1, Dlat1, Dlon1, Model1, Year1, $
                                       Pref2, Ptop2, Dlat2, Dlon2, Model2, Year2, $
                                       Pref3, Ptop3, Dlat3, Dlon3, Model3, Year3, $
                                       title, PSName, Max_Station, FilEst, $
                                       Do_GCHP

      ; (2) seasonal plots at 4 levels
      FilEst  = 'data/netCDF/Sites.CO.prof.mozaic.new'
      PSName  = PsDir+'CO.seas.cycle.mozaic.geos.4lev.ps'
      Plot_4lev_co_geos_3_models_mozaic, Pref1, Ptop1,  Dlat1, Dlon1,  Model1, Year1, $
                                         Pref2, Ptop2,  Dlat2, Dlon2,  Model2, Year2, $
                                         Pref3, Ptop3,  Dlat3, Dlon3,  Model3, Year3, $
                                         title, PSName, 15,    FilEst, Do_GCHP

      ;-----------------------------------------------------------------
      print, '-----MOZAIC O3 profile #1'
      ;-----------------------------------------------------------------
      FilEst  = 'data/netCDF/Sites.O3.prof.mozaic.selected'
      PSName  = PSDir+'O3.profiles.mozaic.geos.2.months.selected.ps'
      Max_Station = 8
      Plot_station_profiles_geos_3_models_2_months_selected_0_5, 1,4,7,10,$
                                         Pref1, Ptop1, Dlat1, Dlon1, Model1, Year1, $
                                         Pref2, Ptop2, Dlat2, Dlon2, Model2, Year2, $
                                         Pref3, Ptop3, Dlat3, Dlon3, Model3, Year3, $
                                         title, PSName, Max_Station, FilEst, $
                                         Do_GCHP

      ;-----------------------------------------------------------------
      Print, '-----MOZAIC O3 profile #2' 
      ;-----------------------------------------------------------------
      FilEst  = 'data/netCDF/Sites.O3.prof.mozaic.selected'
      PSName  = PsDir+'O3.seascycle.3lev.3.mozaic.selected.ps'
      Max_Station = 8
      Plot_3lev_o3_geos_3_models_mozaic, Pref1, Ptop1, Dlat1, Dlon1, Model1, Year1, $
                                         Pref2, Ptop2, Dlat2, Dlon2, Model2, Year2, $
                                         Pref3, Ptop3, Dlat3, Dlon3, Model3, Year3, $
                                         title, PSName, Max_Station, FilEst, $
                                         Do_GCHP

      ;-----------------------------------------------------------------
      Print, '-----MOZAIC O3 profile #3'
      ;-----------------------------------------------------------------
      FilEst  = 'data/netCDF/Sites.O3.prof.mozaic.1'
      PSName  = PsDir+'O3.seascycle.3lev.3.mozaic.all.ps'
      Max_Station = 19
      Plot_3lev_o3_geos_3_models_mozaic, Pref1, Ptop1, Dlat1, Dlon1, Model1, Year1, $
                                         Pref2, Ptop2, Dlat2, Dlon2, Model2, Year2, $
                                         Pref3, Ptop3, Dlat3, Dlon3, Model3, Year3, $
                                         title, PSName, Max_Station, FilEst, $
                                         Do_GCHP
      ;-----------------------------------------------------------------
      Print, '-----MOZAIC O3 profile #4'
      ;-----------------------------------------------------------------
      FilEst      = 'data/netCDF/Sites.O3.prof.mozaic.1'
      PSName      = PsDir +'O3.profiles.mozaic.geos.0.5.ps'
      Max_Station = 19
      Plot_station_profiles_o3_3_models_0_5, 1,     4,     7,     10,      $ 
                                       Pref1, Ptop1, Dlat1, Dlon1, Model1, Year1, $
                                       Pref2, Ptop2, Dlat2, Dlon2, Model2, Year2, $
                                       Pref3, Ptop3, Dlat3, Dlon3, Model3, Year3, $
                                       title, PSName, Max_Station, FilEst, $
                                       Do_GCHP

      ;-----------------------------------------------------------------
      Print, '-----MOZAIC O3 profile #5'
      ;-----------------------------------------------------------------
      FilEst      = 'data/netCDF/Sites.O3.prof.mozaic.1'
      PSName      = PsDir + 'O3.seas.cycle.mozaic.geos.ps'
      Plot_4lev_o3_geos_3_models_mozaic, Pref1, Ptop1, Dlat1, Dlon1, Model1, Year1, $
                                         Pref2, Ptop2, Dlat2, Dlon2, Model2, Year2, $
                                         Pref3, Ptop3, Dlat3, Dlon3, Model3, Year3, $
                                         title, PSName, 19, FilEst,  Do_GCHP

      ; Close files & cleanup
      Close, /all
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Plot models vs. aircraft data
   ;====================================================================
   if ( Do_Aircraft ) then begin

      print, 'IDL_AIRCRAFT: Plot models vs. aircraft data'
 
      ;-----------------------------------------------------------------
      print, '-----CO'
      ;-----------------------------------------------------------------

      ; Read stations
      all_stations_geos, 'CO', 'SpeciesConcVV_CO', 44, pref1, pref1, $
                            year1, ptop1, dlon1, dlat1, model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_geos, 'CO', 'SpeciesConcVV_CO', 44, pref2, pref1, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endif else begin
      all_stations_geos, 'CO', 'SpeciesConcVV_CO', 44, pref2, pref2, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
         all_stations_geos, 'CO', 'SpeciesConcVV_CO', 44, pref3, pref3, $
                            year3, ptop3, dlon3, dlat3, model3, '.3'
      endif
      
      ; Plot
      PSName = PSDir + 'aircraft.profile.CO.geos.ps'
      if ( nVersions eq 2 ) then begin
         plot_gridded_CO_vs_data_geos_3_models, $
            'CO', 44, '.1', '.2', 'None', title, PSName, nalt1, nalt2, nalt3
      endif else begin
         plot_gridded_CO_vs_data_geos_3_models, $
            'CO', 44, '.1', '.2', '.3', title, PSName, nalt1, nalt2, nalt3
      endelse
      
      ;-----------------------------------------------------------------
      print, '-----C2H6'
      ;-----------------------------------------------------------------

      ; Read stations
      all_stations_geos, 'C2H6','SpeciesConcVV_C2H6', 41, pref1, pref1, $
                            year1, ptop1, dlon1, dlat1, model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_geos, 'C2H6','SpeciesConcVV_C2H6', 41, pref2, pref1, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endif else begin
      all_stations_geos, 'C2H6','SpeciesConcVV_C2H6', 41, pref2, pref2, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
      all_stations_geos, 'C2H6', 'SpeciesConcVV_C2H6', 41, pref3, pref3, $
                            year3, ptop3, dlon3, dlat3, model3, '.3'
      endif
      
      ; Plot
      PSName = PSDir + 'aircraft.profile.C2H6.geos.ps'
      if ( nVersions eq 2 ) then begin
         plot_gridded_C2H6_vs_data_geos_3_models, $
            'C2H6', 41, '.1', '.2', 'None', title, PSName, nalt1, nalt2, nalt3
      endif else begin
         plot_gridded_C2H6_vs_data_geos_3_models, $
            'C2H6', 41, '.1', '.2', '.3', title, PSName, nalt1, nalt2, nalt3
      endelse 
         
      ;-----------------------------------------------------------------
      print, '-----C3H8'
      ;-----------------------------------------------------------------
      ; Read stations
      all_stations_geos, 'C3H8','SpeciesConcVV_C3H8', 39, pref1, pref1, $
                            year1, ptop1, dlon1, dlat1, model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_geos, 'C3H8','SpeciesConcVV_C3H8', 39, pref2, pref1, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endif else begin
      all_stations_geos, 'C3H8','SpeciesConcVV_C3H8', 39, pref2, pref2, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
      all_stations_geos, 'C3H8', 'SpeciesConcVV_C3H8', 39, pref3, pref3, $
                            year3, ptop3, dlon3, dlat3, model3, '.3'
      endif
      
      ; Plot
      PSName=PSDir + 'aircraft.profile.C3H8.geos.ps'
      if ( nVersions eq 2 ) then begin
         plot_gridded_C3H8_vs_data_geos_3_models, $
            'C3H8', 39, '.1', '.2', 'None', title, PSName, nalt1, nalt2, nalt3
      endif else begin
         plot_gridded_C3H8_vs_data_geos_3_models, $
            'C3H8', 39, '.1', '.2', '.3', title, PSName, nalt1, nalt2, nalt3
      endelse
         
      ;-----------------------------------------------------------------
      print, '-----H2O2'
      ;-----------------------------------------------------------------

      ; Read stations
      all_stations_geos, 'H2O2', 'SpeciesConcVV_H2O2', 30, pref1, pref1, $
                            year1, ptop1, dlon1, dlat1, model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_geos, 'H2O2','SpeciesConcVV_H2O2', 30, pref2, pref1, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endif else begin
      all_stations_geos, 'H2O2','SpeciesConcVV_H2O2', 30, pref2, pref2, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
      all_stations_geos, 'H2O2', 'SpeciesConcVV_H2O2', 30, pref3, pref3, $
                            year3, ptop3, dlon3, dlat3, model3, '.3'
      endif
      
      ; Plot
      PSName = PSDir + 'aircraft.profile.H2O2.geos.ps'
      if ( nVersions eq 2 ) then begin
         plot_gridded_H2O2_vs_data_geos_3_models, $
            'H2O2', 30, '.1', '.2', 'None', title, PSName, nalt1, nalt2, nalt3
      endif else begin
         plot_gridded_H2O2_vs_data_geos_3_models, $
            'H2O2', 30, '.1', '.2', '.3', title, PSName, nalt1, nalt2, nalt3
      endelse
      
      ;-----------------------------------------------------------------
      print, '-----HNO3'
      ;-----------------------------------------------------------------
      ; Read stations
      all_stations_geos, 'HNO3','SpeciesConcVV_HNO3', 39, pref1, pref1, $
                            year1, ptop1, dlon1, dlat1, model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_geos, 'HNO3','SpeciesConcVV_HNO3', 39, pref2, pref1, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endif else begin
      all_stations_geos, 'HNO3','SpeciesConcVV_HNO3', 39, pref2, pref2, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
      all_stations_geos, 'HNO3', 'SpeciesConcVV_HNO3', 39, pref3, pref3, $
                            year3, ptop3, dlon3, dlat3, model3, '.3'
      endif
      
      ; Plot
      PSName = PSDir + 'aircraft.profile.HNO3.geos.ps'
      if ( nVersions eq 2 ) then begin
         plot_gridded_HNO3_vs_data_geos_3_models, $
            'HNO3', 39, '.1', '.2', 'None', title, PSName, nalt1, nalt2, nalt3
      endif else begin
         plot_gridded_HNO3_vs_data_geos_3_models, $
            'HNO3', 39, '.1', '.2', '.3', title, PSName, nalt1, nalt2, nalt3
      endelse
         
      ;-----------------------------------------------------------------
      Print, '-----NO'
      ;-----------------------------------------------------------------

      ; Read stations
      all_stations_geos, 'NO','SpeciesConcVV_NO', 48, pref1, pref1, $
                            year1, ptop1, dlon1, dlat1, model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_geos, 'NO','SpeciesConcVV_NO', 48, pref2, pref1, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endif else begin
      all_stations_geos, 'NO','SpeciesConcVV_NO', 48, pref2, pref2, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
         all_stations_geos, 'NO', 'SpeciesConcVV_NO', 48, pref3, pref3, $
                            year3, ptop3, dlon3, dlat3, model3, '.3'
      endif
      
      ; Plot
      PSName = PSDir + 'aircraft.profile.NO.geos.ps'
      if ( nVersions eq 2 ) then begin
         plot_gridded_NO_vs_data_geos_3_models, $
            'NO', 48, '.1', '.2', 'None', title, PSName, nalt1, nalt2, nalt3
      endif else begin
         plot_gridded_NO_vs_data_geos_3_models, $
            'NO', 48, '.1', '.2', '.3', title, PSName, nalt1, nalt2, nalt3
      endelse
      
      ;-----------------------------------------------------------------
      Print, '-----O3'
      ;-----------------------------------------------------------------
 
      ; Read stations
      all_stations_geos, 'O3','SpeciesConcVV_O3', 44, pref1, pref1, $
                            year1, ptop1, dlon1, dlat1, model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_geos, 'O3','SpeciesConcVV_O3', 44, pref2, pref1, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endif else begin
      all_stations_geos, 'O3','SpeciesConcVV_O3', 44, pref2, pref2, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
         all_stations_geos, 'O3', 'SpeciesConcVV_O3', 44, pref3, pref3, $
                            year3, ptop3, dlon3, dlat3, model3, '.3'
      endif
      
      ; Plot
      PSName = PSDir + 'aircraft.profile.O3.geos.ps'
      if ( nVersions eq 2 ) then begin
         plot_gridded_O3_vs_data_geos_3_models, $
            'O3', 44, '.1', '.2', 'None', title, PSName, nalt1, nalt2, nalt3
      endif else begin
         plot_gridded_O3_vs_data_geos_3_models, $
            'O3', 44, '.1', '.2', '.3', title, PSName, nalt1, nalt2, nalt3
      endelse
      
      ;-----------------------------------------------------------------
      Print, '-----PAN'
      ;-----------------------------------------------------------------  

      ; Read stations
      all_stations_geos, 'PAN','SpeciesConcVV_PAN', 40, pref1, pref1, $
                            year1, ptop1, dlon1, dlat1, model1, '.1'
      if ( Do_GCHP ) then begin
      all_stations_geos, 'PAN','SpeciesConcVV_PAN', 40, pref2, pref1, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endif else begin
      all_stations_geos, 'PAN','SpeciesConcVV_PAN', 40, pref2, pref2, $
                            year2, ptop2, dlon2, dlat2, model2, '.2'
      endelse
      if ( nVersions eq 3 ) then begin
         all_stations_geos, 'PAN', 'SpeciesConcVV_PAN', 40, pref3, pref3, $
                            year3, ptop3, dlon3, dlat3, model3, '.3'
      endif
      
      ; Plot
      PSName = PSDir + 'aircraft.profile.PAN.geos.ps'
      if ( nVersions eq 2 ) then begin
         plot_gridded_PAN_vs_data_geos_3_models, $
            'PAN', 40, '.1', '.2', 'None', title, PSName, nalt1, nalt2, nalt3
      endif else begin
         plot_gridded_PAN_vs_data_geos_3_models, $
            'PAN', 40, '.1', '.2', '.3', title, PSName, nalt1, nalt2, nalt3
      endelse
      
      ; Close files & cleanup
      Close,       /All
      Ctm_Cleanup, /No_GC 

   endif

   if ( Do_Aerosol ) then begin

      if ( ~ Do_GCHP ) then begin

      ;=================================================================
      ; Plot models vs. surface PM2.5 and IMPROVE data
      ;=================================================================
      
      print, 'IDL_Aerosol: Plot models vs. surface PM2.5 & IMPROVE data'

      ;-----------------------------------------------------------------
      Print, '-----IMPROVE scatter'
      ;-----------------------------------------------------------------
      FilEst    = 'data/pm25_data/monthly_IMPROVE2005.csv'
      Max_Sta   = 208
      PSName    = PSDir + 'Aerosol.scatter.IMPROVE.geos.ps'
      Plot_scatter_improve_geos_3_models,                                   $
                                     Pref1, Ptop1,  Dlat1,   Dlon1, Model1, Year1, $
                                     Pref2, Ptop2,  Dlat2,   Dlon2, Model2, Year2, $
                                     Pref3, Ptop3,  Dlat3,   Dlon3, Model3, Year3, $
                                     Title, PSName, Max_Sta, FilEst
      Close,       /All
      Ctm_Cleanup, /No_GC 


      ;-----------------------------------------------------------------
      Print, '-----IMPROVE seasonal cycle'
      ;-----------------------------------------------------------------
      PSName    = PSDir + 'Aerosol.seascycle.IMPROVE.geos.ps'
      Plot_seas_improve_geos_3_models, Pref1, Ptop1,  Dlat1,   Dlon1, Model1, Year1, $
                                       Pref2, Ptop2,  Dlat2,   Dlon2, Model2, Year2, $
                                       Pref3, Ptop3,  Dlat3,   Dlon3, Model3, Year3, $
                                       Title, PSName, Max_Sta, FilEst
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;-----------------------------------------------------------------
      Print, '-----PM2.5 map'
      ;-----------------------------------------------------------------
      FilEst  = 'data/pm25_data/PM25-2005.csv'
      Max_Sta = 1517
      PSName1 = PSDir + 'Aerosol.map.PM25_complexSOA.geos.ps'
      PSName2 = PSDir + 'Aerosol.map.PM25_simpleSOA.geos.ps'
      Map_pm25_geos_3_models, Pref1, Label1, dlat1, dlon1, Model1, Year1, $
                              Pref2, Label2, dlat2, dlon2, Model2, Year2, $
                              Pref3, Label3, dlat3, dlon3, Model3, Year3, $
                              Title, PSName1, PSName2, Max_Sta, FilEst
      Close,       /All
      Ctm_Cleanup, /No_GC 

      ;-----------------------------------------------------------------
      Print, '-----PM2.5 scatter plot'
      ;-----------------------------------------------------------------
      FilEst  = 'data/pm25_data/PM25-2005.csv'
      Max_Sta = 1517
      PSName1 = PSDir + 'Aerosol.scatter.PM25_complexSOA.geos.ps'
      PSName2 = PSDir + 'Aerosol.scatter.PM25_simpleSOA.geos.ps'
      Plot_scatter_pm25_geos_3_models, Pref1, Ptop1,  Dlat1,   Dlon1, Model1, Year1, $
                                       Pref2, Ptop2,  Dlat2,   Dlon2, Model2, Year2, $
                                       Pref3, Ptop3,  Dlat3,   Dlon3, Model3, Year3, $
                                       Title, PSName1, PSName2, Max_Sta, FilEst

      ; Close files & clean up
      Close, /all
      Ctm_Cleanup, /No_GC 

      endif
      
      ;=================================================================
      ; Plot aerosol vertical profiles
      ;=================================================================
      Print, 'AEROSOL: Plotting vertical profiles'
      
      ; Version numbers
      if ( nVersions eq 2 ) then begin
         Versions = [ Vers1, Vers2 ]
         Prefs    = [ Pref1, Pref2 ]
         Years    = [ Year1, Year2 ]
      endif else begin
         Versions = [ Vers1, Vers2, Vers3 ]
         Prefs    = [ Pref1, Pref2, Pref3 ]
         Years    = [ Year1, Year2, Year3 ]
      endelse
      
      ; Input & output files
      PSName   = PSDir + 'Aerosol_Vertical_Profiles.ps'
      
      if ( nVersions eq 2 ) then begin
         aerosol_vertical_profiles, Prefs, Versions, Model2, Res2, Years, Title, $
                                    DO_GCHP, /PS, OutFileName=PSName, _EXTRA=e
      endif else begin
         aerosol_vertical_profiles, Prefs, Versions, Model3, Res3, Years, Title, $
                                    DO_GCHP, /PS, OutFileName=PSName, _EXTRA=e
      endelse

      Close, /all
      ctm_cleanup, /No_GC

   endif

   ;====================================================================
   ; Plot models vs. PAN observations
   ;====================================================================

   if ( Do_PAN ) then begin

      print, 'PAN: Creating PAN profiles'

      PSName  = PsDir + 'PAN.profiles.ps'

      PAN_profiles, Pref1, Model1, Dlat1, Dlon1, Year1, $
                    Pref2, Model2, Dlat2, Dlon2, Year2, $
                    Pref3, Model3, Dlat3, Dlon3, Year3, $
                    Title, PSName
      
      ; Close files & clean up
      Close, /all
      Ctm_Cleanup, /No_GC 

   endif

   ;====================================================================
   ; Plot models vs. GOME-2 observations
   ;====================================================================

   if ( Do_BrO ) then begin

      print, 'BrO: Plot models vs. GOME-2 observations'

      PSName    = PSDir + 'BrO.seasonal_columns.ps'

      if ( nVersions eq 2 ) then begin

         Title     = Title1 + ';  ' + Title2
         BrDir1    = StrJoin( StrSplit(Dir1, 'OutputDir', /Regex, /Extract, $
                     /Preserve_Null), 'Br_satellite')
         BrDir2    = StrJoin( StrSplit(Dir2, 'OutputDir', /Regex, /Extract, $
                     /Preserve_Null), 'Br_satellite')
         
         satellite_bro_cloud, BrDir1, Dlat1, Dlon1, Model1, $
                              BrDir2, Dlat2, Dlon2, Model2, $
                              Title,  PSName
         
      endif else begin

         Title     = Title2 + ';  ' + Title3
         ;BrDir1    = StrJoin( StrSplit(Dir1, 'netcdf', /Regex, /Extract, $
         ;            /Preserve_Null), 'Br_satellite')
         BrDir2    = StrJoin( StrSplit(Dir2, 'OutputDir', /Regex, /Extract, $
                     /Preserve_Null), 'Br_satellite')
         BrDir3    = StrJoin( StrSplit(Dir3, 'OutputDir', /Regex, /Extract, $
                     /Preserve_Null), 'Br_satellite')
         
         satellite_bro_cloud, BrDir2, Dlat2, Dlon2, Model2, $
                              BrDir3, Dlat3, Dlon3, Model3, $
                              Title,  PSName

         ;--------------------------------------------------------------------
         ; For now we can only plot BrO for two model versions to avoid hitting
         ; the memory limit in IDL
         ;satellite_bro_cloud, BrDir1, Dlat1, Dlon1, Model1, $
         ;                     BrDir2, Dlat2, Dlon2, Model2, $
         ;                     BrDir3, Dlat3, Dlon3, Model3, $
         ;                     Title,  PSName
         ;--------------------------------------------------------------------
         
      endelse
      
      ; Close files & clean up
      Close, /all
      Ctm_Cleanup, /No_GC 
   endif

   if ( Do_STE ) then begin

      if ( ~ Do_GCHP ) then begin

         ;=================================================================
         ; Compute O3 STE flux
         ;=================================================================
      
         print, 'STE: Computing global O3 strat-trop-exchange flux'

         OutFile = PSDir + 'O3_STE_flux.' + RunName + '.txt'


         if ( nVersions eq 2 ) then begin
            geoschem_o3_ste_flux, Pref2, Vers2, Model2, Res2, Year2, $
                                  OutFileName = OutFile, _EXTRA = e
         endif else begin
            geoschem_o3_ste_flux, Pref3, Vers3, Model3, Res3, Year3, $
                                  OutFileName = OutFile, _EXTRA = e
         endelse

         ; Close files & clean up
         Close, /all
         Ctm_Cleanup, /No_GC 

      endif

   endif


   if ( Do_PDF ) then begin

      ; Create PDF files from the postscript files
      Make_PDF, './output/'
 
      ; Remove PS files and only keep PDF files
      Spawn, 'rm -v ./output/*.ps'

   endif

   ; Restore original color table
   TvLct, R, G, B

end
