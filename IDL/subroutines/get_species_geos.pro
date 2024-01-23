; $Id: get_species_geos.pro,v 1.2 2008/03/31 18:51:06 bmy Exp $
function Get_Species_Geos, FileName,                         $
                           Species=Species, $
                           Verbose=Verbose, Lat=FileLat, $
                           Lon=FileLon, _EXTRA=e

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

   ; Expand FILENAME to a full path name
   FileName  = Expand_Path( FileName )

   ; Open file
   fId       = NCDF_Open( FileName )

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

   ; If /VERBOSE is set, then print out quantities
   if ( Verbose ) then begin
      ;print, 'Time:'
      ;print, FileTime
      ;print, 'Longitudes: '
      ;print, FileLon
      ;print, 'Latitudes: '
      ;print, FileLat
      ;print, 'Sigma: '
      ;print, FileSigma
   endif

   ;====================================================================
   ; Read data from the netCDF file
   ; NOTE: dimensions are: [ lon, lat, alt]
   ;====================================================================

   ; Strip out bad characters so we don't generate a lot of error msgs
   NewSpecies = NCDF_Valid_Name( Species )

   ; Read the SPECIES for the given DATE from the file
   ; NOTE: for now, pull out all lon, lat, lev
   OffSet = [ 0,     0,     0,     0      ]
   Count  = [ N_Lon, N_Lat, N_Alt, N_Time ]
   Data   = NCDF_Get( fId, NewSpecies, $
                      OffSet=OffSet, Count=Count, _EXTRA=e )

   ; Convert from v/v to ppbv
   Data =  Data * 1e9
   
   NCDF_CLOSE,FId
   ; Return to calling program
   return, Data
end
