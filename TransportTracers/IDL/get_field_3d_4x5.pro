;added "code" (hyl, 10/14/09, hongyu.liu-1@nasa.gov)

pro get_field_3d_4x5, code, ntrac, category, lev1, lev2, tau, limit, file, field_3d_4x5, xmid_out, ymid_out, zmid_out

print, 'In get_field_3d_4x5, file=', file

;-------------------------------------------------------------------
;ntrac - tracer # (40 for cloud optical depth)
;lev1, lev2 - find the total cloud optical depth between [lev1,lev2]
;tau  - tau value
;limit - [lat1, long1, lat2, long2]
;file - filename for COD 
;field_3d - the total cloud optical depth 
;xmid_out, ymid_out, zmid_out - return XMID and YMID, ZMID
;-------------------------------------------------------------------

modelinfo = CTM_type(code, res=4) ;4x5 resolution
gridinfo  = CTM_grid(modelinfo)
x = gridinfo.xmid
y = gridinfo.ymid
IM = gridinfo.IMX
JM = gridinfo.JMX
PMID = gridinfo.PMID
ZMid_out = gridinfo.ZMID

success = ctm_get_datablock(field_3d_4x5, category,                   $
                             XMid=XMid_out, YMid=YMid_out, ZMid=ZMid, $  
                             Tracer=ntrac, Tau0=tau,           $
                             Lat = [limit(0), limit(2)],      $
                             Lon = [limit(1), limit(3)],      $
                             Lev = [lev1, lev2],      $
                             FileName=file )

;help, xmid_out, ymid_out, zmid_out
;print, xmid_out,ymid_out,zmid_out
;print, PMID, ZMID_out
;stop

if ( not success ) then begin
     Message, 'Could not find field_3d_4x5!'
     stop
endif

xx = n_elements(xmid_out) & yy = n_elements(ymid_out)
zz = n_elements(zmid_out)

print, '---- size :', xx,yy,zz

end
