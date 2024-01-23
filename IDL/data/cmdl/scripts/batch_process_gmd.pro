; Script to process all sites used in the benchmark code

openr,lun1,'sitelist.txt',/get_lun

site=''
while ~eof(lun1) do begin

   readf,lun1,site
   avg_data,site,year=2013

endwhile

close,lun1
free_lun,lun1

end
