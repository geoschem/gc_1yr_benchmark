;this function returns TAU value for a given YEAR and JDAY, e.g.
;     tau = yyyyjday2tau ( '90', 127 )
;hyl,01/19/2012, hongyu.liu-1@nasa.gov
; Modified by Karen Yu, 17 June 2013 

function yyyyjday2tau, IYEAR, JDAY

IYEAR = strtrim ( string(IYEAR), 2)
JDAY  = fix (JDAY)

CASE ( IYEAR ) of

'85': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 0      ;850101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'86': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) =  8760  ;860101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'87': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 17520  ;870101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'88': begin
          NDAYS = 366
          tau = FltArr(NDAYS)
          tau(0) =  26280  ;880101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'89': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 35064  ;890101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'90': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 43824  ;900101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end

'91': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 52584  ;910101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end

'92': begin
          NDAYS = 366     ;leap year
          tau = FltArr(NDAYS)
          tau(0) = 61344  ;920101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end

'93': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 70128  ;930101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end

'94': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 78888  ;940101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end

'95': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 87648  ;950101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end

'96': begin
          NDAYS = 366     ;leap year
          tau = FltArr(NDAYS)
          tau(0) = 96408  ;960101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end


'97': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 105192  ;970101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end


'98': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 113952  ;980101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end


'99': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 122712  ;990101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end


'00': begin
          NDAYS = 366
          tau = FltArr(NDAYS)
          tau(0) = 131472  ;000101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end

'01': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 140256 ;010101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end

'02': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 149016 ;020101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'03': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 157776 ;030101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'04': begin
          NDAYS = 366
          tau = FltArr(NDAYS)
          tau(0) = 166536  ;040101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'05': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 175320 ;050101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'06': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 184080 ;060101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'07': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 192840  ;070101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'08': begin
          NDAYS = 366
          tau = FltArr(NDAYS)
          tau(0) = 201600  ;080101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'09': begin
          NDAYS = 365
          tau = FltArr(NDAYS)
          tau(0) = 210384  ;090101
          for i = 1, NDAYS-1 do begin
          tau(i) = tau(i-1) + 24
          endfor
          return, tau(JDAY-1)
        end
'13': begin
	  NDAYS = 365
	  tau = FltArr(NDAYS)
	  tau(0) = 245448 ;130101
	  for i = 1, NDAYS-1 do begin
	  tau(i) = tau(i-1) + 24
	  endfor
	  return, tau(JDAY-1)
	end
'16': begin
	  NDAYS = 366
	  tau = FltArr(NDAYS)
	  tau(0) = 271728 ; 160101
	  for i = 1, NDAYS-1 do begin
	  tau(i) = tau(i-1) + 24
	  endfor
	  return, tau(JDAY-1)
       end

endcase

end
