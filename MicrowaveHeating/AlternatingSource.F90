FUNCTION AlternatingSource(Model, n, x) RESULT(ht)
  USE DefUtils
  
  IMPLICIT None
  
  TYPE(Model_t) :: Model
  INTEGER :: n
  REAL(KIND=dp) :: x(2),ht
  
  REAL(KIND=dp) :: hvar,hcon,time,tpoint,Poy
  INTEGER :: tlimit
  tpoint = x(1)
  Poy = x(2)

  hvar = abs(Poy)/1050  
  hcon = -4761.9
  tlimit = 400
  !ht = hcon + hvar
  if (tpoint < tlimit) then
  ht = hcon + hvar
  else
  ht = hcon
  end if 
    
END FUNCTION AlternatingSource


 








