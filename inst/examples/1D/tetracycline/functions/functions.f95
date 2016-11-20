module functions 
  implicit none 
  contains

  ! Consult the functions' declaration table to see what these functions do

  double precision function A_part(A_tot, kd_DOM, kd_TSS, DOM, TSS) result (r) 
    double precision, intent(in):: A_tot, kd_DOM, kd_TSS, DOM, TSS 
    r= A_tot * kd_TSS * TSS / (1d0 + kd_DOM * DOM + kd_TSS * TSS)
  end function 

  double precision function A_diss(A_tot, kd_DOM, kd_TSS, DOM, TSS) result (r) 
    double precision, intent(in):: A_tot, kd_DOM, kd_TSS, DOM, TSS 
    r= A_tot - A_part(A_tot, kd_DOM, kd_TSS, DOM, TSS)
  end function

  double precision function A_free(A_tot, kd_DOM, kd_TSS, DOM, TSS) result (r) 
    double precision, intent(in):: A_tot, kd_DOM, kd_TSS, DOM, TSS 
    r= A_tot / (1d0 + kd_DOM * DOM + kd_TSS * TSS)
  end function 

end module
