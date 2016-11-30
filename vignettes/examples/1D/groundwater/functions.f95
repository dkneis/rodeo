! This is file 'functions.f95'
module functions
  implicit none
  contains

  double precision function leak (hAqui, hRiv, hBed, kfBed, tBed, wBed, dx)
    double precision, intent(in):: hAqui, hRiv, hBed, kfBed, tBed, wBed, dx
    double precision:: wetPerim, leakFact
    wetPerim = wBed + 2 * (hRiv - hBed)       ! rectangular x-section
    leakFact = kfBed / tBed * wetPerim / dx
    if (hAqui > hBed) then
      leak = leakFact * (hRiv - hAqui)
    else
      leak = leakFact * (hRiv - hBed)
    end if
  end function

  double precision function rch (time)
    double precision, intent(in):: time
    rch = 0.2d0 / 365d0                          ! held constant at 200 mm/year
  end function

  double precision function hRiv (time)
    double precision, intent(in):: time
    hRiv = 11                                    ! held constant at 11 m
  end function

end module

