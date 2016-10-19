module functions
  implicit none
  contains

  double precision function leak (hAqui, hRiver, hBed, kfBed, thickBed, widthBed, dx)
    double precision, intent(in):: hAqui, hRiver, hBed, kfBed, thickBed, widthBed, dx
    double precision:: wetPerim, leakFact
    wetPerim = widthBed + 2 * (hRiver - hBed)
    leakFact = kfBed / thickBed * wetPerim / dx
    if (hAqui > hBed) then
      leak = leakFact * (hRiver - hAqui)
    else
      leak = leakFact * (hRiver - hBed)
    end if
  end function

  double precision function rch (time)
    double precision, intent(in):: time
    rch = 0.2d0 / 365d0 / 86400d0
  end function

  double precision function hRiver (time)
    double precision, intent(in):: time
    hRiver = 11
  end function

end module

