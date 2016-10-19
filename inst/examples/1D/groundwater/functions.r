leak <- function (hAqui, hRiver, hBed, kfBed, thickBed, widthBed, dx) {
  wetPerim <- widthBed + 2 * (hRiver - hBed)
  leakFact <- kfBed / thickBed * wetPerim / dx
  if (hAqui > hBed)
    leak = leakFact * (hRiver - hAqui)
  else
    leak = leakFact * (hRiver - hBed)
}

rch <- function (time) {
  0.2 / 365 / 86400
}

hRiver <- function (time) {
  11
}

