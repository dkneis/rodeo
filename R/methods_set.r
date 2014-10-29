
rodeo$methods( setVars= function(v) {
    "Initializes field \\code{vars} to the values in the named vector \\code{v}"
   if (!identical(sort(names(v)), sort(names(.self$vars))))
     stop("length/names of input vector not matching length/names of target vector")
   vars[names(v)] <<- v
   return(invisible(NULL))
})

rodeo$methods( setPars= function(v) {
    "Initializes field \\code{pars} to the values in the named vector \\code{v}"
   if (!identical(sort(names(v)), sort(names(.self$pars))))
     stop("length/names of input vector not matching length/names of target vector")
   pars[names(v)] <<- v
   return(invisible(NULL))
})

