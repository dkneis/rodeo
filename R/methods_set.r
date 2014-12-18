
rodeo$methods( setVars= function(v) {
    "Initializes field \\code{vars} to the values in the named vector \\code{v}"
   if (is.null(names(v)))
     stop("input must be a named vector")
   miss= names(.self$vars)[! (names(.self$vars) %in% names(v))]
   if (length(miss) > 0)
     stop(paste0("the following element",ifelse(length(miss)>1,"s are"," is"),
       " missing in the input vector: '",paste(miss,collapse="', '"),"'"))
   vars <<- v[names(vars)]
   return(invisible(NULL))
})

rodeo$methods( setPars= function(v) {
    "Initializes field \\code{pars} to the values in the named vector \\code{v}"
   if (is.null(names(v)))
     stop("input must be a named vector")
   miss= names(.self$pars)[! (names(.self$pars) %in% names(v))]
   if (length(miss) > 0)
     stop(paste0("the following element",ifelse(length(miss)>1,"s are"," is"),
       " missing in the input vector: '",paste(miss,collapse="', '"),"'"))
   pars <<- v[names(pars)]
   return(invisible(NULL))
})

