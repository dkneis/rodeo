
rodeo$methods( sortLikeVars= function(v) {
    "Sorts a named vector \\code{v} according to the order of state variables"
   if (is.null(names(v)))
     stop("input must be a named vector")
   miss= .self$vars[! (.self$vars %in% names(v))]
   if (length(miss) > 0)
     stop(paste0("the following element",ifelse(length(miss)>1,"s are"," is"),
       " missing in the input vector: '",paste(miss,collapse="', '"),"'"))
   return(v[.self$vars])
})

rodeo$methods( sortLikePars= function(v) {
    "Sorts a named vector \\code{v} according to the order of parameters"
   if (is.null(names(v)))
     stop("input must be a named vector")
   miss= .self$pars[! (.self$pars %in% names(v))]
   if (length(miss) > 0)
     stop(paste0("the following element",ifelse(length(miss)>1,"s are"," is"),
       " missing in the input vector: '",paste(miss,collapse="', '"),"'"))
   return(v[.self$pars])
})

