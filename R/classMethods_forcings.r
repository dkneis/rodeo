
#' Initialize forcings of a Fortran-based model
#'
#' Initializes the forcings of a model written in Fortran based on the
#' input supplied to \code{forcingFunctions}. If the data cannot be
#' initialized for any reason, an error is thrown and a hopefully
#' useful message is provided.
#'
#' @name forcings_init
#'
#' @return Returns \code{NULL} invisibly.
#'
#' @note The function takes no arguments and should thus called as
#'   \code{forcings_init()}.
#'
#' @seealso \code{\link{forcings_clear}} to clear the data set by
#'  \code{forcings_init}
#'
#' @author \email{david.kneis@@tu-dresden.de}

rodeo$set("public", "forcings_init", function() {
  if (length(private$lib) > 0) {
    if (is.loaded("forcings_init", PACKAGE=self$libName())) {
      .Fortran("forcings_init", PACKAGE=self$libName())
    } else {
      stop(paste0("A shared Fortran library is connected but it does ",
        "not provide a 'forcings_init' routine. ",
        "Most likely 'forcingFunctions' has not been called and/or ",
        "the output has not been process through 'compile'."))
    }
  } else {
    stop(paste("No shared Fortran library is connected to the object. ",
      "Consider calling 'forcingFunctions' followed by 'compile'."))
  }
  invisible(NULL)
})

#' Clear forcing data of a Fortran-based model
#'
#' Clears the forcings data of a model written in Fortran. This may be
#' useful when data should be updated through another call to to
#' \code{\link{forcings_init}} or for the purpose of memory cleanup.
#'
#' @name forcings_clear
#'
#' @return Returns \code{NULL} invisibly.
#'
#' @note The function takes no arguments and should thus called as
#'   \code{forcings_clear()}.
#'
#' @seealso \code{\link{forcings_init}} to initialize the forcing data
#'
#' @author \email{david.kneis@@tu-dresden.de}

rodeo$set("public", "forcings_clear", function() {
  if (length(private$lib) > 0) {
    if (is.loaded("forcings_clear", PACKAGE=self$libName())) {
      .Fortran("forcings_clear", PACKAGE=self$libName())
    } else {
      stop(paste0("A shared Fortran library is connected but it does ",
        "not provide a 'forcings_clear' routine. ",
        "Most likely 'forcingFunctions' has not been called and/or ",
        "the output has not been process through 'compile'."))
    }
  } else {
    stop(paste("No shared Fortran library is connected to the object. ",
      "Consider calling 'forcingFunctions' followed by 'compile'."))
  }
  invisible(NULL)
})
