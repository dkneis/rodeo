#' Clean-up a \code{rodeo} Object
#'
#' Clean-up method for objects of the \code{\link{rodeo-class}}.
#'
#' @name finalize
#'
#' @return The method is called implicitly for its side effects when a
#'   \code{\link{rodeo}} object is destroyed.
#'
#' @note At present, the method just unloads the object-specific
#'   shared libraries created with the \code{\link{compile}} or
#'   \code{\link{initStepper}} methods.
#'
#' @author \email{david.kneis@@tu-dresden.de}

rodeo$set("public", "finalize", function() {

  # library created with 'compile'
  if (length(private$lib) > 0) {
    if (file.exists(private$lib["file"])) {
      dyn.unload(private$lib["file"])
    }
  }

  # libraries created with 'initStepper'
  if (length(private$steppers) > 0) {
    for (n in names(private$steppers)) {
      if (file.exists(private$steppers[[n]]$libFile)) {
        dyn.unload(private$steppers[[n]]$libFile)
      }
    }
  }

})

