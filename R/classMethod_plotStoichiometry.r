#' Plot Qualitative Stoichiometry Matrix
#'
#' Visualizes the stoichiometry matrix using standard plot methods. The sign
#' of stoichiometric factors is displayed as upward and downward pointing
#' triangles, optionally colored.
#'
#' @name plotStoichiometry
#'
#' @param box A vector of positive integers pointing to a spatial sub-unit of
#'   the model.
#' @param time Time. The value is ignored in the case of autonomous models.
#' @param cex Character expansion factor.
#' @param shade Logical. If \code{TRUE}, the dependence of process rates on
#'   state variables is indicated by shading.
#' @param colPositive Color for positive stoichiometric factors.
#' @param colNegative Color for negative stoichiometric factors.
#' @param colGrid Color of a grid (can be identical to background color).
#' @param colShade Shading color, if used.
#'
#' @return NULL
#'
#' @note The values of state variables and parameters must have been set using
#'   the \code{\link{setVars}} and \code{\link{setPars}} methods. If the
#'   stoichiometric factors are mathematical expressions involving
#'   function references, these functions must be defined in R (even if the
#'   numerical computations are based on generated Fortran code).
#'
#' @author \email{david.kneis@@tu-dresden.de}
#'
#' @seealso See other methods of the \code{\link{rodeo-class}} or
#'   \code{\link{stoichiometry}} for computing the stoichiometric factors only.
#'   Alternative options for displaying stoichiometry information are described
#'   in the package vignette.
#'
#' @examples
#' data(vars, pars, funs, pros, stoi)
#' model <- rodeo$new(vars, pars, funs, pros, stoi, dim=c(1))
#' model$setVars(c(bac=0.1, sub=0.5))
#' model$setPars(c(mu=0.8, half=0.1, yield= 0.1, vol=1000, flow=50, sub_in=1))
#' monod <- function(c,h) {c/(c+h)}
#' model$plotStoichiometry(box=c(1))

rodeo$set("public", "plotStoichiometry", function(box, time=0, cex=1,
  shade=TRUE, colPositive="tomato3", colNegative="steelblue4",
  colGrid="grey", colShade="lemonchiffon"
) {
  m <- self$stoichiometry(box=box, time=time)
  if (!all(is.finite(m))) {
    stop("non-finite elements in stoichiometry matrix")
  }
  s <- replace(m, 1:length(m), "white")
  if (shade) {
    for (pro in rownames(s)) {
      expr <- private$prosTbl$expression[private$prosTbl$name==pro]
      ident <- extractIdentifiers(expr)
      v <- colnames(s)[colnames(s) %in% ident]
      if (length(v) > 0) {
        s[pro, v] <- colShade
      }
    }
  } 
  dx <- 0.2
  dy <- sqrt((dx**2)/2)
  mar <- 0.5
  plot(0, 0, xlim=c(1-mar,(ncol(m)+mar)), ylim=c(1-mar,(nrow(m)+mar)), type="n",
    bty="n", xaxt="n", yaxt="n", xlab="", ylab="")
  mtext(side=3, at=1:ncol(m), colnames(m), line=0.5, las=2, cex=cex)
  mtext(side=2, at=nrow(m):1, rownames(m), line=0.5, las=2, cex=cex)
  for (ic in 1:ncol(m)) {
    for (ir in 1:nrow(m)) {
      rect(xleft=ic-0.5, xright=ic+0.5, ybottom=nrow(m)+1-(ir-0.5), ytop=nrow(m)+1-(ir+0.5),
        col=s[ir,ic], border=NA)
      if (m[ir,ic] > 0) polygon(x=c(ic-dx,ic+dx,ic,ic-dx),
        y=nrow(m)+1-c(ir+dy,ir+dy,ir-dy,ir+dy), col=colPositive, border=NA)
      if (m[ir,ic] < 0) polygon(x=c(ic-dx,ic+dx,ic,ic-dx),
        y=nrow(m)+1-c(ir-dy,ir-dy,ir+dy,ir-dy), col=colNegative, border=NA)
    }
  }
  abline(h=c((1:nrow(m))-0.5,nrow(m)+0.5),
    v=c((1:ncol(m))-0.5,ncol(m)+0.5), col=colGrid)
  return(invisible(NULL))
})

