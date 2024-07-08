#' @import R6
library("R6")
#' @import deSolve
library("deSolve")
#' @import readxl
library("readxl")
#' @import readODS
library("readODS")

#' Package to Facilitate ODE-Based Modeling
#'
#' This package provides methods to
#' \itemize{
#'   \item{} import a conceptual ODE-based model stored in tabular form (i.e.
#'     as text files or spreadsheets).
#'   \item{} generate source code (either R or Fortran) to be passed to an
#'     ODE-solver.
#'   \item{} visualize and export basic information about a model, e.g. for
#'     documentation purposes.
#' }
#'
#' Consult the package vignette for details. The concept of writing an ODE
#'   system in tabular/matrix form is nicely introduced, e. g., in the book of
#'   Reichert, P., Borchardt, D., Henze, M., Rauch, W., Shanahan, P.,
#'   Somlyody, L., and Vanrolleghem, P. A. (2001): River water quality model
#'   No. 1, IWA publishing, ISBN 9781900222822.
#'
#' The current source code repository is \url{https://github.com/dkneis/rodeo}.
#'
#' @section Class and class methods:
#'
#' See \code{\link{rodeo-class}} for the \code{rodeo} class
#'   and the corresponding class methods.
#'
#' @section Non-class methods:
#'
#'   Type \code{help(package="rodeo")} or see the links below to access the
#'   documentation of non-class methods contained in the package.
#'
#' \itemize{
#'   \item{\code{\link{buildFromWorkbook}}} Builds and compiles a model
#'     fully specified in a workbook (supports .xlsx and .ods format).
#'   \item{\code{\link{forcingFunctions}}} Generation of forcing functions
#'     in Fortran.
#'   \item{\code{\link{exportDF}}} Export of data frames as TEX or HTML code.
#'   \item{\code{\link{stoiCreate}}} Creates a stoichiometry matrix from a set
#'     of chemical reactions.
#'   \item{\code{\link{stoiCheck}}} Validates a stoichiometry matrix by checking
#'     for conservation of mass.
#' }
#' 
#' @author \email{david.kneis@@tu-dresden.de}

"_PACKAGE"
