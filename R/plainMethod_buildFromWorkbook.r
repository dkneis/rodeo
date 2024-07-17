#' Build a model from the contents of a workbook
#'
#' The function builds a \code{\link[rodeo]{rodeo}}-based model by importing
#' all declarations and equations from a workbook established with common
#' spreadsheet software.
#'
#' @param workbook File path of the workbook. The file type is guessed from
#'   the extension which must be '.xlsx' or '.ods'. See below for the mandatory
#'   worksheets that must be present in the workbook.
#' @param dim The number of spatial compartments, possibly in multiple
#'   dimensions. For single-box models without spatial resolution, use
#'   \code{dim=1} (default). For a one-dimensional model with 10 compartments
#'   use, e.g., \code{dim=10}. See the \code{dim} argument of the method
#'   \code{\link[rodeo]{initialize}} for further details.
#' @param set_defaults If \code{TRUE}, parameters and initial values will be
#'   set according to the contents of the 'default' columns of the workbook
#'   sheets 'declarations', respectively. If \code{FALSE}, values must be
#'   set explicitly using the class methods \code{\link[rodeo]{setPars}} and
#'   \code{\link[rodeo]{setVars}}. An attempt to use \code{set_defaults=TRUE}
#'   when \code{dim != 1} will be ignored (with a warning).
#' @param fortran Controls the language of code generation. The default
#'   (\code{FALSE}) produces R code. Use \code{TRUE} if you want to use
#'   compiled Fortran code for better performance. In the latter case, you will
#'   need a Fortran compiler which is accessible by R.
#' @param sources Only relevant if \code{fortran=TRUE}. The argument
#'   allows the name(s) of additional source file(s) to be provided
#'   for processing by the Fortran compiler. In any case, the Fortran code
#'   in \code{sources} must implement a module with the fixed name 'functions'.
#'   This module must contain all user-defined functions referenced in any
#'   process rate expressions or any cell of the stoichiometry matrix.
#' @param ... Optional arguments passed to \code{\link[readxl]{read_excel}}
#'   or \code{\link[readODS]{read_ods}}, respectively.
#' 
#' @return An object of class \code{\link[rodeo]{rodeo}}.
#'
#' @note The file provided as \code{workbook} must contain two sheets:
#' \itemize{
#'   \item{'declarations'} Declares the identifiers of state variables,
#'     parameters, and functions used in the model equations. Mandatory columns
#'      are 'type', 'name', 'unit', 'description', and 'default'. Entries in
#'      the type column must be one of 'variable', 'parameter', or 'function'.
#'      If source code is generated for R (\code{fortran=FALSE}), any declared
#'      functions must be accessible in the environment where the model is
#'      run. If \code{fortran=TRUE}, the functions must be implemented in the
#'      file(s) listed in \code{sources} to be included in compilation.
#'   \item{'equations'} Specifies the model equations. Mandatory columns
#'      are 'name', 'unit', 'description', 'rate' plus one column for
#'      every state variable of the model. The 'rate' columns holds math
#'      expressions for the process rates and columns named after state
#'      variables contain the corresponding expressions representing
#'      stoichiometric factors.
#' }
#' The best way to understand the contents of a suitable workbook is to study
#' the examples in the folder 'models' shipped with the package. Type
#' \code{system.file("models", package="rodeo")} at the R prompt to see
#' where this folder is installed on your system. A full example is given below.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#'
#' # Build and run a SEIR type epidemic model
#' m <- buildFromWorkbook(
#'   system.file("models/SEIR.xlsx", package="rodeo")
#' )
#' x <- m$dynamics(times=0:30, fortran=FALSE)
#' print(head(x))

buildFromWorkbook <- function(workbook, dim=1, set_defaults=TRUE,
  fortran=FALSE, sources=NULL, ...) {
  # check inputs
  if (length(workbook) != 1 || !is.character(workbook))
    stop("'workbook' must be a character string")
  if (!is.integer(as.integer(dim)))
    stop("'dim' must be an integer vector")
  if (length(set_defaults) != 1 || !is.logical(set_defaults))
    stop("'set_defaults' must be TRUE or FALSE")
  if (length(fortran) != 1 || !is.logical(fortran))
    stop("'fortran' must be TRUE or FALSE")
  if (!is.null(sources) && !is.character(sources))
    stop("'sources' should be NULL or a vector of character strings")
  if (!file.exists(workbook))
    stop(paste0("file provided as 'workbook' not found: '",workbook,"'"))
  # import sheets
  x <- list()
  for (s in c("declarations","equations")) {
    tryCatch({
      if (grepl(workbook, pattern=".+[.]xlsx$")) {
        x[[s]] <- as.data.frame(readxl::read_excel(workbook, sheet=s, ...=...))
      } else if (grepl(workbook, pattern=".+[.]ods$")) {
        x[[s]] <- as.data.frame(readODS::read_ods(workbook, sheet=s, ...=...))
      } else {
        stop(paste0("file '",workbook,"' not in '.xlsx' or '.ods' format")) 
      }
    }, error= function(e) {
      stop(paste0("failed to read required sheet '",s,"' from '",workbook,"'"))
    })
  }
  decl <- x[["declarations"]]
  eqns <- x[["equations"]]
  rm(x)
  # check declarations
  needed <- c("type","name","unit","description","default")
  missing <- needed[! needed %in% names(decl)]
  if (length(missing) > 0)
    stop(paste0("sheet 'declarations' is lacking mandatory column(s): '",
      paste(missing, collapse="', '"),"'"))
  if (!all(decl[,"type"] %in% c("variable","parameter","function")))
    stop("'type' in sheet 'declarations' must be one of 'variable', 'parameter',
      or 'function'")
  if (!any(decl[,"type"] == "variable"))
    stop('not a single state variable has been declared')
  if (!any(decl[,"type"] == "parameter"))
    stop('not a single parameter has been declared')
  # check equations
  vnames <- decl[decl[,"type"] == "variable", "name"]
  needed <- c("name","unit","description","rate", vnames)
  missing <- needed[! needed %in% names(eqns)]
  if (length(missing) > 0)
    stop(paste0("sheet 'equations' is lacking mandatory column(s): '",
      paste(missing, collapse="', '"),"'"))
  # separate processes and stoichiometry
  stoi <- as.matrix(eqns[, vnames, drop=FALSE])
  rownames(stoi) <- eqns[,"name"]
  pros <- eqns[,c("name","unit","rate","description")]
  names(pros)[names(pros) == "rate"] <- "expression"
  eqns <- NULL
  # build and compile model object
  m <- rodeo$new(
    vars=decl[decl[,"type"]=="variable", names(decl) != "type"],
    pars=decl[decl[,"type"]=="parameter", names(decl) != "type"],
    funs=if (!any(decl[,"type"]=="function")) NULL else
      decl[decl[,"type"]=="function", names(decl) != "type"],
    pros=pros,
    stoi=stoi,
    asMatrix=T,
    dim=dim
  )
  m$compile(fortran=fortran, sources=sources)
  # attempt to set parameters and initial values
  if (set_defaults) {
    if (sum(dim) > 1) {
      warning("ignoring 'set_defaults' because 'dim' is not equal to 1") 
    } else {
      m$setPars(stats::setNames(m$getParsTable()$default, m$getParsTable()$name))
      m$setVars(stats::setNames(m$getVarsTable()$default, m$getVarsTable()$name))
    }
  }
  # return object
  m
}
