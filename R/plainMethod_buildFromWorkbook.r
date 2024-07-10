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
#' @note The file provided as \code{workbook} must contain at least the four
#'   mandatory sheets:
#' \itemize{
#'   \item{'vars'} Declares the state variables of the model. Mandatory columns
#'      are 'name', 'unit', 'description'.
#'   \item{'pars'} Declares the parameters of the model. Mandatory columns
#'      are the same as for sheet 'vars'.
#'   \item{'funs'} Declares user-defined functions appearing in any equations of
#'      the model. Mandatory columns are the same as for sheet 'vars'. If
#'      source code is generated for R (\code{fortran=FALSE}), the declared
#'      functions must be accessible in the environment where the model is
#'      run. If \code{fortran=TRUE}, the functions must be implemented in the
#'      file(s) listed in \code{sources} to be included in compilation.
#'      In case functions are not used at all, you still need do declare a
#'      dummy function (call it 'dummy', for example).
#'   \item{'eqns'} Specifies the model equations. Mandatory columns
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
#' # Build a SEIR type epidemic model
#' m <- buildFromWorkbook(
#'   system.file("models/SEIR.xlsx", package="rodeo")
#' )
#' m$setPars(setNames(m$getParsTable()$default,
#'   m$getParsTable()$name))
#' m$setVars(setNames(m$getVarsTable()$default,
#'   m$getVarsTable()$name))
#' x <- m$dynamics(times=0:30, fortran=FALSE)
#' print(head(x))

buildFromWorkbook <- function(workbook, dim=1, fortran=FALSE, sources=NULL, ...) {
  # check inputs
  if (length(workbook) != 1 || !is.character(workbook))
    stop("'workbook' must be a character string")
  if (!is.integer(as.integer(dim)))
    stop("'dim' must be an integer vector")
  if (length(fortran) != 1 || !is.logical(fortran))
    stop("'fortran' must be TRUE or FALSE")
  if (!is.null(sources) && !is.character(sources))
    stop("'sources' should be NULL or a vector of character strings")
  if (!file.exists(workbook))
    stop(paste0("file provided as 'workbook' not found: '",workbook,"'"))
  # needed sheets and columns
  sheets.needed <- list(
    vars = c("name","unit","description","default"),
    pars = c("name","unit","description","default"),
    funs = c("name","unit","description"),
    eqns = c("name","unit","description","rate")
  )
  # import sheets
  x <- list()
  for (s in names(sheets.needed)) {
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
  # update needed columns in sheet with equations
  sheets.needed[["eqns"]] <- c(sheets.needed[["eqns"]], x[["vars"]][,"name"])
  # check sheets
  for (s in names(sheets.needed)) {
    missing <- sheets.needed[[s]][! sheets.needed[[s]] %in% names(x[[s]])]
    if (length(missing) > 0) {
      stop(paste0("table in sheet '",s,"' of '",workbook,
        "' is lacking mandatory column(s): '",paste(missing, collapse="', '"),"'"))
    }
  }
  # separate processes and stoichiometry
  x[["stoi"]] <- with(x, as.matrix(eqns[,vars[,"name"],drop=FALSE]))
  rownames(x[["stoi"]]) <- with(x, eqns[,"name"])
  x[["pros"]] <- with(x, eqns[,c("name","unit","rate","description")])
  names(x[["pros"]])[names(x[["pros"]]) == "rate"] <- "expression"
  x[["eqns"]] <- NULL
  # build and compile model object
  m <- with(x, rodeo$new(
    vars=vars,
    pars=pars,
    funs=if (fortran && is.null(sources)) NULL else funs,
    pros=pros,
    stoi=stoi,
    asMatrix=T,
    dim=dim
  ))
  m$compile(fortran=fortran, sources=sources)
  
  # return object
  m
}
