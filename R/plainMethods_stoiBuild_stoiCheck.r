# Contains functions to build and validate a stoichiometry matrix.

################################################################################
# PART 1: Non-exported utility functions
################################################################################

# Split reaction equation into left and right hand side and determine direction
splitEquation <- function(str) {
  str <- gsub(pattern="[' ']*", replacement="", x=str)
  fWard <- TRUE
  bWard <- TRUE
  x <- character(0)
  arrows <- c(reversible= "<->", forward= "->", backward= "<-") # order matters !
  for (i in 1:length(arrows)) {
    if (grepl(pattern=arrows[i], x=str, fixed=TRUE)) {
      x <- unname(unlist(strsplit(x=str, split=arrows[i], fixed=TRUE)))
      if (names(arrows)[i] == "backward")
        fWard <- FALSE
      if (names(arrows)[i] == "forward")
        bWard <- FALSE
      break
    }
  }
  if ((length(x) != 2) || any(nchar(x) == 0))
    stop("failed to split reaction equation '",str,
      "' into left and right hand side at one of '",
      paste(arrows, collapse="', '"),"'")
  return(list(left= x[1], right= x[2], backward=bWard, forward=fWard))
}

# Analyzes which characters in a string are within a parenthesized section
parenthesized <- function(str) {
  if (nchar(str) == 0)
    stop("got an empty string")
  x <- strsplit(str, "")[[1]]
  op <- cumsum(x == "(")
  cl <- cumsum(x == ")")
  if (op[length(op)] != cl[length(cl)])
    stop("unbalanced parenthesis in '",str,"'")
  res <- ((op - cl) != 0) & (x != ")")
  return(res)
}

# Split one side of a reaction equation into components; splitting occurs at
# '+' which are not part of a parenthesized section.
# The result is a vector where each element is the substring between the
# splitting locations.
splitComponents <- function(str) {
  if (nchar(str) == 0)
    stop("got an empty string")
  pos <- which((strsplit(str, "")[[1]] == "+") & (!parenthesized(str)))
  if (length(pos) == 0) {
    res <- str
  } else {
    if ((min(pos) == 1) || max(pos == nchar(str)))
    stop("character '+' occurs at bad position within '",str,"'")
    from <- c(1, pos + 1)
    to <- c(pos - 1, nchar(str)) 
    res <- character(length(from))
    for (i in 1:length(from))
      res[i] <- substr(str, from[i], to[i])
  }
  return(res)
}

# Identify the stoichiometric factor for a component; splitting occurs at
# '*' which is not part of a parenthesized section.
# The result is a named vector of length one (name = component name, value =
# the stoichiometric factor).

splitFactor <- function(str) {
  if (nchar(str) == 0)
    stop("got an empty string")
  pos <- which((strsplit(str, "")[[1]] == "*") & (!parenthesized(str)))
  if (length(pos) == 0) {
    res <- stats::setNames(1, str)
  } else if (length(pos) == 1) {
    if (pos %in% c(1,nchar(str)))
    stop("character '*' occurs at bad position within '",str,"'")
    res <- stats::setNames(substr(str, 1, pos-1), substr(str, pos+1, nchar(str)))
  } else {
    stop("need a unique location for splitting '",str,"' at '*' character")
  }
  return(res)
}

# Stoichiometric factors for a single reaction
stoiBuildSingle <- function(str, toRight="_forward", toLeft="_backward"
) {
  x <- splitEquation(str)
  lElems <- sapply(splitComponents(x[["left"]]), splitFactor, USE.NAMES=FALSE)
  rElems <- sapply(splitComponents(x[["right"]]), splitFactor, USE.NAMES=FALSE)
  if (!x[["backward"]]) # forward reaction
    return(matrix(c(paste0("-", lElems), rElems), nrow=1,
      dimnames=list("", c(names(lElems), names(rElems)))))
  else if (!x[["forward"]]) # backward reaction
    return(matrix(c(lElems, paste0("-", rElems)), nrow=1,
      dimnames=list("", c(names(lElems), names(rElems)))))
  else # reversible reaction
    return(matrix(c(paste0("-", lElems), rElems, lElems, paste0("-", rElems)),
      nrow=2, byrow=TRUE,
      dimnames=list(c(toRight,toLeft), c(names(lElems), names(rElems)))))
}

################################################################################
# PART 2: Exported high level functions
################################################################################

#' Stoichiometry Matrix from Reaction Equations
#'
#' Creates a stoichiometry matrix from a set of reaction equations.
#'
#' @param reactions A named vector of character strings, each representing
#'   a (chemical) reaction. See syntax details below.
#' @param eval Logical. If \code{FALSE} (default), the returned matrix is of
#'   type \code{\link[base]{character}} and any mathematical expressions are
#'   returned as text. If \code{TRUE}, an attempt is made to return a
#'   \code{\link[base]{numeric}} matrix by evaluating the expression making use
#'   \code{env}.
#' @param env Only relevant if \code{eval} is \code{TRUE}. Must be an
#'   environment or list supplying constants, functions, and operators needed to
#'   evaluate expressions in the generated matrix.
#' @param toRight Only relevant for reversible reactions. The passed character
#'   string is appended to the name of the respective element of
#'   \code{reactions} to create a unique name for the forward reaction.
#' @param toLeft Like \code{toRight}, but this is the suffix for the backward
#'   reaction.
#'
#' @return A matrix with the following properties:
#' \itemize{
#'   \item{The number of columns equals the total number of components present
#'     in \code{reactions}. The components' names are used as column names.}
#'   \item{The number of rows equals the length of \code{reactions} plus the
#'     number of reversible reactions. Thus, a single row is created for each
#'     non-reversible reaction but two rows are created for reversible ones.
#'     The latter represent the forward and backward reaction (in that order).
#'     The row names are constructed from the names of \code{reactions}, making
#'     use of the suffixes \code{toRight} and \code{toLeft} in the case of
#'     reversible reactions.}
#'   \item{The matrix is filled with the stoichiometric factors extracted from
#'     \code{reactions}. Empty elements are set to zero.}
#'   \item{The type of the matrix (\code{\link[base]{character}} or
#'     \code{\link[base]{numeric}}) depends on the value of \code{eval}.}
#' }
#'
#' @note The syntax rules for reaction equations are as follows (see examples):
#' \itemize{
#'   \item{There must be a left hand side and a right hand side. Sides must be
#'     separated by one of the arrows '->', '<-', or '<->' with the latter
#'     indicating a reversible reaction.}
#'   \item{Names of component(s) must appear at each side of the reaction. These
#'     must be legal row/column names in R. If multiple
#'     components are consumed or produced, they must be separated by '+'.}
#'   \item{Any stoichiometric factors need to appear before the respective
#'     component name using '*' as the separating character. Stoichiometric
#'     factors being equal to unity can be omitted.}
#'   \item{A stoichiometric factor is treated as a mathematical expression.
#'     In common cases, it is just a numeric constant. However, the
#'     expression can also involve references to variables or functions. If such
#'     an expression contains math operators '*' or '+' it needs to be enclosed
#'     in parenthesis.}
#' }
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @seealso Use \code{\link[rodeo]{stoiCheck}} to validate the mass balance of
#'   the generated matrix.
#'
#' @export
#'
#' @examples
#' # EXAMPLE 1: From https://en.wikipedia.org/wiki/Petersen_matrix (July 2016)
#' #
#' reactions <- c(
#'   formS=  "A + 2 * B -> S",
#'   equiES= "E + S <-> ES",
#'   decoES= "ES -> E + P"
#' )
#' stoi <- stoiCreate(reactions, eval=TRUE, toRight="_f", toLeft="_b")
#' print(stoi)
#'
#' # EXAMPLE 2: Decomposition of organic matter (selected equations only)
#' #
#' # Eq. 1 and 2 are from Soetaert et al. (1996), Geochimica et Cosmochimica
#' # Acta, 60 (6), 1019-1040. 'OM' is organic matter. Constants 'nc' and 'pc'
#' # represent the nitrogen/carbon and phosphorus/carbon ratio, respectively.
#' reactions <- c(
#'   oxicDegrad= "OM + O2 -> CO2 + nc * NH3 + pc * H3PO4 + H2O",
#'   denitrific= "OM + 0.8*HNO3 -> CO2 + nc*NH3 + 0.4*N2 + pc*H3PO4 + 1.4*H2O",
#'   dissPhosp1= "H3PO4 <-> H + H2PO4",
#'   dissPhosp2= "H2PO4 <-> H + HPO4"
#' )
#' # Non-evaluated matrix
#' stoi <- stoiCreate(reactions, toRight="_f", toLeft="_b")
#' print(stoi)
#' # Evaluated matrix ('nc' and 'pc' according to Redfield ratio)
#' pars <- list(nc=16/106, pc=1/106)
#' stoi <- stoiCreate(reactions, eval=TRUE, env=pars, toRight="_f", toLeft="_b")
#' print(stoi)

stoiCreate <- function(reactions, eval=FALSE, env=globalenv(),
  toRight="_forward", toLeft="_backward"
) {
  if (!is.character(reactions) || is.null(names(reactions)))
    stop("'reactions' must be a named character vector")
  if (!is.logical(eval) || (length(eval) != 1))
    stop("'eval' should be TRUE or FALSE")
  if (!is.environment(env) && !is.list(env))
    stop("'env' must be an environment or list")
  # examine stoichiometry of individual reactions
  x <- sapply(reactions, stoiBuildSingle, toRight=toRight, toLeft=toLeft, simplify=FALSE)
  # adjust reaction names (important for reversible reactions)
  for (i in 1:length(x))
    rownames(x[[i]]) <- paste0(names(x)[i], rownames(x[[i]]))
  # initialize stoichiometry matrix
  specNames <- unique(as.character(unlist(sapply(x, colnames))))
  procNames <- as.character(unlist(sapply(x, rownames)))
  dup <- procNames[duplicated(procNames)]
  if (length(dup) > 0) # either original duplicates or due to appended suffix
    stop("duplicates in reaction name(s) '",paste(dup, collapse="', '"),"'")
  stoi <- matrix("0", nrow=length(procNames), ncol=length(specNames),
    dimnames=list(procNames, specNames))
  # fill stoichiometry matrix
  for (i in 1:length(x)) {
    for (k in 1:nrow(x[[i]])) {
      stoi[rownames(x[[i]])[k], colnames(x[[i]])] <- x[[i]][k,]
    }
  }
  # if possible, convert from character to numeric
  if (eval)
    tryCatch({
      stoi <- apply(stoi, 1:2, function(x,e){eval(parse(text=x), envir=e)}, e=env)
    }, error= function(msg) {
      stop("failed to evaluate expression in generated matrix; details: ",msg)
    })
  return(stoi)
}

#' Validation of a Stoichiometry Matrix
#'
#' Validates the stoichiometry matrix by checking for conservation of mass
#' (more typically conservation of moles).
#'
#' @param stoi Stoichiometry matrix either in evaluated
#'   (\code{\link[base]{numeric}}) or non-evaluated
#'   (\code{\link[base]{character}}) form. A suitable matrix can be created with
#'   \code{\link{stoiCreate}}, for example.
#' @param comp Matrix defining the elemental composition of compounds.
#'   Column names of \code{comp} need to match column names of \code{stoi} (but
#'   additional columns are allowed and columns can be in different order).
#'   There must be one row per element whose balance is to be checked and the
#'   elements' names must appear as row names. The elements of the matrix
#'   specify how much of an element is contained in a certain amount of a
#'   compound. Typically, these are molar ratios. If one works with mass ratios
#'   (not being a good idea), the information in \code{stoi} must be based on mass
#'   concentrations as well.
#'   The elements of \code{comp} are treated as mathematical expressions. Any
#'   variables, functions, or operators needed to evaluate those expressions
#'   must be provided by the specified environment \code{env}.
#' @param env An environment or list supplying constants, functions, and
#'   operators needed to evaluate expressions in \code{comp} or \code{stoi}.
#' @param zero A number close to zero. If the absolute result value of a mass
#'   balance computation is less than this, the result is set to 0 (exactly).
#'
#' @return A numeric matrix with the following properties:
#' \itemize{
#'   \item{There is one row for each process, thus the number and names of rows
#'     are the same as in \code{stoi}.}
#'   \item{There is one column per checked element, hence column names are
#'     equal to the row names of \code{comp}.}
#'   \item{A matrix element at position \eqn{[i,k]} represent the mass balance
#'     for the process in row \eqn{i} with respect to the element in column
#'     \eqn{k}. A value of zero indicates a close balance. Positive (negative)
#'     values indicate that mass is gained (lost) in the respective process.}
#' }
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @seealso Use \code{\link[rodeo]{stoiCreate}} to create a stoichiometry matrix
#'   from a set of reactions in common notation.
#'
#' @export
#'
#' @examples
#' # Eq. 1 and 2 are from Soetaert et al. (1996), Geochimica et Cosmochimica
#' # Acta, 60 (6), 1019-1040. 'OM' is organic matter. Constants 'nc' and 'pc'
#' # represent the nitrogen/carbon and phosphorus/carbon ratio, respectively.
#' reactions <- c(
#'   oxicDegrad= "OM + O2 -> CO2 + nc * NH3 + pc * H3PO4 + H2O",
#'   denitrific= "OM + 0.8*HNO3 -> CO2 + nc*NH3 + 0.4*N2 + pc*H3PO4 + 1.4*H2O",
#'   dissPhosp1= "H3PO4 <-> H + H2PO4",
#'   dissPhosp2= "H2PO4 <-> H + HPO4"
#' )
#' # Non-evaluated stoichiometry matrix
#' stoi <- stoiCreate(reactions, toRight="_f", toLeft="_b")
#' # Parameters ('nc' and 'pc' according to Redfield ratio)
#' pars <- list(nc=16/106, pc=1/106)
#' # Elemental composition
#' comp <- rbind(
#'   OM=    c(C=1, N="nc", P="pc", H="2 + 3*nc + 3*pc"),
#'   O2=    c(C=0, N=0,    P=0,    H=0),
#'   CO2=   c(C=1, N=0,    P=0,    H=0),
#'   NH3=   c(C=0, N=1,    P=0,    H=3),
#'   H3PO4= c(C=0, N=0,    P=1,    H=3),
#'   H2PO4= c(C=0, N=0,    P=1,    H=2),
#'   HPO4=  c(C=0, N=0,    P=1,    H=1),
#'   H2O=   c(C=0, N=0,    P=0,    H=2),
#'   HNO3=  c(C=0, N=1,    P=0,    H=1),
#'   N2=    c(C=0, N=2,    P=0,    H=0),
#'   H=     c(C=0, N=0,    P=0,    H=1)
#' )
#' # We need the transposed form
#' comp <- t(comp)
#' # Mass balance check
#' bal <- stoiCheck(stoi, comp=comp, env=pars)
#' print(bal)
#' print(colSums(bal) == 0)

stoiCheck <- function(stoi, comp, env=globalenv(), zero=.Machine$double.eps*2) {
  # check arguments
  if (!is.matrix(stoi) || is.null(colnames(stoi)) || is.null(rownames(stoi)))
    stop("'stoi' must be a matrix with row and column names")
  if (!is.matrix(comp) || is.null(colnames(comp)) || is.null(rownames(comp)))
    stop("'comp' must be a matrix with row and column names")
  bad <- colnames(stoi)[!colnames(stoi) %in% colnames(comp)]
  if (length(bad) > 0)
    stop("missing columns in matrix 'comp' for components '",
      paste(bad, collapse="', '"),"'")
  if (!is.environment(env) && !is.list(env))
    stop("'env' must be an environment or list")
  # evaluate expressions in the matrices
  tryCatch({
    stoi <- apply(stoi, 1:2, function(x,e){eval(parse(text=x), envir=e)}, e=env)
  }, error= function(msg) {
    stop("failed to evaluate expression in 'stoi'; details: ",msg)
  })
  tryCatch({
    comp <- apply(comp, 1:2, function(x,e){eval(parse(text=x), envir=e)}, e=env)
  }, error= function(msg) {
    stop("failed to evaluate expression in 'comp'; details: ",msg)
  })
  # compute balance using the composition matrix
  bal <- matrix(NA, ncol=nrow(comp), nrow=nrow(stoi),
    dimnames=list(rownames(stoi), rownames(comp)))
  for (i in 1:nrow(comp)) {
    bal[,i] <- apply(stoi * matrix(rep(comp[i,colnames(stoi)], nrow(stoi)),
      ncol=ncol(stoi), byrow=TRUE), 1, sum)
  }
  bal[abs(bal) <= zero] <- 0
  return(bal)
}

