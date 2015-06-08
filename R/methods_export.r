
# Helper function used in exportDF
setOpt= function(x, defaults, colnames) {
  res= defaults
  if (!is.null(x)) {
    if (is.null(names(x)) || any(names(x) == ""))
      stop("all elements in 'x' must be named")
    if (!all(names(x) %in% colnames))
      stop("element name(s) of 'x' not in 'colnames'")
    i= match(colnames, names(x))
    res= ifelse(is.na(i), res, x[i])
  }
  return(res)
}

#' Export data frame as HTML/TEX code
#'
#' Generates code to include tabular data in a tex document or web site.
#'
#' @param x The data frame being exported.
#' @param tex Logical. Allows to switch between generation of TEX code and HTML.
#' @param colnames Displayed column names. If \code{NULL}, the original names
#'   of \code{x} are used. Otherwise it must be a named vector with element
#'   names corresponding to column names in \code{x}. It is OK to supply
#'   alternative names for selected columns only.
#' @param width Either \code{NULL} (all columns get equal width) or a named
#'   vector with element names corresponding to column names in \code{x}. If
#'   \code{tex == TRUE}, values (between 0 and 1) are needed for columns with
#'   align code 'p' only. They are interpreted as a multiplier for '\\textwidth'.
#'   If \code{tex == FALSE}, values (between 0 and 100) should be
#'   supplied for all columns of \code{x}.
#' @param align Either \code{NULL} (to use automatic alignment) or a named
#'   vector with element names corresponding to column names in \code{x}.
#'   If \code{tex == FALSE} valid alignment codes are 'left', 'right', 'center'.
#'   If \code{tex == TRUE} valid alignment codes are 'l', 'r', 'c', and 'p'. For
#'   columns with code 'p' a corresponding value of \code{width} should be set.
#'   It is OK to supply alignment codes for selected columns only.
#' @param beforeHead Either \code{NULL} or a named character
#'   vector with element names corresponding to column names in \code{x}.
#'   The supplied strings (typically formatting command in the respective target
#'   language, see examples) are inserted before the column names.
#'   It is OK to supply such strings for selected columns only.
#' @param afterHead Lile \code{beforeHead} but these strings are inserted right
#'   after the column names.
#' @param beforeCell Like \code{beforeHead} but for non-header cells.
#' @param afterCell Like \code{afterHead} but for non-header cells.
#' @param lines Logical. Switches table borders on/off.
#' @param indent Integer. Number of blanks used to indent the generated code.
#'
#' @return A character string (usually needs to be exported to a file).
#'
#' @seealso The \code{xtable} packages provides similar functionality with
#'   more sophisticated options. Consider the 'pandoc' software do convert
#'   documents from one markup language to another one. Finally, consider the
#'   latex package 'datatools' for direct inclusion of delimited text files
#'   (e.g. produced by \code{write.table}) in tex documents.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' # Create example table
#' df= data.frame(stringsAsFactors=FALSE, name= c("growth", "dead"),
#'   unit= c("1/d","1/d"), expression= c("r * N * (1 - N/K)"," d * N"))
#'
#' # Export as TEX: header in bold, 1st colum in italics, last column as math
#' tex= exportDF(df, tex=TRUE,
#'   colnames=c(expression="process rate expression"),
#'   width=c(expression=0.5),
#'   align=c(expression="p"),
#'   beforeHead=setNames(rep("\\textbf{",ncol(df)),names(df)),
#'   afterHead=setNames(rep("}",ncol(df)),names(df)),
#'   beforeCell=c(name="\\textit{",expression="$"),
#'   afterCell=c(name="}",expression="$")
#' )
#' cat(tex,"\n")
#'
#' # Export as HTML: non-standard colors are used for all columns
#' tf= tempfile(fileext=".html")
#' write(x= exportDF(df, tex=FALSE,
#'   beforeHead=setNames(rep("<font color='red'>",ncol(df)),names(df)),
#'   afterHead=setNames(rep("</font>",ncol(df)),names(df)),
#'   beforeCell=setNames(rep("<font color='blue'>",ncol(df)),names(df)),
#'   afterCell=setNames(rep("</font>",ncol(df)),names(df))
#' ), file=tf)
#' \dontrun{
#'   browseURL(tf)
#'   file.remove(tf)
#' }

exportDF= function(x,
  tex=FALSE,
  colnames=NULL,
  width= NULL,
  align= NULL,
  beforeHead= NULL,
  afterHead= NULL,
  beforeCell= NULL,
  afterCell= NULL,
  lines=TRUE,
  indent=2
) {
  indent= ifelse(indent <= 0, "", paste0(rep(" ",indent),collapse=""))
  # Check input
  if (is.matrix(x))
    x= as.data.frame(x, stringsAsFactors=FALSE)
  if (!is.data.frame(x))
    stop("'x' must be  data frame")
  # Set options
  left= ifelse(tex, "l", "left")
  right= ifelse(tex, "r", "right")
  boldBeg= ifelse(tex, "\\textbf{", "<b>")
  boldEnd= ifelse(tex, "}", "</b>")
  w= ifelse(tex, 1/ncol(x), floor(100/ncol(x)))
  colnames= setOpt(colnames, names(x), names(x))
  width= setOpt(width, rep(w, ncol(x)), names(x))
  align= setOpt(align, ifelse(unlist(lapply(df, FUN=is.numeric)),right,left), names(x))
  beforeHead= setOpt(beforeHead, rep(boldBeg, ncol(x)), names(x))
  afterHead=  setOpt(afterHead, rep(boldEnd, ncol(x)), names(x))
  beforeCell= setOpt(beforeCell, rep("", ncol(x)), names(x))
  afterCell=  setOpt(afterCell, rep("", ncol(x)), names(x))
  # Assemble code
  out=''

  # tex
  if (tex) {
    i= which(align == "p")
    if (length(i) > 0) {
      align[i]= paste0(align[i],"{",width[i],"\\textwidth}")
    }
    out= paste0(out,indent,'\\begin{tabular}{',paste(align,collapse=""),
      '}',ifelse(lines, '\\hline', ''),'\n')
    out= paste0(out,indent,'  ',
      paste0(paste0('',beforeHead,colnames,afterHead,''),collapse=' & '),' \\\\',
      ifelse(lines, ' \\hline', ''),'\n')
    for (i in 1:nrow(x)) {
      out= paste0(out,indent,'  ',
        paste0(paste0(beforeCell,unlist(x[i,]),afterCell),collapse=' & '),' \\\\',
        ifelse(lines && (i == nrow(x)), ' \\hline', ''),'\n')
    }
    out= paste0(out,indent,'\\end{tabular}\n')

  # html
  } else {
    out= paste0(out,indent,'<table border=',ifelse(lines,1,0),'>\n')
    for (i in 1:length(width)) {
      out= paste0(out,indent,'  <col width="',width[i],'%">\n')
    }
    out= paste0(out,indent,'  <tr>',
      paste0(paste0('<th style="text-align:',align,'"> ',beforeHead,
      colnames,afterHead,' </th>'),collapse=''),' </tr>\n')
    for (i in 1:nrow(x)) {
      out= paste0(out,indent,'  <tr>',
        paste0(paste0('<td style="text-align:',align,'"> ',beforeCell,
        unlist(x[i,]),afterCell,' </td>'),collapse=''),' </tr>\n')
    }
    out= paste0(out,indent,'</table>\n')
  }

  return(out)
}

