# Extracts function names from model description
namesOfFuns= function(obj) {
  # Merge all expressions into a single string
  string= paste(paste(obj$auxx, collapse=";"),paste(obj$proc, collapse=";"),paste(unlist(obj$stox), collapse=";"), sep=";")
  # Collect names of functions
  pos= gregexpr(pattern="[a-zA-Z]+[0-9_a-zA-Z]*[ ]*[(]", text=string)[[1]]
  if (pos[1] == -1) {
    fnames= character(0)
  } else {
    fnames= vector("character",length=length(pos))
    for (i in 1:length(pos))
      fnames[i]= substr(string, pos[i], (pos[i]+attr(pos,which="match.length",exact=TRUE)[i]-2))
  }
  return(sort(fnames))
}

# Extracts parameter names from model description
namesOfPars=  function(obj) {
  # Merge all expressions into a single string
  string= paste(paste(obj$auxx, collapse=";"),paste(obj$proc, collapse=";"),paste(unlist(obj$stox), collapse=";"), sep=";")
  # Collect names of all items
  string= gsub(pattern="[-+*/^(),;]", replacement=" ",x=string)    # replace operators etc.
  string= gsub(pattern="(^[ ]+|[ ]+$)", replacement="", x=string) # trim leading/trailing spaces
  items= unique(strsplit(x=string, split="[ ]+")[[1]])            # split at spaces
  items= as.list(items)
  items[unlist(items) %in% namesOfFuns(obj)]= NULL  # sort out function names
  items[unlist(items) %in% names(obj$stox)]= NULL  # sort out names of state variables
  items[unlist(items) %in% names(obj$auxx)]= NULL  # sort out names of auxiliary expressions
  items[regexpr(pattern="^[^a-zA-Z]",text=unlist(items)) != -1]= NULL  # sort out plain numeric constants
  return(sort(unlist(items)))
}

