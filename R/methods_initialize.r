rodeo$methods(
  initialize = function(file, keyPros="processes", keyStox="stoichiometry",
    keyAuxs="auxiliary"
) {
    "Initializes a \\code{rodeo} object with json-formatted data from \\code{file}"
    if (!is.character(file) || !(length(file) == 1) || !file.exists(file))
      stop("argument must be a valid file name")
    tryCatch({
      x= fromJSON(file=file)
    }, error= function(e) {
      stop(paste("Failed to read JSON data. Details: ",e,sep=""))
    })
    if ((!is.list(x)) || (!all(c(keyPros, keyStox, keyAuxs) %in% names(x))))
      stop("missing components in initialization data")
    m= matrix(rep("0", length(x[[keyStox]])*length(x[[keyPros]])),
      ncol=length(x[[keyStox]]), nrow=length(x[[keyPros]]))
    m= data.frame(m, stringsAsFactors=FALSE, row.names=names(x[[keyPros]]))
    for (i in 1:length(x[[keyStox]])) {
      names(m)[i]= names(x[[keyStox]][i])
      for (k in 1:length(x[[keyStox]][[i]])) {
        if (!(names(x[[keyStox]][[i]][k]) %in% names(x[[keyPros]])))
          stop(paste("found stoichiometry factor for state variable '",
            names(x[[keyStox]][i]),"' referring to undefined process '",
            names(x[[keyStox]][[i]][k]),"'"))
        m[names(x[[keyStox]][[i]][k]), i]= x[[keyStox]][[i]][[k]]
      }
    }
    # Initialize all fields
    auxs <<- unlist(x[[keyAuxs]])
    pros <<- unlist(x[[keyPros]])
    stox <<- m[,names(m)[order(names(m))]]  # sorted by variable names
    rm(m)
    vars <<- names(stox)        # also sorted by names
    funs <<- namesOfFuns(.self) # also sorted by names
    pars <<- namesOfPars(.self) # also sorted by names
    return(invisible(NULL))
  }
)

