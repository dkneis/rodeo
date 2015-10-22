#################################################
###  THIS IS A GENERATED FILE -- DO NOT EDIT  ###
#################################################

reac = function (time, var, par, NLVL, check=TRUE) {

  # Dimension constants
  NVAR=3
  NPAR=8
  NPRO=4

  # Check length of arguments
  if (check) {
    if (length(var) != (NVAR * NLVL))
      stop(paste0("length of argument 'var' is ",length(var),
        " but it should be ",NVAR * NLVL,
        " (number of variables * number of levels)"))
    if (length(par) != (NPAR * NLVL))
      stop(paste0("length of argument 'par' is ",length(par),
        " but it should be ",NPAR * NLVL,
        " (number of parameters * number of levels)"))
  }
  # Lists of array indices
  v0D = list(    c_do=1, c_z=2, v=3  )
  p0D = list(    q_in=1, q_ex=2, kd=3, s_do_z=4, h_do=5, temp=6, wind=7, depth=8  )
  r0D = list(    flow=1, flushing=2, decay=3, aeration=4  )

  # Function to update indices for particular level(s)
  adjIdx= function (x, NLVL, level) { (x - 1) * NLVL + level }

  # Internal function: Process rates at a particular level
  fun_pro0D = function (level) {
    # Update indices
    v = lapply(v0D, adjIdx ,NLVL=NLVL, level=level)
    p = lapply(p0D, adjIdx ,NLVL=NLVL, level=level)
    # Set return vector
    pro0D=c(
      # Process rate 'flow'
      par[p$q_in] - par[p$q_ex]
    ,
      # Process rate 'flushing'
      par[p$q_in] / var[v$v]
    ,
      # Process rate 'decay'
      par[p$kd] * var[v$c_z] * monod(var[v$c_do], par[p$h_do])
    ,
      # Process rate 'aeration'
      ka(par[p$wind],par[p$depth]) * (O2sat(par[p$temp]) - var[v$c_do])
    )
  }

  # Set vector of process rates (all spatial levels)
  pro = as.vector(t(vapply(X= 1:NLVL, FUN= fun_pro0D, 
    FUN.VALUE= numeric(NPRO), USE.NAMES=FALSE)))

  # Internal function: Derivatives at a particular level
  fun_dydt0D = function (level) {
    # Update indices
    v = lapply(v0D, adjIdx ,NLVL=NLVL, level=level)
    p = lapply(p0D, adjIdx ,NLVL=NLVL, level=level)
    r = lapply(r0D, adjIdx ,NLVL=NLVL, level=level)
    # Set return vector
    dydt0D= c(
      # Variable 'c_do'
       pro[r$flushing] * (c_do_in(time) - var[v$c_do]) +  pro[r$decay] * (-par[p$s_do_z]) +  pro[r$aeration] * (1)
    ,
      # Variable 'c_z'
       pro[r$flushing] * (c_z_in(time) - var[v$c_z]) +  pro[r$decay] * (-1)
    ,
      # Variable 'v'
       pro[r$flow] * (1)
    )
  }

  # Set vector of derivatives (all spatial levels)
  dydt = as.vector(t(vapply(X= 1:NLVL, FUN= fun_dydt0D, 
    FUN.VALUE= numeric(NVAR), USE.NAMES=FALSE)))

  # Return a list
  return(list(dydt=dydt,pro=pro))
}

