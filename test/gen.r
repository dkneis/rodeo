#################################################
###  THIS IS A GENERATED FILE -- DO NOT EDIT  ###
#################################################

reac = function (time, var, par, NLVL, check=TRUE) {

  # Check length of arguments
  if (check) {
    if (length(var) != (3 * NLVL))
      stop(paste0("length of argument 'var' is ",length(var),
        " but it should be "3 * NLVL,
        " (number of variables * number of levels)"))
  }
  # Lists of array indices
  v0D = list(    c_do=1, c_z=2, v=3  )
  p0D = list(    q_in=1, q_ex=2, c_z_in=3, c_do_in=4, kd=5, s_do_z=6, h_do=7, temp=8, wind=9, depth=10  )
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
  pro = unname(fun_pro0D(1:NLVL))

  # Internal function: Derivatives at a particular level
  fun_dydt0D = function (level) {
    # Update indices
    v = lapply(v0D, adjIdx ,NLVL=NLVL, level=level)
    p = lapply(p0D, adjIdx ,NLVL=NLVL, level=level)
    r = lapply(r0D, adjIdx ,NLVL=NLVL, level=level)
    # Set return vector
    dydt0D= c(
      # Variable 'c_do'
       pro[r$flushing] * (par[p$c_do_in] - var[v$c_do]) +  pro[r$decay] * (-par[p$s_do_z]) +  pro[r$aeration] * (1)
    ,
      # Variable 'c_z'
       pro[r$flushing] * (par[p$c_z_in] - var[v$c_z]) +  pro[r$decay] * (-1)
    ,
      # Variable 'v'
       pro[r$flow] * (1)
    )
  }

  # Set vector of derivatives (all spatial levels)
  dydt = unname(fun_dydt0D(1:NLVL))

  # Return a list
  return(list(dydt=dydt,pro=pro))
}

