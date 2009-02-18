afc.mc = function(obsv,fcst,m=3){

  #################################
  # OBSV: POLYCHOTOMOUS (ORDINAL) #
  # FCST: CONTINUOUS              #
  #################################

  # input variables:
  # ----------------
  # m    - Number of observation categories (default = 3)
  # fcst - vector with continuous forecast values (real values)
  # obsv - vector with observation categories (1...m)
  #
  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 18 


  # determine number of observations per category
  n.vector = rep(NA,m)
  for (k in 1:m){
    n.vector[k] = length(which(obsv== k))
  }

  # calculate Eq. 18 in MW09
  numer = 0
  denom = 0
  for (k in 1:(m-1)) for (l in (k+1):m){
    # index for obsv of categories which are to be compared
    index.event = which(obsv == k | obsv == l)
    # Determine ranks of forecasts of event 
    fine.index = which(obsv[index.event] == l)
    ranks = rank(fcst[index.event])[fine.index]
    # Calculate inner sum in Eq. 18 of MW09
    numer = numer + sum(ranks) - 0.5*n.vector[l]*(n.vector[l]+1)
    denom = denom + n.vector[k]*n.vector[l]
  }
  p.afc = numer/denom
  type.flag = 1
  return(p.afc)
}
