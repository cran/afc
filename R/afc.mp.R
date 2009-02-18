afc.mp = function(obsv,fcst,m=3){

  #################################
  # OBSV: POLYCHOTOMOUS (ORDINAL) #
  # FCST: PROBABILISTIC           #
  #################################

  # input variables:
  # ----------------
  # m   - Number of observation categories (default = 3)
  # fcst - array(n,m) of n probabilistic forecasts for the m categories
  # obsv - vector with observation categories (1...m)
  #
  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 16 


  # determine number of observations per category
  n.vector = rep(NA,m)
  for (k in 1:m) n.vector[k] = length(which(obsv == k))

  # calculate Eq. 16
  numer = 0
  denom = 0
  # Calculate outer two sums in Eq. 16a
  for (k in 1:(m-1)) for (l in (k+1):m){
    index.k = which(obsv == k)
    index.l = which(obsv == l)
    p.k = fcst[index.k,]
    p.l = fcst[index.l,]
    test = 0
    # Calculate inner two sums in Eq. 16a
    for (i in 1:n.vector[k]) for (j in 1:n.vector[l]){
      p.ki = p.k[i,]
      p.lj = p.l[j,]
      numer11 = 0
      #calculate F in Eq. 16c
      for (r in 1:(m-1)) for (s in (r+1):m){
        numer11 = numer11 + p.ki[r]*p.lj[s]
      }
      f = numer11/(1-sum(p.ki*p.lj))
      if (!is.finite(f)) f = 0.5    
      test = test + 0.5*(f == 0.5) + (f > 0.5)
    }
    numer = numer + test
    denom = denom + n.vector[l]*n.vector[k]
  }

  p.afc = numer/denom
  type.flag = 1
  return(p.afc)
}
