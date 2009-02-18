afc.np = function(obsv,fcst,m=3){

  #################################
  # OBSV: POLYCHOTOMOUS (NOMINAL) #
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
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 17 


  # determine number of observations per category
  n.vector = rep(NA,m)
  for (l in 1:m) n.vector[l] = length(which(obsv == l))

  # Calculate Eq. 17 in MW09
  
  numer = 0
  denom = 0
  # Outer two sums in Eq. 17a
  for (l in 1:m) for (k in (1:m)[-l]){
    index.k = which(obsv == k)
    index.l = which(obsv == l)
    p.kl = fcst[index.k,l]
    p.ll = fcst[index.l,l]
    test = 0
    # Inner two sums in Eq. 17a
    for (i in 1:n.vector[k]) for (j in 1:n.vector[l])
      test = test + 0.5*(p.kl[i] == p.ll[j]) + (p.kl[i]<p.ll[j])
    numer = numer + test
    denom = denom + n.vector[l]*n.vector[k]
  }
  p.afc = numer/denom
  type.flag = 1
  return(p.afc)
}
