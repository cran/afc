afc.nn = function(obsv,fcst,m=3){

  #################################
  # OBSV: POLYCHOTOMOUS (NOMINAL) #
  # FCST: POLYCHOTOMOUS (NOMINAL) #
  #################################

  # input variables:
  # ----------------
  # fcst - vector with forecast categories (1...m)
  # obsv - vector with observation categories (1...m)
  # m   - Number of forecast categories (default = 3)
  #
  # Here it is assumed that both observations and forecasts
  # have the same number of categories

  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 15 (corrigendum)

  
  # Matrix with element[nn,mm] being the number of times
  # that nn has been observed and mm has been forecast
  n.matrix = array(0,dim=c(m,m))
  for (nn in 1:m) for (mm in 1:m){
    n.matrix[nn,mm] = sum((obsv == nn) & (fcst == mm))
  }

  # Solve Eq. 15 of corrigendum of MW09
  numer = 0
  denom = 0
  for (k in 1:m) for (l in (1:m)[-k]){
    term1 = 0
    term2 = 0
    term3 = 0
    for (i in (1:m)[-k]) term1 = term1 + n.matrix[k,k]*n.matrix[l,i]
    for (i in (1:m)[-k]) for (j in (1:m)[-c(k,l)])
      term2 = term2 + n.matrix[k,l]*n.matrix[i,j]
    for (i in 1:m) term3 = term3 + n.matrix[k,i]*n.matrix[l,i]
    numer = numer + term1 + 0.5*term2 + 0.5*term3
    denom = denom + sum(n.matrix[k,])*sum(n.matrix[l,])
  }

  p.afc = numer/denom
  type.flag = 1
  return(p.afc)

}
