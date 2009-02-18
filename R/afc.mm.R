afc.mm = function(obsv,fcst,mv=3,mf=3){

  #################################
  # OBSV: POLYCHOTOMOUS (ORDINAL) #
  # FCST: POLYCHOTOMOUS (ORDINAL) #
  #################################

  # input variables:
  # ----------------
  # fcst - vector with forecast categories (1...mf)
  # obsv - vector with observation categories (1...mv)
  # mv   - Number of observation categories (default = 3)
  # mf   - Number of forecast categories (default = mv)

  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 14


  # set mv = mf if mv is not given
  if (mf == 0) mf=mv

  # Matrix with element[nn,mm] being the number of times
  # that nn has been observed and mm has been forecast
  n.matrix = array(0,dim=c(mv,mf))
  for (nn in 1:mv) for (mm in 1:mf){
    n.matrix[nn,mm] = sum((obsv == nn) & (fcst == mm))
  }

  # Solve Eq. 14 of MW09
  numer = 0
  denom = 0
  for (k in 1:(mv-1)) for (l in (k+1):mv){
    term1 = 0
    term2 = 0
    for (i in 1:(mf-1)) for (j in (i+1):mf){
      term1 = term1 + n.matrix[k,i]*n.matrix[l,j]
    }
    for (i in 1:mf){
      term2 = term2 + n.matrix[k,i]*n.matrix[l,i]
    }
    numer = numer + term1 + 0.5*term2
    denom = denom + sum(n.matrix[k,])*sum(n.matrix[l,])
  }
  p.afc = numer/denom
  type.flag = 1
  return(p.afc)
}
