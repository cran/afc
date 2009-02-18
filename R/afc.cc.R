afc.cc = function(obsv,fcst){

  ####################
  # OBSV: CONTINUOUS #
  # FCST: CONTINUOUS #
  ####################

  # input variables:
  # ----------------
  # fcst - vector with real-valued forecasts
  # obsv - vector with real-valued observations
  #
  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 22 after ranking

  
  # Apply Eq. 22 in MW09
  p.afc = 0.5*(1+cor(fcst,obsv,method="kendall"))
  type.flag = 1
  return(p.afc)

}
