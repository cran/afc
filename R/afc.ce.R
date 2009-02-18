afc.ce = function(obsv,fcst){

  ####################
  # OBSV: CONTINUOUS #
  # FCST: ENSEMBLES  #
  ####################

  # input variables:
  # ----------------
  # fcst - array(n,nens) of n ensemble forecasts with ensemble size nens
  # obsv - vector with real-valued observations
  #
  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 22


  #Rank ensembles
  ranks = rank.ensembles(fcst)

  # Apply Eq. 22 in MW09
  p.afc = 0.5*(1+cor(ranks,obsv,method="kendall"))
  type.flag = 1
  return(p.afc)

}
