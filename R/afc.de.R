afc.de = function(obsv,fcst){

  #####################
  # OBSV: DICHOTOMOUS #
  # FCST: ENSEMBLES   #
  #####################

  # input variables:
  # ----------------
  # fcst - array(n,nens) of n ensemble forecasts with ensemble size nens
  # obsv - vector with corresponding observations (values 0 and 1)

  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 8 after ranking
  

  #determine number of events and non-events
  event.index = which(obsv == 1)
  n1 = length(event.index)
  n0 = length(obsv)-n1

  #condition ranks of ensembles on event-occurrence
  ranks = rank.ensembles(fcst)
  rank.1 = ranks[event.index]

  #calculate Eq. 8 of MW09
  p.afc = (sum(rank.1) - n1*(n1+1)/2)/(n1*n0)
  type.flag=1
  return(p.afc)
}
