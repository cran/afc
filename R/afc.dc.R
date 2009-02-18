afc.dc= function(obsv,fcst){

  #####################
  # OBSV: DICHOTOMOUS #
  # FCST: CONTINUOUS  #
  #####################

  # input variables:
  # ----------------
  # fcst - vector with continuous forecast values (real values)
  # obsv - vector with corresponding observations (values 0 and 1)

  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eqs. 8

  #determine number of events and non-events
  event.index = which(obsv == 1)
  n1 = length(event.index)
  n0 = length(obsv)-n1

  #condition forecasts on whether or not an event has occurred
  fcst.1 = fcst[event.index]
  fcst.0 = fcst[-event.index]

  #determine ranks of forecasts which correspond to event
  rank.1 = rank(fcst)[event.index]

  #calculate Eq. 8 of MW09
  p.afc = (sum(rank.1) - n1*(n1+1)/2)/(n1*n0)
  type.flag=1
  return(p.afc)
}
