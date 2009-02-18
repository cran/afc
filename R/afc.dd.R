afc.dd = function(obsv,fcst){

  #####################
  # OBSV: DICHOTOMOUS #
  # FCST: DICHOTOMOUS #
  #####################

  # input variables:
  # ----------------
  # fcst - vector with forecasts (values 0 and 1)
  # obsv - vector with corresponding observations (values 0 and 1)

  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 2


  # Condition forecasts on whether an event has occured
  fcst.1 = fcst[which(obsv == 1)]
  fcst.0 = fcst[which(obsv == 0)]

  # Calculate entries of contingency table
  a = sum(fcst.1)
  b = sum(fcst.0)
  c = length(fcst.1)-a
  d = length(fcst.0)-b

  p.afc = (a*d + 0.5*(a*b + c*d))/((a+c)*(b+d))

  return(p.afc)
}
