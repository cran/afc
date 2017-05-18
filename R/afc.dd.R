#' 2AFC For Dichotomous Observations And Dichotomous Forecasts
#' 
#' Routine to calculate the Generalized Discrimination Score (aka
#' Two-Alternatives Forced Choice Score 2AFC) for the situation of dichotomous
#' observations and dichotomous forecasts
#' 
#' This routine applies Eq.2 of Mason and Weigel (2009) to calculate the 2AFC.
#' 
#' @param obsv vector with dichotomous observations (values in {0,1})
#' @param fcst vector of same length as \emph{obsv} with dichotomous forecasts
#' (values in {0,1})
#' @return \item{ p.afc }{ Value of Generalized Discrimination (2AFC) Score }
#' @author Andreas Weigel, Federal Office of Meteorology and Climatology,
#' MeteoSwiss, Zurich, Switzerland
#' @seealso \code{\link{afc}}
#' @references S.J. Mason and A.P. Weigel, 2009. A generic verification
#' framework for administrative purposes. Mon. Wea. Rev., 137, 331-349
#' @keywords file
#' @examples
#' 
#'   #Forecasts and observations of Nino-3.4 index
#'   #Load set of dichotomous observations and dichotomous forecasts
#'   data(cnrm.nino34.dd)
#'   obsv = cnrm.nino34.dd$obsv
#'   fcst = cnrm.nino34.dd$fcst
#' 
#'   #Calculate skill score
#'   afc.dd(obsv,fcst)
#' 
#' @export afc.dd
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
