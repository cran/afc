#' 2AFC For Continuous Observations And Continuous Forecasts
#' 
#' Routine to calculate the Generalized Discrimination Score (aka
#' Two-Alternatives Forced Choice Score 2AFC) for the situation of continuous
#' observations and continuous forecasts
#' 
#' This routine applies Eq.22 of Mason and Weigel (2009) to calculate the 2AFC.
#' 
#' @param obsv vector with real-valued observations
#' @param fcst vector of same length as \emph{obsv} with real-valued forecasts
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
#'   #Load set of continuous observations and continuous forecasts
#'   data(cnrm.nino34.cc)
#'   obsv = cnrm.nino34.cc$obsv
#'   fcst = cnrm.nino34.cc$fcst
#' 
#'   #Calculate skill score
#'   afc.cc(obsv,fcst)
#' 
#' @export afc.cc
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
  p.afc = 0.5*(1+stats::cor(fcst,obsv,method="kendall"))
  type.flag = 1
  return(p.afc)

}
