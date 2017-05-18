#' 2AFC For Continuous Observations And Ensemble Forecasts
#' 
#' Routine to calculate the Generalized Discrimination Score (aka
#' Two-Alternatives Forced Choice Score 2AFC) for the situation of continuous
#' observations and ensemble forecasts
#' 
#' This routine first ranks the ensemble forecasts (see
#' \code{\link{rank.ensembles}}) and then calculates the 2AFC-score with Eq.22
#' of Mason and Weigel (2009).
#' 
#' @param obsv vector with real-valued observations
#' @param fcst two-dimensional array with ensemble forecasts; dim(fcst)[1] =
#' length(obsv); dim(fcst)[2] = ensemble size
#' @return \item{ p.afc }{ Value of Generalized Discrimination (2AFC) Score }
#' @author Andreas Weigel, Federal Office of Meteorology and Climatology,
#' MeteoSwiss, Zurich, Switzerland
#' @seealso \code{\link{afc}} \code{\link{rank.ensembles}}
#' @references S.J. Mason and A.P. Weigel, 2009. A generic verification
#' framework for administrative purposes. Mon. Wea. Rev., 137, 331-349
#' @keywords file
#' @examples
#' 
#'   #Forecasts and observations of Nino-3.4 index
#'   #Load set of continuous observations and 9-member ensemble forecasts
#'   data(cnrm.nino34.ce)
#'   obsv = cnrm.nino34.ce$obsv
#'   fcst = cnrm.nino34.ce$fcst
#' 
#'   #Calculate skill score
#'   afc.ce(obsv,fcst)
#' 
#' @export afc.ce
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
  p.afc = 0.5*(1+stats::cor(ranks,obsv,method="kendall"))
  type.flag = 1
  return(p.afc)

}
