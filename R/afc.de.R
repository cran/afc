#' 2AFC For Dichotomous Observations and Ensemble Forecasts
#' 
#' Routine to calculate the Generalized Discrimination Score (aka
#' Two-Alternatives Forced Choice Score 2AFC) for the situation of dichotomous
#' observations and ensemble forecasts
#' 
#' This routine first ranks the ensemble forecasts (see
#' \code{\link{rank.ensembles}}) and then calculates the 2AFC-score with Eq.2
#' of Mason and Weigel (2009).
#' 
#' @param obsv vector with dichotomous observations (values in {0,1})
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
#'   #Load set of dichotomous observations and 9-member ensemble forecasts
#'   data(cnrm.nino34.de)
#'   obsv = cnrm.nino34.de$obsv
#'   fcst = cnrm.nino34.de$fcst
#' 
#'  #Calculate skill score
#'   afc.de(obsv,fcst)
#' 
#' @export afc.de
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
