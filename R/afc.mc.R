#' 2AFC For Ordinal Polychotomous Observations And Continuous Forecasts
#' 
#' Routine to calculate the Generalized Discrimination Score (aka
#' Two-Alternatives Forced Choice Score 2AFC) for the situation of
#' polychotomous observations (ordinal) and continuous forecasts
#' 
#' This routine applies Eq.18 of Mason and Weigel (2009) to calculate the 2AFC.
#' 
#' @param obsv vector with polychotomous observations (values in {1,..,m})
#' @param fcst vector of same length as \emph{obsv} with real-valued forecasts
#' @param m number of observation categories (default = 3)
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
#'   #Load set of polychotomous observations (4 categories) and continuous forecasts
#'   data(cnrm.nino34.mc)
#'   obsv = cnrm.nino34.mc$obsv
#'   fcst = cnrm.nino34.mc$fcst
#' 
#'   #Calculate skill score
#'   afc.mc(obsv,fcst,4)
#' 
#' @export afc.mc
afc.mc = function(obsv,fcst,m=3){

  #################################
  # OBSV: POLYCHOTOMOUS (ORDINAL) #
  # FCST: CONTINUOUS              #
  #################################

  # input variables:
  # ----------------
  # m    - Number of observation categories (default = 3)
  # fcst - vector with continuous forecast values (real values)
  # obsv - vector with observation categories (1...m)
  #
  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 18 


  # determine number of observations per category
  n.vector = rep(NA,m)
  for (k in 1:m){
    n.vector[k] = length(which(obsv== k))
  }

  # calculate Eq. 18 in MW09
  numer = 0
  denom = 0
  for (k in 1:(m-1)) for (l in (k+1):m){
    # index for obsv of categories which are to be compared
    index.event = which(obsv == k | obsv == l)
    # Determine ranks of forecasts of event 
    fine.index = which(obsv[index.event] == l)
    ranks = rank(fcst[index.event])[fine.index]
    # Calculate inner sum in Eq. 18 of MW09
    numer = numer + sum(ranks) - 0.5*n.vector[l]*(n.vector[l]+1)
    denom = denom + n.vector[k]*n.vector[l]
  }
  p.afc = numer/denom
  type.flag = 1
  return(p.afc)
}
