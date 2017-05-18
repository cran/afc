#' 2AFC For Nominal Polychotomous Observations Ans Probabilistic Forecasts
#' 
#' Routine to calculate the Generalized Discrimination Score (aka
#' Two-Alternatives Forced Choice Score 2AFC) for the situation of
#' polychotomous (nominal) observations and discrete probabilistic forecasts
#' 
#' This routine applies Eq.17 of Mason and Weigel (2009) to calculate the 2AFC.
#' 
#' @param obsv vector with dichotomous observations (values in {0,1})
#' @param fcst two-dimensional array with forecast probabilities for the m
#' categories; dim(fcst)[1] = length(obsv); dim(fcst)[2] = m
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
#'   #Load set of polychotomous observations (4 categories) and probabilistic forecasts
#'   data(cnrm.nino34.mp)
#'   obsv = cnrm.nino34.mp$obsv
#'   fcst = cnrm.nino34.mp$fcst
#' 
#'   #Calculate skill score
#'   afc.np(obsv,fcst,4)
#' 
#' @export afc.np
afc.np = function(obsv,fcst,m=3){

  #################################
  # OBSV: POLYCHOTOMOUS (NOMINAL) #
  # FCST: PROBABILISTIC           #
  #################################

  # input variables:
  # ----------------
  # m   - Number of observation categories (default = 3)
  # fcst - array(n,m) of n probabilistic forecasts for the m categories
  # obsv - vector with observation categories (1...m)
  #
  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 17 


  # determine number of observations per category
  n.vector = rep(NA,m)
  for (l in 1:m) n.vector[l] = length(which(obsv == l))

  # Calculate Eq. 17 in MW09
  
  numer = 0
  denom = 0
  # Outer two sums in Eq. 17a
  for (l in 1:m) for (k in (1:m)[-l]){
    index.k = which(obsv == k)
    index.l = which(obsv == l)
    p.kl = fcst[index.k,l]
    p.ll = fcst[index.l,l]
    test = 0
    # Inner two sums in Eq. 17a
    for (i in 1:n.vector[k]) for (j in 1:n.vector[l])
      test = test + 0.5*(p.kl[i] == p.ll[j]) + (p.kl[i]<p.ll[j])
    numer = numer + test
    denom = denom + n.vector[l]*n.vector[k]
  }
  p.afc = numer/denom
  type.flag = 1
  return(p.afc)
}
