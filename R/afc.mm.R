#' 2AFC For Ordinal Polychotomous Observations And Ordinal Polychotomous
#' Forecasts
#' 
#' Routine to calculate the Generalized Discrimination Score (aka
#' Two-Alternatives Forced Choice Score 2AFC) for the situation of
#' polychotomous observations (ordinal) and polychotomous forecasts (ordinal)
#' 
#' This routine applies Eq.14 of Mason and Weigel (2009) to calculate the 2AFC.
#' 
#' @param obsv vector with polychotomous observations (values in {1,..,mv})
#' @param fcst vector of same length as \emph{obsv} with polychotomous
#' forecasts (values in {1,..,mf})
#' @param mv number of observation categories (default = 3)
#' @param mf number of forecast categories (default = 3)
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
#'   #Load set of polychotomous observations (4 categories) and polychotomous forecasts (4 categories)
#'   data(cnrm.nino34.mm)
#'   obsv = cnrm.nino34.mm$obsv
#'   fcst = cnrm.nino34.mm$fcst
#' 
#'   #Calculate skill score
#'   afc.mm(obsv,fcst,4,4)
#' 
#' @export afc.mm
afc.mm = function(obsv,fcst,mv=3,mf=3){

  #################################
  # OBSV: POLYCHOTOMOUS (ORDINAL) #
  # FCST: POLYCHOTOMOUS (ORDINAL) #
  #################################

  # input variables:
  # ----------------
  # fcst - vector with forecast categories (1...mf)
  # obsv - vector with observation categories (1...mv)
  # mv   - Number of observation categories (default = 3)
  # mf   - Number of forecast categories (default = mv)

  # output variable:
  # ----------------
  # p.afc - 2AFC skill score as obtained from MW09 Eq. 14


  # set mv = mf if mv is not given
  if (mf == 0) mf=mv

  # Matrix with element[nn,mm] being the number of times
  # that nn has been observed and mm has been forecast
  n.matrix = array(0,dim=c(mv,mf))
  for (nn in 1:mv) for (mm in 1:mf){
    n.matrix[nn,mm] = sum((obsv == nn) & (fcst == mm))
  }

  # Solve Eq. 14 of MW09
  numer = 0
  denom = 0
  for (k in 1:(mv-1)) for (l in (k+1):mv){
    term1 = 0
    term2 = 0
    for (i in 1:(mf-1)) for (j in (i+1):mf){
      term1 = term1 + n.matrix[k,i]*n.matrix[l,j]
    }
    for (i in 1:mf){
      term2 = term2 + n.matrix[k,i]*n.matrix[l,i]
    }
    numer = numer + term1 + 0.5*term2
    denom = denom + sum(n.matrix[k,])*sum(n.matrix[l,])
  }
  p.afc = numer/denom
  type.flag = 1
  return(p.afc)
}
