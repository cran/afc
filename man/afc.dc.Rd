\name{afc.dc}
\alias{afc.dc}
\title{ 2AFC For Dichotomous Observations And Continuous Forecasts }
\description{
 Routine to calculate the Generalized Discrimination Score (aka
 Two-Alternatives Forced Choice Score 2AFC) for the situation of
 dichotomous observations and continuous forecasts
}
\usage{
afc.dc(obsv, fcst)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{obsv}{ vector with dichotomous observations (values in \{0,1\}) }
  \item{fcst}{ vector of same length as \emph{obsv} with real-valued
  forecasts }
}
\details{
  This routine applies Eq.8 of Mason and Weigel (2009) to calculate the 2AFC.
}
\value{
  \item{ p.afc }{ Value of Generalized Discrimination (2AFC) Score }
}
\references{ S.J. Mason and A.P. Weigel, 2009. A generic verification
  framework for administrative purposes. Mon. Wea. Rev., 137, 331-349 }
\author{ Andreas Weigel, Federal Office of Meteorology and Climatology,
  MeteoSwiss, Zurich, Switzerland }
\seealso{ \code{\link{afc}} }
\examples{
  #Forecasts and observations of Nino-3.4 index
  #Load set of dichotomous observations and continuous forecasts
  data(cnrm.nino34.dc)
  obsv = cnrm.nino34.dc$obsv
  fcst = cnrm.nino34.dc$fcst

  #Calculate skill score
  afc.dc(obsv,fcst)
}
\keyword{ file }
