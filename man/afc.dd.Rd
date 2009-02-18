\name{afc.dd}
\alias{afc.dd}
\title{ 2AFC For Dichotomous Observations And Dichotomous Forecasts }
\description{
 Routine to calculate the Generalized Discrimination Score (aka
 Two-Alternatives Forced Choice Score 2AFC) for the situation of
 dichotomous observations and dichotomous forecasts
}
\usage{
afc.dd(obsv, fcst)
}
\arguments{
  \item{obsv}{ vector with dichotomous observations (values in \{0,1\}) }
  \item{fcst}{ vector of same length as \emph{obsv} with dichotomous
  forecasts (values in \{0,1\}) }
}
\details{
  This routine applies Eq.2 of Mason and Weigel (2009) to calculate the 2AFC.
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
  #Load set of dichotomous observations and dichotomous forecasts
  data(cnrm.nino34.dd)
  obsv = cnrm.nino34.dd$obsv
  fcst = cnrm.nino34.dd$fcst

  #Calculate skill score
  afc.dd(obsv,fcst)
}
\keyword{ file }

