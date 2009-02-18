\name{afc.dm}
\alias{afc.dm}
\title{2AFC For Dichotomous Observations And Ordinal Polychotomous Forecasts}
\description{
 Routine to calculate the Generalized Discrimination Score (aka
 Two-Alternatives Forced Choice Score 2AFC) for the situation of
 dichotomous observations and (ordinal) polychotomous forecasts
}
\usage{
afc.dm(obsv, fcst, mf=3)
}
\arguments{
  \item{obsv}{ vector with dichotomous observations (values in \{0,1\}) }
  \item{fcst}{ vector of same length as \emph{obsv} with polychotomous
  forecasts (values in \{1,..,m\}) }
  \item{mf}{ number of forecast categories (default = 3) }
}
\details{
  This routine applies Eq.5 of in Mason and Weigel (2009) to calculate
  the 2AFC.
}
\value{
  \item{ p.afc }{  Value of Generalized Discrimination (2AFC) Score }
}
\references{ S.J. Mason and A.P. Weigel, 2009. A generic verification
  framework for administrative purposes. Mon. Wea. Rev., 137, 331-349 }
\author{ Andreas Weigel, Federal Office of Meteorology and Climatology,
  MeteoSwiss, Zurich, Switzerland }
\seealso{ \code{\link{afc}} }
\examples{
  #Forecasts and observations of Nino-3.4 index
  #Load set of dichotomous observations and polychotomous forecasts (4 categories)
  data(cnrm.nino34.dm)
  obsv = cnrm.nino34.dm$obsv
  fcst = cnrm.nino34.dm$fcst

  #Calculate skill score
  afc.dm(obsv,fcst,4)
}
\keyword{ file }% at least one, from doc/KEYWORDS
