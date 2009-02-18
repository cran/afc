\name{afc.np}
\alias{afc.np}.
\title{ 2AFC For Nominal Polychotomous Observations Ans Probabilistic Forecasts }
\description{
 Routine to calculate the Generalized Discrimination Score (aka
 Two-Alternatives Forced Choice Score 2AFC) for the situation of
 polychotomous (nominal) observations and discrete probabilistic forecasts
}
\usage{
afc.np(obsv, fcst, m = 3)
}
\arguments{
  \item{obsv}{ vector with dichotomous observations (values in \{0,1\}) }
  \item{fcst}{ two-dimensional array with forecast
  probabilities for the m categories; dim(fcst)[1] = length(obsv);
  dim(fcst)[2] = m }
  \item{m}{ number of observation categories (default = 3)} 
}
\details{
  This routine applies Eq.17 of Mason and Weigel (2009) to calculate the 2AFC.
}
\value{
  \item{ p.afc }{ Value of Generalized Discrimination (2AFC) Score  }
}
\references{ S.J. Mason and A.P. Weigel, 2009. A generic verification
  framework for administrative purposes. Mon. Wea. Rev., 137, 331-349 }
\author{ Andreas Weigel, Federal Office of Meteorology and Climatology,
  MeteoSwiss, Zurich, Switzerland }
\seealso{  \code{\link{afc}} }
\examples{
  #Forecasts and observations of Nino-3.4 index
  #Load set of polychotomous observations (4 categories) and probabilistic forecasts
  data(cnrm.nino34.mp)
  obsv = cnrm.nino34.mp$obsv
  fcst = cnrm.nino34.mp$fcst

  #Calculate skill score
  afc.np(obsv,fcst,4)
}
\keyword{ file }% at least one, from doc/KEYWORDS
