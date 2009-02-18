\name{afc.de}
\alias{afc.de}
\title{2AFC For Dichotomous Observations and Ensemble Forecasts}
\description{
 Routine to calculate the Generalized Discrimination Score (aka
 Two-Alternatives Forced Choice Score 2AFC) for the situation of
 dichotomous observations and ensemble forecasts
}
\usage{
afc.de(obsv, fcst)
}
\arguments{
  \item{obsv}{ vector with dichotomous observations (values in \{0,1\}) }
  \item{fcst}{ two-dimensional array with ensemble
  forecasts; dim(fcst)[1] = length(obsv); dim(fcst)[2] = ensemble size }
}
\details{
  This routine first ranks the ensemble forecasts
  (see \code{\link{rank.ensembles}}) and then calculates the 2AFC-score
  with Eq.2 of Mason and Weigel (2009).
}
\value{
  \item{ p.afc }{ Value of Generalized Discrimination (2AFC) Score }
}
\references{ S.J. Mason and A.P. Weigel, 2009. A generic verification
  framework for administrative purposes. Mon. Wea. Rev., 137, 331-349 }
\author{ Andreas Weigel, Federal Office of Meteorology and Climatology,
  MeteoSwiss, Zurich, Switzerland }
\seealso{ \code{\link{afc}} \code{\link{rank.ensembles}} }
\examples{
  #Forecasts and observations of Nino-3.4 index
  #Load set of dichotomous observations and 9-member ensemble forecasts
  data(cnrm.nino34.de)
  obsv = cnrm.nino34.de$obsv
  fcst = cnrm.nino34.de$fcst

 #Calculate skill score
  afc.de(obsv,fcst)
}
\keyword{ file }% at least one, from doc/KEYWORDS
