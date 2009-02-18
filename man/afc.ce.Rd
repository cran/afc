\name{afc.ce}
\alias{afc.ce}
\title{ 2AFC For Continuous Observations And Ensemble Forecasts }
\description{
 Routine to calculate the Generalized Discrimination Score (aka
 Two-Alternatives Forced Choice Score 2AFC) for the situation of
 continuous observations and ensemble forecasts
}
\usage{
afc.ce(obsv, fcst)
}
\arguments{
  \item{obsv}{ vector with real-valued observations }
  \item{fcst}{ two-dimensional array with ensemble
  forecasts; dim(fcst)[1] = length(obsv); dim(fcst)[2] = ensemble size }
}
\details{
 This routine first ranks the ensemble forecasts
  (see \code{\link{rank.ensembles}}) and then calculates the 2AFC-score
  with Eq.22 of Mason and Weigel (2009).
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
  #Load set of continuous observations and 9-member ensemble forecasts
  data(cnrm.nino34.ce)
  obsv = cnrm.nino34.ce$obsv
  fcst = cnrm.nino34.ce$fcst

  #Calculate skill score
  afc.ce(obsv,fcst)
}
\keyword{ file }

