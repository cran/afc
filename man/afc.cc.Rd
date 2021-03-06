% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/afc.cc.R
\name{afc.cc}
\alias{afc.cc}
\title{2AFC For Continuous Observations And Continuous Forecasts}
\usage{
afc.cc(obsv, fcst)
}
\arguments{
\item{obsv}{vector with real-valued observations}

\item{fcst}{vector of same length as \emph{obsv} with real-valued forecasts}
}
\value{
\item{ p.afc }{ Value of Generalized Discrimination (2AFC) Score }
}
\description{
Routine to calculate the Generalized Discrimination Score (aka
Two-Alternatives Forced Choice Score 2AFC) for the situation of continuous
observations and continuous forecasts
}
\details{
This routine applies Eq.22 of Mason and Weigel (2009) to calculate the 2AFC.
}
\examples{

  #Forecasts and observations of Nino-3.4 index
  #Load set of continuous observations and continuous forecasts
  data(cnrm.nino34.cc)
  obsv = cnrm.nino34.cc$obsv
  fcst = cnrm.nino34.cc$fcst

  #Calculate skill score
  afc.cc(obsv,fcst)

}
\references{
S.J. Mason and A.P. Weigel, 2009. A generic verification
framework for administrative purposes. Mon. Wea. Rev., 137, 331-349
}
\seealso{
\code{\link{afc}}
}
\author{
Andreas Weigel, Federal Office of Meteorology and Climatology,
MeteoSwiss, Zurich, Switzerland
}
\keyword{file}
