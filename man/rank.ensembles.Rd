\name{rank.ensembles}
\alias{rank.ensembles}
\title{ Rank Ensembles }
\description{
  Routine to rank a set of given ensemble forecasts according to their
  "value" 
}
\usage{
rank.ensembles(fcst)
}
\arguments{
  \item{fcst}{ two-dimensional array with ensemble
  forecasts; dim(fcst)[1] = number of ensemble forecasts; dim(fcst)[2] =
  number of ensemble members}
}
\details{
  This routine ranks a set of ensemble forecasts according to their
  "value". The higher the "value" of an ensemble forecasts, the
  higher the rank. The following principle is applied: Assume two ensembles
  A and B are to be ranked. Without loss of generality, we define A>B if
  the probability of a random ensemble member of A being larger than a
  random ensemble member of B exceeds 0.5. This probability is calculated by a
  2AFC-like approach based on Eq. 8 of Mason and Weigel (2009). By
  pairwise comparison of all ensembles, the final ranking is obtained. 
}
\value{
  \item{ ranks }{ vector with the ranks of the ensemble forecasts }
}
\references{ S.J. Mason and A.P. Weigel, 2009. A generic verification
  framework for administrative purposes. Mon. Wea. Rev., 137, 331-349 }
\author{ Andreas Weigel, Federal Office of Meteorology and Climatology,
  MeteoSwiss, Zurich, Switzerland }
\seealso{ \code{\link{afc.de}} \code{\link{afc.me}} \code{\link{afc.ce}} \code{\link{afc}} }
\examples{
  #Load a set of ensemble forecasts
  data(cnrm.nino34.ce)
  fcst = cnrm.nino34.ce$fcst

  #Rank ensemble forecasts
  rank.ensembles(fcst)
}
\keyword{ file }
