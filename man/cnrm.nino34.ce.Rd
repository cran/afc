\name{cnrm.nino34.ce}
\alias{cnrm.nino34.ce}
\docType{data}
\title{ Example Data of Continuous Observations and Ensemble Forecasts }
\description{
  Continuous observations (obsv) and 9-member ensemble forecasts (fcst)
  of the Nino-3.4 index for January 1961-2000. The forecast data stem from
  model runs initialized using data for the preceding August 1960-1999.
}
\usage{data(cnrm.nino34.ce)}
\format{
  The format is:\cr
List of 3\cr
 \$ years: int [1:40] 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 ...\cr
 \$ obsv : num [1:40] 26.2 26.2 25.9 27.4 25.8 ...\cr
 \$ fcst : num [1:40, 1:9] 26.5 25.9 27.0 27.5 25.8 ...
}
\details{
  The forecast data are from the coupled ocean-atmosphere model of the
  Centre National de Recherches Meteorologiques (CNRM) of Meteo France
  and were generated as part of the "Development of
  a European Multimodel Ensemble System for Seasonal to Interannual
  Prediction (DEMETER)" project (Palmer et al. 2004). Note that also the
  examples presented in the 2AFC paper by Mason and Weigel (2009) are
  based on these data. 
}
\references{
  Palmer, T.N. and Coauthors, 2004: Development of a European ensemble
  system for seasonal to inter-annual prediction
  (DEMETER). Bull. Amer. Meteor. Soc., 85, 853-872. \cr
  Mason, S.J. and A.P. Weigel, 2009: A generic forecast verification
  framework for administrative purposes. Mon. Wea. Rev., 137, 331-349
}
\keyword{datasets}
