\name{cnrm.nino34.dc}
\alias{cnrm.nino34.dc}
\docType{data}
\title{ Example Data of Dichotomous Observations and Continuous Forecasts }
\description{
  Dichotomous observations (obsv) and continuous forecasts (fcst) of the
  Nino-3.4 index for January 1961-2000. The forecast data stem from
  model runs initialized using data for the preceding August
  1960-1999. Definition of an "event" in the observations: obsv >= 27 C. 
}
\usage{data(cnrm.nino34.dc)}
\format{
  The format is:\cr
List of 3\cr
 \$ years: int [1:40] 1961 1962 1963 1964 1965 1966 1967 1968 1969 1970 ...\cr
 \$ obsv : num [1:40] 0 0 0 1 0 1 0 0 1 1 ...\cr
 \$ fcst : num [1:40] 26.5 26.3 26.6 27.7 25.8 ...
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

