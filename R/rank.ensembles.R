rank.ensembles = function(fcst){

  ################################
  # DETERMINE RANKS OF ENSEMBLES #
  ################################

  # input variable:
  # ----------------
  # fcst - array(n,nens) of n ensemble forecasts with ensemble size nens
  #
  # output variable:
  # ----------------
  # ranks - vector with ranks of ensemble
  # ... determined for each pair of ensembles with Eq. 8 in MW09

  # Determine number of forecasts and ensemble size
  nens    = dim(fcst)[2]
  n       = dim(fcst)[1]

  # Initialize array with ensemble ranks
  ranks = rep(1,n)

  # Compare all pairs of ensembles
  for (i in 2:n) for (j in 1:(i-1)){

    # Interpret ensemble "i" as event
    # Interpret ensemble "j" as non-event
    fcst.tmp.event = fcst[i,]
    fcst.tmp.nonev = fcst[j,]

    # Deterimine ranks of ensemble members i compared with j
    rank.1 = rank(c(fcst.tmp.event,fcst.tmp.nonev))[1:nens]

    # Use Eq. 8 to answer whether ensemble i > ensemble j
    p.afc = (sum(rank.1) - nens*(nens+1)/2)/nens^2

    # Add "1" to the rank of the larger ensemble
    # Respectively 0.5 to both, if they cannot be distinguished
    if (p.afc > 0.5) ranks[i] = ranks[i]+1
    if (p.afc < 0.5) ranks[j] = ranks[j]+1
    if (p.afc == 0.5) {
      ranks[i] = ranks[i]+0.5
      ranks[j] = ranks[j]+0.5
    }
  }
  
  return(ranks)
}
