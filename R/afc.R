# This is the master routine to be called to calculate the 2AFC score
# following the paper of MW09

# This routines makes some tests whether the input format is correct
# and then calls the appropriate routine to calculate the 2AFC

# input variables:
# ----------------
#
# obsv: obsverations (format specified below)
#
# fcst: forecasts (format specified below)
#
# obsv.type:
#  "d": dichtomous
#  "m": polychotomous (ordinal)
#  "n": polychotomous (nominal)
#  "c": continuous
#
# fcst.type:
#  "d": dichotomous
#  "m": polychotomous (ordinal)
#  "n": polychotomous (nominal)
#  "c": continuous
#  "p": probabilistic (discrete)
#  "e": ensembles of continuous values
#
# mv: Number of verification categories
#
# mf: Number of forecast categories
# Only Required for obsv.type/fcst.type= "m-m"
#
#
# format of obsv for obsv-types:
# ------------------------------
# (assuming that nf is the number of verification samples)
# "d": vector of length nf with values 1 (event) and 0 (non-event)
# "m": vector of length nf with values in [1,...,mv]
# "n": vector of length nf with values in [1,...,mv]
# "c": vector of length nf with real-valued observations
#
# format of fcst for fcst-types:
# ------------------------------
# (assuming that nf is the number of verification samples)
# "d": vector of length nf with values 1 (event) and 0 (non-event)
# "m": vector of length nf with values in [1,...,mf]
# "n": vector of length nf with values in [1,...,mv]
# "p": (1) if type.obsv = "d" - vector of length nf with probabilities
#      (2) if type.obsv = "m" - array(nf,mv) with probabilities
# "c": vector of length nf with real-valued observations
# "e": array(nf,nens) of ensemble forecasts (nens = ensemble size)
#
# Valid combinations:
# dd,dm,dp,dc,de,mm,mp,mc,me,nn,np,cc,ce


afc = function(obsv,fcst,obsv.type,fcst.type,m=0,m2=0){

  type.ok.flag = 0

  if (obsv.type == "d" & fcst.type == "d"){
    # Check input files
    if (length(dim(obsv)) > 0 | length(dim(fcst)) > 0)
      stop("observations / forecasts must be vectors")
    if (length(obsv) != length(fcst))
      stop("observations and forecasts must have same length")
    if (length(which(obsv != 0 & obsv != 1)) > 0)
      stop("observations can only have values 1 and 0")
    if (length(which(fcst != 0 & fcst != 1)) > 0)
      stop("forecasts can only have values 1 and 0")
    # Calculate score
    afc.score = afc.dd(obsv,fcst)
    type.ok.flag = 1
  }

  if (obsv.type == "d" & fcst.type == "m"){
    # Check input
    if (m == 0) stop("number of forecast categories missing")
    if (length(dim(obsv)) > 0 | length(dim(fcst)) > 0)
      stop("observations / forecasts must be vectors")
    if (length(obsv) != length(fcst))
      stop("observations and forecasts must have same length")
    if (length(which(obsv != 0 & obsv != 1)) > 0)
      stop("observations can only have values 1 and 0")
    if (length(which(!(fcst %in% 1:m))) > 0)
      stop("forecasts can only have values [1,2,...,m]")
    # Calculate score
    afc.score = afc.dm(obsv,fcst,m)       
    type.ok.flag = 1
  } 

  if (obsv.type == "d" & fcst.type == "p"){
    # Check input 
    if (length(dim(obsv)) > 0 | length(dim(fcst)) > 0)
      stop("observations / forecasts must be vectors")
    if (length(obsv) != length(fcst))
      stop("observations and forecasts must have same length")
    if (length(which(obsv != 0 & obsv != 1)) > 0)
      stop("observations can only have values 1 and 0")
    if (length(which(fcst < 0 | fcst > 1)) > 0)
      stop("forecasts must be probabilities, i.e. values between 0 and 1")   
    # Calculate score
    afc.score = afc.dp(obsv,fcst)       
    type.ok.flag = 1
  } 
 

  if (obsv.type == "d" & fcst.type == "c"){
    # Check input 
    if (length(dim(obsv)) > 0 | length(dim(fcst)) > 0)
      stop("observations / forecasts must be vectors")
    if (length(obsv) != length(fcst))
      stop("observations and forecasts must have same length")
    if (length(which(obsv != 0 & obsv != 1)) > 0)
      stop("observations can only have values 1 and 0")
    # Calculate score
    afc.score = afc.dc(obsv,fcst)     
    type.ok.flag = 1
  } 

  if (obsv.type == "d" & fcst.type == "e"){
    # Check input
    if (length(dim(obsv)) > 0)
      stop("observations must be a vector")
    if (length(which(obsv != 0 & obsv != 1)) > 0)
      stop("observations can only have values 0 and 1")   
    if (length(dim(fcst)) != 2)
      stop("forecasts must be array with dimensions length(obsv) x ens.size") 
    if (dim(fcst)[1] != length(obsv))
      stop("number of forecasts must be equal to number of observations")
    # Calculate score
    afc.score = afc.de(obsv,fcst)         
    type.ok.flag = 1
  }

  if (obsv.type == "m" & fcst.type == "m"){
    # Check input
    if (m == 0 | m2 == 0) stop("Please enter number of BOTH observation and forecast categories")
    if (length(dim(obsv)) > 0 | length(dim(fcst)) > 0)
      stop("observations / forecasts must be vectors")
    if (length(obsv) != length(fcst))
      stop("observations and forecasts must have same length")
    if (length(which(!(obsv %in% 1:m))) > 0)
      stop("observations can only have values [1,2,...,mv]")    
    if (length(which(!(obsv %in% 1:m2))) > 0)
      stop("forecasts can only have values [1,2,...,mf]")        
    # Calculate score
    afc.score = afc.mm(obsv,fcst,m,m2)             
    type.ok.flag = 1
  }
  
  if (obsv.type == "n" & fcst.type == "n"){
    # Check input
    if (m == 0) stop("Please enter number of categories")
    if (length(dim(obsv)) > 0 | length(dim(fcst)) > 0)
      stop("observations / forecasts must be vectors")
    if (length(obsv) != length(fcst))
      stop("observations and forecasts must have same length")
    if ( (length(which(!(obsv %in% 1:m))) > 0) | (length(which(!(fcst %in% 1:m))) > 0) )
      stop("observations/forecasts can only have values [1,2,...,m]")        
    # Calculate score
    afc.score = afc.nn(obsv,fcst,m)                 
    type.ok.flag = 1
  }

  if (obsv.type == "m" & fcst.type == "p"){
    # Check input
    if (m == 0) stop("Please enter number of observation categories")
    if (length(dim(obsv)) > 0 )
      stop("observations must be vector")
    if (length(dim(fcst)) != 2)
      stop("forecasts must be array with dimensions length(obsv) x m")
    if (dim(fcst)[1] != length(obsv))
      stop("number of forecasts must be equal to number of observations")
    if (dim(fcst)[2] != m)
      stop("Probabilities must be assigned to each category")
    if (length(which(fcst < 0 | fcst > 1)) > 0)
      stop("forecasts must be probabilities, i.e. values between 0 and 1")
    if (length(which(!(obsv %in% 1:m))) > 0)
      stop("observations can only have values [1,2,...,m]")     
    # Calculate score
    afc.score = afc.mp(obsv,fcst,m)                     
    type.ok.flag = 1
  }

  if (obsv.type == "n" & fcst.type == "p"){
    # Check input
    if (m == 0) stop("Please enter number of observation categories")
    if (length(dim(obsv)) > 0)
      stop("observations must be vector")
    if (length(dim(fcst)) != 2)
      stop("forecasts must be array with dimensions length(obsv) x m")
    if (dim(fcst)[1] != length(obsv))
      stop("number of forecasts must be equal to number of observations")
    if (dim(fcst)[2] != m)
      stop("Probabilities must be assigned to each category")
    if (length(which(fcst < 0 | fcst > 1)) > 0)
      stop("forecasts must be probabilities, i.e. values between 0 and 1")
    if (length(which(!(obsv %in% 1:m))) > 0)
      stop("observations can only have values [1,2,...,m]")     
    # Calculate score
    afc.score = afc.np(obsv,fcst,m)              
    type.ok.flag = 1
  }

  if (obsv.type == "m" & fcst.type == "c"){
    #Check input
    if (m == 0) stop("Please enter number of categories")
    if (length(dim(obsv)) > 0 | length(dim(fcst)) > 0)
      stop("observations / forecasts must be vectors")
    if (length(obsv) != length(fcst))
      stop("observations and forecasts must have same length")    
    if (length(which(!(obsv %in% 1:m))) > 0)
      stop("observations can only have values [1,2,...,m]")     
    # Calculate score
    afc.score = afc.mc(obsv,fcst,m)         
    type.ok.flag = 1
  }
  
  if (obsv.type == "m" & fcst.type == "e"){
    # Check input
    if (m == 0) stop("Please enter number of categories")
    if (length(dim(obsv)) > 0)
      stop("observations must be vector")
    if (length(which(!(obsv %in% 1:m))) > 0)
      stop("observations can only have values [1,2,...,m]")
    if (length(dim(fcst)) != 2)
      stop("forecasts must be array with dimensions length(obsv) x ens.size") 
    if (dim(fcst)[1] != length(obsv))
      stop("number of forecasts must be equal to number of observations")
    # Calculate score
    afc.score = afc.me(obsv,fcst,m)                  
    type.ok.flag = 1
  }
  
  if (obsv.type == "c" & fcst.type == "c"){
    # Check input
    if (length(dim(obsv)) > 0 | length(dim(fcst)) > 0)
      stop("observations / forecasts must be vectors")
    if (length(obsv) != length(fcst))
      stop("observations and forecasts must have same length") 
    # Calculate score
    afc.score = afc.cc(obsv,fcst)                      
    type.ok.flag = 1
  }

  if (obsv.type == "c" & fcst.type == "e"){
    # Check input
    if (length(dim(obsv)) > 0)
      stop("observations must be a vector")   
    if (length(dim(fcst)) != 2)
      stop("forecasts must be array with dimensions length(obsv) x ens.size") 
    if (dim(fcst)[1] != length(obsv))
      stop("number of forecasts must be equal to number of observations")
    # Calculate score
    afc.score = afc.ce(obsv,fcst)     
    type.ok.flag = 1
  }

  if (type.ok.flag == 0) stop("invalid combination of obsv.type and fcst.type")
  
  return(afc.score)
  
}

