add.samples.brute = function(
  object,
  n.attempts = 1E6,
  excess.width = 0.1
){
  start.time = Sys.time()
  
  # Need some error handling in case user selects multiple extents across
  # different runs!
  
  extents = apply(
    object$x, 
    2, 
    function(z){
      span = max(z) - min(z)
      c(
        min(z) - excess.width * span, 
        max(z) + excess.width * span
      )
    }
  )
  
  test_points = sapply(
    1:ncol(object$x),
    function(i){
      width = max(object$x[,i]) - min(object$x[,i])
      runif(
        n.attempts, 
        min = extents[1, i],
        max = extents[2, i]
      )
    }
  )
  
  
  object$retained.samples = rbind(
    object$retained.samples,
    test_points[predict(object$classifier, test_points), ]
  )
  object$n.attempted.samples = object$n.attempted.samples + n.attempts
  object$extents = extents
  
  end.time = Sys.time()
  object$sampling.time = object$sampling.time + (end.time - start.time)
  
  object
}


# Megaphone.metropolis works completely differently from add.samples.brute.
# In particular, it can't be used for estimating total volume & its samples
# are autocorrelated. 

library(MASS)
add.samples.metropolis = function(
  object, 
  N = 1E6, 
  df = 5, 
  thin = 100,
  Sigma
){
  start.time = Sys.time()
  
  p = ncol(object$x) # Number of dimensions
  
  if(missing(Sigma)){
    Sigma = cov.trob(object$x[object$classifier$fitted, ], nu = df)$cov
  }
  
  
  num.proposals = 0
  num.acceptances = 0
  positions = matrix(NA, nrow = N %/% thin, ncol = p)
  
  # initialize position with observed point that is nearest to origin (after scaling)
  # Procedure assumes this point is enclosed by hypervolume. Probably true usually?
  position = object$x[which.min(apply(scale(object$x), 1, function(x){sum(x^2)})), , drop = FALSE]
  
  # Faster to pre-generate the proposal *moves* w/ vectorization rather than
  # generating a brand new proposal each time through the loop.  
  # t distribution can make larger jumps than the corresponding Gaussian.
  proposed.moves = rmvt(N, sigma = Sigma, df = df)
  
  for(i in 1:N){
    if(i%%1000 == 0){
      print(i)
    }
    
    trial.position = position + proposed.moves[i, , drop = FALSE]
    
    inside = predict(object$classifier, trial.position)
    
    if(inside){
      position = trial.position
    }
    
    if(i %% thin == 0){
      positions[i %/% thin, ] = position  
    }
  }
  end.time = Sys.time()
  
  
  object$retained.samples = rbind(
    object$retained.samples,
    positions
  )
  
  # n.attempted.samples is no longer meaningful if we're not doing rejection
  # sampling
  object$n.attempted.samples = NA
  object$sampling.time = object$sampling.time + (end.time - start.time)
  
  
  object
}

