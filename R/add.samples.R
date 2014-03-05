add.samples.brute = function(
  object,
  n.attempts = 1E6
){
  test_points = sapply(
    1:ncol(object$x),
    function(i){
      runif(
        n.attempts, 
        min = min(object$x[,i]), 
        max = max(object$x[,i])
      )
    }
  )
  
  object$retained.samples = rbind(
    object$retained.samples,
    test_points[predict(object$classifier, test_points), ]
  )
  object$n.attempted.samples = object$n.attempted.samples + n.attempts
  
  object
}