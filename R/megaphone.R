
megaphone.fit = function(
  x,
  cross = 10,
  gamma = 1/ncol(x),
  nu = 0.5
){
  out = list()
  out$classifier = svm(
    ~.,
    data = as.data.frame(x), 
    type = "one-classification", 
    gamma = gamma,
    nu = nu,
    cross = cross
  )
  out$cv.accuracy = summary(out$classifier)$tot.accuracy
  out$retained.samples = matrix(
    ncol = ncol(x), 
    nrow = 0, 
    dimnames = dimnames(x)
  )
  out$n.attempted.samples = 0
  out$x = x
    
  class(out) = "megaphone"
  
  out
}