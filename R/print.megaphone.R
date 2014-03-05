print.megaphone = function(object){
  print(
    list(
      classifier = object$classifier,
      cv.accuracy = object$cv.accuracy,
      retained.samples = head(object$retained.samples, 0),
      n.attempted.samples = object$n.attempted.samples,
      x = head(object$x, 0)
    )
  )
}
