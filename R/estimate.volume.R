estimate.volume = function(object){
  
  max.volume = prod(object$extents)
  proportion.inside = nrow(object$retained.samples) / object$n.attempted.samples
  
  raw.ci = qbinom(
    c(.025, .975), 
    size = object$n.attempted.samples, 
    prob = proportion.inside
  )
  proportion.ci = raw.ci / object$n.attempted.samples
  volume.estimate = max.volume * proportion.inside
  volume.ci = max.volume * proportion.ci
  
  list(
    max.volume = max.volume,
    volume.estimate = volume.estimate,
    volume.ci = volume.ci,
    proportion.inside = proportion.inside,
    proportion.ci = proportion.ci
  )
}
