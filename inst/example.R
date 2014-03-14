devtools::load_all()
load("data/x.Rdata")

object = megaphone.fit(scale(x), gamma = 1/2, nu = .05)
object2 = object

object


for(i in 1:5){
  message("adding samples: round ", i)
  object = add.samples.brute(object)
}


estimate.volume(object)

plot(
  object$retained.samples[, c("bio5", "bio18")],
  pch = 16
)





# Metropolis --------------------------------------------------------------
library(coda)

object2 = add.samples.metropolis(object2, N = 5E4)

# Geometric mean is more sensitive to variables with small size
n.eff = exp(mean(log(effectiveSize(object2$retained.samples))))
n.eff


# plotting ----------------------------------------------------------------
par(mfrow = c(1, 2))

plot(
  object$x[object$classifier$fitted, c("bio16", "bio8")],
  col = 2, 
  pch = 16, 
  cex = .4, 
  asp = 1
)
points(object$retained.samples[ , c("bio16", "bio8")], cex = .6, pch = 16, col = 4)
abline(h = object$extents[, "bio8"], v = object$extents[, "bio16"])

plot(
  object$x[object2$classifier$fitted, c("bio16", "bio8")],
  col = 2, 
  pch = 16, 
  cex = .4, 
  asp = 1
)
points(object2$retained.samples[ , c("bio16", "bio8")], cex = .6, pch = 16)
abline(h = object$extents[, "bio8"], v = object$extents[, "bio16"])

par(mfrow = c(1, 1))

###########

object$sampling.time
nrow(object$retained.samples)

object2$sampling.time
n.eff
