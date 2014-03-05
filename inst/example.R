load("data/x.Rdata")

object = megaphone.fit(x, gamma = 1/2, nu = .05)

object

for(i in 1:10){
  message("adding samples: round ", i)
  object = add.samples.brute(object)
}

estimate.volume(object)

plot(object, col = "black")

plot(
  object$retained.samples[, c("bio5", "bio18")],
  pch = 16
)
