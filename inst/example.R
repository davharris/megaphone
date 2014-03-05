load("data/x.Rdata")

object = megaphone.fit(scale(x), gamma = 1/2, nu = .05)

object

for(i in 1:5){
  message("adding samples: round ", i)
  object = add.samples.brute(object)
}

estimate.volume(object)

plot(object, col = "black")
