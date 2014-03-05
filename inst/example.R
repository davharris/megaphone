load("../mistnet-manuscript/x.Rdata")
library(e1071)
object = megaphone.fit(scale(x), gamma = 1/8, nu = .05)

object

object = add.samples.brute(object)

estimate.volume(object)

plot(object)