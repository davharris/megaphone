load("../mistnet-manuscript/x.Rdata")
library(e1071)
object = megaphone.fit(x)
object = add.samples.brute(object)
