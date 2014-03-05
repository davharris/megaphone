plot.megaphone = function(object, pch = 16, cex = 0.5, col = "#00000020", ...){
  pairs(object$retained.samples, pch = pch, cex = cex, col = col, ...)
}
