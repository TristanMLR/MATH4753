#' mybin1
#'
#' @param iter a number
#' @param n a number
#' @param p a number
#'
#' @return a barplot
#' @export
#' @import grDevices
#' @examples mybin(iter = 100, n = 10, p = 0.7)
mybin = function(iter = 1000, n = 10, p = 0.5) {
  sam.mat = matrix(NA, nrow = n, ncol = iter, byrow = TRUE)

  succ = c()

  for( i in 1:iter) {
    sam.mat[,i] = sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))

    succ[i] = sum(sam.mat[,i])
  }

  succ.tab = table(factor(succ, levels = 0:n))

  iter.lab = paste0("iter = ", iter)
  n.lab = paste0("n = ", n)
  p.lab = paste0("p = ", p)
  lab = paste(iter.lab, n.lab, p.lab, sep = ", ")
  barplot(succ.tab / (iter), col = rainbow(n + 1), main = "Binomial Simulation", sub = lab, xlab = "Number of Successes")
  succ.tab / iter
}
