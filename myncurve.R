#' myncurve
#'
#'@param x a vector
#' @param mu mean
#' @param sigma standard deviation
#'
#' @return a graph
#' @export
#'
#' @examples myncurve(x,mu=2, sigma=4)
myncurve = function(x, mu, sigma){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

}
