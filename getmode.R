#' getmode
#'
#' @param v a vector
#'
#' @return a number
#' @export
#'
#' @examples
#' getmode(c(1,2,3,3,4))
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

