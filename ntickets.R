#' ntickets
#'
#' @param N the total number of trials
#' @param gamma the acceptable amount of pain
#' @param p the probability of success
#'
#' @return a named list and plots
#' @export
#' @importFrom stats dnorm
#' @importFrom stats pbinom
#' @importFrom stats pnorm
#' @importFrom graphics abline
#' @importFrom graphics barplot
#' @importFrom graphics curve
#' @importFrom graphics layout
#' @examples ntickets(200,0.02,0.95)
ntickets <- function(N, gamma, p) {

  ndisc <- c(N:(N+20)) # going trough N+20 points
  discrete = (1-gamma)-pbinom(N,ndisc,p) # objective function
  nd <- which.min(abs(discrete)) # finding the index of the 0
  nd <- ndisc[nd] # finding the 0
  ncont <- seq(N,N+20,length=100000) # going trough N+20 interval with
  # a lot of points
  continuous <- (1-gamma) - pnorm(N+0.5,mean=ncont*p, # objective function
                                  sd=sqrt(ncont*p*(1-p)))
  nc <- which.min(abs(continuous)) # finding the index of the 0
  nc <- ncont[nc] # finding the 0
  layout(matrix(1:2,nrow=2,ncol=2)) # proper layout
  plot(ndisc,discrete,type='b',pch=21,bg="blue", # plotting the discrete
       main=paste("Objective vs n to find optimal tickets sold\n",
                  "(", nd, ")", "gamma=", gamma, ", N=", N,
                  ", Discrete"),ylab="Objective",xlab="n")
  abline(h=0,v=nd,lwd=2,col="red") # lines on the graph
  plot(ncont,continuous,type='l',pch=21,bg="blue", # plotting the continuous
       main=paste("Objective vs n to find optimal tickets sold\n",
                  "(", nc, ")", "gamma=", gamma, ", N=", N,
                  ", Continuous"),ylab="Objective",xlab="n",col="black")
  abline(h=0,v=nc,lwd=2,col="blue") # lines on the graph
  print(list(nd=nd,nc=nc,N=N,p=p,gamma=gamma)) # named list
}
