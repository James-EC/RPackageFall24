#' Title
#'
#' @param N number of seats on the flight
#' @param gamma accaptable probability of overbooking
#' @param p probability someone shows up to their flight
#'
#' @return Two graphs showing the ideal number of tickets sold, one continuous and one discrete in math
#'          and a list of the input variables and the outputs
#' @importFrom graphics abline
#' @importFrom graphics lines
#' @importFrom stats pbinom
#' @importFrom stats qnbinom
#' @export
#'
#' @examples ntickets(N=200,gamma=0.02,p=0.95)
ntickets = function(N,gamma,p) {

  #make axes for the discrete graph
  xd=(N:(N+N*(1-p)))
  yd=1-gamma-pbinom(N,xd,0.95)

  #make axes for the continuous graph
  xc=seq(N,N+N*(1-p), length = 1000)
  yc=1-gamma-pnorm(N,xc*p,sqrt(xc*p*(1-p)))

  #find the values of n for continuous and discrete
  nd=N+qnbinom(gamma,N,p)
  nc=xc[which.min(abs(yc))]

  #discrete plot
  plot(xd,yd, xlab="n",ylab="Objective",pch=16,col="#5455F3",
       main="Objective Vs n to find optimal tickets sold",cex=1.2,
       sub=paste("(",nd,") gamma=",gamma," N=",N,"discrete"))
  abline(v=nd, col = "#C0D0F0")
  abline(h=0.0, col = "#C0D0F0")
  lines(xd,yd,col="#75CFD4")

  #continuous plot
  plot(xc,yc, xlab="n",ylab="Objective",type="l",
       main="Objective Vs n to find optimal tickets sold",
       sub=paste("(",round(nc,4),") gamma=",gamma," N=",N,"continuous"))
  abline(v=nc, col = "#CC0000")
  abline(h=0.0, col = "#CC0000")

  #make a list
  list(nd=nd, nc=nc, N=N,p=p,gamma=gamma)
}
