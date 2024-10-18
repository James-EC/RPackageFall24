#' Title
#'
#' @param mu mean of the curve
#' @param sigma standard deviation of the curve
#' @param a value x is less than or equal to
#' @param color color of shaded region
#'
#' @return a plot of the distribution
#' @export
#'
#' @examples
#' myncurve(2,0,1,color="Red")
myncurve = function(a, mu, sigma, color="Blue") {

  x=seq(mu-3*sigma, mu+3*sigma, length=1000)

  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu+3*sigma))

  xcurve=seq(mu-3*sigma, a, length = 1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col=color)

  prob=pnorm(a, mean=mu, sd=sigma)
  prob=round(prob,4)
  list("mu"=mu, "sigma"=sigma,"a"=a, "probability"=prob)
}
