#' Create a density curve and area polygon of a given normal distribution.
#'
#' @param mu Average of the distribution
#' @param sigma Standard deviation of the distribution
#' @param a Upper bound of the probability
#' @return A density curve of the given distribution and probability
#' @examples
#' \dontrun{
#' myncurve(10, 3, 7)
#' myncurve(-2, 1.5, -3)
#' }

myncurve = function(mu, sigma, a){
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve <- seq(mu-3*sigma, a, length=1000)
  ycurve <- dnorm(xcurve, mean=mu, sd=sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col="Red")

  area = pnorm(a, mean=mu, sd=sigma)
  round(area, 4)
}
