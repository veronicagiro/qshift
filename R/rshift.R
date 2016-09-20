#' Perform Random generation for a shifted distribution
#' @description This function generates the random number generator function of a shifted specified distribution.
#' Passing the distribution conventional name as argument, it returns the random number generator function of the shifted specified distribution.
#' @param dist character string indicating distribution name. It can be set as:
#' \itemize{
#'   \item beta Beta Distribution
#'   \item binom Binomial Distribution
#'   \item cauchy Cauchy Distribution
#'   \item chisq Chi-Square Distribution
#'   \item exp Exponential Distribution
#'   \item f F Distribution
#'   \item gamma Gamma Distribution
#'   \item geom Geometric Distribution
#'   \item hyper Hypergeometric Distribution
#'   \item logis Logistic Distribution
#'   \item lnorm Log Normal Distribution
#'   \item nbinom Negative Binomial Distribution
#'   \item norm Normal Distribution
#'   \item pois Poisson Distribution
#'   \item t Student t Distribution
#'   \item unif Uniform Distribution
#'   \item weibull Weibull Distribution
#'   \item wilcox Wilcoxon Rank Sum Statistic Distribution
#'   \item signrank Wilcoxon Signed Rank Statistic Distribution
#' }
#'
#'
#' @return random number generator function of a shifted specified distribution
#' @export
#'
#' @examples
#'
#' # Binomial Distribution
#' rb <- rshift("binom")
#' set.seed(1)
#' zero_shift <- rb(n = 10000, size = 6, prob = 0.5, shift = 0)
#' zero_shift
#' set.seed(1)
#' one_shift <- rb(n = 10000, size = 6, prob = 0.5, shift = 1)
#' one_shift
#' set.seed(1)
#' two_shift <- rb(n = 10000, size = 6, prob = 0.5, shift = 2)
#' two_shift
#'
#' # Weibull Distribution
#' rw <- rshift("weibull")
#' set.seed(1)
#' zero_shift <- rw(n = 1000, shape = 1.4, scale = 3, shift = 0)
#' zero_shift
#' set.seed(1)
#' one_shift <- rw(n = 1000, shape = 1.4, scale = 3, shift = 1)
#' one_shift
#' set.seed(1)
#' two_shift <- rw(n = 1000, shape = 1.4, scale = 3, shift = 2)
#' two_shift
#'
rshift <- function(dist){

  rdist <- paste("r", dist, sep = "")
  # gets random generation function
  rdist <-  get(rdist, mode = "function")
  # gets argument of random generation function
  rargs <-  formals(rdist)

  random <- function(...){

    # gets random generation arguments
    call <- as.list(match.call())[-1]

    # intersect random number generator function and the call (rdist), giving to random number generator function arguments the values set to the corresponding arguments of the call (rdist)
    rargs <- intersect_args(x = rargs, y = call)

    # method for computing random generated values for shifted distributions
    random <- do.call(rdist, as.list(rargs))
    random <- random + shift

    return(random)

  }

  # add to random generation function formal shift with values as passed with rshift
  formals(random) <-  c(formals(rdist), eval(substitute(alist(shift=0))))
  # return random generation function
  return(random)
}



