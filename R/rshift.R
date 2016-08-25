#' Perform Random generation for a shifted distribution
#'
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
#' @return numeric vector of random generated values from a specified distribution
#' @export
#'
#' @examples
#'
#' # Binomial Distribution
#' rb <- rshift("binom")
#' zero_shift <- rb(n = 10000, size = 6, prob = 0.5, shift = 0)
#' zero_shift
#' one_shift <- rb(n = 10000, size = 6, prob = 0.5, shift = 1)
#' one_shift
#' two_shift <- rb(n = 10000, size = 6, prob = 0.5, shift = 2)
#' two_shift
#'
#' # Weibull Distribution
#' rw <- rshift("weibull")
#' zero_shift <- rw(n = 1000, shape = 1.4, scale = 3, shift = 0)
#' zero_shift
#' one_shift <- rw(n = 1000, shape = 1.4, scale = 3, shift = 1)
#' one_shift
#' two_shift <- rw(n = 1000, shape = 1.4, scale = 3, shift = 2)
#' two_shift
#'
rshift <- function(dist){

  rdist <- paste("r", dist, sep = "")
  # gets random generation function
  rdist <-  get(rdist, mode = "function")
  # gets argument of random generation function
  rargs <-  formals(rdist)

  # qdist <- paste("q", dist, sep = "")
  # qdist <-  get(qdist, mode = "function")
  # qargs <-  formals(qdist)
  #
  # pdist <- paste("p", dist, sep = "")
  # pdist <-  get(pdist, mode = "function")
  # pargs <-  formals(pdist)

  random <- function(...){

    # gets random generation arguments
    call <- as.list(match.call())[-1]

    # as a result, the whole string gets all unique arguments belonging to random generation function and rdist
    rargs <- intersect_args(x = rargs, y = call)
    # pargs <- intersect_args(x = pargs, y = call)
    # qargs <- intersect_args(x = qargs, y = call)


    #qargs$p <- runif(n)
    #random <- do.call(qdist, as.list(qargs))

    set.seed(1)

    # method for computing random generated values for shifted distributions
    random <- do.call(rdist, as.list(rargs))
    random <- random + shift

    return(random)

  }

  # add to random generation function formals shift with values as passed with rshift
  formals(random) <-  c(formals(rdist), eval(substitute(alist(shift=0))))
  # return random generation function
  return(random)
}



