#' Compute density function of a shifted distribution
#'
#' @param dist character string indicating the distribution name. It can be set as:
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
#' @return numeric vector of the density function of a specified distribution
#' @export
#'
#' @examples
#' # Binomial distribution
#' db <- dshift("binom")
#' zero_shift <- db(x = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 0)
#' zero_shift
#' one_shift <- db(x = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 1)
#' one_shift
#' two_shift <- db(x = c(1,2,3,4,5), size = 6, prob = 0.5, shift = 2)
#' two_shift
#'
#' # Normal Distribution
#' dn <- dshift("norm")
#' zero_shift <- dn(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 0)
#' zero_shift
#' one_shift <- dn(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 1)
#' one_shift
#' two_shift <- dn(x = c(1,2,3,4,5), mean = 2, sd = 1, shift = 2)
#' two_shift
#'
dshift <- function(dist){

    ddist=paste("d", dist, sep = "")

    # gets density function
    ddist <-  get(ddist, mode = "function")

    # gets argument of density function
    dargs <- formals(ddist)

    # Output function starts here
    density <- function(){

      # gets density arguments
      call <- as.list(match.call())[-1]

      # as a result, the whole string gets all unique arguments belonging to density function and ddist
      dargs <- intersect_args(x = dargs, y = call)

      # method for computing density values for shifted distributions
      dargs$x <- eval(dargs$x)
      dargs$x <- dargs$x - shift
      density <- do.call("ddist", as.list(dargs))

      # returns density values for shifted distributions
      return(density)

    }

    # add to density function formals shift with values as passed with dshift
    formals(density) <-  c(formals(ddist), eval(substitute(alist(shift=0))))
    # return density function
    return(density)
  }
