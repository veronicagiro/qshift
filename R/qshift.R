#' Compute quantile function of a shifted distribution
#'
#' @param dist dist character string indicating distribution name. It can be set as:
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
#' @return numeric vector of the quantile function of a specified distribution
#' @export
#'
#' @examples
#'
#' # Negative Binomial distribution
#' qnb <- qshift("nbinom")
#' zero_shift <- qnb(p = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), size = 6, mu = 4, shift = 0)
#' zero_shift
#' one_shift <- qnb(p = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), size = 6, mu = 4, shift = 1)
#' one_shift
#' two_shift <- qnb(p = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), size = 6, mu = 4, shift = 2)
#' two_shift
#'
#' # Chi Squared Distribution
#' qc <- qshift("chisq")
#' zero_shift <- qc(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), df = 2, shift = 0)
#' zero_shift
#' one_shift <- qc(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), df = 2, shift = 1)
#' one_shift
#' two_shift <- qc(p = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1), df = 2, shift = 2)
#' two_shift
#'
qshift <- function (dist){

    qdist=paste("q", dist, sep = "")

    # uncomment if use solution 1
    # gets distribution function
    #pdist=paste("p", dist, sep = "")

    # gets quantile function
    # gets argument of quantile function
    qdist <-  get(qdist, mode = "function")
    qargs <-  formals(qdist)

    # uncomment if use solution 1
    # gets argument of distribution function
    # pdist <-  get(pdist, mode = "function")
    # pargs <- formals(pdist)

    # Output function starts here
    quantile <- function() {

      # gets quantile arguments
      call <- as.list(match.call())[-1]

      # as a result, the whole string gets all unique arguments belonging to quantile function and qdist
      qargs <- intersect_args(x = qargs, y = call)

      # uncomment if use solution 1
      #pargs <- c(pargs[!is.element(names(pargs), names(call))], call[is.element(names(call), names(pargs))])
      #pargs <- intersect_args(x = pargs, y = call)

      # method for computing quantile values for shifted distributions
      # Solution 1
      #quantile_1 <- do.call("qdist", as.list(qargs))
      #pargs$q <- quantile_1 + shift
      #probability <- do.call("pdist", as.list(pargs))
      #qargs$p <- probability
      #quantile <- do.call("qdist", as.list(qargs))

      # Solution 2 (correct?)
       quantile <- do.call("qdist", as.list(qargs))
       quantile <- quantile + shift

       # returns quantile values for shifted distributions
      return(quantile)

    }
    # add to quantile function formals shift with values as passed with qshift
    formals(quantile) <-  c(formals(qdist), eval(substitute(alist(shift=0))))
    # return quantile function
    return(quantile)

}

