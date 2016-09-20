#' Compute distribution function of a shifted distribution
#' @description This function generates the distribution function of a shifted specified distribution.
#' Passing the distribution conventional name as argument, it returns the distribution function of the shifted specified distribution.
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
#' @return distribution function of the truncated specified distribution
#' @export
#'
#' @examples
#' # Poisson Distribution
#' pp <- pshift("pois")
#' zero_shift <- pp(q = c(1,2,3,4,5), lambda = 1.5, shift = 0)
#' zero_shift
#' one_shift <- pp(q = c(1,2,3,4,5), lambda = 1.5, shift = 1)
#' one_shift
#' two_shift <- pp(q = c(1,2,3,4,5), lambda = 1.5, shift = 2)
#' two_shift
#'
#' # Student t distribution
#' pt <- pshift("t")
#' zero_shift <- pt(q = c(1,2,3,4,5), df = 40, shift = 0)
#' zero_shift
#' one_shift <- pt(q = c(1,2,3,4,5), df = 40, shift = 1)
#' one_shift
#' two_shift <- pt(q = c(1,2,3,4,5), df = 40, shift = 2)
#' two_shift
#'
pshift <- function(dist){
  # generate distribution function name of the specified distribution
  pdist <- paste("p", dist, sep = "")
  # get distribution function and its arguments
  pdist <-  get(pdist, mode = "function")
  pargs <-  formals(pdist)
  
  # Output function starts here
  probability <- function(){
    
    # gets distribution arguments
    call <-  as.list(match.call())[-1]
    # intersect distribution function and the call (pdist), giving to distribution function arguments the values set to the corresponding arguments of the call (pdist)
    pargs <- intersect_args(x = pargs, y = call)
    
    # method for computing distribution values for shifted distribution
    pargs$q <- q - shift
    probability <-  do.call("pdist", as.list(pargs))
    
    # returns distribution values for shifted distributions
    return(probability)
    
  }
  
  # add to distribution function formals shift with values as passed with pshift
  formals(probability) <- c(pargs, eval(substitute(alist(shift=0))))
  # return distribution function
  return(probability)
  
}

