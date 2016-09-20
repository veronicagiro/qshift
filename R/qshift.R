#' Compute quantile function of a shifted distribution
#' @description This function generates the quantile function of a shifted specified distribution.
#' Passing the distribution conventional name as argument, it returns the quantile function of the shifted specified distribution.
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
#' @return quantile function of a shifted specified distribution
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
qshift <- function(dist){
  # generate quantile function name of the specified distribution
  qdist=paste("q", dist, sep = "")
  
  # gets quantile function and its arguments
  qdist <-  get(qdist, mode = "function")
  qargs <-  formals(qdist)
  
  # Output function starts here
  quantile <- function(){
    
    # gets quantile arguments
    call <- as.list(match.call())[-1]
    
    # intersect quantile function and the call (qdist), giving to quantile function arguments the values set to the corresponding arguments of the call (qdist)
    qargs <- intersect_args(x = qargs, y = call)
    
    # method for computing quantiles values for shifted distributions
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

