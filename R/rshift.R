#' Random generation for a shifted distribution
#'
#' @param dist character string indicating function name
#'
#' @return random generator function
#' @export
#'
#' @examples d
rshift <- function (dist){

  rdist <- paste("r", dist, sep = "")
  rdist <-  get(rdist, mode = "function")
  rargs <-  formals(rdist)

  qdist <- paste("q", dist, sep = "")
  qdist <-  get(qdist, mode = "function")
  qargs <-  formals(qdist)

  pdist <- paste("p", dist, sep = "")
  pdist <-  get(pdist, mode = "function")
  pargs <-  formals(pdist)

  random <- function(...){

    call <- as.list(match.call())[-1]

    pargs <- intersect_args(x = pargs, y = call)

    qargs <- intersect_args(x = qargs, y = call)

    rargs <- intersect_args(x = rargs, y = call)

    #qargs$p <- runif(n)

    #random <- do.call(qdist, as.list(qargs))
    set.seed(1)
    random <- do.call(rdist, as.list(rargs))
    random <- random + shift

    return(random)

  }

  formals(random) <-  c(formals(rdist), eval(substitute(alist(shift=0))))

  return(random)
}



