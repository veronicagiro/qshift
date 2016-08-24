#' Title
#'
#' @param dist
#'
#' @return
#' @export
#'
#' @examples
qshift <- function (dist){

    #dist <- deparse(substitute(dist))

    qdist=paste("q", dist, sep = "")
    pdist=paste("p", dist, sep = "")

    qdist <-  get(qdist, mode = "function")
    qargs <-  formals(qdist)

    pdist <-  get(pdist, mode = "function")
    pargs <- formals(pdist)

    quantile <- function ()
    {
      #if (L > U) stop("U must be greater than or equal to L")

      call <- as.list(match.call())[-1]
      #qargs <- c(qargs[!is.element(names(qargs), names(call))], call[is.element(names(call), names(qargs))])
      qargs <- intersect_args(x = qargs, y = call)


      pargs <- c(pargs[!is.element(names(pargs), names(call))], call[is.element(names(call), names(pargs))])
      pargs <- intersect_args(x = pargs, y = call)

      #pargs$q <- eval(pargs$q) - shift
      #pargs$q <- eval(qargs$p) - shift
      #probability <-  do.call("pdist", as.list(pargs))

      #pUargs <- pLargs <- pargs
      #pUargs$q <- U
      #pLargs$q <- L

      #if ( do.call("pdist", as.list(pUargs)) == 0) stop("U below lower support limit")
      #if ( do.call("pdist", as.list(pLargs)) == 1) stop("L above upper support limit")

      #qargs$p <- do.call("pdist", as.list(pLargs)) + p * (do.call("pdist", as.list(pUargs)) - do.call("pdist", as.list(pLargs)))

      #qp <- do.call("qdist", as.list(qargs))
      #quantile <- do.call("qdist", as.list(probability))
      #quantile <- pmin(pmax(L,do.call("qdist", as.list(qargs))),U)
      #quantile <- pmin(pmax(L, qp ), U)

      #quantile_1 <- do.call("qdist", as.list(qargs))

      #pargs$q <- quantile_1 + shift
      #probability <-  do.call("pdist", as.list(pargs))

      #qargs$p <- probability
      #quantile <- do.call("qdist", as.list(qargs))

      # stessa cosa che fare
       quantile <- do.call("qdist", as.list(qargs))
       quantile <- quantile + shift

      return(quantile)

    }

    formals(quantile) <-  c(formals(qdist), eval(substitute(alist(shift=0))))

    return(quantile)

  }

## prova

# > qnorm(0.2, mean = 2, sd=1)
# [1] 1.158379
# > 1.158379  -2
# [1] -0.841621
# > pnorm(q = -0.841621, 2, 1)
# [1] 0.002244241
# > qnorm(0.002244241, mean = 2, sd=1)
# [1] -0.841621

