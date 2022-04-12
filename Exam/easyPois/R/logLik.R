#' Calculate a log likelihood in \code{PoisMLE} object
#'
#' For internal use only. This function calculates the log likelihood for a given 
#' vector of observed data, conditional on the value of the \eqn{\lambda} parameter 
#' in the associated Poisson distribution. The calculation is performed according to
#' \deqn{LL(\lambda) = -n\lambda-\sum^n_[i=1]\ln(y_i!) + \ln(\lambda)\summ^n_[i=1]y_i}.
#' The computed value is ultimately returned when initializing an object of class \code{PoisMLE}.
#'
#' @param y A vector of positive integer observations.
#' @param lambda The assumed value of \eqn{\lambda}, see \code{mle}.
#'
#' @return A log likelihood for consumption of objects of class \code{PoisMLE}
#'  \item{LL}{The log likelihood value}
#' @author Alma Velazquez
#' @examples
#' \dontrun{y_vec <- c(1,2,3,4)
#' logLik(y_vec, mle(y_vec))}
#' @seealso mle, PoisMLE-class
#' @rdname logLik
#' @aliases logLik
#' 
#' @keywords internal
logLik <- function(y, lambda){
  n <- length(y)
  # Using n and lambda, depend on gmp to apply this formula and avoid floating point errors.
  ll_result <- (-n * lambda) - sum(log(gmp::factorialZ(y))) + log(lambda) * sum(y)
  return(ll_result)
}
