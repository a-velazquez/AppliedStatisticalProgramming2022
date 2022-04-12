#' Fit \code{PoisMLE} model
#'
#' For a vector of observed data, returns an object of class \code{PoisMLE}, 
#' including an estimate of its associated \eqn{\lambda} parameter.
#'
#' @param y A vector of positive integer observations.
#' @param SEtype A character string of \code{"basic"} or \code{"bootstrap"}; see \code{standardError}.
#' @param B An integer; the default is set to \code{B=10}. This argument is not used
#' if \code{SEtype="basic"}.
#'
#' @return An object of class \code{'PoisMLE'} containing
#'  \item{y}{The original vector of positive integer observations.}
#'  \item{MLE}{The maximum likelihood estimator for given observations.} 
#'  \item{LL}{The log likelihood calculated from the observed data using the MLE estimator.}
#'  \item{SE}{The standard error for the MLE.}
#'  \item{SEtype}{The method used to calculate \code{SE}, supplied by the user.}
#' 
#' @author Alma Velazquez
#' 
#' @examples
#' \donttest{y_vec <- c(1,2,3,4)
#' estimatePois(y_vec, "bootstrap", B=10)}
#' 
#' @seealso PoisMLE-class, mle, logLik
#' @rdname estimatePois
#' @aliases estimatePois
#' 
#' @export
setGeneric(name="estimatePois",
           def=function(y, SEtype, B=10)
           {standardGeneric("estimatePois")}
)
#' @export
setMethod(f="estimatePois",
          definition=function(y, SEtype, B=10){
            return(new("PoisMLE", 
                       y = y, 
                       MLE = mle(y), 
                       LL = logLik(y, mle(y)),
                       SE = standardError(y, SEtype, B),
                       SEtype = SEtype))
          }
)
