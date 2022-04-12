#' A fitted MLE Poisson model
#' 
#' Objects of class \code{PoisMLE} are created by the \code{estimatePois} function
#'
#' 
#' An object of the class `PoisMLE' has the following slots:
#' \itemize{
#' \item \code{y } The original vector of non-negative integer observations.
#' \item \code{MLE } The maximum likelihood estimator for given observations.
#' \item \code{LL } The log likelihood calculated from the observed data using the MLE estimator.
#' \item \code{SE } The standard error for the MLE.
#' \item \code{SEtype } The method used to calculate the standard error.
#' }
#'
#' @author Alma Velazquez: \email{a.m.velazquez@@wustl.edu}
#' @aliases estimatePois-method 
#' @rdname PoisMLE
#' @export
setClass(Class="PoisMLE", 
         representation = representation(
           # All of these will be numeric except SEtype.
           y = "numeric",
           MLE = "numeric",
           LL = "numeric",
           SE = "numeric",
           SEtype = "character"
         ),
         prototype = prototype(
           # Set a "zeroed-out" prototype
           y = 0,
           MLE = 0,
           LL = 0,
           SE = 0,
           SEtype = "basic"
         )
)

setValidity("PoisMLE", function(object){
  # Use internal functions to test
  # Test that input data is appropriate
  stopifnot("All observations must be non-negative" = all(object@y>=0))
  stopifnot("All observations must be integers" = all(as.integer(object@y)==object@y))
  
  # Test MLE
  stopifnot("Invalid MLE" = mle(object@y)==object@MLE)
  
  # Test LL
  stopifnot("Invalid LL" = logLik(object@y, object@MLE)==object@LL)
  
  # Note: SE cannot be validated because B is not stored in the object.
  
  # Test SEtype
  stopifnot("Invalid SEtype" = object@SEtype %in% c("basic", "bootstrap"))
  
}

)

#' @rdname PoisMLE
#' @export
setMethod("initialize", "PoisMLE", 
          function(.Object, ...){
            value = methods::callNextMethod()
            # Call validator
            methods::validObject(value)
            return(value)
          }
) 


#' @rdname PoisMLE
#' @export
setMethod(f = "plot",
          # Define signature and definition in terms of x;
          # this is what the plot generic expects
          signature(x = "PoisMLE"),
          definition = function(x){
            plot(x@MLE)
          })


#' @export
#' @import ggplot2
setMethod(f = "plot",
          signature(x = "PoisMLE"),
          definition = function(x){
            
            # This defines a sequence of 100 lambdas from 1 up 
            # to the value stored in the object.
            lambdas <- seq(1, 2*x@MLE, 2*x@MLE/100)
            
            # This finds the log likelihood given the object's input
            # data and each lambda from the sequence above.
            ll <- logLik(x@y, lambdas)
            
            # Put them together to use in the plot.
            jitter <- data.frame("jitter_x"=lambdas, 
                                 "jitter_y"=ll)
            
            # Create a df that just contains the estimate in 
            # the object and its associated log likelihood.
            df <- data.frame("x"=x@MLE, "y"=x@LL)
            
            # Depend on ggplot to create this plot of log likelihood ~ lambda
            plt <- ggplot2::ggplot()+
              # A line and a point where the object's lambda MLE is.
              geom_vline(xintercept = x@MLE, color="red")+
              geom_point(aes(x=x, y=y), data=df, color="red", size=3)+
              # 2 more lines where +/- 1.96 standard errors are.
              geom_vline(xintercept = x@MLE+1.96*x@SE, color="blue", linetype="dashed")+
              geom_vline(xintercept = x@MLE-1.96*x@SE, color="blue", linetype="dashed")+
              # Add a ribbon to this region to make it cleater.
              geom_ribbon(aes(y=jitter_y, xmax = x@MLE+1.96*x@SE, xmin=x@MLE-1.96*x@SE),
                          data = jitter, fill = "blue", alpha = .1)+
              # Add the jittered points created above for comparison.
              geom_point(aes(x=jitter_x, y=jitter_y), data = jitter, alpha=0.4)+
              labs(x="Lambda", y="Log Likelihood")+
              theme_minimal()
            
            return(plt)
          })