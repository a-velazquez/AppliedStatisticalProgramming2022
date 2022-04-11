### Creating and testing functions

# Test objects
l <- test_fun1(vec)
vec = c(1,2,3,4)
N <- length(vec)
LL <- test_fun2(vec, l)

### MLE
test_fun1 <- function(y){
  n <- length(y)
  mle_result <- sum(y, na.rm = TRUE) / n
  return(mle_result)
}

test_fun1(vec)