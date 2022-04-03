## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #This will need to be changed to match your directory

## This is run once when the package strcuture is first created
# This creates the package directory
# devtools::create("integrateIt")


## This can be run many times as the code is updates
current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)


## Let's try it out
test_fx <- function(x){x^2}
testIntegral_Trap <- integrateIt(test_fx, 2000, c(1,3), rule="Trapezoid")
testIntegral_Simp <- integrateIt(test_fx, 3, c(1,3), rule="Simpson")

print(testIntegral_Simp)
print(testIntegral_Trap)

tolTest(test_fx, 3, 0.01, c(1,3), "Simpson")

# This installs the package
devtools::install(current.code)

# Call help page on our method
?integrateIt
?`Simpson-class`
?tolTest

# This builds the package
# devtools::build("integrateIt")


