library(devtools)
library(roxygen2)
# This sets the working directory to where this file is saved.
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# Uncomment and run once to create the package directory, then moved files into the R folder.
# devtools::create("easyPois")


## This can be run many times as the code is updates
current.code <- as.package("easyPois")
load_all(current.code)
document(current.code)

# Test both types of SE methods
estimatePois(c(50, 70, 80, 99, 82, 43), "basic", 223)
new_test <- estimatePois(c(50, 70, 80, 99, 82, 43), "bootstrap", 223)

testPOIS <- estimatePois(rpois(4, 2.5), "bootstrap", B=7000)

