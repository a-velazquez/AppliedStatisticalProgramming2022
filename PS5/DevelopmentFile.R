
## Load libraries and set working directory
library(devtools)
library(roxygen2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) #This will need to be changed to match your directory

## This is run once when the package strcuture is first created


## This can be run many times as the code is updates
current.code <- as.package("integrateIt")
load_all(current.code)
document(current.code)

# roxygen2::update_collate("multiplySquares.R")

## Let's look at a function

## Let's try it out





