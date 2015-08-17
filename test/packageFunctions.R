# Creating Your Own Package
# =========================

# step 1: install R package tools
install.packages("devtools")
devtools::install_github("klutometis/roxygen")
library("devtools")
library("roxygen2")

# step 2: create basic package directory
setwd("~/projects/r_programs")
create("cfyRSF")

# step 3: add file and documentation
#    - update DESCRIPTION file
#    - copy files over --> ~/projects/r_programs/cfyRSF/R
#    - update function documentation --> https://github.com/klutometis/roxygen#roxygen2

# step 4a: process documentation
getwd()
setwd("~/projects/r_programs/cfyRSF")
document()

# step 4b: restart R (ctrl-shift-f10)
library("devtools")
library("roxygen2")

# step 5: install new package
setwd("~/projects/r_programs")
install("cfyRSF")
library(cfyRSF)
setwd("~/projects/r_programs/cfyRSF")

?pop.sd
?pop.var
?pop.covar
?sample.covar

testData <- c(4,5,6,5,6,7,1,4,5,3,4,3,4,8,7,6)
pop.sd(testData)
pop.var(testData)
pop.covar(testData)
sample.covar(testData)
