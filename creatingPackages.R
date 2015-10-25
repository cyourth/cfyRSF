# Creating Your Own Package
# =========================

# http://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch

# example of docs
library(ggplot2)
?ggplot2
?mean

# step 1: install R package tools
install.packages("devtools")
devtools::install_github("klutometis/roxygen")
library("devtools")
library("roxygen2")

# step 2: add file and documentation
#    - update DESCRIPTION file
#    - create file with code --> ./cfyRSF/R/test_functions.R
#    - update function documentation --> https://github.com/klutometis/roxygen#roxygen2

# step 3: process documentation
getwd()
document()

# step 4: restart R (ctrl-shift-f10)
library("devtools")
library("roxygen2")

# step 5: install new package
getwd()
setwd("~/projects/r_programs/cfyRSF")
install("cfyRSF")
library(cfyRSF)

?pop.sd
?pop.var
?pop.covar
?sample.covar
sample.covar

# step 6: make the package a GitHub repo
#    - book --> http://r-pkgs.had.co.nz/
#    - github tutorial --> http://kbroman.org/github_tutorial/
#    - github example --> https://github.com/kbroman/broman
#    - project templates --> http://projecttemplate.net/index.html
install_github('<repo_name>','<github_username>')
install_github('cfyRSF', 'cyourth')

# -------------------------------------------------------------------------------

setwd("./cfyRSF")
document()

# restart R (ctrl-shift-f10)
library("devtools")
library("roxygen2")

setwd("~/projects/r_programs/cfyRSF")
install("cfyRSF")
library(cfyRSF)

?pop.sd
?pop.var
?pop.covar
?sample.covar

