# system("defaults write org.R-project.R force.LANG en_US.UTF-8")
# to turn off all errors about LC_....
# Cannot set LC_CTYPE to default locale: No such file or directory
install.packages("tidyverse")
install.packages("testthat")
install.packages("roxygen2")
#https://github.com/Rdatatable/data.table/wiki/Installation
# on mac to enable parallel comp need to install homebrew/llvm/... see link
install.packages("data.table")
install.packages("glmnet")
install.packages("sqldf")
install.packages("TeachingDemos") # history
install.packages("arules")
install.packages("rmarkdown")
install.packages("RJDBC")
install.packages("RPostgreSQL")
install.packages("pryr")
# needed zlib 
install.packages("devtools")

# https://google.github.io/styleguide/Rguide.xml
# http://adv-r.had.co.nz/Style.html style guide

#install.packages("Hmisc)
#install.packages("plyr")
#install.packages("ggplot2)
#install.packages("stringr)
