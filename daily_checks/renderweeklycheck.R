##this script knits the rmd
# Set Pandoc path
Sys.setenv(RSTUDIO_PANDOC = "C:/Program Files (x86)/pandoc-3.7.0.1")
setwd("C:/Users/LaGorgaL.S/Documents/WS_Checks")
library(rmarkdown)
render("C:/Users/LaGorgaL.S/Documents/WS_Checks/anything_WEIRd.Rmd")