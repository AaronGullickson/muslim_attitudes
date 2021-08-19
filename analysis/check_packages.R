#check for packages that the R scripts will use. If missing, try to install.
#code borrowed from here:
#http://www.vikram-baliga.com/blog/2015/7/19/a-hassle-free-way-to-verify-that-r-packages-are-installed-and-loaded

#add new packages to the chain here
packages = c("foreign","psych","corrgram","ggplot2","mice","lme4","lattice","arm",
             "ggmap","ggalt","tidyr","purrr","dplyr","reshape2","ggrepel","egg",
             "tidyverse","mapproj","texreg","parallel","tabulizer","tibble",
             "ggalt","corrplot","lavaan","semTools","mirt")

package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})
