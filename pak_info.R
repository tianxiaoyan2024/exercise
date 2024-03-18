# --------------------------------------------
# Script Name: pak_info.R
# Purpose: This scribes how to access information 
#          about the tidyverse package.

# Author:     Xiaoyan Tian
# Email:      xiaoyantian@mail.ustc.edu.cn
# Date:       2024-03-17
#
# --------------------------------------------
cat("\014") # Clears the console
rm(list = ls()) # Remove all variables

####################################################

# Find the package of 'tidyverse'
install.packages("packagefinder", dependencies = TRUE)
library(packagefinder)
findPackage("tidyverse") 

####################################################

# Install the package from CRAN

# using CRAN to install R packages
install.packages('tidyverse')

# using pak to install R packages

# first install pak package
install.packages("pak")
install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")

# Install packages from CRAN or Bioconductor
pak::pkg_install("tidyverse")

####################################################

# Vignettes Demonstrations
vignette("tidyverse")
browseVignettes(package="tidyverse")
demo(package="tidyverse")

# Searching for Help

# Search all sources of documentation and 
# return those that match the search
help.search('tidyverse')

??tidyverse

# Access the documentation of functions and data sets
help(tidyverse)

?tidyverse

help(package="tidyverse")

library(help = "tidyverse")

## List some functions from the tidyverse package
functions <- ls("package:tidyverse")
head(functions)

apropos("^tidyverse")

ls("package:tidyverse")

# For example, let's use the dplyr::filter function from the tidyverse
filtered_data <- dplyr::filter(iris, Sepal.Length > 5)
print(filtered_data)
