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

# --------------------------------------------
# Find the package of 'tidyverse'
# --------------------------------------------
install.packages("packagefinder", dependencies = TRUE)
library(packagefinder)
findPackage("tidyverse") 

# --------------------------------------------
# Install the package
# --------------------------------------------

# There are two ways to install the package

# 1.using CRAN 
install.packages('tidyverse')

# 2.using pak to install R packages

# first install pak package with 
install.packages("pak")

install.packages("pak", repos = "https://r-lib.github.io/p/pak/dev/")

# Install packages from CRAN or Bioconductor
pak::pkg_install("tidyverse")

# --------------------------------------------
# Vignettes Demonstrations
# --------------------------------------------
library(tidyverse)
vignette("tidyverse")
browseVignettes(package="tidyverse")
demo(package="tidyverse")

# --------------------------------------------
# Searching for Help
# --------------------------------------------

# A: Search all sources of documentation and return those that match the search 
# Note: Both help.search() and ?? operator perform similar tasks

help.search('tidyverse')

??tidyverse

# B: Access the documentation of functions and data sets
# Note: These four lines of code perform the same task

help(tidyverse)

?tidyverse

help(package="tidyverse")

library(help = "tidyverse")

# ----------------------------------------------
# List some functions from the tidyverse package
# ----------------------------------------------

# Search for functions in all loaded packages that start with "tidyverse"
library(tidyverse)
functions <- ls("package:tidyverse")
head(functions)

# This can be used to find functions related to the tidyverse,
# but may include functions from other packages
apropos("^tidyverse")

# Alternatively, this command is similar to the first ls() command and may be used interchangeably
ls("package:tidyverse")

# For example, let's use the dplyr::filter function from the tidyverse

filtered_data <- dplyr::filter(iris, Sepal.Length > 5)
print(filtered_data)

surveys <- read_csv("data/portal_data_joined.csv")

install.packages("systemfonts")
install.packages("textshaping")
```R
install.packages("textshaping", dependencies = TRUE)
sudo apt install libcurl4-openssl-dev libfontconfig1-dev libfreetype6-dev libfribidi-dev libharfbuzz-dev libtiff-dev libxml2-dev pandoc
