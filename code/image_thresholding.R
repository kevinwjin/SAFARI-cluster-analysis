## Thresholding of greyscale images utilizing Otsu's method
## Author: Kevin Jin

# Install dependencies
install.packages("BiocManager") # SAFARI dependency
BiocManager::install("EBImage") # Image thresholding and SAFARI dependency
install.packages("remotes") # Installing SAFARI
remotes::install_github("kevinwjin/
                        SAFARI") # Forked from estfernandez with read.image fix
install.packages("tidyverse") # dplyr and ggplot2

# Include libraries
library(EBImage)
library(SAFARI) # Image processing
library(dplyr) 

# Retrieve list of all images (set working directory to image folder)
file_list <- dir(pattern = "gif$")

# Read image
x <- readImage("lena_gray_256.tif")
display(x)

# Threshold with Otsu's method
y <- x > otsu(x)
display(y)
