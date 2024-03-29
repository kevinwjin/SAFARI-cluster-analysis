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
file_list <- dir(pattern = "tiff$")

# Read image
x <- readImage("KA.AN1.39.tiff")
display(x)

# Threshold with Otsu's method
image_thresholding <- function(img) {
  x <- readImage("KA.AN1.39.tiff")
  
  if (colorMode(x) != "Grayscale") {
    colorMode(x) <- "Grayscale"  # convert to grayscale
  }
  
  y <- x > otsu(x)
  return(y)
}

# Threshold all images in folder
binary <- sapply(file_list, FUN = image_thresholding)
