# Cluster analysis of SAFARI-analyzed MPEG-7 features

## Description
This is a cluster analysis of 29 shape feature parameters derived from the 
MPEG-7 dataset, which contains 1400 binary images of reference shapes, 
processed with the R package `SAFARI`. There are 70 shapes represented by 20 
images each, with minor differences in between. The goal of this project is to 
evaluate different clustering methods of image shape features and devise a 
novel model-based clustering method.

## Contents
* `data/` - Datasets
* `plots/` - Generated plots comparing clustering method accuracy
* `code/cluster_analysis.R` - R script that executes the cluster analysis
* `code/image_thresholding.R` - R script that thresholds grayscale images to binary

## Dependencies
* `BiocManager` - `SAFARI` dependency
* `EBImage` - `SAFARI` dependency and image thresholding
* `remotes` - For installing `SAFARI`
* `SAFARI` - For segmenting shapes within images and extracting shape features
* `tidyverse` - Data manipulation with `dplyr` and graphics with `ggplot2`
* `mclust` - Hierarchical and Gaussian mixture model clustering
* `factoextra` - Beautiful cluster visualizations
* `parallel` - Parallel computation in R