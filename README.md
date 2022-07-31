# Cluster analysis of SAFARI-extracted features

## Description

This is a cluster analysis of 29 shape feature parameters derived from several
image sets, which contain binary images of reference shapes, segmented and processed
with the R package `SAFARI`. The goal of this project is to evaluate the performance of
different clustering methods based on similarity to the ground truth and devise a
novel model-based clustering method.

## Datasets

* `MPEG-7` - 70 classes (shapes) of 20 images each with minor differences in between.
* `ETH-80` - Binary version of the famous dataset. 8 classes (objects) with 10 
subclasses each, with 41 images in each subclass.
* `Iris` - Famous dataset from 1936. 150 observations with 4 variables.
* `maps` - Raw maps data of the countries of the world, the 48 contiguous 
US states, and all 254 counties in Texas. Includes binary shape outlines to
scale of 49 European countries (excluding Russia), the above mentioned US 
states, and the 13 Texas counties comprising the DFW metroplex.

## Contents

* `data/` - Datasets
* `plots/` - Generated plots comparing clustering method accuracy
* `code/mpeg_7.R` - Cluster analysis of MPEG-7 features
* `code/eth_80.R` - Cluster analysis of ETH-80 features
* `code/iris.R` - Cluster analysis of the Iris dataset
* `code/maps.R` - Cluster analysis of the maps dataset
* `code/image_thresholding.R` - Thresholds grayscale images to binary

## Dependencies

* `BiocManager` - `SAFARI` dependency
* `EBImage` - `SAFARI` dependency and image thresholding
* `remotes` - For installing `SAFARI`
* `SAFARI` - For segmenting shapes from binary images and extracting shape features
* `tidyverse` - Data manipulation with `dplyr` and graphics with `ggplot2`
* `mclust` - Gaussian mixture model clustering and Adjusted Rand Index
* `factoextra` - Beautiful cluster visualizations
* `parallel` - Parallel computation in R
