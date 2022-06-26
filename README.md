# Cluster analysis of SAFARI-extracted features

## Description

This is a cluster analysis of 29 shape feature parameters derived from several
image sets, which contain binary images of reference shapes, segmented and processed
with the R package `SAFARI`. The goal of this project is to evaluate the performance of
different clustering methods based on similarity to the ground truth and devise a
novel model-based clustering method.

## Datasets

* `MPEG-7` - 70 classes (shapes) of 20 images each with minor differences in between.
* `ETH-80` - Binary version of the famous dataset. 8 classes (objects) with 10 subclasses
each with 41 images in each subclass.
* `Iris` - Famous dataset from 1936. 150 observations with 4 variables.

## Contents

* `data/` - Datasets
* `plots/` - Generated plots comparing clustering method accuracy
* `code/cluster_analysis.R` - Image feature clustering execution script
* `code/image_thresholding.R` - Thresholds grayscale images to binary
* `code/cluster_evaluation.R` - Traditional dataset clustering execution script

## Dependencies

* `BiocManager` - `SAFARI` dependency
* `EBImage` - `SAFARI` dependency and image thresholding
* `remotes` - For installing `SAFARI`
* `SAFARI` - For segmenting shapes from binary images and extracting shape features
* `tidyverse` - Data manipulation with `dplyr` and graphics with `ggplot2`
* `mclust` - Gaussian mixture model clustering and Adjusted Rand Index
* `factoextra` - Beautiful cluster visualizations
* `parallel` - Parallel computation in R
