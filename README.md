# Cluster analysis of 1400 reference images processed with SAFARI

## Description
This is a cluster analysis of 29 shape feature parameters derived from 1400 binary images of reference shapes, processed with the R package `SAFARI`. There are 70 shapes represented by 20 images each, with minor differences in between. The goal of this project is to evaluate different clustering methods of image shape features and devise a novel model-based clustering method.

## Contents
* `gif/` - 1400 binary images of reference shapes
* `cluster_analysis.R` - R script that executes the cluster analysis.

## Dependencies
* `BiocManager` - `SAFARI` dependency
* `EBImage` - `SAFARI` dependency
* `remotes` - For installing `SAFARI`
* `SAFARI` - For segmenting shapes within images and extracting shape features
* `tidyverse` - Data manipulation with `dplyr` and beautiful graphics with `ggplot2`
* `mclust` - Hierarchical and Gaussian mixture model clustering
* `factoextra` - Beautiful cluster visualizations
* `parallel` - Parallel computation in R