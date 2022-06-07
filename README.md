# Cluster analysis of 1396 reference images processed with SAFARI

## Description
This is a cluster analysis of 29 shape feature parameters derived from 1396 binary reference images, processed with the R package `SAFARI`.

## Contents
* `gif/binary/` - Image dataset
* `gif/non_binary/` - Images considered non-binary and not factored into the analysis
* `cluster_analysis.R` - R script that executes the cluster analysis.

## Dependencies
* `BiocManager` - `SAFARI` dependency
* `EBImage` - `SAFARI` dependency
* `remotes` - For installing `SAFARI`
* `SAFARI` - For segmenting shapes within images and extracting shape features
* `tidyverse` - Data manipulation with `dplyr` and beautiful graphics with `ggplot2`
* `factoextra` - Beautiful visualizations of clusters