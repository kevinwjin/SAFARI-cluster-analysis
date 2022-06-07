## Cluster analysis of 1396 images processed with SAFARI 
#
## To-do:
## 1. For non-binary images, change all non-zero values to 1 even if there are 
## multiple non-zero values (0 is background (black); 1 is shape (white))
##
## 2. Pass invert = TRUE for Glas-12.gif during read.image
##
## 3. For binary.segmentation, use filter = 150 (noise removal of anything 
## smaller than 150 pixels; be careful because largest shape may be smaller
## than 150 for some images - always choose the largest filter per image) and 
## k = 3 (enlarge shape by a factor of 3 to avoid 0 thickness and revisiting 
## the same pixel when tracing boundary)
##
## 4. Remove non-binary images from analysis
##
## 5. True number of clusters can be extracted by substringing the first five 
## letters of each file name (around 70 for 1400). 
##
## 6. Next step: Evaluating the unsupervised learning algorithms. In
## supervised learning, we use a contingency table to evaluate algorithm
## sensitivity. In unsupervised learning, we use the Rand index. Here, we will
## use the Adjusted Rand index to evaluate the k-means clustering, and push
## for a higher ARI than k-means clustering, hierarchical clustering, and 
## Gaussian mixture model clustering.
##
## 7. First, we ensure that the data is clean. Check case by case to make sure 
## that the images are properly loaded, with 0s for background and 1s for the 
## shape, and invert as necessary. See 1 and 2.
##
## 8. Next, find a package to calculate the ARI. Install packages also for 
## hierarchical clustering, and Gaussian mixture model clustering.
##
## 9. Based on shape features, run k-means clustering from 2:100 k-values. To
## conserve computational resources, it is possible to choose a subset of the 
## image set, ensuring you have as different images as possible (~300 images, 
## representing 10 different objects). Then, run hierarchical clustering, and 
## Gaussian mixture model clustering.
## 
## 10. Cake a plot with k-value on the x-axis, and ARI on the y-axis. On this 
## plot, generate curves of scaled and unscaled data (unscaled would probably 
## give low ARI) for each clustering method. This is 6 curves in total on the
## same plot.

# Install dependencies
install.packages("BiocManager") # For using SAFARI
BiocManager::install("EBImage") # For using SAFARI
install.packages("remotes") # For installing SAFARI
install_github("estfernandez/SAFARI") # SAFARI 0.1.1; newer than CRAN
install.packages("tidyverse") # dplyr and ggplot2
install.packages("mclust") # Hierarchical clustering and Gaussian mixture models
install.packages("factoextra") # Visualization of clusters
#install.packages("animation") # Visualization of clusters

# Load packages
library(SAFARI) # Image processing
library(dplyr) # Data handling
library(ggplot2) # Beautiful plots
library(mclust) # Hierarchical clustering and Gaussian mixture models
library(factoextra) # Visualization of clusters

# Load images to be processed
# setwd("C:/Users/kevin/Downloads/project/gif/_batch_test/") # Set working directory (doesn't work in Rmd)
file.list <- dir(pattern = "gif$") # Retrieve list of all images in pwd
#file.list.divided <- split(file.list, ceiling(seq_along(file.list)/50)) # Divide list into increments of 50

# Identify and remove non-binary images from the file list
is.binary <- function(img) 
{
  length(unique(c(img))) <= 2
}
for (file in file.list) {
  if (is.binary(caTools::read.gif(file)$image) == FALSE) {
    print(file)
    file.list <- file.list[names(file.list) != file]
  }
}

# Segment shape and extract 29 features of the segmented shape
extract.parameters <- function(img) {
  this_img <- read.image(img) 
  img_segs <- binary.segmentation(this_img,
                                  id = c("NLST", "AA00474", "11030"),
                                  filter = 150,
                                  k = 3, 
                                  categories = c("geometric", # get all features
                                                 "boundary", 
                                                 "topological"))
  parameters <- data.frame(img_segs[["desc"]][-c(1,1:4)]) # delete first 4 columns
  parameters <- cbind(c(img), parameters) # add first column with file name
  return(parameters)
}

# Extract 29 features from images
start_time <- Sys.time() # Measure execution time
params <- extract.parameters("apple-1.gif") # Read in first image
for(file in file.list) { # Read in the rest of the images
  params <- add_row(params, extract.parameters(file))
}
params <- params[-1, ] # Remove redundant first row (unnecessary?)
params <- data.frame(params, row.names = 1) # Change first column to row names
#params <- sapply(file.list, extract.parameters) # R runs out of RAM quickly
end_time <- Sys.time() # Measure execution time
end_time - start_time # Display execution time

# k-means clustering of 29 parameters
rescale_params <- params %>% mutate_all(scale) # scale all variables to ensure
# weights equal so that it does not influence the clustering result

# Optimizing k (elbow method)
kmean_withinss <- function(k) { 
  cluster <- kmeans(rescale_params[1:40, ], k) # Only try first 40 images
  return(cluster$tot.withinss)
}
max_k <- 20 # Set maximum clusters 
wss <- sapply(2:max_k, kmean_withinss) # Run algorithm over a range of k 
elbow <- data.frame(2:max_k, wss) # Create a data frame to plot the graph

# Plot graph of elbow method over 20 k-values
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  labs(title = "Elbow method (total within-cluster sum of squares)",
       x = "k-value",
       y = "Total within-cluster sum of squares") +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

# # Silhouette method for determining optimal clusters (not working)
# install.packages("cluster")
# library(cluster)
# avg_sil <- function(k) {
#   km.res <- kmeans(rescale_params, centers = k, nstart = 25)
#   ss <- silhouette(km.res$cluster, dist(rescale_params))
#   mean(ss[, 3])
# }
# k.values <- 2:15 # Compute and plot wss for k = 2 to k = 15
# avg_sil_values <- map_dbl(k.values, avg_sil)
# plot(k.values, avg_sil_values,
#      type = "b", pch = 19, frame = FALSE, 
#      xlab = "Number of clusters K",
#      ylab = "Average Silhouettes")
# fviz_nbclust(df, kmeans, method = "silhouette")
# 
# # Gap statistic method for determining optimal clusters (not working)
# library(NbClust)
# gap_stat <- clusGap(rescale_params, FUN = kmeans, nstart = 25,
#                     K.max = 20, B = 50)
# fviz_gap_stat(gap_stat)

# Train the k-means model with optimal k
pc_cluster <- kmeans(rescale_params[1:40,], 7) # True k-value is 2 for 40 images
#kmeans.ani(rescale_params, 4) # Train model with animations

# Visualize clusters
fviz_cluster(pc_cluster, # No labels
             data = rescale_params,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "Cluster analysis of SAFARI output of 1400 images (k = 7)")

fviz_cluster(pc_cluster, # Labels included
             data = params[1:40,],
             main = "Cluster analysis of SAFARI output of 1400 images (k = 2)")