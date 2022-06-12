## Title: Cluster analysis of 1400 images processed with SAFARI 
## Author: Kevin Jin
##
## To-do:
## 1. For non-binary images, change all non-zero values to 1 even if there are 
## multiple non-zero values (0 is background (black); 1 is shape (white)). This 
## should bring the number of possible images back to 1400.
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
## 10. Create a plot with k-value on the x-axis, and ARI on the y-axis. On this 
## plot, generate curves of scaled and unscaled data (unscaled would probably 
## give low ARI) for each clustering method. This is 6 curves in total on the
## same plot.

##### PREPARATION #####

# Install dependencies
install.packages("BiocManager") # SAFARI dependency
BiocManager::install("EBImage") # SAFARI dependency
install.packages("remotes") # For installing SAFARI
install_github("estfernandez/SAFARI") # SAFARI 0.1.1; newer than CRAN
install.packages("tidyverse") # dplyr and ggplot2
install.packages("mclust") # Hierarchical clustering and Gaussian mixture models
install.packages("factoextra") # Cluster analysis visualization

# Load packages
library(SAFARI) # Image processing
library(dplyr) # Data handling
library(ggplot2) # Beautiful plots
library(mclust) # Hierarchical clustering and Gaussian mixture models
library(factoextra) # Visualization of clusters

##### LOAD IMAGES #####

# List images to be processed (set working directory beforehand)
file.list <- dir(pattern = "gif$") # Retrieve list of all images

is_binary <- function(img) 
{
  length(unique(c(img))) <= 2
}

for (file in file_list) { # Remove non-binary images from list
  if (is_binary(caTools::read.gif(file)$image) == FALSE) {
    print(file)
    file_list <- file.list[names(file_list) != file]
  }
}

##### PROCESS IMAGES AND EXTRACT FEATURES #####

# Segment shape and extract 29 features of segmented shape
extract_features <- function(img) {
  this_img <- read.image(img) 
  img_segs <- binary.segmentation(this_img,
                                  id = c("NLST", "AA00474", "11030"),
                                  filter = 150,
                                  k = 3, 
                                  categories = c("geometric", # get all features
                                                 "boundary", 
                                                 "topological"))
  features <- data.frame(img_segs[["desc"]][-c(1,1:4)]) # delete first 4 columns
  features <- cbind(c(img), features) # add first column with file name
  return(features)
}

# Extract 29 features from images
features <- extract.features("apple-1.gif") # Read in first image
for(file in file_list) { # Read in the rest of the images
  features <- add_row(features, extract.features(file))
}
features <- features[-1, ] # Remove redundant first row (unnecessary?)
features <- na.omit(features) # Remove any missing observations
features <- data.frame(features, row.names = 1) # Change first column to row names

##### CLUSTER ANALYSIS #####

# k-means clustering
rescaled_features <- features %>% mutate_all(scale) # Standardize all variables

# Optimizing k (elbow method)
kmean_withinss <- function(k) { 
  cluster <- kmeans(rescaled_features[1:40, ], k) # Only try first 40 images
  return(cluster$tot.withinss)
}

max_k <- 20 # Set maximum clusters 
wss <- sapply(2:max_k, kmean_withinss) # Run k-means over all k-values
elbow <- data.frame(2:max_k, wss) # Create a data frame to plot elbow graph

# Plot elbow method over 20 k-values
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  labs(title = "Elbow method (total within-cluster sum of squares)",
       x = "k-value",
       y = "Total within-cluster sum of squares") +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 20, by = 1))

# Train the k-means model with optimal k
pc_cluster <- kmeans(rescaled_features[1:40,], 7) # True k is 2 for 40 images

# Visualize clusters
fviz_cluster(pc_cluster, # No labels
             data = rescaled_features,
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw(),
             main = "Cluster analysis of SAFARI output of 1400 images (k = 7)")

fviz_cluster(pc_cluster, # Labels included
             data = features[1:40,],
             main = "Cluster analysis of SAFARI output of 1400 images (k = 7)")

# Hierarchical clustering

# Gaussian mixture model clustering

##### EVALUATE CLUSTERING ACCURACY #####

# Calculate adjusted Rand index for each clustering method

# Plot ARI and k-values for each clustering method
