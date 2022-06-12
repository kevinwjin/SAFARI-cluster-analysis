## Cluster analysis of shapes extracted from 1400 images processed with SAFARI 
## Author: Kevin Jin
##
## To-do:
##
## 1. First, ensure that the data is clean. Check case by case to make sure 
## that the images are properly loaded, with 0s for background and 1s for the 
## shape, and invert as necessary. See 1 and 2.
##
## 2. For non-binary images, change all non-zero values to 1 even if there are 
## multiple non-zero values (0 is background (black); 1 is shape (white)). This 
## should bring the number of possible images back to 1400.
##
## 3. Next. evaluate three different clustering algorithms (k-means clustering, 
## hierarchical clustering, and Gaussian mixture model clustering). In
## supervised learning, we use a contingency table to evaluate algorithm
## sensitivity. In unsupervised learning, we use the Rand index. Here, we will
## use the adjusted Rand index to evaluate the three clustering methods, and 
## eventually push for a higher ARI than all three.
##
## 4. The true number of clusters can be extracted by substringing the first 
## five letters of each file name (~70 clusters for 1400 images with 20 images
## in each cluster). 
##
## 5. Find a package to calculate the ARI. Install packages also for 
## hierarchical clustering, and Gaussian mixture model clustering.
##
## 6. Based on shape features, run k-means for k-values from 2-100. To
## conserve computational resources, you may choose a subset of the 
## image set, ensuring you have as different images as possible (e.g. ~300 
## images, representing 10 different objects). Then, run hierarchical
## and Gaussian mixture model clustering.
## 
## 7. Create a plot with k-value on the x-axis and ARI on the y-axis. On this 
## plot, generate curves of scaled and unscaled data (unscaled would probably 
## give low ARI) for each clustering method. This makes 6 curves in total on the
## same plot.

##### PREPARATION #####

# Install dependencies
install.packages("BiocManager") # SAFARI dependency
BiocManager::install("EBImage") # SAFARI dependency
install.packages("remotes") # For installing SAFARI
remotes::install_github("kevinwjin/SAFARI") # Forked from estfernandez with read.image fix
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

# Retrieve list of all images (set working directory to image folder)
file_list <- dir(pattern = "gif$") 

##### PROCESS IMAGES AND EXTRACT FEATURES #####

# Segment shape from image and extract 29 features
extract_features <- function(img) {
  if (img == "Glas-12.gif") { # Special cases of inverted images
    this_img <- read.image(img, invert = TRUE)
  } else {
    this_img <- read.image(img)
  }
    img_segs <- binary.segmentation(this_img,
                                    id = c("NLST", "AA00474", "11030"), # Placeholder IDs
                                    filter = 150, # Noise removal of anything smaller than 150 pixels;
                                                  # Be careful because largest shape may be smaller
                                                  # than 150 for some images;
                                                  # Always choose the largest filter per image
                                    k = 3, # Enlarge shape by a factor of 3 to avoid 0 thickness and revisiting 
                                           # the same pixel when tracing boundary
                                    categories = c("geometric", # Extract all features
                                                   "boundary", 
                                                   "topological"))
    features <- data.frame(img_segs[["desc"]][-c(1,1:4)]) # Delete irrelevant first 4 columns
    features <- cbind(c(img), features) # Add first column with file name
  
  return(features)
}

# Process all images
features <- extract_features("apple-1.gif") # Read in first image
for(file in file_list) { # Read in the rest of the images
  features <- add_row(features, extract_features(file))
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

# Calculate adjusted Rand index for each clustering method (compare with true k = 20)

# Plot ARI and k-values for each clustering method
