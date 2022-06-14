## Cluster analysis of shape features of 1400 images extracted with SAFARI
## Author: Kevin Jin
##
##### TO-DO: ##### 
##
## 1. First, ensure that the data is clean. Check case by case to make sure
## that the images are properly loaded, with 0s for background and 1s for the
## shape, and invert as necessary. See 1 and 2.
##
## 2. Next. evaluate three different clustering algorithms (k-means clustering,
## hierarchical clustering, and Gaussian mixture model clustering). In
## supervised learning, we use a contingency table to evaluate algorithm
## sensitivity. In unsupervised learning, we use the Rand index. Here, we will
## use the adjusted Rand index to evaluate the three clustering methods, and
## eventually push for a higher ARI than all three.
##
## 3. The true number of clusters can be extracted by substringing the first
## five letters of each file name (~70 clusters for 1400 images with 20 images
## in each cluster).
##
## 4. Based on shape features, run k-means for k-values from 2-100. To
## conserve computational resources, you may choose a subset of the
## image set, ensuring you have as different images as possible (e.g. ~300
## images, representing 10 different objects). Then, run hierarchical
## and Gaussian mixture model clustering.
##
## 5. Create a plot with k-value on the x-axis and ARI on the y-axis. On this 
## plot, generate curves of scaled and unscaled data (unscaled would probably 
## give low ARI) for each clustering method. This makes 6 curves in total on the
## same plot.
##
## 6. Clean up code and increase readability and efficiency.

##### PREPARATION #####

# Install dependencies
install.packages("BiocManager") # SAFARI dependency
BiocManager::install("EBImage") # SAFARI dependency
install.packages("remotes") # Installing SAFARI
remotes::install_github("kevinwjin/SAFARI") # Forked from estfernandez with read.image fix
install.packages("tidyverse") # dplyr and ggplot2
install.packages("mclust") # Gaussian mixture models and adjusted Rand Index
install.packages("factoextra") # Cluster analysis visualization
install.packages("parallel") # Parallel computations in R

# Include libraries
library(SAFARI) # Image processing
library(dplyr) # Data handling
library(ggplot2) # Beautiful plots
library(mclust) 
library(factoextra) 
library(parallel) 

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

# Process all images in parallel
cl <- makeCluster(8) # Allocate 8 cores; modify as appropriate for system
clusterExport(cl, varlist = c("read.image", "binary.segmentation")) # Pass function dependencies to each core
features <- parSapply(cl, file_list, FUN = extract_features) # Parallelized version of sapply
stopCluster(cl) # De-allocate cores and expunge R sessions from memory

# Clean up data frame
features <- data.frame(t(features), row.names = 1) # Transpose result and delete redundant first column
features <- data.frame(lapply(features, unlist)) # Flatten column-lists to normal

##### CLUSTER ANALYSIS #####

# k-means clustering
rescaled_features <- features %>% mutate_all(scale) # Standardize all variables

# Elbow method to find optimal k-value
kmean_withinss <- function(k) {
  cluster <- kmeans(rescaled_features, k)
  return(cluster$tot.withinss)
}
max_k <- 100 # Set maximum clusters
wss <- sapply(2:max_k, kmean_withinss) # Run k-means over all k-values
elbow <- data.frame(2:max_k, wss) # Create a data frame to plot elbow graph

# Visualize elbow method over k-values 1:100 
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  labs(title = "Elbow method (total within-cluster sum of squares)",
       x = "k-value",
       y = "Total within-cluster sum of squares") +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 100, by = 10))

# Compute k-means clusters with optimal k (True k = 70 for 1400 images)
kmeans_scaled_truth <- kmeans(rescaled_features, 70) # Cluster scaled features
kmeans_unscaled_truth <- kmeans(features, 70) # Cluster unscaled features
kmeans_scaled <- kmeans(rescaled_features, 20)
kmeans_scaled_truth <- kmeans(rescaled_features, 20)

# Visualize k-means clusters (ggplot2 supports a maximum of 25 levels) 
fviz_cluster(kmeans_cluster, # No labels
             data = rescaled_features,
             geom = "point",
             ellipse.type = "convex",
             ggtheme = theme_bw(),
             main = "k-means clustering of shape features from 1400 images (k = 7)")

fviz_cluster(kmeans_cluster, # Labels included
             data = rescaled_features,
             main = "k-means clustering of shape features from 1400 images (k = 7)")

# Hierarchical clustering
d <- dist(rescaled_features, method = "euclidean") # Dissimilarity matrix
hc1 <- hclust(d, method = "complete") # Complete linkage hierarchical clustering
plot(hcl, cex = 0.6, hang = -1) # Plot dendrogram

hc2 <- agnes(rescaled_features, method = "complete") # Agnes function
hc2$ac # Agglomerative coefficient

# Gaussian mixture model clustering using BIC
gmm_scaled <- Mclust(rescaled_features, G = 70) # number of clusters

# Optimal selected model
gmm_scaled$modelName

# Optimal number of clusters
gmm_scaled$G

# Probability for an observation to be in a given cluster
head(gmm_scaled$z)

# Classification matrix
head(gmm_scaled$classification)

# Probabilities, means, and variances
summary(gmm_scaled, parameters = TRUE)

# Visualize GMM clusters
plot(gmm_scaled, what = c("classification"))

##### EVALUATE CLUSTERING ACCURACY #####

# Calculate adjusted Rand index for each clustering method 
max_k <- 100
temp <- kmeans(x = rescaled_features, centers = 2)[["cluster"]] # Get column names
kmeans_mat <- matrix(nrow = 100, ncol = 1400, byrow = TRUE)

# Generate k-means clustering from scaled features
for (k in 1:max_k) {
  kmeans_mat[k, ] <- kmeans(x = rescaled_features, centers = k)[["cluster"]]
}
kmeans_scaled <- as.data.frame(kmeans_mat)
names(kmeans_scaled) <- names(temp)

# Generate k-means clustering from unscaled features
for (k in 1:max_k) {
  kmeans_mat[k, ] <- kmeans(x = features, centers = k)[["cluster"]] # Select clustering results
}
kmeans_unscaled <- as.data.frame(kmeans_mat)
names(kmeans_unscaled) <- names(temp)

# Calculate ARI values for k-means clustering over k = 1:100 (Ground truth: k = 70)
kmeans_scaled_truth <- kmeans(rescaled_features, 70)[["cluster"]] # Scaled truth
kmeans_unscaled_truth <- kmeans(features, 70)[["cluster"]] # Unscaled truth

kmeans_ARI <- matrix(nrow = 100, ncol = 2, byrow = TRUE)
for (i in 1:max_k) {
  # ARI for k-means clustering of scaled features
  kmeans_ARI[i, 1] <- adjustedRandIndex(unlist(kmeans_scaled[i, ]), 
                                        kmeans_scaled_truth)
  # ARI for k-means clustering of unscaled features
  kmeans_ARI[i, 2] <- adjustedRandIndex(unlist(kmeans_unscaled[i, ]), 
                                        kmeans_unscaled_truth)
}
kmeans_ARI <- as.data.frame(kmeans_ARI)
names(kmeans_ARI) <- c("ARI_scaled", "ARI_unscaled")
k_values <- 1:max_k
kmeans_ARI <- mutate(kmeans_ARI, k_values = as.numeric(row.names(kmeans_ARI)))

# Visualize k-means clustering accuracy
ggplot(kmeans_ARI, aes(x = k_values)) + 
  geom_point(aes(y = ARI_scaled, 
                 color = "darkred")) + 
  geom_line(aes(y = ARI_scaled, 
                color = "darkred")) + 
  geom_point(aes(y = ARI_unscaled, 
                 color = "steelblue")) + 
  geom_line(aes(y = ARI_unscaled, 
                color = "steelblue")) + 
  geom_vline(xintercept = 70, 
             color = "red") +
  labs(title = "k-means clustering accuracy (ground truth: k = 70)",
       x = "k-value",
       y = "Adjusted Rand Index",
       color = "Features") +
  scale_color_hue(labels = c("Scaled", "Unscaled")) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

# Generate hierarchical clusters
hier_scaled <- dist(rescaled_features, method = "euclidean") # Dissimilarity matrix
hc1 <- hclust(d, method = "complete") # Complete linkage hierarchical clustering
plot(hcl, cex = 0.6, hang = -1) # Plot dendrogram

# Generate Gaussian mixture model clusters
max_k <- 100
temp <- Mclust(rescaled_features, G = 1) # Get column names
gmm_mat <- matrix(nrow = 100, ncol = 1400, byrow = TRUE)

# Generate GMM clustering from scaled features
for (k in 1:max_k) {
  gmm_mat[k, ] <- Mclust(rescaled_features, G = k)$classification
}
gmm_scaled <- as.data.frame(gmm_mat)
names(gmm_scaled) <- row.names(rescaled_features)

# Generate GMM clustering from unscaled features
for (k in 1:max_k) {
  gmm_mat[k, ] <- Mclust(features, G = k)$classification
}
gmm_unscaled <- as.data.frame(gmm_mat)
names(gmm_unscaled) <- row.names(features)

# Calculate ARI values for GMM clustering over k = 1:100 (Ground truth: k = 70)
gmm_scaled_truth <- Mclust(rescaled_features, G = 70)$classification # Scaled truth
gmm_unscaled_truth <- Mclust(features, G = 70)$classification # Unscaled truth

gmm_ARI <- matrix(nrow = 100, ncol = 2, byrow = TRUE)
for (i in 1:max_k) {
  # ARI for GMM clustering of scaled features
  gmm_ARI[i, 1] <- adjustedRandIndex(unlist(gmm_scaled[i, ]), 
                                     gmm_scaled_truth)
  # ARI for GMM clustering of unscaled features
  gmm_ARI[i, 2] <- adjustedRandIndex(unlist(gmm_unscaled[i, ]), 
                                     gmm_unscaled_truth)
}
gmm_ARI <- as.data.frame(gmm_ARI)
names(gmm_ARI) <- c("ARI_scaled", "ARI_unscaled")
k_values <- 1:max_k
gmm_ARI <- mutate(gmm_ARI, k_values = as.numeric(row.names(gmm_ARI)))

# Visualize GMM clustering accuracy
ggplot(gmm_ARI, aes(x = k_values)) + 
  geom_point(aes(y = ARI_scaled, 
                 color = "darkred")) + 
  geom_line(aes(y = ARI_scaled, 
                color = "darkred")) + 
  geom_point(aes(y = ARI_unscaled, 
                 color = "steelblue")) + 
  geom_line(aes(y = ARI_unscaled, 
                color = "steelblue")) + 
  geom_vline(xintercept = 70, 
             color = "red") +
  labs(title = "GMM clustering accuracy (ground truth: k = 70)",
       x = "k-value",
       y = "Adjusted Rand Index",
       color = "Features") +
  scale_color_hue(labels = c("Scaled", "Unscaled")) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

# Visualize accuracy of all clustering methods
accuracy <- right_join(kmeans_ARI, gmm_ARI, by = "k_values")

names(accuracy) <- c("kmeans_ARI_scaled", 
                     "kmeans_ARI_unscaled", 
                     "k_values",
                     "gmm_ARI_scaled",
                     "gmm_ARI_unscaled")

ggplot(accuracy, aes(x = k_values)) +
  #geom_point(aes(y = kmeans_ARI_scaled,
  #              color = "darkred")) +
  geom_line(aes(y = kmeans_ARI_scaled,
                color = "darkred",
                linetype = "solid")) +
  #geom_point(aes(y = kmeans_ARI_unscaled,
  #              color = "darkred")) +
  geom_line(aes(y = kmeans_ARI_unscaled,
                color = "darkred",
                linetype = "twodash")) +
  #geom_line(aes(y = ARI_hierarchical_scaled), 
  #          color = "steelblue") +
  #geom_line(aes(y = ARI_hierarchical_unscaled), 
  #          color = "steelblue", 
  #          linetype = "twodash") +
  #geom_point(aes(y = gmm_ARI_scaled,
  #                color = "seagreen")) +
  geom_line(aes(y = gmm_ARI_scaled,
                color = "seagreen",
                linetype = "solid")) +
  #geom_point(aes(y = gmm_ARI_unscaled,
  #                color = "seagreen")) +
  geom_line(aes(y = gmm_ARI_unscaled,
                color = "seagreen", 
                linetype = "twodash")) +
  geom_vline(xintercept = 70, 
             color = "red") +
  labs(title = "Performance of Several Clustering Methods",
       x = "Number of clusters (k-value)",
       y = "Adjusted Rand Index") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) + 
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_hue(labels = c("k-means", "GMM")) +
  scale_linetype(labels = c("Scaled", "Unscaled"))