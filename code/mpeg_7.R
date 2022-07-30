## Cluster analysis of SAFARI-extracted shape features from MPEG-7
## Author: Kevin Jin

##### TO-DO: #####
## 1. First, ensure that the data is clean. Check case by case to make sure
## that the images are properly loaded, with 0s for background and 1s for the
## shape, and invert as necessary. See 1 and 2.
##
## 2. In the SAFARI fork, set objects for 0 = background and 1 = foreground for
## each image.
##
## 3. Clean up code and increase readability and efficiency.

##### PREPARATION #####

# Install dependencies
install.packages("BiocManager") # SAFARI dependency
BiocManager::install("EBImage") # SAFARI dependency
install.packages("remotes") # Installing SAFARI
remotes::install_github("kevinwjin/
                        SAFARI") # Forked from estfernandez with read.image fix
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
library(stringr) # String extraction

##### LOAD IMAGES #####

# Retrieve list of all images (Set directory to image folder
file_list <- dir(pattern = "gif$") # Choose appropriate image extension

# Extract ground truth
truth <- NULL
for (file in file_list) {
  if (str_detect(file, "(?:device)")) { # Device class is a special case
    truth <- c(truth, str_extract(file, "(?:device\\d)"))
  } else {
    truth <- c(truth, str_extract(file, "^[A-z]+")) # Extract one part of file name
  }
}
truth <- truth %>%
  as.factor() %>%
  as.numeric()

##### PROCESS IMAGES AND EXTRACT FEATURES #####

# Segment shape from image and extract 29 features
extract_features <- function(img) {
  if (img == "Glas-12.gif") { # Special case of inverted image in MPEG-7
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
    categories = c(
      "geometric", # Extract all features
      "boundary",
      "topological"
    )
  )
  features <- data.frame(img_segs
  [["desc"]]
  [-c(1, 1:4)]) # Delete irrelevant first 4 columns
  features <- cbind(
    c(img), # Add first column with file name
    features
  )
  return(features)
}

# Process all images in parallel
cl <- makeCluster(detectCores()) # Allocate number of cores as detected
clusterExport(cl, # Pass function dependencies to each core
  varlist = c(
    "read.image",
    "binary.segmentation"
  )
)
features <- parSapply(cl, # Parallel sapply
  file_list,
  FUN = extract_features
)
stopCluster(cl) # De-allocate cores and expunge R sessions from memory

# Clean up data frame
features <- data.frame(t(features), # Transpose result
  row.names = 1 # Delete redundant first column
)
features <- data.frame(lapply(features, unlist)) # Flatten column-lists
features_scaled <- features %>% mutate_all(scale) # Standardize all variables

##### CLUSTER ANALYSIS #####

# k-means clustering
features_scaled <- features %>% mutate_all(scale) # Standardize all variables
# Elbow method to find optimal k-value
kmean_withinss <- function(k) { # k-means within-cluster sum of squares
  cluster <- kmeans(features_scaled, k)
  return(cluster$tot.withinss)
}
max_k <- 100 # Set maximum clusters
wss <- sapply(2:max_k, kmean_withinss) # Calculate WSS for all k-values
elbow <- data.frame(2:max_k, wss) # Create a data frame to plot elbow graph

# Visualize elbow method over k-values 1:100
ggplot(elbow, aes(x = X2.max_k, y = wss)) +
  labs(
    title = "Elbow method (total within-cluster sum of squares)",
    x = "k-value",
    y = "Total within-cluster sum of squares"
  ) +
  geom_point() +
  geom_line() +
  scale_x_continuous(breaks = seq(1, 100, by = 10))

# Compute k-means clusters with optimal k (True k = 70 for 1400 images)
kmeans_scaled_truth <- kmeans(features_scaled, 70) # Cluster scaled features
kmeans_unscaled_truth <- kmeans(features, 70) # Cluster unscaled features

# Visualize k-means clusters (ggplot2 supports a maximum of 25 levels)
fviz_cluster(kmeans_scaled_truth, # No labels
  data = features_scaled,
  geom = "point",
  ellipse.type = "convex",
  ggtheme = theme_bw(),
  main = "k-means clustering of shape features from 1400 images (k = 70)"
)

fviz_cluster(kmeans_cluster, # Labels included
  data = features_scaled,
  main = "k-means clustering of shape features from 1400 images (k = 70)"
)

# Hierarchical clustering
hier_scaled <- hclust(dist(features_scaled, # Generate dissimilarity matrix
  method = "euclidean"
),
method = "ward.D2"
) # Complete linkage hierarchical clustering

hier_unscaled <- hclust(dist(features,
  method = "euclidean"
),
method = "ward.D2"
)

plot(hier_scaled, cex = 0.1, hang = -0.01) # Plot dendrogram
fit <- cutree(hier_scaled, k = 70)
table(fit)

plot(hier_scaled, cex = 0.1, hang = -0.01)
rect.hclust(hier_scaled, k = 70, border = "red")

hc2 <- agnes(features_scaled, method = "complete") # Agnes function
hc2$ac # Agglomerative coefficient

# Gaussian mixture model clustering using BIC
gmm_scaled <- Mclust(features_scaled, G = 70) # number of clusters

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
max_k <- 100
kmeans_mat <- matrix(nrow = max_k, ncol = length(file_list), byrow = TRUE)

# Generate k-means clustering from scaled features
for (k in 1:max_k) {
  kmeans_mat[k, ] <- kmeans(
    x = features_scaled,
    centers = k
    # nstart = 20,
    # iter.max = 30
  )[["cluster"]]
}
kmeans_scaled <- as.data.frame(kmeans_mat)

# Generate k-means clustering from unscaled features
for (k in 1:max_k) {
  kmeans_mat[k, ] <- kmeans(
    x = features,
    centers = k
    # nstart = 20,
    # iter.max = 30
  )[["cluster"]] # Select cluster results
}
kmeans_unscaled <- as.data.frame(kmeans_mat)

# Calculate ARI values for k-means over k = 1:100 (Ground truth: k = 70)
kmeans_ari <- matrix(nrow = max_k, ncol = 2, byrow = TRUE)
for (i in 1:max_k) {
  # ARI for k-means clustering of scaled features
  kmeans_ari[i, 1] <- adjustedRandIndex(
    unlist(kmeans_scaled[i, ]),
    truth
  )
  # ARI for k-means clustering of unscaled features
  kmeans_ari[i, 2] <- adjustedRandIndex(
    unlist(kmeans_unscaled[i, ]),
    truth
  )
}
kmeans_ari <- as.data.frame(kmeans_ari)
names(kmeans_ari) <- c("ARI_scaled", "ARI_unscaled")
k_values <- 1:max_k
kmeans_ari <- mutate(kmeans_ari, k_values = as.numeric(row.names(kmeans_ari)))

# Visualize k-means clustering accuracy
ggplot(kmeans_ari, aes(x = k_values)) +
  geom_point(aes(
    y = ARI_scaled,
    color = 3
  )) +
  geom_smooth(aes(
    y = ARI_scaled,
    color = 3
  )) +
  # geom_point(aes(
  #   y = ARI_unscaled,
  #   color = "steelblue"
  # )) +
  # geom_line(aes(
  #   y = ARI_unscaled,
  #   color = "steelblue"
  # )) +
  geom_vline(
    xintercept = 70, # Ground truth
    color = "red"
  ) +
  labs(
    title = "k-means Clustering Accuracy (Data: Normalized MPEG-7 Shape Features)",
    x = "k-value",
    y = "Adjusted Rand Index"
    # color = "Features"
  ) +
  theme(legend.position = "none")
# scale_color_hue(labels = c("Scaled", "Unscaled")) +
# scale_x_continuous(breaks = seq(0, max_k, by = 10)) +
# scale_y_continuous(breaks = seq(0, 1, by = 0.1))

# Generate hierarchical clusters
hier_scaled_tree <- hclust(dist(features_scaled, method = "euclidean"),
  method = "complete" # Complete linkage
)
hier_unscaled_tree <- hclust(dist(features, method = "euclidean"),
  method = "complete"
)

max_k <- 100
hier_mat <- matrix(nrow = max_k, ncol = length(file_list), byrow = TRUE)

# Generate hierarchical clustering from scaled features
for (k in 1:max_k) {
  hier_mat[k, ] <- cutree(hier_scaled_tree, k = k)
}
hier_scaled <- as.data.frame(hier_mat)

# Generate hierarchical clustering from unscaled features
for (k in 1:max_k) {
  hier_mat[k, ] <- cutree(hier_unscaled_tree, k = k)
}
hier_unscaled <- as.data.frame(hier_mat)

# Calculate ARI values for hierarchical over k = 1:100 (Ground truth: k = 70)
hier_ari <- matrix(nrow = max_k, ncol = 2, byrow = TRUE)
for (i in 1:max_k) {
  # ARI for hierarchical clustering of scaled features
  hier_ari[i, 1] <- adjustedRandIndex(
    unlist(hier_scaled[i, ]),
    truth
  )
  # ARI for hierarchical clustering of unscaled features
  hier_ari[i, 2] <- adjustedRandIndex(
    unlist(hier_unscaled[i, ]),
    truth
  )
}
hier_ari <- as.data.frame(hier_ari)
names(hier_ari) <- c("ARI_scaled", "ARI_unscaled")
k_values <- 1:max_k
hier_ari <- mutate(hier_ari, k_values = as.numeric(row.names(hier_ari)))

# Visualize hierarchical clustering accuracy
ggplot(hier_ari, aes(x = k_values)) +
  geom_point(aes(
    y = ARI_scaled,
    color = 3
  )) +
  geom_smooth(aes(
    y = ARI_scaled,
    color = 3
  )) +
  # geom_point(aes(
  #   y = ARI_unscaled,
  #   color = "steelblue"
  # )) +
  # geom_line(aes(
  #   y = ARI_unscaled,
  #   color = "steelblue"
  # )) +
  geom_vline(
    xintercept = 70, # Ground truth
    color = "red"
  ) +
  labs(
    title = "Hierachical Clustering Accuracy (Data: Normalized MPEG-7 Shape Features)",
    x = "k-value",
    y = "Adjusted Rand Index",
    # color = "Features"
  ) +
  theme(legend.position = "none")
# scale_color_hue(labels = c("Scaled", "Unscaled")) +
# scale_x_continuous(breaks = seq(0, max_k, by = 10)) +
# scale_y_continuous(breaks = seq(0, 1, by = 0.1))

# Generate Gaussian mixture model clusters
max_k <- 100
gmm_mat <- matrix(nrow = max_k, ncol = length(file_list), byrow = TRUE)

# Generate GMM clustering from scaled features
for (k in 1:max_k) {
  gmm_mat[k, ] <- Mclust(features_scaled, G = k)$classification
}
gmm_scaled <- as.data.frame(gmm_mat)

# Generate GMM clustering from unscaled features
for (k in 1:max_k) {
  gmm_mat[k, ] <- Mclust(features, G = k)$classification
}
gmm_unscaled <- as.data.frame(gmm_mat)

# Calculate ARI values for GMM over k = 1:100 (Ground truth: k = 70)
gmm_ari <- matrix(nrow = max_k, ncol = 2, byrow = TRUE)
for (i in 1:max_k) {
  # ARI for GMM clustering of scaled features
  gmm_ari[i, 1] <- adjustedRandIndex(
    unlist(gmm_scaled[i, ]),
    truth
  )
  # ARI for GMM clustering of unscaled features
  gmm_ari[i, 2] <- adjustedRandIndex(
    unlist(gmm_unscaled[i, ]),
    truth
  )
}
gmm_ari <- as.data.frame(gmm_ari)
names(gmm_ari) <- c("ARI_scaled", "ARI_unscaled")
k_values <- 1:max_k
gmm_ari <- mutate(gmm_ari, k_values = as.numeric(row.names(gmm_ari)))

# Visualize GMM clustering accuracy
ggplot(gmm_ari, aes(x = k_values)) +
  geom_point(aes(
    y = ARI_scaled,
    color = 3
  )) +
  geom_smooth(aes(
    y = ARI_scaled,
    color = 3
  )) +
  # geom_point(aes(
  #   y = ARI_unscaled,
  #   color = "steelblue"
  # )) +
  # geom_line(aes(
  #   y = ARI_unscaled,
  #   color = "steelblue"
  # )) +
  geom_vline(
    xintercept = 70, # Ground truth
    color = "red"
  ) +
  labs(
    title = "GMM Clustering Accuracy (Data: Normalized MPEG-7 Shape Features)",
    x = "k-value",
    y = "Adjusted Rand Index",
    # color = "Features"
  ) +
  theme(legend.position = "none")
# scale_color_hue(labels = c("Scaled", "Unscaled")) +
# scale_x_continuous(breaks = seq(0, max_k, by = 10)) +
# scale_y_continuous(breaks = seq(0, 1, by = 0.1))

# Visualize accuracy of all clustering methods
accuracy <- right_join(kmeans_ari, gmm_ari, by = "k_values")
accuracy <- right_join(accuracy, hier_ari, by = "k_values")

names(accuracy) <- c(
  "kmeans_ari_scaled",
  "kmeans_ari_unscaled",
  "k_values",
  "gmm_ari_scaled",
  "gmm_ari_unscaled",
  "hier_ari_scaled",
  "hier_ari_unscaled"
)

ggplot(accuracy, aes(x = k_values)) +
  geom_smooth(aes(
    y = kmeans_ari_scaled,
    color = "darkred"
    # linetype = "solid"
  )) +
  geom_point(aes(
    y = kmeans_ari_scaled,
    color = "darkred"
  )) +
  # geom_line(aes(
  #   y = kmeans_ari_unscaled,
  #   color = "darkred",
  #   linetype = "twodash"
  # )) +
  geom_smooth(aes(
    y = hier_ari_scaled,
    color = "steelblue"
    # linetype = "solid"
  )) +
  geom_point(aes(
    y = hier_ari_scaled,
    color = "steelblue"
  )) +
  # geom_line(aes(
  #   y = hier_ari_unscaled,
  #   color = "steelblue",
  #   linetype = "twodash"
  # )) +
  geom_smooth(aes(
    y = gmm_ari_scaled,
    color = "seagreen"
    # linetype = "solid"
  )) +
  geom_point(aes(
    y = gmm_ari_scaled,
    color = "seagreen"
  )) +
  # geom_line(aes(
  #   y = gmm_ari_unscaled,
  #   color = "seagreen",
  #   linetype = "twodash"
  # )) +
  geom_vline(
    xintercept = 70,
    color = "red"
  ) +
  labs(
    title = "Accuracy of Several Clustering Methods (Data: Normalized MPEG-7 Shape Features)",
    x = "k-value",
    y = "Adjusted Rand Index"
  ) +
  # scale_x_continuous(breaks = seq(0, max_k, by = 10)) +
  # scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_hue(labels = c("k-means", "GMM", "Hierarchical"))
# scale_linetype(labels = c("Scaled", "Unscaled"))
