## Cluster analysis of the Iris dataset
## Author: Kevin Jin

## To-do:
## 1. Extract ground truth for Iris and redo all ARI plots.
## 
## 2. The true number of clusters can be extracted by substringing the first
## five letters of each file name.
##
## 3. Clean up code and increase readability and efficiency.

library(dplyr)
library(ggplot2)
library(mclust) # Gaussian mixture models

# Load data
data(iris)

# Scale data
unscaled <- iris %>% select(-Species) # Remove species column
scaled <- unscaled %>% mutate_all(scale) 

##### EXTRACT GROUND TRUTH #####
truth <- as.numeric(iris$Species)  # Ground truth: 4 species

##### K-MEANS #####
max_k <- 20
kmeans_mat <- matrix(nrow = max_k, 
                     ncol = nrow(iris), 
                     byrow = TRUE)

# Generate k-means clustering from scaled features
for (k in 1:max_k) {
  kmeans_mat[k, ] <- kmeans(
    x = scaled,
    centers = k,
    iter.max = 30,
    nstart = 20
  )[["cluster"]]
}
kmeans_scaled <- as.data.frame(kmeans_mat)

# Generate k-means clustering from unscaled features
for (k in 1:max_k) {
  kmeans_mat[k, ] <- kmeans(
    x = unscaled,
    centers = k,
    iter.max = 30,
    nstart = 20
  )[["cluster"]] 
}
kmeans_unscaled <- as.data.frame(kmeans_mat)

# Calculate adjusted Rand index for k-means over 1:20
kmeans_ari <- matrix(nrow = max_k, 
                     ncol = 2, 
                     byrow = TRUE)
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
    color = "darkred"
  )) +
  geom_line(aes(
    y = ARI_scaled,
    color = "darkred"
  )) +
  geom_point(aes(
    y = ARI_unscaled,
    color = "steelblue"
  )) +
  geom_line(aes(
    y = ARI_unscaled,
    color = "steelblue"
  )) +
  geom_vline(
    xintercept = 3,
    color = "red"
  ) +
  labs(
    title = "k-means clustering accuracy (ground truth: k = 3)",
    x = "k-value",
    y = "Adjusted Rand Index",
    color = "Features"
  ) +
  scale_color_hue(labels = c("Scaled", "Unscaled")) +
  scale_x_continuous(breaks = seq(0, max_k, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

##### HIERARCHICAL #####

# Generate hierarchical clusters
hier_scaled_tree <- hclust(dist(scaled, method = "euclidean"),
                           method = "complete" # Complete linkage
)

hier_unscaled_tree <- hclust(dist(unscaled, method = "euclidean"),
                             method = "complete"
)

max_k <- 20
hier_mat <- matrix(nrow = max_k, 
                   ncol = nrow(iris), 
                   byrow = TRUE)

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

# Calculate ARI values for GMM over k = 1:20 (Ground truth: 3 species)
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
    color = "darkred"
  )) +
  geom_line(aes(
    y = ARI_scaled,
    color = "darkred"
  )) +
  geom_point(aes(
    y = ARI_unscaled,
    color = "steelblue"
  )) +
  geom_line(aes(
    y = ARI_unscaled,
    color = "steelblue"
  )) +
  geom_vline(
    xintercept = 3,
    color = "red"
  ) +
  labs(
    title = "Hierarchical clustering accuracy (ground truth: k = 3)",
    x = "k-value",
    y = "Adjusted Rand Index",
    color = "Features"
  ) +
  scale_color_hue(labels = c("Scaled", "Unscaled")) +
  scale_x_continuous(breaks = seq(0, max_k, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

##### GAUSSIAN MIXTURE MODEL #####

# Generate Gaussian mixture model clusters
max_k <- 20
gmm_mat <- matrix(nrow = max_k, 
                  ncol = nrow(iris), 
                  byrow = TRUE)

# Generate GMM clustering from scaled features
for (k in 1:max_k) {
  gmm_mat[k, ] <- Mclust(scaled, G = k)$classification
}
gmm_scaled <- as.data.frame(gmm_mat)

# Generate GMM clustering from unscaled features
for (k in 1:max_k) {
  gmm_mat[k, ] <- Mclust(unscaled, G = k)$classification
}
gmm_unscaled <- as.data.frame(gmm_mat)

# Calculate ARI values for GMM over k = 1:20 (Ground truth: 3 species)
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
    color = "darkred"
  )) +
  geom_line(aes(
    y = ARI_scaled,
    color = "darkred"
  )) +
  geom_point(aes(
    y = ARI_unscaled,
    color = "steelblue"
  )) +
  geom_line(aes(
    y = ARI_unscaled,
    color = "steelblue"
  )) +
  geom_vline(
    xintercept = 3,
    color = "red"
  ) +
  labs(
    title = "GMM clustering accuracy (ground truth: k = 3)",
    x = "k-value",
    y = "Adjusted Rand Index",
    color = "Features"
  ) +
  scale_color_hue(labels = c("Scaled", "Unscaled")) +
  scale_x_continuous(breaks = seq(0, max_k, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

##### ALL METHODS #####

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
  geom_line(aes(
    y = kmeans_ari_scaled,
    color = "darkred",
    linetype = "solid"
  )) +
  geom_line(aes(
    y = kmeans_ari_unscaled,
    color = "darkred",
    linetype = "twodash"
  )) +
  geom_line(aes(
    y = hier_ari_scaled,
    color = "steelblue",
    linetype = "solid"
  )) +
  geom_line(aes(
    y = hier_ari_unscaled,
    color = "steelblue",
    linetype = "twodash"
  )) +
  geom_line(aes(
    y = gmm_ari_scaled,
    color = "seagreen",
    linetype = "solid"
  )) +
  geom_line(aes(
    y = gmm_ari_unscaled,
    color = "seagreen",
    linetype = "twodash"
  )) +
  geom_vline(
    xintercept = 3,
    color = "red"
  ) +
  labs(
    title = "Performance of Several Clustering Methods (Ground truth: k = 3)",
    x = "Number of clusters (k-value)",
    y = "Adjusted Rand Index"
  ) +
  scale_x_continuous(breaks = seq(0, max_k, by = 2)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_hue(labels = c("k-means", "GMM", "Hierarchical")) +
  scale_linetype(labels = c("Scaled", "Unscaled"))
