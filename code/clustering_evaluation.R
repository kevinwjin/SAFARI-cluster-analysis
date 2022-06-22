library(dplyr)
library(ggplot2)
library(mclust)

data(iris)

# Scale data
iris_unscaled <- iris %>% select(-Species)
iris_scaled <- iris_unscaled %>% mutate_all(scale) 

##### K-MEANS #####

# Calculate adjusted Rand index for each clustering method
max_k <- 100
kmeans_mat <- matrix(nrow = max_k, 
                     ncol = nrow(iris), 
                     byrow = TRUE)

# Generate k-means clustering from scaled features
for (k in 1:max_k) {
  kmeans_mat[k, ] <- kmeans(
    x = iris_scaled,
    centers = k
  )[["cluster"]]
}
kmeans_scaled <- as.data.frame(kmeans_mat)

# Generate k-means clustering from unscaled features
for (k in 1:max_k) {
  kmeans_mat[k, ] <- kmeans(
    x = iris_unscaled,
    centers = k
  )[["cluster"]] 
}
kmeans_unscaled <- as.data.frame(kmeans_mat)

# Calculate ARI values for k-means over k = 1:100 (Ground truth: 4 species)
kmeans_scaled_truth <- kmeans(
  x = iris_scaled,
  centers = 4
)[["cluster"]] # Scaled truth
kmeans_unscaled_truth <- kmeans(
  x = iris_unscaled,
  centers = 4
)[["cluster"]] # Unscaled truth

kmeans_ari <- matrix(nrow = 100, ncol = 2, byrow = TRUE)
for (i in 1:max_k) {
  # ARI for k-means clustering of scaled features
  kmeans_ari[i, 1] <- adjustedRandIndex(
    unlist(kmeans_scaled[i, ]),
    kmeans_scaled_truth
  )
  # ARI for k-means clustering of unscaled features
  kmeans_ari[i, 2] <- adjustedRandIndex(
    unlist(kmeans_unscaled[i, ]),
    kmeans_unscaled_truth
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
    xintercept = 4,
    color = "red"
  ) +
  labs(
    title = "k-means clustering accuracy (ground truth: k = 4)",
    x = "k-value",
    y = "Adjusted Rand Index",
    color = "Features"
  ) +
  scale_color_hue(labels = c("Scaled", "Unscaled")) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

##### HIERARCHICAL #####

# Generate hierarchical clusters
hier_scaled_tree <- hclust(dist(iris_scaled, method = "euclidean"),
                           method = "complete" # Complete linkage
)

hier_unscaled_tree <- hclust(dist(iris_unscaled, method = "euclidean"),
                             method = "complete"
)

max_k <- 100
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

# Calculate ARI values for hierarchical over k = 1:100
hier_scaled_truth <- cutree(hier_scaled_tree, k = 4) # Scaled truth
hier_unscaled_truth <- cutree(hier_unscaled_tree, k = 4) # Unscaled truth

hier_ari <- matrix(nrow = 100, ncol = 2, byrow = TRUE)
for (i in 1:max_k) {
  # ARI for hierarchical clustering of scaled features
  hier_ari[i, 1] <- adjustedRandIndex(
    unlist(hier_scaled[i, ]),
    hier_scaled_truth
  )
  # ARI for hierarchical clustering of unscaled features
  hier_ari[i, 2] <- adjustedRandIndex(
    unlist(hier_unscaled[i, ]),
    hier_unscaled_truth
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
    xintercept = 4,
    color = "red"
  ) +
  labs(
    title = "Hierarchical clustering accuracy (ground truth: k = 4)",
    x = "k-value",
    y = "Adjusted Rand Index",
    color = "Features"
  ) +
  scale_color_hue(labels = c("Scaled", "Unscaled")) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1))

##### GAUSSIAN MIXTURE MODEL #####

# Generate Gaussian mixture model clusters
max_k <- 100
gmm_mat <- matrix(nrow = max_k, 
                  ncol = nrow(iris), 
                  byrow = TRUE)

# Generate GMM clustering from scaled features
for (k in 1:max_k) {
  gmm_mat[k, ] <- Mclust(iris_scaled, G = k)$classification
}
gmm_scaled <- as.data.frame(gmm_mat)

# Generate GMM clustering from unscaled features
for (k in 1:max_k) {
  gmm_mat[k, ] <- Mclust(iris_unscaled, G = k)$classification
}
gmm_unscaled <- as.data.frame(gmm_mat)

# Calculate ARI values for GMM over k = 1:100 (Ground truth: k = 4)
gmm_scaled_truth <- Mclust(iris_scaled,
                           G = 4
)$classification # Scaled truth
gmm_unscaled_truth <- Mclust(iris_unscaled,
                             G = 4
)$classification # Unscaled truth

gmm_ari <- matrix(nrow = 100, ncol = 2, byrow = TRUE)
for (i in 1:max_k) {
  # ARI for GMM clustering of scaled features
  gmm_ari[i, 1] <- adjustedRandIndex(
    unlist(gmm_scaled[i, ]),
    gmm_scaled_truth
  )
  # ARI for GMM clustering of unscaled features
  gmm_ari[i, 2] <- adjustedRandIndex(
    unlist(gmm_unscaled[i, ]),
    gmm_unscaled_truth
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
    xintercept = 4,
    color = "red"
  ) +
  labs(
    title = "GMM clustering accuracy (ground truth: k = 4)",
    x = "k-value",
    y = "Adjusted Rand Index",
    color = "Features"
  ) +
  scale_color_hue(labels = c("Scaled", "Unscaled")) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
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
    xintercept = 4,
    color = "red"
  ) +
  labs(
    title = "Performance of Several Clustering Methods",
    x = "Number of clusters (k-value)",
    y = "Adjusted Rand Index"
  ) +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_color_hue(labels = c("k-means", "Hierarchical", "GMM")) +
  scale_linetype(labels = c("Scaled", "Unscaled"))



