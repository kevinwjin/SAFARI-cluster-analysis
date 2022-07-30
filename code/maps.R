# Load libraries
library(ggplot2)

# load map
load("/Users/Qiwei_Li/Dropbox/data/shape/maps/maps.Rdata")
name <- "europe"
map <- europe_map
map <- subset(map, is.na(subregion))
if (name == "europe") {
  map <- subset(map, region != "russia")
}

# Find the centroid of the mainland of each region
regions <- unique(map$region)
regions_ctr <- matrix(0, nrow = length(regions), ncol = 2)
rownames(regions_ctr) <- regions
colnames(regions_ctr) <- c("long", "lat")
for (r in regions) {
  pc_temp <- subset(map, region == r)[, c("long", "lat", "group")]
  pc_temp <- subset(pc_temp, group == as.numeric(names(which.max(table(pc_temp$group)))))[, c("long", "lat")]
  n <- dim(pc_temp)[1]
  x <- pc_temp$long
  y <- pc_temp$lat
  unnormalized_A <- 0
  unnormalized_Cx <- 0
  unnormalized_Cy <- 0
  for (i in 1:(n - 1)) {
    unnormalized_A <- unnormalized_A + x[i]*y[i + 1] - x[i + 1]*y[i]
    unnormalized_Cx <- unnormalized_Cx + (x[i] + x[i + 1])*(x[i]*y[i + 1] - x[i + 1]*y[i])
    unnormalized_Cy <- unnormalized_Cy + (y[i] + y[i + 1])*(x[i]*y[i + 1] - x[i + 1]*y[i])
  }
  A <- unnormalized_A/2
  Cx <- unnormalized_Cx/6/A
  Cy <- unnormalized_Cy/6/A
  # plot(pc_temp, type = "l")
  # points(Cx, Cy, col = 2)
  regions_ctr[r, "long"] <- Cx
  regions_ctr[r, "lat"] <- Cy
}

# Compute the adjustment
map_adj <- map
for (r in regions) {
  index <- which(map_adj$region == r)
  map_adj$long[index] <- map_adj$long[index] - regions_ctr[r, "long"]
  map_adj$lat[index] <- map_adj$lat[index] - regions_ctr[r, "lat"]
}
long_max <- max(map_adj$long)
long_min <- min(map_adj$long)
lat_max <- max(map_adj$lat)
lat_min <- min(map_adj$lat)
ggplot(data = map_adj) + geom_polygon(mapping = aes(x = long, y = lat, group = group)) + coord_quickmap()

# Plot each region
for (r in regions) {
  png(filename = paste0("~/Dropbox/data/shape/maps/", name, "/", gsub(" ", "_", tolower(r)), ".png"), width = 1200, height = 1200)
  print(ggplot(data = subset(map, region == r)) + geom_polygon(mapping = aes(x = long, y = lat, group = group), fill = "black") + 
    coord_quickmap() +
    xlim(regions_ctr[r, "long"] + long_min, regions_ctr[r, "long"] + long_max) +
    ylim(regions_ctr[r, "lat"] + lat_min, regions_ctr[r, "lat"] + lat_max) +
    theme_classic() +
    theme(axis.text = element_blank(), #remove axis labels
          axis.ticks = element_blank(), #remove axis ticks
          axis.line = element_blank()
          ) +
    labs(x = "", y = ""))
  dev.off()
}
