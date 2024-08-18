# Spatial Analysis for Retail Site Selection

# Setup --------------------------------------------------------------------
# Install required packages if not already installed
#install.packages(c("sf", "raster", "spatstat", "dplyr","ape"))

# Load libraries
library(sf)
library(raster)
library(spatstat)
library(dplyr)
library(ape)

# Generating Fake Data -----------------------------------------------------
set.seed(42) # For reproducibility

# Create a simple grid of points representing potential store locations
locations <- expand.grid(x = seq(0, 10, by = 1), y = seq(0, 10, by = 1))

# Convert to sf object
locations_sf <- st_as_sf(locations, coords = c("x", "y"), crs = 4326)

# Generate random population density and income data
locations_sf$population_density <- rnorm(nrow(locations_sf), mean = 1000, sd = 300)
locations_sf$income <- rnorm(nrow(locations_sf), mean = 50000, sd = 10000)

# Plot the data
plot(locations_sf)

# Spatial Autocorrelation --------------------------------------------------
# Calculate spatial weights matrix using inverse distance
coords <- st_coordinates(locations_sf)
dist_matrix <- as.matrix(dist(coords))
inv_dist_matrix <- 1 / dist_matrix
diag(inv_dist_matrix) <- 0

# Calculate Moran's I for population density
moran_population <- Moran.I(locations_sf$population_density, inv_dist_matrix)
print(moran_population)

# Calculate Moran's I for income
moran_income <- Moran.I(locations_sf$income, inv_dist_matrix)
print(moran_income)

# Point Pattern Analysis ---------------------------------------------------
# Generate random existing store locations
n_stores <- 15
existing_stores <- data.frame(x = runif(n_stores, 0, 10), y = runif(n_stores, 0, 10))
existing_stores_sf <- st_as_sf(existing_stores, coords = c("x", "y"), crs = 4326)

# Convert to ppp object for spatstat analysis
store_ppp <- as.ppp(st_coordinates(existing_stores_sf), W = owin(c(0, 10), c(0, 10)))

# Perform Ripley's K analysis
K <- Kest(store_ppp)
plot(K)

# Perform Quadrat test for spatial uniformity
quadrat_test <- quadrat.test(store_ppp, nx = 5, ny = 5)
print(quadrat_test)
plot(quadrat_test)

# Identifying Optimal Retail Locations -------------------------------------
# Define a simple scoring system: higher population density and income is better
locations_sf$score <- scale(locations_sf$population_density) + scale(locations_sf$income)

# Select top 5 locations based on the score
top_locations <- locations_sf %>% 
  arrange(desc(score)) %>% 
  slice(1:5)

# Plot the top locations
plot(st_geometry(locations_sf), col = "lightgrey")
plot(st_geometry(existing_stores_sf), col = "red", add = TRUE)
plot(st_geometry(top_locations), col = "blue", pch = 19, cex = 1.5, add = TRUE)
legend("topright", legend = c("Existing Stores", "Top Locations"), col = c("red", "blue"), pch = 19)
