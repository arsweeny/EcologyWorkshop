# Load necessary libraries
library(vegan)  # For ordination
library(ggplot2)
library(dplyr)
library(MASS)  # For negative binomial distribution

# Set seed for reproducibility
set.seed(123)

# Generate samples
n_samples <- 500  # Number of samples
n_taxa <- 50      # Number of microbial taxa

# Simulate host groups
host_group <- sample(c("Population 1", "Population 2"), n_samples, replace = TRUE)

# Generate taxa counts using a negative binomial distribution
# Population 1 and Population 2 with overlapping means and added dispersion
generate_taxa_counts <- function(group) {
  base_mu <- if (group == "Population 1") {
    30  # Base mean for Population 1
  } else {
    27  # Base mean for Population 2
  }
  
  # Ensure mu is always positive by taking the absolute value of the random noise
  mu_values <- base_mu + abs(rnorm(n_taxa, mean = 0, sd = 20))
  
  # Use a minimum threshold for mu to prevent issues with very low means
  mu_values <- pmax(mu_values, 1)  # Ensure mu is at least 1
  
  # Generate taxa counts from a negative binomial distribution with positive mu
  rnbinom(n_taxa, size = 5, mu = mu_values)  # Generate the counts
}

# Apply function to each sample to generate the taxa counts
taxa_counts <- t(sapply(host_group, generate_taxa_counts))

# Ensure counts are integers and non-negative (inherent to negative binomial)
taxa_counts <- pmax(taxa_counts, 0)  # Redundant step but ensures non-negative counts

# Convert to a data frame
colnames(taxa_counts) <- paste0("Taxa_", 1:n_taxa)
data <- data.frame(SampleID = 1:n_samples, HostGroup = host_group, taxa_counts)

# Inspect the first few rows of the generated data
head(data)

# Compute Bray-Curtis dissimilarity matrix
bray_curtis <- vegdist(data[,3:22], method = "bray")

# Perform PCoA
pcoa_result <- cmdscale(bray_curtis, eig = TRUE, k = 2)  # k = 2 for 2D visualization

# Extract PCoA coordinates
pcoa_coords <- as.data.frame(pcoa_result$points)
colnames(pcoa_coords) <- c("PCoA1", "PCoA2")
pcoa_coords$HostGroup <- host_group  # Add host group information

# Variance explained by each axis
variance_explained <- round(100 * (pcoa_result$eig / sum(pcoa_result$eig)), 1)

# Plot the PCoA results
ggplot(pcoa_coords, aes(x = PCoA1, y = PCoA2, color = HostGroup)) +
  geom_point(alpha = 0.7, size = 3) +
  stat_ellipse(type = "norm", level = 0.68, size = 1) +
  labs(
    title = "PCoA of Beta Diversity",
    x = paste0("PCoA1 (", variance_explained[1], "%)"),
    y = paste0("PCoA2 (", variance_explained[2], "%)")
  ) +
  theme_minimal(base_size = 20) +
  theme(legend.position = "bottom") +
  scale_color_brewer(palette = "Set2") -> beta 


# Prepare the data for PERMANOVA
# Use the original taxa counts (not scaled) and the host group
taxa_counts_for_permanova <- as.data.frame(taxa_counts)  # Convert matrix to data frame

# Run PERMANOVA
permanova_result <- adonis2(
  taxa_counts_for_permanova ~ HostGroup,  # Model: taxa counts explained by host group
  data = data,                           # Metadata containing the HostGroup variable
  method = "bray",                       # Bray-Curtis dissimilarity
  permutations = 999                     # Number of permutations
)

permanova_result

beta + ggsave('Figures/BetaDiv.png', units="mm",
              height=150, width=140, dpi=300)
