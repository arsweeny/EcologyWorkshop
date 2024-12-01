# Load necessary libraries
library(ggplot2)
library(dplyr)

# Generate synthetic data
set.seed(123)
n <- 300

# Create groups
group <- sample(c("Forest", "Grassland", "Wetland"), n, replace = TRUE)

# Generate species richness

#species_richness <- rnorm(n, mean = 50, sd = 10)

species_richness <- ifelse(group == "Forest",
                          rnorm(n, mean = 75, sd = 5),  # Lower baseline for Forest
                          ifelse(group == "Grassland",
                                 rnorm(n, mean = 50 , sd = 5),  # Medium baseline
                                 rnorm(n, mean = 25, sd = 5))) # Higher baseline for Wetland


# Habitat quality: positive relationship within groups, but offset group means to create a negative overall trend
habitat_quality <- ifelse(group == "Forest",
                          0.5 * species_richness + rnorm(n, mean = 30, sd = 5),  # Lower baseline for Forest
                          ifelse(group == "Grassland",
                                 0.5 * species_richness + rnorm(n, mean = 60, sd = 5),  # Medium baseline
                                 0.5 * species_richness + rnorm(n, mean = 90, sd = 5))) # Higher baseline for Wetland


# Create the data frame
data <- data.frame(Group = group,
                   SpeciesRichness = species_richness,
                   HabitatQuality = habitat_quality)

# Fit aggregated linear model
aggregated_model <- lm(HabitatQuality ~ SpeciesRichness, data = data)

# Fit within-group models
within_group_models <- data %>%
  group_by(Group) %>%
  summarize(Slope = coef(lm(HabitatQuality ~ SpeciesRichness))[2],
            Intercept = coef(lm(HabitatQuality ~ SpeciesRichness))[1])

# Plot: Within-group relationships
plot_data <- ggplot(data, aes(x = SpeciesRichness, y = HabitatQuality, color = Group)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, aes(group = Group)) +
  labs(title = "Within-Group Relationships",
       x = "Species Richness",
       y = "Habitat Quality") +
  theme_minimal(base_size=20) 

# Plot: Aggregated relationship
plot_aggregated <- ggplot(data, aes(x = SpeciesRichness, y = HabitatQuality)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(title = "Aggregated Relationship",
       x = "Species Richness",
       y = "Habitat Quality") +
  theme_minimal(base_size = 20)

# Plot - both 
plot_both <- data %>% 
  ggplot(.) + 
  geom_point(aes(x = SpeciesRichness, y = HabitatQuality, color = Group),
             alpha = 0.6) +
  geom_smooth(aes(x = SpeciesRichness, y = HabitatQuality, 
                        color = Group, group = Group), 
              method = "lm", se = FALSE) +
  theme_minimal() + 
  geom_smooth(aes(x = SpeciesRichness, y = HabitatQuality), 
              method = "lm", se = FALSE, colour="black", size=1.2) + 
  labs(title = "Aggreated Relationship",
       x = "Species Richness",
       y = "Habitat Quality") +
  theme_minimal(base_size = 20)
  

# Print results and plots
print(plot_data)
print(plot_aggregated)


# Save 

plot_data + ggsave("Figures/WithinGroupSimp.png", units="mm", height=120, width=140, dpi=300)
plot_both + ggsave("Figures/AggregatedSimp.png", units="mm", height=120, width=140, dpi=300)
