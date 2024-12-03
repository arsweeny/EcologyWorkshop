
# Load necessary libraries
library(ggplot2)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Generate posterior distributions
n_samples <- 1000  # Number of posterior samples

# 1. Significant effect, low magnitude, low variance
posterior_significant <- rnorm(n_samples, mean = 0.30, sd = 0.1)  # Narrow distribution

# 2. High magnitude effect, high variance, insignificant
posterior_insignificant <- rnorm(n_samples, mean = 3.0, sd = 1.8)  # Wide distribution


# Combine the data into a single data frame
posterior_data <- data.frame(
  Value = c(posterior_significant, posterior_insignificant),
  Type = rep(c("Significant Effect", "Insignificant Effect"), each = n_samples)
)

# Calculate 95% confidence intervals for each distribution
ci_data <- posterior_data %>%
  group_by(Type) %>%
  summarise(
    LowerCI = quantile(Value, 0.025),  # 2.5th percentile
    UpperCI = quantile(Value, 0.975),  # 97.5th percentile
    Mean = mean(Value),
    P_value = 2 * min(mean(Value > 0), mean(Value < 0))  # Two-tailed p-value
  )

# Print the confidence intervals and p-values
print(ci_data)

# Plot the posterior distributions

pal <- RColorBrewer::brewer.pal(4, "Set2")

ggplot(posterior_data %>% 
         filter(Type=="Significant Effect"), aes(x = Value, fill = Type)) +
  geom_density(alpha = 0.6, fill=pal[2]) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(data = ci_data %>%  filter(Type=="Significant Effect"), 
             aes(xintercept = LowerCI), 
             linetype = "dotted", size=1, colour=pal[2]) +
  geom_vline(data = ci_data %>% filter(Type=="Significant Effect"), 
             aes(xintercept = UpperCI), 
             linetype = "dotted", size=1, colour=pal[2]) +
  #facet_wrap(~ Type) +
  labs(
    title = "Posterior Distributions of Model Effects",
    x = "Effect Size",
    y = "Density",
    fill = "Effect Type"
  ) +
  scale_x_continuous(limits=c(-2.5, 7.5)) + 
  theme_minimal(base_size=20) +
  theme(legend.position = "none") -> Twin 

ggplot(posterior_data, aes(x = Value, fill = Type)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  geom_vline(data = ci_data, aes(xintercept = LowerCI, color = Type), 
             linetype = "dotted", size=1) +
  geom_vline(data = ci_data, aes(xintercept = UpperCI, color = Type), 
             linetype = "dotted", size=1) + 
  #facet_wrap(~ Type) +
  labs(
    title = "Posterior Distributions of Model Effects",
    x = "Effect Size",
    y = "Density",
    fill = "Effect Type"
  ) +
  scale_x_continuous(limits=c(-2.5, 7.5)) + 
  theme_minimal(base_size=20) +
  #theme(legend.position = "none") +
  scale_fill_manual(values = pal[1:2], labels= c("Sex", "Twin Status")) + 
  scale_colour_manual(values = pal[1:2], 
                      labels= c("Sex", "Twin Status"), guide = "none")-> Comp 


Twin + ggsave('Figures/SignifPosterior.png', units ="mm", 
              height = 180, width = 180)

Comp + ggsave('Figures/CompPosteriors.png', units ="mm", 
              height = 180, width = 210)


### now let's add a grouping variable 

# Number of posterior samples
n_samples <- 1000

# Generate posterior distributions
posterior_significant <- rnorm(n_samples, mean = 0.30, sd = 0.1)  # Narrow, significant
posterior_group1 <- rnorm(n_samples, mean = 5.0, sd = 0.5)  # Significant subgroup
posterior_group2 <- rnorm(n_samples, mean = 1, sd = 1.5)  # Insignificant subgroup

# Combine into a single data frame
posterior_data <- data.frame(
  Value = c(posterior_significant, posterior_group1, posterior_group2),
  Type = c(
    rep("Significant Effect", n_samples),
    rep("Subgroup 1 (Significant)", n_samples),
    rep("Subgroup 2 (Insignificant)", n_samples)
  )
)

# Calculate 95% confidence intervals and p-values for each group
ci_data <- posterior_data %>%
  group_by(Type) %>%
  summarise(
    LowerCI = quantile(Value, 0.025),  # 2.5th percentile
    UpperCI = quantile(Value, 0.975),  # 97.5th percentile
    Mean = mean(Value),
    P_value = 2 * min(mean(Value > 0), mean(Value < 0))  # Two-tailed p-value
  )

# Print the confidence intervals and p-values
print(ci_data)

# Plot the posterior distributions with confidence intervals
ggplot(posterior_data, aes(x = Value, fill = Type)) +
  geom_density(alpha = 0.6) +
  geom_vline(data = ci_data, aes(xintercept = LowerCI, color = Type), linetype = "dotted") +
  geom_vline(data = ci_data, aes(xintercept = UpperCI, color = Type), linetype = "dotted") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  #facet_wrap(~ Type) +
  labs(
    title = "Posterior Distributions with Subgroup Accounted for",
    x = "Effect Size",
    y = "Density",
    fill = "Effect Type"
  ) +
  theme_minimal(base_size = 20) +
  #theme(legend.position = "none") +
  scale_fill_manual(values = pal[c(2,1,3)], labels= c("Twin Status", "Female - Calved",
                                                   "Female - No Calf")) + 
  scale_colour_manual(values = pal[c(2,1,3)], 
                      labels= c("Twin Status", "Female - Calved",
                                "Female - No Calf"), guide = "none") -> GroupComp 

GroupComp + ggsave('Figures/GroupCompPosteriors.png', units ="mm", 
              height = 180, width = 215)



