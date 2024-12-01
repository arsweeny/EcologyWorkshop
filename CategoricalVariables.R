# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggpubr) 

# Generate synthetic data
set.seed(123)

n <- 300
x<- 10 

# Set seed for reproducibility
set.seed(123)

# Generate host age (1 to 10 years)
n <- 200  # Number of data points
host_age <- runif(n, min = 0, max = 10) %>% round()

# Simulate viral load with a negative relationship to host age
# Adding random noise to make the data more realistic
viral_load <- 100 - 8 * host_age + rnorm(n, mean = 0, sd = 15)  # Linear negative relationship
viral_load <- 100 * exp(-0.3 * host_age) + rnorm(n, mean = 0, sd = 3)  # Exponential decay with noise
viral_load <- ifelse(
  host_age < 8,
  100 * exp(-0.1 * host_age) + rnorm(n, mean = 0, sd = 6),  # Exponential decay for ages < 9
  55 + 10 * (host_age - 7) + rnorm(n, mean = 0, sd = 6)      # Increase for ages 9-10
)

# Ensure viral load is within the range of 1 to 100
#viral_load <- pmin(pmax(viral_load, 1), 100)

# Combine into a data frame
data <- data.frame(HostAge = host_age, ViralLoad = viral_load) %>% 
  mutate(AgeGroup = case_when(
    HostAge >= 8 & HostAge <= 10 ~ "Geriatric",
    HostAge > 0 & HostAge < 8    ~ "Adult",
    HostAge == 0                 ~ "Juvenile"
  ))

# Plot the simulated data
ggplot(data, aes(x = HostAge, y = ViralLoad)) +
  geom_jitter(alpha = 0.6, width=0.4, height=0.8) +
  geom_smooth(method = "loess", color = "blue", se = FALSE) +
  scale_x_continuous(breaks=seq(0,10,1)) + 
  labs(x = "Host Age (years)",
       y = "Viral Load") +
  theme_minimal(base_size = 20) -> years 

ggplot(data, aes(x = AgeGroup, y = ViralLoad)) +
  geom_boxplot() + 
  geom_jitter(aes(colour=AgeGroup), alpha = 0.8) +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(x = "Host Age Group",
       y = "Viral Load") +
  scale_x_discrete(limits=c("Juvenile", "Adult", "Geriatric")) + 
  theme_minimal(base_size = 20) +
  theme(legend.position = "none") -> group 

# Summary of the simulated data
summary(data)

years + ggsave("Figures/AgeContinuous.png", units="mm", height=120, width=140, dpi=300)
group  + ggsave("Figures/AgeGroup.png", units="mm", height=120, width=140, dpi=300)

