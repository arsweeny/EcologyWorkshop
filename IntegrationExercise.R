
### create a simulated study 

# load necessary packages 
library(tidyverse)
library(ggregplot)

set.seed(123)  # For reproducibility

# Number of observations
n <- 1000

# Simulating unique individuals
n_individuals <- 200  # Total number of unique individuals
individual_id <- sample(1:n_individuals, size = n, replace = TRUE)  # Repeated sampling of individuals

# Simulating host traits
age <- sample(c("juvenile", "adult"), size = n_individuals, replace = TRUE, prob = c(0.4, 0.6))
sex <- sample(c("male", "female"), size = n_individuals, replace = TRUE, prob = c(0.5, 0.5))

# Map individual traits to observations
age <- age[individual_id]
sex <- sex[individual_id]


# Simulating host traits
age <- sample(c("juvenile", "adult"), size = n, replace = TRUE, prob = c(0.4, 0.6))
sex <- sample(c("male", "female"), size = n, replace = TRUE, prob = c(0.5, 0.5))
body_condition <- round(rnorm(n, mean = 6, sd = 1), 1)
body_condition[body_condition > 10] <- 10
body_condition[body_condition < 1] <- 1


# Simulating environmental variables
habitat <- sample(c("forest", "grassland", "edge"), size = n, replace = TRUE, prob = c(0.5, 0.3, 0.2))
temperature <- rnorm(n, mean = 15, sd = 5) + rnorm(n, mean = 0, sd = 2)
rainfall <- rnorm(n, mean = 50, sd = 20) + rnorm(n, mean = 0, sd = 5)


# Additional variables
trap_type <- sample(c("live_trap", "pitfall_trap"), size = n, replace = TRUE, prob = c(0.7, 0.3))
handler_experience <- sample(c("novice", "experienced"), size = n, replace = TRUE, prob = c(0.4, 0.6))
season <- sample(c("spring", "summer", "fall", "winter"), size = n, replace = TRUE)
host_density <- round(runif(n, min = 10, max = 100))  # Density of individuals per unit area
previous_exposure <- rbinom(n, size = 1, prob = 0.4)  # 40% have previous exposure


# introduce missing data for body condition 
#missing_indices <- sample(1:n, size = n * 0.1, replace = FALSE)
#body_condition[missing_indices] <- NA

# adjust temperature and rainfall by season 

# Disease probability adjustments
base_prob <- 0.2
age_effect <- ifelse(age == "juvenile", 0.1, -0.05)
body_condition_effect <- (10 - body_condition) / 50
habitat_effect <- ifelse(habitat == "forest", 0.05, 
                         ifelse(habitat == "grassland", -0.05, 0.1))
temperature_effect <- (temperature - 15) / 100
rainfall_effect <- (rainfall - 50) / 100
density_effect <- (host_density - 50) / 500  # Higher density increases risk slightly
exposure_effect <- ifelse(previous_exposure == 1, -0.1, 0.05)  # Previously exposed less likely to be infected

# Final probability of infection
prob_infection <- base_prob + age_effect + body_condition_effect + habitat_effect +
  temperature_effect + rainfall_effect + density_effect + exposure_effect
prob_infection <- pmin(pmax(prob_infection, 0), 1)

# Generate disease status
disease_status <- rbinom(n, size = 1, prob = prob_infection)

# Misclassification in disease status (5% misclassified)
#misclassification_indices <- sample(1:n, size = round(n * 0.05), replace = FALSE)
#disease_status[misclassification_indices] <- 1 - disease_status[misclassification_indices]

# Viral load: overdispersed and zero-inflated
viral_load <- ifelse(disease_status == 1,
                     # Infected individuals: generate overdispersed positive values
                     rnbinom(sum(disease_status == 1), size = 2, mu = 10),  # Negative binomial
                     # Uninfected individuals: zero viral load
                     0)

# Ensure the length of `viral_load` matches the number of observations
viral_load <- ifelse(disease_status == 1,
                     rnbinom(n = sum(disease_status == 1), size = 2, mu = 10),
                     rep(0, n))


# Create data frame
data <- data.frame(
  age = age,
  sex = sex,
  body_condition = body_condition,
  habitat = habitat,
  temperature_celsius = round(temperature, 1),
  rainfall = round(rainfall, 1),
  trap_type = trap_type,
  handler_experience = handler_experience,
  season = season,
  host_density = host_density,
  previous_exposure = previous_exposure,
  disease_status = disease_status, 
  viral_load = viral_load
)


# some modifications that make biological or collection sense -------------


# Add seasonal effects for temperature and rainfall
data <- data %>% 
  mutate(temperature_celsius = ifelse(season == "spring", rnorm(n, mean = 12, sd = 3),
                      ifelse(season == "summer", rnorm(n, mean = 25, sd = 5),
                             ifelse(season == "fall", rnorm(n, mean = 15, sd = 4),
                                    rnorm(n, mean = 5, sd = 5))))) 
data  <- data %>% 
  mutate(rainfall = ifelse(season == "spring", rnorm(n, mean = 60, sd = 15),
                   ifelse(season == "summer", rnorm(n, mean = 30, sd = 10),
                          ifelse(season == "fall", rnorm(n, mean = 50, sd = 12),
                                 rnorm(n, mean = 70, sd = 20))))) 

# Introduce observer effect on body condition
# Novices overestimate body condition by +0.5 on average with higher variability
# Experienced observers measure body condition more accurately
data <- data %>% 
  mutate(body_condition= ifelse(handler_experience == "novice",
                         body_condition + rnorm(n, mean = 0.5, sd = 1.5),  # Bias and more noise
                         body_condition + rnorm(n, mean = 0, sd = 0.5))) %>%  # Less bias and noise
  mutate(body_condition= ifelse(body_condition >10, 10, 
                                ifelse(
                                body_condition < 1, 1, 
                                body_condition)))


# Save dataset
write.csv(data, "wild_animal_disease_dataset_with_issues.csv", row.names = FALSE)

# View first few rows
head(data)


# explore the data to check our nuances are what we want  ----------------

SinaGraph(data, "handler_experience", "body_condition")
SinaGraph(data, "season", "rainfall")
SinaGraph(data, "season", "temperature_celsius")

SinaGraph(data, "season", "viral_load")
BarGraph(data, "season", "disease_status")


