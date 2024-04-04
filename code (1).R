# Load necessary library
library(dplyr)
library(ggplot2)
library(rstanarm)

# Define parameters
n_hospitals <- 5
n_years <- 20
n_cancer_types <- 5

# Generate hospital names
hospital_names <- paste("Hospital", LETTERS[1:n_hospitals])

# Generate cancer types
cancer_types <- paste("Cancer Type", 1:n_cancer_types)

# Simulate some data
set.seed(123) 
data <- data.frame(
  Year = rep(2003:2022, times = 5),
  CancerType = rep(c("Lung", "Breast", "Prostate", "Colorectal", "Skin"), each = 20),
  Deaths = sample(50:200, 100, replace = TRUE)
)

ggplot(data, aes(x = Year, y = Deaths, color = CancerType)) +
  geom_line() +
  geom_point() +
  theme_minimal() +
  labs(title = "Cancer Deaths Over Time by Type",
       x = "Year",
       y = "Number of Deaths",
       color = "Cancer Type")

model <- stan_glm(Deaths ~ Year + CancerType, data = data, family = gaussian())
print(summary(model))

