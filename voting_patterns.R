"
  voting_patterns.R
  
  In this script, we ...
"

install.packages('dplyr')
install.packages('ggplot2')

# Load required library
library(dplyr)
library(ggplot2)


"
  Loading datasets
"

# Read the CSV file
votes <- read.csv("data/votes.csv")

"
  Observed Results
"

# Calculate average points given from each country to every other country
observed_average_points <- votes %>%
  filter(from_country != to_country) %>%
  group_by(from_country, to_country) %>%
  summarise(avg_points = mean(total_points))

# View the result
print(observed_average_points)

"
  Expected Voting Patterns
  -- Random Voting with no bias or patterns
"

# Create a dataframe with all possible combinations of countries
all_countries <- unique(c(votes$from_country, votes$to_country))
all_combinations <- expand.grid(from_country = all_countries, to_country = all_countries)

# Generate random average points for each country-to-country pair
random_average_points <- all_combinations %>%
  filter(from_country != to_country) %>%
  mutate(avg_points = runif(n(), min = 0, max = 14))  # todo: adjust min and max
# View the result
print(random_average_points)

"
  Comparison of observed and expected dataframes
"

# Merge observed and expected dataframes
comparison <- merge(observed_average_points, random_average_points, by = c("from_country", "to_country"))

# Calculate the differences between observed and expected values
comparison <- comparison %>%
  mutate(difference = avg_points_observed - avg_points_expected)

# View the comparison
print(comparison)

"
  Statistical Testing
"
# Convert average points columns to numeric
comparison$avg_points.x <- as.numeric(comparison$avg_points.x)
comparison$avg_points.y <- as.numeric(comparison$avg_points.y)

# Check for missing values
sum(is.na(comparison$avg_points.x))
sum(is.na(comparison$avg_points.y))

# Remove rows with missing values if necessary
comparison <- na.omit(comparison)

# Perform t-test
t_test_result <- t.test(comparison$avg_points.x, comparison$avg_points.y)

# Print the result
print(t_test_result)


"
  Visualisation
"

