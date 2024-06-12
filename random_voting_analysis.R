"
  random_voting_analysis.R
  ````````````````````````
  Decription:
  ```````````
  This script performs a comprehensive analysis of random voting patterns 
  created in random_voting_creation.R, together with the ESC's actual voting
  data.
  
  The goal is to compare random voting data with actual voting data to 
  understand patterns and biases that may exist in the actual voting process.
  
  Datasets Used:
  ``````````````
  'random_votes.csv' --> The dataset extracted from random_voting_creation.R
  containing a list of randomly generated votes, in a similar format as the 
  actual voting data found in votes.csv.
  'votes.csv' --> Contains the voting history of each country from 1957-2023.
  Shows whether the vote was taken during a semi-final or final, the country
  which voted, and to whom, the total amount of points, and when relevant,
  the number of total points which were provided through a jury vote, and 
  which where received from televoting.
  
  'wld' was removed as discussed in the paper, and NA values were replaced with
  0.
  
  Heatmap
  ```````
  A heatmap was created of the random voting dataset. This is correlated and
  compared with the heatmap created with the actual voting data.
  
  Statistical Analysis
  ````````````````````
  Correlation analysis is performed to be able to understand the relationship 
  between countries in both random and actual voting datasets.
  Chi-Square Test is used to compare the distribution of votes.
  T-Test compares the means of total points between random and actual voting 
  data.
"

# install.packages('ggplot2')
# install.packages('reshape2')
# install.packages('dplyr')
# install.packages('tidyr')

library(ggplot2)
library(reshape2)
library(dplyr)
library(tidyr)

# Loading the CSV file created in 'random_voting_creation.R' script
random_votes <- read.csv("outputs/random_votes.csv", header = TRUE)

# Filtering out records coming from the non-participating world
random_votes <- random_votes %>%
  filter(!grepl("wld", from_country) & !grepl("wld", to_country))

# Replacing NA values in tele_points and jury_points with 0
random_votes$tele_points[is.na(random_votes$tele_points)] <- 0
random_votes$jury_points[is.na(random_votes$jury_points)] <- 0

"
    Visualising the dataset:
    A general Heat Map of the Random Voting Dataset
"

# Aggregating data to calculate total points given by each country to another
voting_matrix <- aggregate(total_points ~ from_country + to_country, data = random_votes, FUN = sum)

# Heatmap plotting using ggplot
heatmap <- ggplot(voting_matrix, aes(x = to_country, y = from_country, fill = total_points)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Random Voting Patterns", x = "To Country", y = "From Country", fill = "Total Points") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")
print(heatmap)

# Extracting information to csv file
write.csv(voting_matrix, "outputs/random_heatmap_data.csv", row.names = FALSE)

"
  Loading and fixing the actual voting data for comparison
"

# Loading the actual voting data
actual_votes <- read.csv("data/votes.csv", header=TRUE)

# Removing the columns "from_country_id" and "to_country_id"
actual_votes <- actual_votes[, !names(actual_votes) %in% c("from_country_id", "to_country_id")]

# Filtering out records coming from the non-participating world
actual_votes <- actual_votes[!grepl("wld", actual_votes$from_country) & !grepl("wld", actual_votes$to_country), ]

# Replacing NA values in tele_points and jury_points with 0
actual_votes$tele_points[is.na(actual_votes$tele_points)] <- -1
actual_votes$jury_points[is.na(actual_votes$jury_points)] <- -1

# Replacing 'Serbia & Montenegro' with 'Serbia'
actual_votes$from_country <- gsub("cs", "rs", actual_votes$from_country)
actual_votes$to_country <- gsub("cs", "rs", actual_votes$to_country)

"
  Chi-Square Test
"

# Getting unique countries from both actual and random data
all_countries <- unique(c(levels(factor(actual_votes[["from_country"]])), 
                          levels(factor(actual_votes[["to_country"]])), 
                          levels(factor(random_votes[["from_country"]])), 
                          levels(factor(random_votes[["to_country"]]))))

# Preparing the contingency table for actual data
actual_table <- table(actual_votes[["from_country"]], actual_votes[["to_country"]])
actual_table <- actual_table[all_countries, all_countries]

# Preparing the contingency table for random data
random_table <- table(random_votes[["from_country"]], random_votes[["to_country"]])
random_table <- random_table[all_countries, all_countries]

chi_square_result <- chisq.test(actual_table, p = random_table)
chi_square_result

"
Output:
  Pearson's Chi-squared test

  data:  actual_table
  X-squared = 5586.9, df = 2500, p-value < 2.2e-16
"

"
  T-Test
"

t_test_result <- t.test(actual_votes$total_points, random_votes$total_points)
print(t_test_result)

"
Welch Two Sample t-test

  data:  actual_votes$total_points and random_votes$total_points
  t = -37.248, df = 102640, p-value < 2.2e-16
  alternative hypothesis: true difference in means is not equal to 0
  95 percent confidence interval:
   -1.0835667 -0.9752316
  sample estimates:
  mean of x mean of y 
   3.122078  4.151477 
"


"
  Correlation Analysis
"

# Correlation matrix for actual voting data
correlation_matrix_actual <- cor(actual_table)

# Correlation matrix for random voting data
correlation_matrix_random <- cor(random_table)

# Visualising correlation matrices
heatmap_correlation_actual <- ggplot(data = reshape2::melt(correlation_matrix_actual),
                                     aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Correlation Matrix for Actual Voting Patterns", x = "To Country", y = "From Country", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "right")

heatmap_correlation_random <- ggplot(data = reshape2::melt(correlation_matrix_random),
                                     aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Correlation Matrix for Random Voting Patterns", x = "To Country", y = "From Country", fill = "Correlation") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "right")

# Displaying the correlation heatmaps
print(heatmap_correlation_actual)
print(heatmap_correlation_random)

