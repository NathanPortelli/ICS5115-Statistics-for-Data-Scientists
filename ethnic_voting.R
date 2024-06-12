"
  ethnic_voting.R
  ```````````````
  Description:
  ```````````
  This script conducts an analysis of voting patterns in the ESC based on 
  shared ethnicities between participating countries. It explores whether 
  countries with shared ethnic groups tend to vote for each other more 
  frequently.
  
  Datasets:
  `````````
  votes.csv --> Contains voting history data for the ESC, including the 
  total points given by each country to others.
  separated_ethnicities_coded.csv --> The output from 
  'ethnic_data_preparation.R'. Provides information on the ethnicities in each 
  country, coded with country codes.
  
  Comparing Same Ethnicities Between Countries:
  `````````````````````````````````````````````
  After filtering out irrelevant records, all possible combinations of countries 
  are created for comparison, excluding self-comparisons. Using a custom 
  function, it is determined whether two countries share any common ethnic 
  groups and the results are recorded.
  
  Plotting:
  `````````
  A network graph of countries with shared ethnicities is plotted. It filters 
  the data to include only countries that share ethnicities and visualises 
  their connections in a circular layout. For better visualisation, 
  a focus on countries with a Russian minority to explore their connected 
  ethnicities.
  
  Correlation Analysis:
  `````````````````````
  The script calculates the correlation between the total points awarded and 
  the presence of shared ethnicities between countries. By merging the voting 
  and ethnicity data, it evaluates the relationship between these variables, 
  providing insights into voting behavior based on ethnic affiliations.
  
  Network Metrics:
  ````````````````
  Network centrality measures are then computed, including degree centrality and 
  closeness centrality, to assess the importance and influence of countries 
  with shared ethnicities within the voting network.
  
  Statistical Tests
  `````````````````
  Finally, the script conducts statistical tests to determine the significance 
  of the relationship between shared ethnicities and total points awarded. It 
  performs a chi-square test to assess the association between these variables 
  and a t-test to compare the mean total points awarded between countries with 
  shared ethnicities and those without. The results provide statistical 
  evidence of the impact of shared ethnicities on voting patterns in the ESC.
"

# Loading the required libraries
library(dplyr)
library(igraph)

"
  Loading the datasets
"

# Load the votes dataset
# Source: https://github.com/Spijkervet/eurovision-dataset
votes_data <- read.csv("data/votes.csv")

separated_ethnicities_coded <- read.csv("outputs/separated_ethnicities_coded.csv")

# Removing the columns "from_country_id" and "to_country_id"
votes_data <- votes_data[, !names(votes_data) %in% c("from_country_id", "to_country_id")]

# Filtering out records coming from the non-participating world (WLD vote from 2023)
votes_data <- votes_data[!grepl("wld", votes_data$from_country, ignore.case = TRUE), ]

# Replacing NA values in tele_points and jury_points with 0
votes_data$tele_points[is.na(votes_data$tele_points)] <- -1
votes_data$jury_points[is.na(votes_data$jury_points)] <- -1

# Aggregating total points for each combination of from_country and to_country
top_countries <- aggregate(total_points ~ from_country + to_country, data = votes_data, sum)

# Ordering the DataFrame in descending order of total points
top_countries <- top_countries[order(-top_countries$total_points), ]

# Grouping by from_country and select the top 3 entries for each group
top_countries <- do.call(rbind, by(top_countries, top_countries$from_country, head, n = 3))

# Uppercasing the values to ensure consistency
top_countries$from_country <- toupper(top_countries$from_country)
top_countries$to_country <- toupper(top_countries$to_country)

# Merging top_countries with separated_langauges_coded to obtain country codes
top_countries <- merge(top_countries, separated_langauges_coded, by.x = "from_country", by.y = "Country_Code", all.x = TRUE)
top_countries <- merge(top_countries, separated_langauges_coded, by.x = "to_country", by.y = "Country_Code", all.x = TRUE, suffixes = c("_from", "_to"))

"
  Comparing same ethnicities between countries
"

# Loading the separated_ethnicities_coded.csv file generated from 'ethnic_data_preparation.R' script
ethnicity_data <- read.csv("outputs/separated_ethnicities_coded.csv")

# Setting country names to uppercase to ensure consistency
ethnicity_data$Country_Code <- toupper(ethnicity_data$Country_Code)

# Creating all possible combinations of countries for comparison
country_combinations <- expand.grid(unique(ethnicity_data$Country_Code), unique(ethnicity_data$Country_Code))
colnames(country_combinations) <- c("country_from", "country_to")

# Excluding combinations where country_from equals country_to
country_combinations <- country_combinations[country_combinations$country_from != country_combinations$country_to, ]

# Checking if two countries share any common ethnic groups
check_same_ethnicity <- function(country_from, country_to) {
  ethnic_groups_from <- unique(ethnicity_data$Ethnic.group[ethnicity_data$Country_Code == country_from])
  ethnic_groups_to <- unique(ethnicity_data$Ethnic.group[ethnicity_data$Country_Code == country_to])
  if (length(intersect(ethnic_groups_from, ethnic_groups_to)) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
country_combinations$has_same_ethnicity <- mapply(check_same_ethnicity, country_combinations$country_from, country_combinations$country_to)

# Saving the results to a new CSV file
write.csv(country_combinations, "outputs/ethnicity_comparison.csv", row.names = FALSE)

"
  Plotting
"

# Filtering the DataFrame to only include rows where has_same_ethnicity is TRUE
edge_list_ethnicity <- country_combinations[country_combinations$has_same_ethnicity, c("country_from", "country_to")]

# Converting the DataFrame to a graph object
graph_ethnicity <- graph_from_data_frame(edge_list_ethnicity, directed = FALSE)

# Plotting the graph in a circular layout
plot(graph_ethnicity, 
     layout = layout_in_circle(graph_ethnicity),
     edge.color = "green",
     vertex.color = "lightblue",  
     vertex.size = 10,  
     vertex.label.cex = 0.8,  
     edge.arrow.size = 0.5,  
     main = "Circular Network Graph of Countries with Shared Ethnicities")

# If we only show countries with a language spoken in e.g. Russia:

# Filtering the DataFrame to only include rows where has_same_ethnicity is TRUE
russia_connected_ethnic_countries <- subset(russia_connected_countries, has_same_ethnicity == TRUE)

# Creating a graph object
graph_ethnicity_russia <- graph_from_data_frame(russia_connected_ethnic_countries, directed = FALSE)

# Plotting the graph in a circular layout
plot(graph_ethnicity_russia, 
     layout = layout_in_circle(graph_ethnicity_russia),
     edge.color = "blue",
     vertex.color = "lightblue",  
     vertex.size = 10,  
     vertex.label.cex = 0.8,  
     edge.arrow.size = 0.5,  
     main = "Network Graph of Countries Connected to Russia based on Ethnicities")

"
  Calculating percentage of ethnic voting for the top 3 points per country
"

# Re-loading the data
ethnicity_comparison <- read.csv("outputs/ethnicity_comparison.csv")

# Merging ethnicity_comparison with separated_ethnicities_coded to obtain country names
ethnicity_comparison <- merge(ethnicity_comparison, separated_ethnicities_coded, by.x = "country_from", by.y = "Country_Code", all.x = TRUE)
ethnicity_comparison <- merge(ethnicity_comparison, separated_ethnicities_coded, by.x = "country_to", by.y = "Country_Code", all.x = TRUE, suffixes = c("_from", "_to"))

# Counting the number of connected countries with the same ethnicity
total_connected <- nrow(ethnicity_comparison)
same_ethnicity <- sum(ethnicity_comparison$has_same_ethnicity, na.rm = TRUE)
percentage_same_ethnicity <- (same_ethnicity / total_connected) * 100

# Total percentage of connected countries with the same ethnicity (%):
percentage_same_ethnicity

"
  Correlation Analysis
"

# Re-loading the datasets
votes_data <- read.csv("data/votes.csv")
ethnicity_data <- read.csv("outputs/ethnicity_comparison.csv")

# Preprocessing the data
votes_data$from_country <- toupper(votes_data$from_country)
votes_data$to_country <- toupper(votes_data$to_country)
ethnicity_data$country_from <- toupper(ethnicity_data$country_from)
ethnicity_data$country_to <- toupper(ethnicity_data$country_to)

# Merging the datasets
merged_data <- merge(votes_data, ethnicity_data, by.x = c("from_country", "to_country"), by.y = c("country_from", "country_to"), all.x = TRUE)

# Calculating the correlation, ignoring NA values in has_same_ethnicity
correlation <- cor(merged_data$total_points, as.numeric(merged_data$has_same_ethnicity), use = "complete.obs")
print(correlation)  # Output: 0.1009814

"
  Network Metrics
"
# Calculating centrality measures ensuring the consistency in vertices
# Degree centrality
degree_centrality <- degree(graph_ethnicity, mode = "all")

# Closeness centrality
closeness_centrality <- closeness(graph_ethnicity, mode = "all")

# Betweenness centrality
betweenness_centrality <- betweenness(graph_ethnicity, normalized = TRUE)

# Ensuring all centrality measures have the same countries (vertices)
common_countries <- intersect(intersect(names(degree_centrality), names(closeness_centrality)), names(betweenness_centrality))

# Combining
centrality_measures <- data.frame(
  country = common_countries,
  degree_centrality = degree_centrality[common_countries],
  closeness_centrality = closeness_centrality[common_countries],
  betweenness_centrality = betweenness_centrality[common_countries]
)

# Write the combined centrality measures to a CSV file
write.csv(centrality_measures, "outputs/ethnic_centrality_measures.csv", row.names = FALSE)

"
  Statistical Tests: Chi-Square Test & T-Test
"

# Re-loading the ethnicity comparison data
ethnicity_comparison <- read.csv("outputs/ethnicity_comparison.csv")

# Aggregating the total points by unique country pairs
aggregated_data <- aggregate(total_points ~ from_country + to_country, data = merged_data, sum)

# Merging the aggregated voting data with the language data
merged_ethnicity_voting_data <- merge(aggregated_data, ethnicity_comparison, by.x = c("from_country", "to_country"), by.y = c("country_from", "country_to"), all.x = TRUE)

# Removing rows with NA values in the has_same_language column
merged_ethnicity_voting_data <- merged_ethnicity_voting_data[complete.cases(merged_ethnicity_voting_data$has_same_ethnicity), ]

# Contingency table
contingency_table <- table(merged_data$has_same_ethnicity, merged_data$total_points)

"
  Chi-Square Test
"

chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

"
Output:
Pearson's Chi-squared test
data:  contingency_table
X-squared = 552.35, df = 22, p-value < 2.2e-16
"

"
  T-Test
"

# Separating the data into two groups based on whether they have the same ethnicity or not
same_ethnicity_group <- merged_data[merged_data$has_same_ethnicity == TRUE, "total_points"]
different_ethnicity_group <- merged_data[merged_data$has_same_ethnicity == FALSE, "total_points"]

t_test_result <- t.test(same_ethnicity_group, different_ethnicity_group)
print(t_test_result)

"
Output:

Welch Two Sample t-test

  data:  same_ethnicity_group and different_ethnicity_group
  t = 19.858, df = 19706, p-value < 2.2e-16
  alternative hypothesis: true difference in means is not equal to 0
  95 percent confidence interval:
    0.908918 1.108000
  sample estimates:
    mean of x mean of y 
  4.010761  3.002302 
"