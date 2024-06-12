"
  language_voting.R
  `````````````````
  Decription:
  ```````````
  After retrieving and preparing the Eurovision voting dataset and language 
  information in 'votes.csv' and 'separated_langauges_coded.csv' respectively, 
  this script analyses voting patterns based on shared languages between 
  countries participating in the ESC. 
  
  The script aims to investigate whether countries with shared languages tend 
  to vote for each other more frequently.
  
  Datasets Used:
  ``````````````
  votes.csv --> Contains voting history data for the ESC, including the 
  total points given by each country to others.
  separated_langauges_coded.csv --> Provides information on the languages 
  spoken in each country, coded with country codes for ease of analysis.

  Comparing Same Languages Between Countries
  ``````````````````````````````````````````
  The script identifies countries with shared languages by comparing the 
  languages spoken in each country pair. It generates a comparison result 
  indicating whether two countries share any common language.
  
  Plotting
  ````````
  Network graphs are constructed to visualize relationships between countries 
  with shared languages. The script plots a network graph of all countries with 
  the same language and, optionally, a graph of countries linked to a specific 
  country (in this case, Russia) based on language connections.
  
  The percentage of country pairs sharing the same language is calculated to 
  determine the prevalence of shared language relationships among participating
  countries.
  
  Correlation Analysis:
  `````````````````````
  Pearson's correlation coefficient is computed to assess the relationship 
  between the total points awarded and whether countries share the same 
  language. This analysis aims to discern any correlation between language 
  similarity and voting behavior.
  
  Network Metrics:
  ````````````````
  Degree centrality, closeness centrality, and betweenness centrality measures 
  are calculated to evaluate the importance of countries in the network based 
  on shared languages.
  
  Statistical Tests:
  ``````````````````
  A chi-square test is performed to determine the association between countries 
  sharing the same language and the total points they allocate to each other.
  A t-test is also conducted to compare the mean total points given by 
  countries with shared languages against those without shared languages, 
  aiming to ascertain whether there is a significant difference in voting 
  behaviour based on language similarity.
"

#install.packages('igraph')
#install.packages('tidyr')

# Loading the required library
library(igraph)
library(dplyr)
library(tidyr)

"
  The below has been copied from borders_voting.R
"

# Loading the votes dataset
# Source: https://github.com/Spijkervet/eurovision-dataset
votes_data <- read.csv("data/votes.csv")

separated_langauges_coded <- read.csv("outputs/separated_langauges_coded.csv")

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

# Grouping by from_country and select the top entries for each group
top_countries <- do.call(rbind, by(top_countries, top_countries$from_country, head, n = 3))

# Uppercasing the values to ensure consistency
top_countries$from_country <- toupper(top_countries$from_country)
top_countries$to_country <- toupper(top_countries$to_country)

# Merging top_countries with separated_langauges_coded to obtain country codes
top_countries <- merge(top_countries, separated_langauges_coded, by.x = "from_country", by.y = "Country_Code", all.x = TRUE)
top_countries <- merge(top_countries, separated_langauges_coded, by.x = "to_country", by.y = "Country_Code", all.x = TRUE, suffixes = c("_from", "_to"))

"
  Comparing same languages between countries
"

# Loading the separated_language_coded dataset
separated_language <- read.csv("outputs/separated_langauges_coded.csv")

# Ensuring all country names are uppercase for consistency
separated_language$Country_Code <- toupper(separated_language$Country_Code)

# Getting unique list of countries
countries <- unique(separated_language$Country_Code)

# Creating an empty DataFrame to store comparison results
language_comparison <- data.frame(country_from = character(),
                                  country_to = character(),
                                  has_same_language = logical(),
                                  stringsAsFactors = FALSE)

# Iterating through each combination of countries
for (i in 1:length(countries)) {
  for (j in (i + 1):length(countries)) {
    country1 <- countries[i]
    country2 <- countries[j]
    
    # Skipping the comparison if either country is NA
    if (is.na(country1) || is.na(country2)) {
      next
    }
    
    # Getting languages spoken in each country
    languages_country1 <- unique(separated_language$Languages.Spoken[separated_language$Country_Code == country1])
    languages_country2 <- unique(separated_language$Languages.Spoken[separated_language$Country_Code == country2])
    
    # Checking if any language is common between the two countries
    has_common_language <- any(languages_country1 %in% languages_country2)
    
    # Storing the comparison result
    comparison_result <- data.frame(country_from = country1,
                                    country_to = country2,
                                    has_same_language = has_common_language)
    
    # Appending the comparison result to the language_comparison dataframe
    language_comparison <- rbind(language_comparison, comparison_result)
  }
}

write.csv(language_comparison, "outputs/language_comparison.csv", row.names = FALSE)

"
  Plotting
"

# Creating a DataFrame for edge list based on unique edge connections
unique_edges <- unique(language_comparison[, c("country_from", "country_to", "has_same_language")])
edge_list_lang <- data.frame(
  from = unique_edges$country_from,
  to = unique_edges$country_to,
  is_same_language = unique_edges$has_same_language
)

# Converting 'is_same_language' column to logical type
edge_list_lang$is_same_language <- as.logical(edge_list_lang$is_same_language)

# Filtering edges to only include those where is_same_language is TRUE
edge_list_lang <- edge_list_lang[edge_list_lang$is_same_language, ]

# Creating a graph object
graph_lang <- graph_from_data_frame(edge_list_lang, directed = FALSE)

plot(graph_lang, 
     layout = layout_in_circle(graph_lang),
     edge.color = "green",
     vertex.color = "lightblue",  
     vertex.size = 10,  
     vertex.label.cex = 0.8,  
     edge.arrow.size = 0.5,  
     main = "Network Graph of Countries with the Same Language")

# If we only show countries with a language spoken in e.g. Russia:

# Filtering the edge list to only include countries connected to Russia
edge_list_russia <- edge_list_lang[edge_list_lang$from == "RU" | edge_list_lang$to == "RU", ]
graph_russia <- graph_from_data_frame(edge_list_russia, directed = FALSE)

plot(graph_russia, 
     layout = layout_in_circle(graph_russia),
     edge.color = "green",
     vertex.color = "lightblue",  
     vertex.size = 10,  
     vertex.label.cex = 0.8,  
     edge.arrow.size = 0.5,  
     main = "Network Graph of Countries Linked to Russia")

"
  Percentage
"

# Total number of rows
total_rows_lang <- nrow(language_comparison)

# Calculating the number of same languages
same_language_rows <- sum(language_comparison$has_same_language == "TRUE", na.rm = TRUE)

# Calculating the percentage
if (total_rows_lang > 0) {
  percentage_same_language <- (same_language_rows / total_rows_lang) * 100
} else {
  percentage_same_language <- 0
}
# Percentage of rows where is_same_language = TRUE (in %):
percentage_same_language

write.csv(edge_list_lang, "outputs/same_language_edges.csv", row.names = FALSE)


"
  Calculating percentage of language voting for the top 3 points per country
"

# Aggregating total points for each combination of from_country and to_country
top_countries <- aggregate(total_points ~ from_country + to_country, data = votes_data, sum)

# Ordering the DataFrame in descending order of total points
top_countries <- top_countries[order(-top_countries$total_points), ]

# Grouping by from_country and select the top entries for each group
top_countries <- top_countries %>%
  group_by(from_country) %>%
  slice_head(n = 3)

# Uppercasing the values to ensure consistency
top_countries$from_country <- toupper(top_countries$from_country)
top_countries$to_country <- toupper(top_countries$to_country)

# Merging top_countries with language_comparison to obtain language information
top_countries <- merge(top_countries, language_comparison, by.x = c("from_country", "to_country"), by.y = c("country_from", "country_to"), all.x = TRUE)

# Calculating the percentage of connected countries that have the same language
total_connected <- nrow(top_countries)
same_language <- sum(top_countries$has_same_language, na.rm = TRUE)
percentage_same_language <- (same_language / total_connected) * 100

# Total percentage of connected countries with the same language (%):
print(percentage_same_language)

"
  Correlation Analysis
"

# Re-loading the datasets
votes_data <- read.csv("data/votes.csv")
language_data <- read.csv("outputs/language_comparison.csv")

# Preprocessing the data
votes_data$from_country <- toupper(votes_data$from_country)
votes_data$to_country <- toupper(votes_data$to_country)
language_data$country_from <- toupper(language_data$country_from)
language_data$country_to <- toupper(language_data$country_to)

# Merging the datasets
merged_data <- merge(votes_data, language_data, by.x = c("from_country", "to_country"), by.y = c("country_from", "country_to"), all.x = TRUE)

# Calculating the correlation, ignoring NA values in has_same_language
correlation <- cor(merged_data$total_points, as.numeric(merged_data$has_same_language), use = "complete.obs")

print(correlation)  # Output: 0.1020263

"
  Network Metrics
"

# Degree centrality
degree_centrality <- degree(graph_lang, mode = "all")

# Closeness centrality
closeness_centrality <- closeness(graph_lang, mode = "all")

# Betweenness centrality
betweenness_centrality <- betweenness(graph_lang, normalized = TRUE)

# Combine centrality measures into a data frame
centrality_measures <- data.frame(
  country = V(graph_lang)$name,
  degree_centrality = degree_centrality,
  closeness_centrality = closeness_centrality,
  betweenness_centrality = betweenness_centrality
)
write.csv(centrality_measures, "outputs/lang_centrality_measures.csv", row.names = FALSE)

"
  Statistical Tests: Chi-Square Test & T-Test
"

# Load the language comparison data
language_data <- read.csv("outputs/language_comparison.csv")

# Aggregating the total points by unique country pairs
aggregated_data <- aggregate(total_points ~ from_country + to_country, data = merged_data, sum)

# Merging the aggregated voting data with the language data
merged_language_voting_data <- merge(aggregated_data, language_data, by.x = c("from_country", "to_country"), by.y = c("country_from", "country_to"), all.x = TRUE)

# Removing rows with NA values in the has_same_language column
merged_language_voting_data <- merged_language_voting_data[complete.cases(merged_language_voting_data$has_same_language), ]

# Contingency table
contingency_table <- table(merged_language_voting_data$has_same_language, merged_language_voting_data$total_points)

"
  Chi-Square Test
"

chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

"
Outputs:
Pearson's Chi-squared test
data:  contingency_table
X-squared = 545.03, df = 469, p-value = 0.008625
"

"
  T-Test
"

# Separating the data into two groups based on whether they have the same language or not
same_language_group <- merged_language_voting_data[merged_language_voting_data$has_same_language == TRUE, "total_points"]
different_language_group <- merged_language_voting_data[merged_language_voting_data$has_same_language == FALSE, "total_points"]

t_test_result <- t.test(same_language_group, different_language_group)
print(t_test_result)

"
Outputs:

Welch Two Sample t-test
data:  same_language_group and different_language_group
t = 5.7492, df = 287.55, p-value = 2.288e-08
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 22.80814 46.55473
sample estimates:
mean of x mean of y 
 99.02232  64.34088 
"