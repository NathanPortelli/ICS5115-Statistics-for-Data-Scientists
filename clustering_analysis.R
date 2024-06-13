"
  clustering_analysis.R
  `````````````````````
  Description:
  ````````````
  This script is designed to perform clustering coefficient analysis on voting 
  data between countries across multiple years. It analyzes both actual voting 
  data and randomly generated data to compare clustering patterns. It utilises 
  various R packages for data manipulation, graph analysis, clustering, and 
  visualisation.
  
  Datasets Used:
  ``````````````
  votes.csv --> Contains voting history data for the ESC, including the 
  total points given by each country to others.
  random_votes.csv --> Contains the random voting data created by 
  'random_voting_analysis.R'. It serves as a comparison by simulating random 
  voting patterns between countries.
  
  Clustering Coefficient Analysis
  ```````````````````````````````
  Summary statistics (mean, median, standard deviation) of clustering 
  coefficients are calculated for each country based on voting patterns.
  Clustering coefficient analysis is done for both the actual and random data 
  for the total countries, as well as the individual countries between the 
  years 2013 and 2023.
  This statistical summary results of the actual data is then saved to 
  'clustering_stats_votes.csv', whilst the results with the random data
  are saved to 'clustering_stats_random_votes.csv'.
  
  The coefficients are also plotted on a column chart, with the countries
  being colour-coded based on the regions described by the Mantzaris et. al 
  (https://doi.org/10.1007/s42001-018-0020-2).
  
  Community Detection (Louvain Method)
  ````````````````````````````````````
  The Louvain Method is applied to identify clusters of countries based on 
  their voting patterns. These clusters are then mapped onto a map of Europe
  (adapted from https://warin.ca/posts/rcourse-datavisualizationwithr-maps/),
  for ease of visualisation, with each cluster being superimposed using a 
  unique colour.
"

# install.packages('dplyr')
# install.packages('igraph')
# install.packages('ggplot2')
# install.packages('ggrepel')
# install.packages('readr')
# install.packages('maps')
# install.packages('mapdata')

# Loading required packages
library(dplyr)
library(igraph)
library(ggplot2)
library(ggrepel)
library(readr)
library(reshape2)
library(maps)
library(mapdata)

"
  ========================================
  clustering Coefficient Analysis
  - Individual years (2013 - 2023)
  - Random and Actual Data
  ========================================
"

# Calculating summary statistics for random & actual data for every individual year
calculate_summary_stats <- function(data, years) {
  # DataFrame to store the summary statistics for every year
  summary_stats_all_years <- data.frame(year = integer(), mean = numeric(), median = numeric(), sd = numeric())
  
  # Looping through every year
  for (year_num in years) {
    # Filtering data for current year
    votes_filtered <- subset(data, year == year_num & total_points > 0 & round == 'final')
    
    edges <- subset(votes_filtered, select = c("from_country", "to_country"))  # Edge list
    g <- graph_from_data_frame(edges, directed = TRUE)  # Creating the graph
    
    # Clustering coefficient for each node
    clustering_coefficients <- transitivity(g, type = "local", isolates = "zero")
    
    # Adding clustering coefficients to DataFrame for better visualisation
    clustering_df <- data.frame(country = V(g)$name, clustering_coefficient = clustering_coefficients)
    
    # Ensure "WLD" is not present -- 2023 data
    clustering_df <- clustering_df %>% filter(country != "wld")
    
    # Calculating summary statistics
    summary_stats <- clustering_df %>%
      summarize(mean = mean(clustering_coefficient),
                median = median(clustering_coefficient),
                sd = sd(clustering_coefficient))
    
    summary_stats <- cbind(year = year_num, summary_stats)
    summary_stats_all_years <- rbind(summary_stats_all_years, summary_stats)
  }
  
  return(summary_stats_all_years)
}

# Set to last 10 years for ease of visualising data
years <- 2013:2023

# Loading votes dataset
votes_data <- read.csv("data/votes.csv")
# Filtering dataset, removing 'wld' records, and uppercasing
votes_data <- votes_data %>%
  filter(!is.na(from_country) & !is.na(to_country) & 
           from_country != "wld" & to_country != "wld") %>%
  mutate(from_country = toupper(from_country),
         to_country = toupper(to_country))

# Loading random votes dataset
random_votes_data <- read.csv("outputs/random_votes.csv")
# Filtering dataset, removing 'wld' records, and uppercasing
random_votes_data <- random_votes_data %>%
  filter(!is.na(from_country) & !is.na(to_country) & 
           from_country != "wld" & to_country != "wld") %>%
  mutate(from_country = toupper(from_country),
         to_country = toupper(to_country))

# Summary statistics for actual data
summary_stats_votes <- calculate_summary_stats(votes_data, years)
print("Summary statistics for actual data (2013-2023):")
print(summary_stats_votes)

# Summary statistics for random data
summary_stats_random_votes <- calculate_summary_stats(random_votes_data, years)
print("Summary statistics for random data (2013-2023):")
print(summary_stats_random_votes)

write.csv(summary_stats_votes, "outputs/clustering_stats_votes.csv", row.names = FALSE)
write.csv(summary_stats_random_votes, "outputs/clustering_stats_random_votes.csv", row.names = FALSE)

"
  ========================================
  clustering Coefficient Analysis
  - Combined Years
  - Random and Actual Data
  ========================================
"

"
  For Actual Data
"

# Re-loading votes dataset
votes_data <- read.csv("data/votes.csv")
# Filtering dataset, removing 'wld' records, and uppercasing
votes_data <- votes_data %>%
  filter(!is.na(from_country) & !is.na(to_country) & 
           from_country != "wld" & to_country != "wld") %>%
  mutate(from_country = toupper(from_country),
         to_country = toupper(to_country))

# Edge list
edges <- subset(votes_data, select = c("from_country", "to_country"))

# Creating the graph
g <- graph_from_data_frame(edges, directed = TRUE)

# Clustering coefficient for each node
clustering_coefficients <- transitivity(g, type = "local", isolates = "zero")

# Adding clustering coefficients to DataFrame for better visualisation
clustering_df <- data.frame(country = V(g)$name, clustering_coefficient = clustering_coefficients)

# Ensuring "WLD" is not present -- 2023 data
clustering_df <- clustering_df %>% filter(country != "wld")

# Summary statistics
summary_stats <- clustering_df %>%
  summarize(mean = mean(clustering_coefficient),
            median = median(clustering_coefficient),
            sd = sd(clustering_coefficient))
print(summary_stats)

write.csv(clustering_df, "outputs/clustering_coefficients.csv", row.names = FALSE)


"
  For Random Data
"

# Re-loading random votes dataset
random_votes_data <- read.csv("outputs/random_votes.csv")

edges <- subset(random_votes_data, select = c("from_country", "to_country"))  # Edge list
g <- graph_from_data_frame(edges, directed = TRUE)  # Creating the graph

# Clustering coefficient for each node
clustering_coefficients <- transitivity(g, type = "local", isolates = "zero")

# Adding clustering coefficients to DataFrame for better visualisation
clustering_df <- data.frame(country = V(g)$name, clustering_coefficient = clustering_coefficients)

# Ensuring "WLD" is not present -- 2023 data
clustering_df <- clustering_df %>% filter(country != "wld")

# Summary statistics
summary_stats <- clustering_df %>%
  summarize(mean = mean(clustering_coefficient),
            median = median(clustering_coefficient),
            sd = sd(clustering_coefficient))
print(summary_stats)

write.csv(clustering_df, "outputs/random_votes_clustering_coefficients.csv", row.names = FALSE)

"
  Plotting the clustering coefficient
"

# Getting the region colour based on country
# Regions based on: https://doi.org/10.1007/s42001-018-0020-2
get_region_colour <- function(country_code) {
  if (country_code %in% c("PT", "ES", "MT", "SM", "AD", "MC", "MA", "IT")) {
    return("red")
  } else if (country_code %in% c("GB", "IE", "BE", "FR", "LU")) {
    return("turquoise")
  } else if (country_code %in% c("IS", "DK", "NO", "SE", "FI", "SK")) {
    return("lightslateblue")
  } else if (country_code %in% c("DE", "AT", "NL", "CH", "SI", "CZ", "HU")) {
    return("gray")
  } else if (country_code %in% c("GR", "ME", "CY", "AL", "BG", "HR", "BA", "TR", "MK", "RO", "RS", "IL", "YU")) {
    return("orange")
  } else if (country_code %in% c("RU", "UA", "MD", "BY", "PL", "GE", "AM", "AZ", "EE", "LT", "LV")) {
    return("green")
  } else if (country_code == "AU") {
    return("white")
  } else {
    return("pink") # Default colour for unknown regions
  }
}

# Creating a new column for region based on the country code
clustering_df$region <- sapply(clustering_df$country, get_region_colour)

# Sorting the data by region, and then alphabetically within each region
clustering_df <- clustering_df[order(clustering_df$region, clustering_df$country),]

# Column graph
ggplot(clustering_df, aes(x = reorder(country, clustering_coefficient), y = clustering_coefficient, fill = region)) +
  geom_col() +
  scale_fill_identity() +
  labs(title = "Clustering Coefficients by Country and Region",
       x = "Country",
       y = "Clustering Coefficient",
       fill = "Region") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

"
  ====================================
  Community Detection - Louvain Method
  ====================================
"

# Re-loading the votes dataset
votes <- read.csv("data/votes.csv")

set.seed(123)  # Setting a specific random seed for reproducibility

# Preprocessing the data, removing "WLD". and filtering years to the last 20 years
votes_filtered <- votes %>%
  filter(!is.na(from_country) & !is.na(to_country) & 
           from_country != "wld" & to_country != "wld" &
           year >= 2003 & year <= 2023) %>%
  mutate(from_country = toupper(from_country),
         to_country = toupper(to_country))

# Aggregating the votes between each pair of countries
votes_aggregated <- votes_filtered %>%
  group_by(from_country, to_country) %>%
  summarize(total_votes = sum(total_points), .groups = 'drop')

# Extracting relevant columns for graph
edges <- votes_aggregated[, c("from_country", "to_country", "total_votes")]

# Graph object with weights
g <- graph_from_data_frame(edges, directed = TRUE, vertices = NULL)
E(g)$weight <- edges$total_votes

# Converting directed graph to undirected
g_undirected <- as.undirected(g, mode = "collapse", edge.attr.comb = "sum")

# Community detection using Louvain method
louvain <- cluster_louvain(g_undirected, weights = E(g_undirected)$weight)
num_communities <- length(membership(louvain))  # Number of communities detected

# List of clustering with the countries they contain
clustering <- split(names(membership(louvain)), membership(louvain))
print(clustering)

# Output:
# [1] "AD" "ES" "FR" "MC" "PT"
# [2] "AL" "AT" "BA" "BG" "CH" "CS" "HR" "ME" "MK" "RS" "SI" "TR"
# [3] "AM" "AZ" "BY" "CY" "GE" "GR" "IL" "IT" "LT" "LV" "MD" "MT" "RO" "RU" "SM" "UA"
# [4] "AU" "BE" "CZ" "DE" "DK" "EE" "FI" "GB" "HU" "IE" "IS" "NL" "NO" "PL" "SE" "SK"

# Network graph
plot(louvain, g_undirected, vertex.color = membership(louvain), vertex.label = V(g_undirected)$name)

"
  Mapping the communities
"
# While I am sure that there are easier ways to plot these clusters onto a map,
# nothing seemed to have been working :).
# Map adapted from: https://warin.ca/posts/rcourse-datavisualizationwithr-maps/

# Defining the clusters manually
first_cluster <- c("Andorra", "Spain", "France", "Monaco", "Portugal")
second_cluster <- c("Albania", "Austria", "Bosnia and Herzegovina", "Bulgaria", "Switzerland",
                    "Serbia and Montenegro", "Croatia", "Montenegro", "North Macedonia", 
                    "Serbia", "Slovenia", "Turkey")
third_cluster <- c("Armenia", "Azerbaijan", "Belarus", "Cyprus", "Georgia", "Greece", 
                     "Israel", "Italy", "Lithuania", "Latvia", "Moldova", "Malta", 
                     "Romania", "Russia", "San Marino", "Ukraine")
fourth_cluster <- c("Australia", "Belgium", "Czech Republic", "Germany", "Denmark", 
                      "Estonia", "Finland", "UK", "Hungary", "Ireland", 
                      "Iceland", "Netherlands", "Norway", "Poland", "Sweden", "Slovakia")

# Filtering map data to include only European countries
world_map <- map_data(map = "world")
europe <- subset(world_map, region %in% c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan",
                                          "Belarus", "Belgium", "Bosnia and Herzegovina", "Bulgaria",
                                          "Croatia", "Cyprus", "Czech Republic","Denmark","Estonia","Finland", 
                                          "France","Georgia", "Germany", "Greece","Hungary","Iceland", 
                                          "Ireland", "Italy", "Kosovo", "Latvia","Liechtenstein", 
                                          "Lithuania", "Luxembourg","Malta","Moldova","Monaco","Montenegro",
                                          "North Macedonia", "Netherlands","Norway","Poland","Portugal","Romania",
                                          "Russia","San Marino","Serbia","Slovakia","Slovenia","Spain",
                                          "Sweden","Switzerland","Turkey","Ukraine","UK","Vatican", "Israel"))

# Plotting the map with coloured clusters
ggplot(data = europe, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill = ifelse(region %in% first_cluster, "red", 
                                 ifelse(region %in% second_cluster, "blue", 
                                        ifelse(region %in% third_cluster, "green",
                                               ifelse(region %in% fourth_cluster, "orange", "white"))))), color = "grey") +
  theme_void() +
  coord_fixed(ratio=1.6, xlim = c(-50, 80)) +
  guides(fill = FALSE)

