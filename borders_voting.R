"
  borders_voting.R
  ````````````````
  
  Description:
  ````````````
  After data preparation and pre-processing in 'borders_data_preparation.R',
  the processed information is then used by this script for visualisation
  through heatmaps and network graphs, as well as calculations and data 
  analysis with the goal of better analysing the voting patterns of countries 
  that physically border one another, e  ither by land or sea, to see whether
  they have a higher chance of voting for each other throughout the years.
  
  Datasets Used:
  ``````````````
  Processed data from 'borders_data_preparation.R' was used through their
  saved CSV file in order for this script to be able to run independent of 
  the global environment, for ease of use.
  'filtered_countryborders_data.csv' --> Contains the country code and
  name of each country and every country that is bordered by them.
  
  'votes.csv' --> Contains the voting history of each country from 1957-2023.
  Shows whether the vote was taken during a semi-final or final, the country
  which voted, and to whom, the total amount of points, and when relevant,
  the number of total points which were provided through a jury vote, and 
  which where received from televoting.
  
  Heatmaps:
  `````````
  A general Heat Map of the Voting Patterns in the ESC, with the x-axis 
  containing the country to which the votes were given, and the y-axis showing
  which country gave the vote. The higher the total votes provided, the more
  the colour intensity increases. If the colour intensity is diagonally 
  symmetrical, it means that the voting trend is reciprocated between the 
  two countries.
  
  For ease of analysis, the information gathered from this heatmap is 
  saved to 'outputs/heatmap_data.csv'.
  
  Network Graphs:
  ```````````````
  Network graphs were also used as a visualisation tool to represent and 
  analyse the relationships between the countries. In this script, these graphs
  are used to visualise the voting patterns of countries with a shared border,
  as characterised in 'borders_data_preparation.R'. The goal was to visually 
  explore if neighbouring countries exhibit higher instances of mutual voting.
  
  An undirected and unweighted network graph is also developed for 2023, as well
  as a directed and weighted network graph showing Poland's voting record in 
  2023, to better visualise the scale of relationships present in the dataset.
  
  Network Metrics
  ```````````````
  Two primary metrics were calculated, being Degree and Closeness Centrality.
  Betweeness Centrality was also measured, however this was not used in the 
  analysis and was merely done to show the high degree of connectiveness 
  between the countries in the dataset.
  
  Correlation Analysis
  ````````````````````
  Pearson's correlation coefficient are used to quantify the relationship 
  the proportion of neighbouring points relative to the total points.

  Statistical Tests
  `````````````````
  Two key tests are used to check whether the patterns observed are 
  statistically significant, or due to random change, being the Chi-Square Test 
  and the T-Test. 
"

# install.packages('stringi',type='win.binary')
# install.packages('ggplot2')
# install.packages('tidyr')
# install.packages('gtools')
# install.packages('mapproj')
# install.packages('igraph')
# install.packages('readr')
# install.packages('maps')
# install.packages('mapdata')
# install.packages('sf')
# install.packages('rnaturalearth')
# install.packages('rnaturalearthdata')

# Loading required packages
library(dplyr)
library(sf)
library(ggplot2)
library(igraph)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(maps)
library(mapdata)

"
  Loading datasets
"

# Loading the filtered_countryborders_data.csv file created from 
# 'borders_data_preparation.R' script.
filtered_countryborders_data <- read.csv("outputs/filtered_countryborders_data.csv")

# Loading the votes dataset
# Source: https://github.com/Spijkervet/eurovision-dataset
votes_data <- read.csv("data/votes.csv")

# Removing the unnecessary columns
votes_data <- votes_data[, !names(votes_data) %in% c("from_country_id", "to_country_id")]

# Replacing NA values in tele_points and jury_points with 0
votes_data$tele_points[is.na(votes_data$tele_points)] <- -1
votes_data$jury_points[is.na(votes_data$jury_points)] <- -1

# Replacing 'Serbia & Montenegro' with 'Serbia' 
# (check borders_data_preparation.R for explanation)
votes_data$from_country <- gsub("cs", "rs", votes_data$from_country)
votes_data$to_country <- gsub("cs", "rs", votes_data$to_country)

# Filtering out records coming from the non-participating world (WLD vote from 2023)
votes_data <- votes_data[!grepl("wld", votes_data$from_country, ignore.case = TRUE), ]
 
"
  Visualising the dataset
  A general Heat Map of the Voting Patterns in the ESC
"

# Aggregating votes_data to calculate total points given by each country to another
voting_matrix <- aggregate(total_points ~ from_country + to_country, data = votes_data, FUN = sum)

# Creating the heatmap using ggplot
heatmap <- ggplot(voting_matrix, aes(x = to_country, y = from_country, fill = total_points)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Voting Patterns in Eurovision Song Contest") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

print(heatmap)

# Extracting information to csv file
heatmap_data <- voting_matrix
write.csv(heatmap_data, "outputs/heatmap_data.csv", row.names = FALSE)

"
  Undirected & Unweighted Network Graph for the year 2023
"

# Increasing the size of the plot window
options(repr.plot.width=10, repr.plot.height=8)

# Filtering the data for the year 2023, final round, and total_points > 0
votes_2023 <- subset(votes_data, year == 2023 & total_points > 0 & round == 'final')

edges <- subset(votes_2023, select = c("from_country", "to_country"))

# Undirected graph
graph <- graph_from_data_frame(edges, directed = FALSE)

plot(
  graph,
  layout = layout.circle,
  vertex.label.dist = 0.5,
  vertex.label.cex = 0.8,
  edge.arrow.size = 0.5,
  edge.curved = FALSE,
  edge.color = "black",
  vertex.color = "lightblue",
  vertex.frame.color = "black"
)

"
  Directed & Weighted Network Graph for Poland in the year 2023
"

# Increasing the size of the plot window
options(repr.plot.width=10, repr.plot.height=8)

# Filtering the data for the year 2023, final round, total_points > 0, and using Poland as a filtered example
votes_2023_pl <- subset(votes_data, year == 2023 & total_points > 0 & round == 'final' & from_country == 'pl')

edges <- subset(votes_2023_pl, select = c("from_country", "to_country"))

# Directed graph
graph <- graph_from_data_frame(edges, directed = TRUE)

# Extracting total_points for each edge
edge_points <- as.character(votes_2023_pl$total_points)

plot(
  graph,
  layout = layout.circle,
  edge.label = edge_points,
  edge.label.cex = 0.8,
  edge.arrow.size = 0.5,
  edge.curved = FALSE,
  edge.color = "black",
  vertex.color = "lightblue",
  vertex.frame.color = "black"
)

"
  Network Graph - Voting Patterns of the ESC
"

# Aggregating total points for each combination of from_country and to_country
top_countries <- aggregate(total_points ~ from_country + to_country, data = votes_data, sum)

# Ordering in descending order of total points
top_countries <- top_countries[order(-top_countries$total_points), ]

# Grouping by from_country and selecting the top entries for each group 
# (n = top number of entries)
top_countries <- do.call(rbind, by(top_countries, top_countries$from_country, head, n = 1))

# Uppercasing the values to ensure consistency
top_countries$from_country <- toupper(top_countries$from_country)
top_countries$to_country <- toupper(top_countries$to_country)

# Checking if the combination of from_country and to_country exists in filtered_countryborders_data
top_countries$is_neighbour <- mapply(function(from, to) {
  any(filtered_countryborders_data$country_code == from & filtered_countryborders_data$country_border_code == to)
}, top_countries$from_country, top_countries$to_country)

top_countries$is_neighbour <- ifelse(top_countries$is_neighbour, "TRUE", "FALSE")

print(top_countries)

# Creating a DataFrame from top_countries
edge_list <- data.frame(
  from = top_countries$from_country,
  to = top_countries$to_country,
  is_neighbour = top_countries$is_neighbour
)

edge_list$is_neighbour <- as.logical(edge_list$is_neighbour)

graph <- graph_from_data_frame(edge_list, directed = TRUE)  # Undirected graph
plot(graph, 
     edge.color = ifelse(edge_list$is_neighbour, "green", "red"),
     vertex.color = "white",
     vertex.size = 13,
     vertex.label.cex = 0.7,
     edge.arrow.size = 0.2,
     main = "Network Graph of Bordering Countries",
     layout = layout_with_fr)

"
  Calculating percentage of neighbour voting for the top 3 points per country
"

total_rows <- nrow(top_countries)

neighbour_rows <- sum(top_countries$is_neighbour == "TRUE", na.rm = TRUE)

# Calculating the percentage
if (total_rows > 0) {
  percentage_neighbours <- (neighbour_rows / total_rows) * 100
} else {
  percentage_neighbours <- 0
}

# Percentage of rows where is_neighbour = TRUE (in %):
print(percentage_neighbours)

"
  Outputting to CSV
"

# DataFrame for vertices
vertices <- data.frame(
  country = c(unique(edge_list$from), unique(edge_list$to)),
  color = c(rep("green", length(unique(edge_list$from))), rep("red", length(unique(edge_list$to))))
)

write.csv(edge_list, "outputs/borders_voting_edges.csv", row.names = FALSE)

"
  European Map of countries that voted for 'Denmark'
"

votes_data <- read.csv("data/votes.csv")

# Creating a mapping of country codes to match the shapefile
country_code_mapping <- data.frame(
  from_country_id = c("ad", "al", "am", "at", "au", "az", "be", "bg", "hr", "cy", "cz", "dk", "ee", "fi", "fr", "ge", "de", "gr", "hu", "is", "ie", "il", "it", "lv", "lt", "lu", "mt", "md", "mc", "me", "nl", "mk", "no", "pl", "pt", "ro", "ru", "sm", "rs", "sk", "si", "es", "se", "ch", "ua", "gb"),
  GID_0 = c("AND", "ALB", "ARM", "AUT", "AUS", "AZE", "BEL", "BGR", "HRV", "CYP", "CZE", "DNK", "EST", "FIN", "FRA", "GEO", "DEU", "GRC", "HUN", "ISL", "IRL", "ISR", "ITA", "LVA", "LTU", "LUX", "MLT", "MDA", "MCO", "MNE", "NLD", "MKD", "NOR", "POL", "PRT", "ROU", "RUS", "SMR", "SRB", "SVK", "SVN", "ESP", "SWE", "CHE", "UKR", "GBR")
)

# Joining the votes data with the mapping
votes_to_at <- votes_data %>%
  filter(to_country_id == "dk") %>%
  group_by(from_country_id) %>%
  summarise(total_votes = sum(total_points, na.rm = TRUE)) %>%
  left_join(country_code_mapping, by = "from_country_id")

europe <- st_read("figures/europe/Europe_merged.shp")

# Merging the aggregated votes data with the shapefile using the mapped GID_0
europe_votes <- europe %>%
  left_join(votes_to_at, by = c("GID_0"))

summary(europe_votes$total_votes)

# Plotting the map using ggplot2
ggplot(data = europe_votes) +
  geom_sf(aes(fill = total_votes)) +
  scale_fill_continuous(name = "Total Votes for Denmark", 
                        low = "lightblue", 
                        high = "darkblue", 
                        na.value = "grey50") +
  theme_minimal() +
  labs(title = "Votes Given to Denmark by European Countries") +
  theme(legend.position = "bottom")

"
  Correlation Analysis
"

# Re-loading the datasets
votes_data <- read.csv("data/votes.csv")
borders_data <- read.csv("outputs/filtered_countryborders_data.csv")

# Preprocessing votes_data
votes_data <- votes_data %>%
  mutate(from_country = toupper(from_country),
         to_country = toupper(to_country))

# Preprocessing borders_data
borders_data <- borders_data %>%
  select(country_code, country_border_code)

# Merging votes_data with borders_data to identify neighboring votes
neighboring_votes <- votes_data %>%
  left_join(borders_data, by = c("from_country" = "country_code")) %>%
  mutate(is_neighbor = ifelse(!is.na(country_border_code), 1, 0))

# Calculating total points from neighboring and non-neighboring countries
neighboring_agg <- neighboring_votes %>%
  group_by(to_country) %>%
  summarise(total_points = sum(total_points),
            neighboring_points = sum(total_points * is_neighbor),
            non_neighboring_points = sum(total_points * (1 - is_neighbor)))

# Calculating the proportion of votes from neighboring countries
neighboring_agg <- neighboring_agg %>%
  mutate(proportion_neighboring = neighboring_points / total_points)

# Calculating correlation between total points and proportion of neighboring votes
correlation <- cor(neighboring_agg$total_points, neighboring_agg$proportion_neighboring, use = "complete.obs")
print(correlation)
# Output: 0.8393266

"
  The high positive correlation suggests that countries receiving 
  more total points tend to have a higher proportion of these points coming from 
  neighboring countries.
"

"
  Network Metrics [Degree Centrality / Closeness Centality / Betweenness Centality]
"

# Re-loading the voting data
votes_data <- read.csv("data/votes.csv")
# Keeping only information about countries and total points
votes_subset <- votes_data[, c("from_country", "to_country", "total_points")]

# Creating a directed graph from the voting data
graph <- graph_from_data_frame(votes_subset, directed = TRUE)

# Degree centrality
degree_centrality <- degree(graph, mode = "all")

# Closeness centrality
closeness_centrality <- closeness(graph, mode = "all")

# Betweenness centrality
betweenness_centrality <- betweenness(graph, normalized = TRUE)
print(betweenness_centrality)

"
  The reason for betweenness centrality being all extremely low is due to the
  structure of the network, since every country is highly connected to almost 
  every other country through the votes they provide to all of them, therefore 
  multiple paths between countries, no single country acts as a bridge 
  connecting others.
"

centrality_measures <- data.frame(
  country = V(graph)$name,
  degree_centrality = degree_centrality,
  closeness_centrality = closeness_centrality
)
write.csv(centrality_measures, "outputs/borders_centrality_measures.csv", row.names = FALSE)

"
  Statistical Tests: Chi-Square Test & T-Test
"

# Re-loading the data
filtered_countryborders_data <- read.csv("outputs/filtered_countryborders_data.csv")
votes <- read.csv("data/votes.csv")

# Uppercasing the data for consistency
votes <- votes %>%
  mutate(from_country = toupper(from_country),
         to_country = toupper(to_country))

# Filtering votes data to include only bordering countries
border_votes <- votes %>%
  inner_join(filtered_countryborders_data, by = c("from_country" = "country_code", "to_country" = "country_border_code")) %>%
  select(from_country, to_country, total_points)

# Contingency table
contingency_table <- table(border_votes$from_country, border_votes$to_country)

"
  The contingency table shows the total points given by each country to its 
  bordering neighbours. Each row represents a country and each column represents 
  its neighbour. The values in the table represent the total points given by the
  corresponding country to its neighbour.
"

"
  Chi-Square Test
"

chi_square_result <- chisq.test(contingency_table)
print(chi_square_result)

"
  The chi-square test was performed on the contingency table to determine whether 
  there is a significant association between countries sharing borders and the
  total points they give to each other. The p-value is less than 2.2e-16, 
  indicating a highly significant association. 
  Therefore, we reject the null hypothesis and conclude that there is a 
  significant association between countries sharing borders and the total points
  they give to each other.
"

"
  T-Test
"

# Calculate the average points given by neighboring and non-neighboring countries
neighboring_avg <- neighboring_votes %>%
  group_by(is_neighbor) %>%
  summarise(avg_points = mean(total_points, na.rm = TRUE),
            sd_points = sd(total_points, na.rm = TRUE),
            n = n())

# Extract the points given by neighboring and non-neighboring countries
neighbor_points <- neighboring_votes$total_points[neighboring_votes$is_neighbor == 1]
non_neighbor_points <- neighboring_votes$total_points[neighboring_votes$is_neighbor == 0]

# Perform t-test
t_test_result <- t.test(neighbor_points, non_neighbor_points)

# Print the results
print(t_test_result)


"
Output:

	Welch Two Sample t-test

data:  neighbor_points and non_neighbor_points
t = -2.9316, df = 1914.6, p-value = 0.003412
alternative hypothesis: true difference in means is not equal to 0
95 percent confidence interval:
 -0.5363390 -0.1063769
sample estimates:
mean of x mean of y 
 3.100864  3.422222 
"

#TODO: WHAT TO DO WITH AVG POINTS:

# Calculating the average total points given by bordering countries to each other
border_avg_points <- mean(border_votes$total_points)

# Calculating average total points given by all other countries
non_border_votes <- votes %>%
  anti_join(filtered_countryborders_data, by = c("from_country" = "country_code", "to_country" = "country_border_code")) %>%
  filter(!is.na(total_points))  # Removing NA values
non_border_avg_points <- mean(non_border_votes$total_points)

# Display average points
print(border_avg_points)
print(non_border_avg_points)
