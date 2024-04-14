"
  language_voting.R
  `````````````````
  Decription:
"

install.packages('igraph')

# Load the required library
library(igraph)



"
  The below has been copied from borders_voting.R
"

# Load the votes dataset
# Source: https://github.com/Spijkervet/eurovision-dataset
votes_data <- read.csv("data/votes.csv")

separated_langauges_coded <- read.csv("outputs/separated_langauges_coded.csv")

# Remove the columns "from_country_id" and "to_country_id"
votes_data <- votes_data[, !names(votes_data) %in% c("from_country_id", "to_country_id")]

# Replace NA values in tele_points and jury_points with 0
votes_data$tele_points[is.na(votes_data$tele_points)] <- -1
votes_data$jury_points[is.na(votes_data$jury_points)] <- -1

# Aggregate total points for each combination of from_country and to_country
top_countries <- aggregate(total_points ~ from_country + to_country, data = votes_data, sum)

# Order the dataframe in descending order of total points
top_countries <- top_countries[order(-top_countries$total_points), ]

# Group by from_country and select the top 3 entries for each group
top_countries <- do.call(rbind, by(top_countries, top_countries$from_country, head, n = 3))

# Uppercase the values of "from_country" and "to_country" columns in top_countries
top_countries$from_country <- toupper(top_countries$from_country)
top_countries$to_country <- toupper(top_countries$to_country)

# Merge top_countries with separated_langauges_coded to obtain country codes
top_countries <- merge(top_countries, separated_langauges_coded, by.x = "from_country", by.y = "Country_Code", all.x = TRUE)
top_countries <- merge(top_countries, separated_langauges_coded, by.x = "to_country", by.y = "Country_Code", all.x = TRUE, suffixes = c("_from", "_to"))


"
  Share of same languages
"

# Check if the countries share the same language
top_countries$is_same_language <- top_countries$Languages.Spoken_from == top_countries$Languages.Spoken_to

# Convert logical values to TRUE/FALSE
top_countries$is_same_language <- ifelse(top_countries$is_same_language, "TRUE", "FALSE")

# Print the updated top_countries
print(top_countries)


"
  Plotting
"


# Create a dataframe for edge list based on unique edge connections
unique_edges <- unique(top_countries[, c("from_country", "to_country", "is_same_language")])
edge_list_lang <- data.frame(
  from = unique_edges$from_country,
  to = unique_edges$to_country,
  is_same_language = unique_edges$is_same_language
)

# Convert is_same_language column to logical
edge_list_lang$is_same_language <- as.logical(edge_list_lang$is_same_language)

# Create a graph object
graph_lang <- graph_from_data_frame(edge_list_lang, directed = FALSE)

# Plot the network graph
plot(graph_lang, 
     edge.color = ifelse(edge_list_lang$is_same_language, "green", "red"),  
     vertex.color = ifelse(degree(graph_lang) > 0, ifelse(edge_list_lang$from %in% get.edgelist(graph_lang)[,1] & edge_list_lang$is_same_language, "green", "red"), "white"),  
     vertex.size = 10,  
     vertex.label.cex = 0.8,  
     edge.arrow.size = 0.5,  
     main = "Network Graph of Countries with the Same Language",  
     layout = layout_with_fr(graph_lang))

"
  Percentage
"

# Calculate the total number of rows
total_rows_lang <- nrow(top_countries)

# Calculate the number of rows where is_same_language is TRUE
same_language_rows <- sum(top_countries$is_same_language == "TRUE", na.rm = TRUE)

# Calculate the percentage
if (total_rows_lang > 0) {
  percentage_same_language <- (same_language_rows / total_rows_lang) * 100
} else {
  percentage_same_language <- 0
}

# Output the percentage
print(paste("Percentage of rows where is_same_language = TRUE:", percentage_same_language, "%"))

# Export edge list to CSV
write.csv(edge_list_lang, "outputs/same_language_edges.csv", row.names = FALSE)




"
  todo: Trying stuff out, fix the below:
"

"
  Centrality Measures
"

# Centrality Measures
# Degree Centrality
degree_cent <- degree(graph_lang)

# Betweenness Centrality
betweenness_cent <- betweenness(graph_lang)

# Closeness Centrality
closeness_cent <- closeness(graph_lang)

# Eigenvector Centrality
eigenvector_cent <- evcent(graph_lang)$vector

# Combine centrality measures into a data frame
centrality_measures <- data.frame(
  Country = V(graph_lang)$name,
  Degree = degree_cent,
  Betweenness = betweenness_cent,
  Closeness = closeness_cent,
  Eigenvector = eigenvector_cent
)

# Print centrality measures
print(centrality_measures)


"
  Community Detection
"

# Community Detection
# Using the Louvain algorithm for community detection
louvain_communities <- cluster_louvain(graph_lang)

# Get the membership vector
membership <- membership(louvain_communities)

# Add community membership to the node attributes
V(graph_lang)$community <- membership

# Plot the graph with communities color-coded
plot(graph_lang, vertex.color = membership, 
     vertex.label.color = "black", 
     main = "Community Detection",
     layout = layout_with_fr(graph_lang))


"
  Temporal Analysis
"


