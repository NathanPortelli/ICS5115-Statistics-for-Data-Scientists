"
  borders_voting.R
  
  In this script, we ...
"

#install.packages('stringi',type='win.binary')
#install.packages('ggplot2')
#install.packages('tidyr')
#install.packages('gtools')
#install.packages('rworldmap')
#install.packages('mapproj')
#install.packages('igraph')
#install.packages('readr')

library(ggplot2)
library(igraph)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)


"
  List of votes dataset
"

# Load the votes dataset
# Source: https://github.com/Spijkervet/eurovision-dataset
votes_data <- read.csv("data/votes.csv")

# Remove the columns "from_country_id" and "to_country_id"
votes_data <- votes_data[, !names(votes_data) %in% c("from_country_id", "to_country_id")]

# Replace NA values in tele_points and jury_points with 0
votes_data$tele_points[is.na(votes_data$tele_points)] <- -1
votes_data$jury_points[is.na(votes_data$jury_points)] <- -1

# Print the updated dataset
print(votes_data)

"
  Network Graph - Voting Patterns of the ESC
"

# Aggregate total points for each combination of from_country and to_country
top_countries <- aggregate(total_points ~ from_country + to_country, data = votes_data, sum)

# Order the dataframe in descending order of total points
top_countries <- top_countries[order(-top_countries$total_points), ]

# Group by from_country and select the top 3 entries for each group
top_countries <- do.call(rbind, by(top_countries, top_countries$from_country, head, n = 3))

# Step 1: Uppercase the from_country and to_country values
top_countries$from_country <- toupper(top_countries$from_country)
top_countries$to_country <- toupper(top_countries$to_country)

# Step 2: Check if the combination of from_country and to_country exists in filtered_countryborders_data
top_countries$is_neighbour <- mapply(function(from, to) {
  any(filtered_countryborders_data$country_code == from & filtered_countryborders_data$country_border_code == to)
}, top_countries$from_country, top_countries$to_country)

# Step 3: Convert logical values to TRUE/FALSE
top_countries$is_neighbour <- ifelse(top_countries$is_neighbour, "TRUE", "FALSE")

# Print the updated top_countries
print(top_countries)


# Step 2: Create a dataframe for edge list based on top_countries dataframe
edge_list <- data.frame(
  from = top_countries$from_country,
  to = top_countries$to_country,
  is_neighbour = top_countries$is_neighbour
)

# Step 3: Convert is_neighbour column to logical
edge_list$is_neighbour <- as.logical(edge_list$is_neighbour)

# Step 4: Plot the network graph
graph <- graph_from_data_frame(edge_list, directed = FALSE)  # Undirected graph
plot(graph, 
     edge.color = ifelse(edge_list$is_neighbour, "green", "red"),  # Color edges based on is_neighbour
     vertex.color = "white",  # Set node color
     vertex.size = 10,  # Set node size
     vertex.label.cex = 0.8,  # Set node label size
     edge.arrow.size = 0.5,  # Set edge arrow size
     main = "Network Graph of Countries",  # Set main title
     layout = layout_with_fr)  # Use force-directed layout for better visualization

# Percentage of neighbour voting for the top 3 points per country

# Calculate the total number of rows
total_rows <- nrow(top_countries)

# Calculate the number of rows where is_neighbour is TRUE
neighbour_rows <- sum(top_countries$is_neighbour == "TRUE", na.rm = TRUE)

# Calculate the percentage
if (total_rows > 0) {
  percentage_neighbours <- (neighbour_rows / total_rows) * 100
} else {
  percentage_neighbours <- 0
}

# Output the percentage
print(paste("Percentage of rows where is_neighbour = TRUE:", percentage_neighbours, "%"))


"
  Visualising the dataset
  
  Heat Map - Voting Patterns in the ESC
"

# Aggregate votes_data to calculate total points given by each country to another
voting_matrix <- aggregate(total_points ~ from_country + to_country, data = votes_data, FUN = sum)

# Create a heatmap
heatmap <- ggplot(voting_matrix, aes(x = to_country, y = from_country, fill = total_points)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Voting Patterns in Eurovision Song Contest") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

# Display the heatmap
print(heatmap)






"
Extra:
=========================================
"













# TOP 20 GENERAL RELATIONS

# Aggregate votes_data to calculate total points given by each country to another
voting_matrix <- aggregate(total_points ~ from_country + to_country, data = votes_data, FUN = sum)

# Find the top 20 relations based on total points
top_relations <- head(voting_matrix[order(voting_matrix$total_points, decreasing = TRUE), ], 50)

# Create a network graph
graph <- graph_from_data_frame(top_relations, directed = TRUE)

# Plot the network graph
plot(graph, edge.arrow.size = 0.5, edge.color = "green", vertex.size = 8, vertex.label.dist = 1.5, vertex.label.cex = 0.7)


"
Non functional -- see above
"

## TOP 2 COUNTRIES COUNTRIES VOTE FOR

# Aggregate votes_data to calculate total points given by each country to another
voting_matrix <- aggregate(total_points ~ from_country + to_country, data = votes_data, FUN = sum)

# Find the top 3 relations for each country
top_relations <- by(voting_matrix, voting_matrix$from_country, function(x) head(x[order(x$total_points, decreasing = TRUE), ], 2))

# Combine top_relations into a single data frame
top_relations_df <- do.call(rbind, top_relations)

# Create a network graph
graph <- graph_from_data_frame(top_relations_df, directed = TRUE)

# Plot the network graph
plot(graph, edge.arrow.size = 0.5, edge.color = "green", vertex.size = 8, vertex.label.dist = 1.5, vertex.label.cex = 0.7)


# Add edge attribute to indicate neighboring countries
E(graph)$is_neighbour <- FALSE
for (i in 1:nrow(filtered_countryborders_data)) {
  from_country <- filtered_countryborders_data[i, "country_code"]
  to_country <- filtered_countryborders_data[i, "country_border_code"]
  if (from_country %in% V(graph)$name && to_country %in% V(graph)$name) {
    if (are.connected(graph, from_country, to_country)) {
      graph <- set.edge.attribute(graph, "is_neighbour", index = which(E(graph, from = from_country, to = to_country)), value = TRUE)
    }
  }
}

# Plot the network graph
plot(graph, edge.arrow.size = 0.5, vertex.size = 8, vertex.label.dist = 1.5, vertex.label.cex = 0.7,
     edge.color = ifelse(E(graph)$is_neighbour, "red", "green"), vertex.color = "white")



"
  Time Series Analysis - Voting Patterns of the ESC over time
"
# todo

"
  Cluster Analysis
"

# todo

"
  Geospacial Visualisation
"

# todo

