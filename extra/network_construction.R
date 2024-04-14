"
  network_construction.R
  ``````````````````````
  Decription:
"

#install.packages('igraph')

# Load required libraries
library(igraph)

# Read data
borders_data <- read.csv("outputs/filtered_countryborders_data.csv")
languages_data <- read.csv("outputs/separated_langauges_coded.csv")

# Rename the column in languages_data
colnames(languages_data)[colnames(languages_data) == "Country_Code"] <- "country_code"

# Merge datasets
merged_data <- merge(borders_data, languages_data, by = "country_code")

# Create edge list based on voting patterns
# Here you would need the actual voting data to create edge weights
edge_list <- data.frame(
  from = merged_data$country_code,
  to = merged_data$country_border_code,
  weight = sample(1:12, nrow(merged_data), replace = TRUE) # Sample weights for demonstration
)

# Remove rows with missing values
edge_list <- na.omit(edge_list)

# Create the graph
voting_graph <- graph_from_data_frame(edge_list, directed = TRUE)

# Filter out nodes with low degree (few connections)
degree_threshold <- 2 # Adjust as needed
filtered_nodes <- names(V(voting_graph))[degree(voting_graph) > degree_threshold]
filtered_graph <- induced_subgraph(voting_graph, filtered_nodes)

# Plot the graph with improved layout and styling
plot(filtered_graph, 
     edge.arrow.size = 0.1, # Set edge arrow size
     vertex.label.cex = 0.8, # Set node label size
     vertex.size = 8, # Set node size
     vertex.label.dist = 1, # Increase label distance from nodes
     main = "Voting Network Graph", # Set main title
     layout = layout_with_fr) # Use force-directed layout for better visualization
