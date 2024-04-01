"
  language_voting.R
  `````````````````
  Decription:
"


"
  The below has been copied from borders_voting.R
"

# Load the votes dataset
# Source: https://github.com/Spijkervet/eurovision-dataset
votes_data <- read.csv("data/votes.csv")

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
  todo: fix
"


# Filter top_countries to keep only records with same Languages.Spoken_to and Languages.Spoken_from
same_language_top_countries <- top_countries[top_countries$Languages.Spoken_from == top_countries$Languages.Spoken_to, ]

# Create a dataframe for edge list based on same_language_top_countries
edge_list <- data.frame(
  from = same_language_top_countries$from_country,
  to = same_language_top_countries$to_country,
  is_same_language = TRUE  # We already filtered for same language, so all connections are green
)

# Plot the network graph
graph <- graph_from_data_frame(edge_list, directed = FALSE)  # Undirected graph
plot(graph, 
     edge.color = ifelse(edge_list$is_same_language, "blue", "red"),  # Color edges green (same language) or red (different language)
     vertex.color = "white",  # Set node color
     vertex.size = 10,  # Set node size
     vertex.label.cex = 0.8,  # Set node label size
     edge.arrow.size = 0.5,  # Set edge arrow size
     main = "Network Graph of Countries",  # Set main title
     layout = layout_with_fr)  # Use force-directed layout for better visualization
