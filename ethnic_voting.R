"
  ethnic_voting.R
  ```````````````
  Decription:
"

"
  Loading the datasets
"

# Load the votes dataset
# Source: https://github.com/Spijkervet/eurovision-dataset
votes_data <- read.csv("data/votes.csv")

separated_ethnicities_coded <- read.csv("outputs/separated_ethnicities_coded.csv")

# Remove the columns "from_country_id" and "to_country_id"
votes_data <- votes_data[, !names(votes_data) %in% c("from_country_id", "to_country_id")]

# Filter out records coming from the non-participating world
votes_data <- votes_data[!grepl("wld", votes_data$from_country, ignore.case = TRUE), ]

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
  todo: issue ... need to get the dominant ethnicity from each country to determine who the minorities
  from other countries are voting for.....
"



# Create a dataframe for edge list based on unique edge connections
unique_edges <- unique(top_countries[, c("from_country", "to_country", "same_ethnic")])
edge_list_lang <- data.frame(
  from = unique_edges$from_country,
  to = unique_edges$to_country,
  is_same_language = unique_edges$is_same_language
)

# Convert is_same_language column to logical
edge_list_lang$is_same_language <- as.logical(edge_list_lang$is_same_language)

# Plot the network graph
plot(graph_lang, 
     edge.color = ifelse(edge_list_lang$is_same_language, "green", "red"),  # Color edges based on is_same_language
     vertex.color = ifelse(degree(graph_lang) > 0, ifelse(edge_list_lang$from %in% get.edgelist(graph_lang)[,1] & edge_list_lang$is_same_language, "green", "red"), "white"),  # Set node color based on edge connections
     vertex.size = 10,  # Set node size
     vertex.label.cex = 0.8,  # Set node label size
     edge.arrow.size = 0.5,  # Set edge arrow size
     main = "Network Graph of Countries with the Same Language",  # Set main title
     layout = layout_with_fr(graph_lang))  # Use force-directed layout for better visualization









