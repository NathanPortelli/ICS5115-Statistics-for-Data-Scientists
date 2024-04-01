"
  language_data_preparation.R
  ```````````````````````````
  Decription:
"

#install.packages('tidyr')

# Load the required library
library(stringr)
library(tidyr)


# Load the country languages dataset
# Source: https://www.kaggle.com/datasets/shubhamptrivedi/languages-spoken-across-various-nations
languages_dataset <- read.csv("data/countries-languages.csv")

# Change "Macedonia" to "North Macedonia" in the "Country" column
languages_dataset$Country <- gsub("Macedonia", "North Macedonia", languages_dataset$Country)

# Define a function to extract words starting with uppercase
extract_words <- function(text) {
  words <- unlist(str_extract_all(text, "\\b[A-Z][a-zA-Z]*\\b"))
  return(paste(words, collapse = " "))
}

# Apply the function to the Languages Spoken column
languages_dataset$Languages.Spoken <- sapply(languages_dataset$Languages.Spoken, extract_words)


"
  Taking only countries taking part in the ESC
"

# Filter languages_dataset with unique_countries
filtered_languages <- languages_dataset[languages_dataset$Country %in% unique_countries, ]

# Print the filtered dataset
print(filtered_languages)

"
  Splitting Languages
"
# Initialize an empty data frame to store the separated records
separated_langauges <- data.frame(Country = character(), Languages.Spoken = character(), stringsAsFactors = FALSE)

# Loop through each row in filtered_languages
for (i in 1:nrow(filtered_languages)) {
  country <- filtered_languages$Country[i]
  languages <- unlist(strsplit(filtered_languages$Languages.Spoken[i], " "))
  
  # Create a new row for each language spoken in the country
  for (language in languages) {
    separated_langauges <- rbind(separated_langauges, data.frame(Country = country, Languages.Spoken = language))
  }
}

# Reset row names
row.names(separated_langauges) <- NULL

# Print the separated records
print(separated_langauges)

"
  Converting to country codes
"

# Assuming you have loaded filtered_countryborders_data into a data frame called filtered_countryborders_data

# Remove duplicates from filtered_countryborders_data
filtered_countryborders_data_unique <- unique(filtered_countryborders_data[c("country_name", "country_code")])

# Perform a join operation
separated_langauges_coded <- merge(separated_langauges, filtered_countryborders_data_unique, by.x = "Country", by.y = "country_name", all.x = TRUE)

# Select relevant columns and rename
separated_langauges_coded <- separated_langauges_coded[, c("Country", "Languages.Spoken", "country_code")]
names(separated_langauges_coded)[3] <- "Country_Code"

# Print the merged data
print(separated_langauges_coded)





"
Not being used
==========

"



"
  List of langauges rather than countries
"

# Define a function to split languages and create a new dataset
create_languages_list <- function(data) {
  languages <- unlist(strsplit(data$Languages.Spoken, ", "))
  countries <- rep(data$Country, times = sapply(strsplit(data$Languages.Spoken, ", "), length))
  return(data.frame(Language = languages, Country = countries))
}

# Apply the function to the filtered dataset
languages_list <- create_languages_list(filtered_languages)

# Print the new dataset
print(languages_list)
