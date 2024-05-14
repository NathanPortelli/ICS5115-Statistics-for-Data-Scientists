"
  language_data_preparation.R
  ```````````````````````````
  Decription:
"

#install.packages('stringr')
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

unique_countries <- read.csv("outputs/unique_countries.csv")

# Filter languages_dataset with unique_countries
filtered_languages <- languages_dataset[languages_dataset$Country %in% unique_countries$unique_countries, ]

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

# Assuming borders_data_preparation.R has been run and 
# filtered_countryborders_data.csv exists in outputs folder

# Read filtered_countryborders_data from CSV file
filtered_countryborders_data <- read.csv("outputs/filtered_countryborders_data.csv")

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
  Saving modified dataset to csv file
"

# Save separated_langauges_coded as a CSV file
output_file <- "outputs/separated_langauges_coded.csv"
write.csv(separated_langauges_coded, file = output_file, row.names = FALSE)






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
