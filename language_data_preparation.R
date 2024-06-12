"
  language_data_preparation.R
  ```````````````````````````
  Decription:
  ```````````
  This script processes language data for Eurovision participants. It leverages 
  the `countries-languages.csv` and `unique_countries.csv` datasets to produce a
  comprehensive dataset that lists the languages spoken in countries 
  participating in the ESC.
  
  Datasets Used:
  ``````````````
  countries-languages.csv --> Contains information about the languages spoken
  in various countries around the world.
  unique_countries.csv --> Lists countries that have participated in the 
  ESC from 1957 to 2023. This information was processed in
  'borders_data_preparation.R' script.
  
  Adding Information:
  ```````````````````
  To ensure accuracy, the script modifies country names where necessary, such as
  changing 'Macedonia' to 'North Macedonia'. It also extracts and processes 
  language names to maintain consistency and clarity.
  
  Data Cleaning:
  ``````````````
  The script performs several data cleaning operations, including standardising 
  country names and extracting properly formatted language names from the raw 
  data. 
  It also ensures that only countries participating in the ESC are included in 
  the final dataset.
  
  Filtering Data:
  ``````````````
  The script filters the language data based on the list of Eurovision 
  participating countries. It then splits the languages spoken in each country 
  into individual records, creating a detailed dataset of languages associated 
  with each country.
  
  
  The final output is saved in `outputs/separated_languages_coded.csv`, 
  containing columns `Country`, `Languages.Spoken`, and `Country_Code`. 
  This processed dataset is saved to `outputs/separated_languages_coded.csv`.
"

# install.packages('stringr')
# install.packages('tidyr')
# install.packages('stringi',type='win.binary')

# Loading required packages
library(stringr)
library(tidyr)

# Loading the country languages dataset
# Source: https://www.kaggle.com/datasets/shubhamptrivedi/languages-spoken-across-various-nations
languages_dataset <- read.csv("data/countries-languages.csv")

# Changing "Macedonia" to "North Macedonia" for consistency
languages_dataset$Country <- gsub("Macedonia", "North Macedonia", languages_dataset$Country)

# Extract words starting with uppercase (sign that word is a language)
extract_words <- function(text) {
  matches <- gregexpr("\\b[A-Z][a-zA-Z]*\\b", text)
  words <- regmatches(text, matches)
  return(paste(unlist(words), collapse = " "))
}

# Applying the function to the Languages.Spoken column
languages_dataset$Languages.Spoken <- sapply(languages_dataset$Languages.Spoken, extract_words)

"
  Taking only countries taking part in the ESC
"

unique_countries <- read.csv("outputs/unique_countries.csv")

# Filtering languages_dataset with unique_countries
filtered_languages <- languages_dataset[languages_dataset$Country %in% unique_countries$unique_countries, ]
print(filtered_languages)

"
  Splitting Languages
"

# To store the separated records
separated_langauges <- data.frame(Country = character(), Languages.Spoken = character(), stringsAsFactors = FALSE)

# Looping through each row in filtered_languages
for (i in 1:nrow(filtered_languages)) {
  country <- filtered_languages$Country[i]
  languages <- unlist(strsplit(filtered_languages$Languages.Spoken[i], " "))
  
  # Creating a new row for each language spoken in the country
  for (language in languages) {
    separated_langauges <- rbind(separated_langauges, data.frame(Country = country, Languages.Spoken = language))
  }
}
# Reset row names
row.names(separated_langauges) <- NULL

print(separated_langauges)

"
  Converting to country codes
"

filtered_countryborders_data <- read.csv("outputs/filtered_countryborders_data.csv")

# Removing duplicates from filtered_countryborders_data
filtered_countryborders_data_unique <- unique(filtered_countryborders_data[c("country_name", "country_code")])

# Performing a join operation
separated_langauges_coded <- merge(separated_langauges, filtered_countryborders_data_unique, by.x = "Country", by.y = "country_name", all.x = TRUE)

# Selecting relevant columns and renaming
separated_langauges_coded <- separated_langauges_coded[, c("Country", "Languages.Spoken", "country_code")]
names(separated_langauges_coded)[3] <- "Country_Code"

print(separated_langauges_coded)

"
  Saving modified dataset to csv file
"

output_file <- "outputs/separated_langauges_coded.csv"
write.csv(separated_langauges_coded, file = output_file, row.names = FALSE)
