"
  ethnic_data_preparation.R
  `````````````````````````
  
  Decription:
  ```````````
  This script processes ethnic group data for countries participating in the 
  ESC. It uses the `cia-factbook-ethnic-groups.csv` and `unique_countries.csv` 
  datasets to create a refined dataset of ethnic groups for these countries.

  Datasets Used:
  ``````````````
  cia-factbook-ethnic-groups.csv --> Data scraped from cia.gov containing 
  information about ethnic groups in various countries. The Python script used
  to scrape the information can be found in 'extra\pyCIAEthnicGroups'.
  unique_countries.csv --> Lists countries that have participated in the ESC 
  from 1957 to 2023. This information was processed in 
  'borders_data_preparation.R' script.
  
  Data Cleaning:
  ``````````````
  The script cleans ethnic group names by removing unnecessary characters and 
  spaces, and specific words such as 'EU' and 'European'. The ethnicities are
  then split from one another, each being directly linked to the associated
  country.
  
  It was also deemed necessary to provide further cleaning of data after 
  manually reviewing the outputted dataset, such as the removal of duplicate 
  records, and removing extra wording such as '-born' from the end of the
  ethnicity.

  Filtering Data:
  ```````````````
  The script filters the ethnic data to include only countries participating in 
  the ESC. It splits ethnic groups into individual records to create a detailed 
  dataset.
  
  Integrating Data to Country Info:
  `````````````````````````````````
  The script matches country names with their respective country codes using a 
  pre-existing dataset, and integrates this information into the final dataset.


  The final output is saved in `separated_ethnicities_coded.csv`, containing 
  columns `Country`, `Ethnic.group`, and `Country_Code`. This processed dataset 
  is then saved to `outputs/separated_ethnicities_coded.csv`.
"

"
  Loading the datasets
"

# Loading the country languages dataset
# Source: https://www.cia.gov/the-world-factbook/about/archives/2022/field/ethnic-groups
ethnic_dataset <- read.csv("data/cia-factbook-ethnic-groups.csv")

"
  Removing unnecessary characters from Ethnic Group column
"

# Cleaning ethnic group names
clean_ethnic_group <- function(ethnic_group) {
  # Unwanted characters and spaces
  cleaned_group <- gsub("[\"~%().;:,-<>]", "", ethnic_group)
  cleaned_group <- gsub("\\b(?![A-Z])\\S+\\b", "", cleaned_group, perl = TRUE)
  # Removing specific words
  cleaned_group <- gsub("\\bEU\\b", "", cleaned_group, ignore.case = TRUE)
  cleaned_group <- gsub("\\bEuropean\\b", "", cleaned_group, ignore.case = TRUE)
  cleaned_group <- gsub("\\Soviet Union\\b", "", cleaned_group, ignore.case = TRUE)
  cleaned_group <- gsub("\\North Macedonia\\b", "", cleaned_group, ignore.case = TRUE)
  # Additional cleaning for encoding issues
  cleaned_group <- iconv(cleaned_group, from = "UTF-8", to = "ASCII", sub = " ")
  # Removeing leading and trailing whitespaces and extra spaces between words
  cleaned_group <- gsub("\\s+", " ", cleaned_group)
  cleaned_group <- trimws(cleaned_group)
  return(cleaned_group)
}

# Applying the cleaning function to the Ethnic Group column
ethnic_dataset$Ethnic.group <- sapply(ethnic_dataset$Ethnic.group, clean_ethnic_group)

"
  Taking only countries taking part in the ESC
"

unique_countries <- read.csv("outputs/unique_countries.csv")

# Filtering ethnic_dataset based on unique_countries
filtered_ethnic <- ethnic_dataset[ethnic_dataset$Country %in% unique_countries$unique_countries, ]

"
  Splitting Ethnicities
"

# Initialising an empty DataFrame to store the separated records
separated_ethnicities <- data.frame(Country = character(), Ethnic.group = character(), stringsAsFactors = FALSE)

# Looping through each row in filtered_ethnic
for (i in 1:nrow(filtered_ethnic)) {
  country <- filtered_ethnic$Country[i]
  ethnicgroups <- unlist(strsplit(filtered_ethnic$Ethnic.group[i], " "))
  
  # Creating a new row for each ethnicity in the country
  for (ethnicgroup in ethnicgroups) {
    separated_ethnicities <- rbind(separated_ethnicities, data.frame(Country = country, Ethnic.group = ethnicgroup))
  }
}

# Resetting row names
row.names(separated_ethnicities) <- NULL
print(separated_ethnicities)

"
  Converting to country codes
"

# Loading the filtered country borders dataset from 'borders_voting.R'
filtered_countryborders_data <- read.csv("outputs/filtered_countryborders_data.csv")

# Removing duplicates from filtered_countryborders_data
filtered_countryborders_data_unique <- unique(filtered_countryborders_data[c("country_name", "country_code")])

# Performing a join operation
separated_ethnicities_coded <- merge(separated_ethnicities, filtered_countryborders_data_unique, by.x = "Country", by.y = "country_name", all.x = TRUE)

# Selecting relevant columns and renaming
separated_ethnicities_coded <- separated_ethnicities_coded[, c("Country", "Ethnic.group", "country_code")]
names(separated_ethnicities_coded)[3] <- "Country_Code"

write.csv(separated_ethnicities_coded, "outputs/separated_ethnicities_coded.csv", row.names = FALSE)

"
  Further cleaning of data after manually reviewing the previous output
"

# Checking for duplicates
duplicate_rows <- duplicated(separated_ethnicities_coded)
if (any(duplicate_rows)) {
  cat("Duplicate row/s found.\n")
  separated_ethnicities_coded <- separated_ethnicities_coded[!duplicate_rows, ]
}

# For Israel's ethnicities: Removing "born" from ethnic group names
remove_specific_words <- function(ethnic_group) {
  words_to_remove <- c("born")
  cleaned_group <- gsub(paste0("\\b", paste(words_to_remove, collapse = "|"), "\\b"), "", ethnic_group, ignore.case = TRUE)
  return(cleaned_group)
}
separated_ethnicities_coded$Ethnic.group <- sapply(separated_ethnicities_coded$Ethnic.group, remove_specific_words)

# Removing any record with the word "Republic" in the "Ethnic.group" column
separated_ethnicities_coded <- separated_ethnicities_coded[!grepl("Republic", separated_ethnicities_coded$Ethnic.group), ]

# Changing "AsianAsian" to "Asian"
separated_ethnicities_coded$Ethnic.group <- gsub("AsianAsian", "Asian", separated_ethnicities_coded$Ethnic.group)

# Modified dataset
write.csv(separated_ethnicities_coded, "outputs/separated_ethnicities_coded.csv", row.names = FALSE)