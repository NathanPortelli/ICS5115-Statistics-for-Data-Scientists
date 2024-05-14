"
  ethnic_data_preparation.R
  `````````````````````````
  Decription:
"

"
  todo: issue ... Need to get the dominant ethnicity from each country to determine who the minorities
  from other countries are voting for.....
"







"
  Loading the datasets
"

# Load the country languages dataset
# Source: https://www.cia.gov/the-world-factbook/about/archives/2022/field/ethnic-groups
# Load the dataset
ethnic_dataset <- read.csv("data/cia-factbook-ethnic-groups.csv")

"
  Removing unnecessary characters from Ethnic Group column
"

# Function to clean ethnic group names
clean_ethnic_group <- function(ethnic_group) {
  # Use regular expression to remove unwanted characters and spaces
  cleaned_group <- gsub("[\"~%().;:,-<>]", "", ethnic_group)  # Include " character to be removed
  cleaned_group <- gsub("\\b(?![A-Z])\\S+\\b", "", cleaned_group, perl = TRUE)
  # Remove specific words
  cleaned_group <- gsub("\\bEU\\b", "", cleaned_group, ignore.case = TRUE)  # Remove "EU" (case-insensitive)
  cleaned_group <- gsub("\\bEuropean\\b", "", cleaned_group, ignore.case = TRUE)  # Remove "European" (case-insensitive)
  cleaned_group <- gsub("\\Soviet Union\\b", "", cleaned_group, ignore.case = TRUE)
  cleaned_group <- gsub("\\North Macedonia\\b", "", cleaned_group, ignore.case = TRUE)
  # Additional cleaning for encoding issues
  cleaned_group <- iconv(cleaned_group, from = "UTF-8", to = "ASCII", sub = " ")  # Convert to ASCII
  # Remove leading and trailing whitespaces and extra spaces between words
  cleaned_group <- gsub("\\s+", " ", cleaned_group)
  cleaned_group <- trimws(cleaned_group)
  return(cleaned_group)
}

# Apply the cleaning function to the Ethnic Group column
ethnic_dataset$Ethnic.group <- sapply(ethnic_dataset$Ethnic.group, clean_ethnic_group)

# Print the cleaned dataset
print(ethnic_dataset)

"
  Taking only countries taking part in the ESC
"

unique_countries <- read.csv("outputs/unique_countries.csv")

# Filter ethnic_dataset with unique_countries
filtered_ethnic <- ethnic_dataset[ethnic_dataset$Country %in% unique_countries$unique_countries, ]

# Print the filtered dataset
print(filtered_ethnic)


"
  Splitting Ethnicities
"

# Initialize an empty data frame to store the separated records
separated_ethnicities <- data.frame(Country = character(), Ethnic.group = character(), stringsAsFactors = FALSE)

# Loop through each row in filtered_ethnic
for (i in 1:nrow(filtered_ethnic)) {
  country <- filtered_ethnic$Country[i]
  ethnicgroups <- unlist(strsplit(filtered_ethnic$Ethnic.group[i], " "))
  
  # Create a new row for each ethnicity in the country
  for (ethnicgroup in ethnicgroups) {
    separated_ethnicities <- rbind(separated_ethnicities, data.frame(Country = country, Ethnic.group = ethnicgroup))
  }
}

# Reset row names
row.names(separated_ethnicities) <- NULL

# Print the separated records
print(separated_ethnicities)


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
separated_ethnicities_coded <- merge(separated_ethnicities, filtered_countryborders_data_unique, by.x = "Country", by.y = "country_name", all.x = TRUE)

# Select relevant columns and rename
separated_ethnicities_coded <- separated_ethnicities_coded[, c("Country", "Ethnic.groups", "country_code")]
names(separated_ethnicities_coded)[3] <- "Country_Code"

# Print the merged data
print(separated_ethnicities_coded)

"
  Saving modified dataset to csv file
"

# Save filtered_ethnic to a CSV file
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

# Apply the function to the Ethnic.group column
separated_ethnicities_coded$Ethnic.group <- sapply(separated_ethnicities_coded$Ethnic.group, remove_specific_words)


# Removing any record with the word "Republic" in the "Ethnic.group" column
separated_ethnicities_coded <- separated_ethnicities_coded[!grepl("Republic", separated_ethnicities_coded$Ethnic.group), ]

# Changing "AsianAsian" to "Asian"
separated_ethnicities_coded$Ethnic.group <- gsub("AsianAsian", "Asian", separated_ethnicities_coded$Ethnic.group)

# Modified dataset
write.csv(separated_ethnicities_coded, "outputs/separated_ethnicities_coded.csv", row.names = FALSE)


