"
  borders_data_preparation.R
  ``````````````````````````
  Decription:
  
  This script is making use of GEODATASOURCE-COUNTRY-BORDERS.CSV and 
  contestants.csv in order to create a filtered list of Eurovision participants
  and their bordering neighbours.
  
  Datasets Used:
  ``````````````
  GEODATASOURCE-COUNTRY-BORDERS.CSV --> A dataset containing every country in 
  the world, and their bordering neighbours
  contestants.csv --> A list of every contestant in the Eurovision, from 1957
  until 2023
  
  Adding Information:
  ``````````````````
  As GEODATASOURCE-COUNTRY-BORDERS.CSV only contains countries that have a land
  border, neighbouring states such as Denmark-Sweden, Malta-Italy etc. were 
  ignored. These nations who neighbour each other by sea where included into the
  database. 
  Israel and Australia also received an 'NA' for country_border, so that they
  do not disappear from the dataset when their non-participanting neighbours
  are filtered out.
  
  Data Cleaning:
  ``````````````
  Various cleaning operations are performed on the data to fix inconsistencies 
  in country names. For example, 'Russian Federation' is replaced with 'Russia',
  'Bosnia and Herzegovina' is replaced with 'Bosnia & Herzegovina', etc.
  
  Filtering Data:
  ``````````````
  The script filters the country borders dataset based on whether the country 
  names are present in the unique list of countries that have participated in 
  the ESC This involves filtering both the country_name column and the 
  country_border_name column.
  
  
  The final output was placed into 'filtered_countryborders_data', consisting of
  the columns 'country_code', 'country_name', 'country_border_code' and 
  'country_border_name', consisting of only countries which at some point
  participated in the ESC.
"

"
  Country borders from GEODATASOURCE-COUNTRY-BORDERS.csv
"

# Load the country borders dataset
# Source: https://github.com/geodatasource/country-borders
countryborders_dataset <- read.csv("data/GEODATASOURCE-COUNTRY-BORDERS.CSV")

# Adding bordering countries of extinct nations
new_records <- data.frame(
  country_code = rep("YU", 6),
  country_name = rep("Yugoslavia", 6),
  country_border_code = c("IT", "AT", "RO", "BG", "GR", "AL"),
  country_border_name = c("Italy", "Austria", "Romania", "Bulgaria", "Greece", "Albania")
)

# Duplicate new_records and swap country_code and country_border_code
new_records_swapped <- new_records[, c("country_border_code", "country_border_name", "country_code", "country_name")]
colnames(new_records_swapped) <- colnames(new_records)

# Add new records to the countryborders_dataset
countryborders_dataset <- rbind(countryborders_dataset, new_records, new_records_swapped)


# Adding neighbours not connected by land

# Malta to Italy and vice-versa
malta_italy <- data.frame(
  country_code = c("MT", "IT"),
  country_name = c("Malta", "Italy"),
  country_border_code = c("IT", "MT"),
  country_border_name = c("Italy", "Malta")
)
# Denmark to Norway and Sweden and Vice-Versa
denmark_norway_sweden <- data.frame(
  country_code = c("DK", "DK", "NO", "NO", "SE", "SE"),
  country_name = c("Denmark", "Denmark", "Norway", "Norway", "Sweden", "Sweden"),
  country_border_code = c("NO", "SE", "DK", "SE", "DK", "NO"),
  country_border_name = c("Norway", "Sweden", "Denmark", "Sweden", "Denmark", "Norway")
)
# UK to Iceland and vice-versa
uk_iceland <- data.frame(
  country_code = c("GB", "IS"),
  country_name = c("United Kingdom", "Iceland"),
  country_border_code = c("IS", "GB"),
  country_border_name = c("Iceland", "United Kingdom")
)
# Iceland to Norway and vice-versa
iceland_norway <- data.frame(
  country_code = c("IS", "NO"),
  country_name = c("Iceland", "Norway"),
  country_border_code = c("NO", "IS"),
  country_border_name = c("Norway", "Iceland")
)
# Cyprus to Turkey and vice-versa
cyprus_turkey <- data.frame(
  country_code = c("CY", "TR"),
  country_name = c("Cyprus", "Turkey"),
  country_border_code = c("TR", "CY"),
  country_border_name = c("Turkey", "Cyprus")
)
# Israel
israel <- data.frame(
  country_code = c("IL", NA),
  country_name = c("Israel", NA),
  country_border_code = NA,
  country_border_name = NA
)
# Australia
australia <- data.frame(
  country_code = c("AU", NA),
  country_name = c("Australia", NA),
  country_border_code = NA,
  country_border_name = NA
)

# Add new records to the countryborders_dataset
countryborders_dataset <- rbind(countryborders_dataset, malta_italy, denmark_norway_sweden, cyprus_turkey, uk_iceland, iceland_norway, israel, australia)

# Define a function to replace country names and remove items within parentheses
# Todo: Replace -- done for Moldova
replace_country_names <- function(data, old_name, new_name) {
  data$country_name <- gsub("\\(.*?\\)", "", gsub(old_name, new_name, data$country_name))
  data$country_border_name <- gsub("\\(.*?\\)", "", gsub(old_name, new_name, data$country_border_name))
  return(data)
}

# Replace country names
countryborders_dataset <- replace_country_names(countryborders_dataset, "Russian Federation", "Russia")
countryborders_dataset <- replace_country_names(countryborders_dataset, "Bosnia and Herzegovina", "Bosnia & Herzegovina")
countryborders_dataset <- replace_country_names(countryborders_dataset, "Czechia", "Czech Republic")
countryborders_dataset <- replace_country_names(countryborders_dataset, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")

# Todo: Replace -- see replace_country_names function above
countryborders_dataset <- replace_country_names(countryborders_dataset, "Moldova", "Moldova")

# View the first few rows of the datasets
print(countryborders_dataset)

"
  List of unique contestants from contestants.csv dataset
"

# Load the contestants dataset
# Source: https://github.com/Spijkervet/eurovision-dataset
contestants_data <- read.csv("data/contestants.csv")

# Fixing country names, duplicates, & extinct countries
contestants_data$to_country <- gsub("North MacedoniaN.Macedonia", "North Macedonia", contestants_data$to_country)
contestants_data$to_country <- gsub("Czechia", "Czech Republic", contestants_data$to_country)
contestants_data$to_country <- gsub("Serbia & Montenegro", "Serbia", contestants_data$to_country)

# Get unique items in the "to_country" column
unique_countries <- unique(contestants_data$to_country)

print(unique_countries)

"
  Filtering the countries to those that have at some point been in the Eurovision
"

# Filter records based on whether country_name is in unique_countries list after trimming whitespace
filtered_countryborders_data <- countryborders_dataset[trimws(countryborders_dataset$country_name) %in% trimws(unique_countries), ]

# Filter out records where country_border_code is empty but not NA
filtered_countryborders_data <- filtered_countryborders_data[!(filtered_countryborders_data$country_border_code == "" & !is.na(filtered_countryborders_data$country_border_code)), ]

"
  Removing any remnants - extra spaces after word
"

# Apply trimws() to each column of filtered_countryborders_data
filtered_countryborders_data <- lapply(filtered_countryborders_data, function(x) trimws(x))

# Convert the list back to a dataframe
filtered_countryborders_data <- as.data.frame(filtered_countryborders_data)

# Print the updated filtered_countryborders_data
print(filtered_countryborders_data)
