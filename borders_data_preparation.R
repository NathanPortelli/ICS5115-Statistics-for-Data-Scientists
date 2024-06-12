"
  borders_data_preparation.R
  ``````````````````````````
  Decription:
  ``````````
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
  participated in the ESC. This modified dataset is saved to 
  'outputs/filtered_countryborders_data.csv'.
"

"
  Country borders taken from 'GEODATASOURCE-COUNTRY-BORDERS.csv'
"

# Loading the country borders dataset
# Source: https://github.com/geodatasource/country-borders
countryborders_dataset <- read.csv("data/GEODATASOURCE-COUNTRY-BORDERS.CSV")

# Adding bordering countries of extinct nations
# Countries that were created from this nation were not included as neighbours
# of it.
new_records <- data.frame(
  country_code = rep("YU", 6),
  country_name = rep("Yugoslavia", 6),
  country_border_code = c("IT", "AT", "RO", "BG", "GR", "AL"),
  country_border_name = c("Italy", "Austria", "Romania", "Bulgaria", "Greece", "Albania")
)

# Duplicating new_records and swapping country_code and country_border_code
new_records_swapped <- new_records[, c("country_border_code", "country_border_name", "country_code", "country_name")]
colnames(new_records_swapped) <- colnames(new_records)

# Adding new_records to the countryborders_dataset
countryborders_dataset <- rbind(countryborders_dataset, new_records, new_records_swapped)

"
  The dataset used only considered countries with land borders as neighbours.
  This does not accurately portray the state of Europe, with many countries
  sharing a common identity despite being connected only by sea.
  Therefore, the following countries were added into this dataframe as neighbours
  as well.
"

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
# France to United Kingdom and vice-versa
france_uk <- data.frame(
  country_code = c("FR", "GB"),
  country_name = c("France", "United Kingdom"),
  country_border_code = c("GB", "FR"),
  country_border_name = c("United Kingdom", "France")
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
# The following countries were included with no neighbours
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

# Addding the new records
countryborders_dataset <- rbind(countryborders_dataset, france_uk, malta_italy, denmark_norway_sweden, cyprus_turkey, uk_iceland, iceland_norway, israel, australia)

"
  Cleaning up the data
"

# Fixing the output for 'Moldova' due to issues with formatting on dataset
replace_country_names <- function(data, old_name, new_name) {
  data$country_name <- gsub("\\(.*?\\)", "", gsub(old_name, new_name, data$country_name))
  data$country_border_name <- gsub("\\(.*?\\)", "", gsub(old_name, new_name, data$country_border_name))
  return(data)
}

# Formalising names of countries with multiple used names
countryborders_dataset <- replace_country_names(countryborders_dataset, "Russian Federation", "Russia")
countryborders_dataset <- replace_country_names(countryborders_dataset, "Bosnia and Herzegovina", "Bosnia & Herzegovina")
countryborders_dataset <- replace_country_names(countryborders_dataset, "Czechia", "Czech Republic")
countryborders_dataset <- replace_country_names(countryborders_dataset, "United Kingdom of Great Britain and Northern Ireland", "United Kingdom")

# Fixing the output for Moldova
countryborders_dataset <- replace_country_names(countryborders_dataset, "Moldova", "Moldova")

print(countryborders_dataset)

"
  List of unique contestants from contestants.csv dataset
"

# Loading the contestants dataset
# Source: https://github.com/Spijkervet/eurovision-dataset
contestants_data <- read.csv("data/contestants.csv")

# Fixing & formalising names of countries with multiple used names
contestants_data$to_country <- gsub("North MacedoniaN.Macedonia", "North Macedonia", contestants_data$to_country)
contestants_data$to_country <- gsub("Czechia", "Czech Republic", contestants_data$to_country)

"
  While it was determined that the data from Yugoslavia should not be attributed
  to a modern state, the 'Union State of Serbia & Montenegro' was heavily
  dominated by the modern state of Serbia, and therefore, to reduce confusion
  in the output, the two years that the Union State participated in the ESC
  were completely attributed to Serbia.
"

# Data from the 'Union State of Serbia & Montenegro' contributed to 'Serbia'
contestants_data$to_country <- gsub("Serbia & Montenegro", "Serbia", contestants_data$to_country)

"
  A list of every country that has ever participated in the ESC is extracted
  and saved to 'unique_countries.csv' to be used in various stages of this
  research to filter out countries that have never participated from lists.
"

# Unique items in the "to_country" column
unique_countries <- unique(contestants_data$to_country)
print(unique_countries) 

# Dataframe
unique_countries_df <- data.frame(unique_countries)
names(unique_countries_df) <- "unique_countries"

# Outputting to csv
output_file <- "outputs/unique_countries.csv"
write.csv(unique_countries_df, file = output_file, row.names = FALSE)

"
  Filtering the countries to those that have at some point been in the ESC
"

# Filtering records based on whether country_name is in unique_countries list after trimming whitespace
filtered_countryborders_data <- countryborders_dataset[trimws(countryborders_dataset$country_name) %in% trimws(unique_countries), ]

# Filtering out records where country_border_code is empty but not NA (due to Australia and Israel)
filtered_countryborders_data <- filtered_countryborders_data[!(filtered_countryborders_data$country_border_code == "" & !is.na(filtered_countryborders_data$country_border_code)), ]

"
  Removing any remnants - extra spaces after words
"

filtered_countryborders_data <- lapply(filtered_countryborders_data, function(x) trimws(x))
# Converting list back to a DataFrame
filtered_countryborders_data <- as.data.frame(filtered_countryborders_data)

print(filtered_countryborders_data)

"
  Saving final modified dataset to 'filtered_countryborders_data.csv'
"

output_file <- "outputs/filtered_countryborders_data.csv"
write.csv(filtered_countryborders_data, file = output_file, row.names = FALSE)