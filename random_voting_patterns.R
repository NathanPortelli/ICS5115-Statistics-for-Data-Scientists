"
  random_voting_patterns.R
  ````````````````````````
  Decription:

  Generates random voting patterns for Eurovision Song Contest years 1957-2023 based on specific rules and conditions.

  Setting up empty votes:
    The script loads two datasets: unique_countries.csv and votes.csv. It removes all votes and stores the modified dataset as empty_votes. 

  Shuffling To-Country:
    A function randomize_to_country is defined to randomize the to_country within each group of from_country, making use of unique_countries.csv.
    The function shuffles the indices of to_country within each group and assigns shuffled to_country based on the shuffled indices. 
    This function is applied to empty_votes to shuffle the to_country column.
    This is done to better ensure randomness.
    
  The changing rules on voting throughout the years:  
    1957 to 1961, 1967 to 1970, 1974:
      Random voting patterns are generated for these years with each country providing 10 points distributed among entries.
    1962:
      A voting system similar to previous is applied, where each country provides votes ranging from 1 to 3 points.
    1963:
      Similar to 1962, but votes range from 1 to 5 points.
    1964 to 1966:
      A unique voting system is implemented where countries provide votes of 0, 1, 3, 5, or 6 points. 
      Each country can provide either 1, 3 and 5 votes, or 3 and 6 votes, with a total of 9 votes.
      Certain rules govern the selection of points to maintain fairness.
    1971 to 1973:
      Each country rates entries with points ranging from 0 to 5.
    1975 to 2015:
      A voting system with available votes of 0 to 8, 10, and 12 points is implemented. 
      Countries provide random votes following these rules.
    2016 to 2023:
      Similar to 1975 to 2015, but with the addition of two new columns: tele_points and jury_points. 
      Random votes are generated for these years, and the tele and jury points are summed up to obtain the total points. 
      The script ensures each country provides a particular vote only once to each recipient.
      
  The script writes the resulting random voting patterns to a CSV file named random_votes.csv.
"

# install.packages('purrr')
# install.packages('dplyr')

library(dplyr)
library(purrr)

"
  Load datasets
"

# Load datasets
unique_countries <- read.csv("outputs/unique_countries.csv", header = TRUE)
actual_votes <- read.csv("data/votes.csv", header = TRUE)

# Remove unwanted columns and votes
empty_votes <- actual_votes %>%
  select(-from_country_id, -to_country_id) %>%
  mutate(
    total_points = ifelse(is.na(total_points), NA, ""),
    tele_points = ifelse(is.na(tele_points), NA, ""),
    jury_points = ifelse(is.na(jury_points), NA, "")
  )

"
  Shuffling to_country
"

# Function to randomize to_country within each group of from_country
randomize_to_country <- function(df) {
  # Group by from_country
  grouped_df <- df %>%
    group_by(year, round, from_country) 
  
  # Randomly shuffle the indices of to_country within each group
  shuffled_indices <- grouped_df %>%
    mutate(rand_index = sample(n(), n(), replace = FALSE)) %>%
    pull(rand_index)
  
  # Assign shuffled to_country based on shuffled indices
  df$to_country <- df$to_country[shuffled_indices]
  
  return(df)
}

# Apply the function to empty_votes
randomized_empty_votes <- empty_votes %>%
  group_by(year) %>%
  group_split() %>%
  map_dfr(randomize_to_country)

# Make sure to_country and from_country are not the same
empty_votes <- randomized_empty_votes %>%
  mutate(to_country = ifelse(to_country == from_country, lag(to_country), to_country))

# Ensure unique combinations of year, to_country, and from_country
unique_empty_votes <- randomized_empty_votes %>%
  distinct(year, to_country, from_country, .keep_all = TRUE)

write.csv(unique_empty_votes, "outputs/random_votes.csv", row.names = FALSE)

"
  1957 to 1961, 1974
"

# Function to generate random number with higher chance of being less than 5
generate_random_number <- function() {
  if (runif(1) < 0.9) {
    return(sample(0:4, 1))
  } else {
    return(sample(5:10, 1))
  }
}

# Initialize counters
current_country <- ""
remaining_votes <- 0

# Iterate over each row in empty_votes
for (i in 1:nrow(empty_votes)) {
  # Check if year is between 1957 and 1961
  if (empty_votes$year[i] >= 1957 && empty_votes$year[i] <= 1961 || empty_votes$year[i] >= 1967 && empty_votes$year[i] <= 1970 || empty_votes$year[i] == 1974) {
    # Check if from_country is different from previous record's from_country
    if (empty_votes$from_country[i] != current_country) {
      current_country <- empty_votes$from_country[i]
      remaining_votes <- 10
    }
    
    # Set total_points to random number
    if (remaining_votes > 0) {
      random_points <- generate_random_number()
      # Ensure remaining_votes does not go negative
      if (remaining_votes - random_points < 0) {
        random_points <- remaining_votes
      }
      empty_votes$total_points[i] <- random_points
      remaining_votes <- remaining_votes - random_points
    } else {
      empty_votes$total_points[i] <- 0
    }
  }
}

# Print the updated empty_votes dataframe
print(empty_votes)

write.csv(empty_votes, "outputs/random_votes.csv", row.names = FALSE)

"
  1962
"

library(dplyr)

# Function to generate random number with a higher chance of being 0
generate_random_number <- function() {
  if (runif(1) < 0.4) {  # Adjust the probability for 0, e.g., 0.6 for 60% chance
    return(0)
  } else {
    return(sample(1:3, 1))
  }
}

# Initialize counters
current_country <- ""
remaining_votes <- c(1, 2, 3)  # Votes available for 1, 2, and 3 points
last_row_index <- NA

# Iterate over each row in empty_votes
for (i in 1:nrow(empty_votes)) {
  # Check if year is 1962
  if (empty_votes$year[i] == 1962) {
    # Check if from_country is different from previous record's from_country
    if (empty_votes$from_country[i] != current_country) {
      current_country <- empty_votes$from_country[i]
      remaining_votes <- c(1, 2, 3)
    }
    
    # Set total_points to random number
    if (sum(remaining_votes) > 0) {
      random_points <- generate_random_number()
      # Ensure random_points is not exceeding remaining votes
      while (random_points > sum(remaining_votes)) {
        random_points <- generate_random_number()
      }
      if (random_points > 0) {
        # Assign random_points to one of the available options
        vote_index <- sample(length(remaining_votes))[1]
        empty_votes$total_points[i] <- remaining_votes[vote_index]
        remaining_votes[vote_index] <- 0
      } else {
        empty_votes$total_points[i] <- 0
      }
      if (sum(remaining_votes) == 0) {
        last_row_index <- i
      }
    } else {
      empty_votes$total_points[i] <- 0
    }
  }
}

# Check if all possible votes (1, 2, and 3) have been assigned
if (!is.na(last_row_index) && sum(remaining_votes) > 0) {
  remaining_votes <- remaining_votes[remaining_votes > 0]  # Remove already assigned votes
  for (i in seq_along(remaining_votes)) {
    # Assign remaining votes to the last records
    empty_votes$total_points[last_row_index] <- empty_votes$total_points[last_row_index] + remaining_votes[i]
    last_row_index <- last_row_index - 1
  }
}

# Print the updated empty_votes dataframe for the year 1962
print(empty_votes[empty_votes$year == 1962, ])

write.csv(empty_votes, "outputs/random_votes.csv", row.names = FALSE)


"
  1963 - Same system as 1962 but 5 votes instead of 3.
  Repeated code for division of voting methods.
"

# Function to generate random number with a higher chance of being 0
generate_random_number <- function() {
  if (runif(1) < 0.3) {  # Adjust the probability for 0, e.g., 0.6 for 60% chance
    return(0)
  } else {
    return(sample(1:5, 1))
  }
}

# Initialize counters
current_country <- ""
remaining_votes <- c(1, 2, 3, 4, 5)  # Votes available for 1, 2, and 3 points
last_row_index <- NA

# Iterate over each row in empty_votes
for (i in 1:nrow(empty_votes)) {
  # Check if year is 1962
  if (empty_votes$year[i] == 1963) {
    # Check if from_country is different from previous record's from_country
    if (empty_votes$from_country[i] != current_country) {
      current_country <- empty_votes$from_country[i]
      remaining_votes <- c(1, 2, 3, 4, 5)
    }
    
    # Set total_points to random number
    if (sum(remaining_votes) > 0) {
      random_points <- generate_random_number()
      # Ensure random_points is not exceeding remaining votes
      while (random_points > sum(remaining_votes)) {
        random_points <- generate_random_number()
      }
      if (random_points > 0) {
        # Assign random_points to one of the available options
        vote_index <- sample(length(remaining_votes))[1]
        empty_votes$total_points[i] <- remaining_votes[vote_index]
        remaining_votes[vote_index] <- 0
      } else {
        empty_votes$total_points[i] <- 0
      }
      if (sum(remaining_votes) == 0) {
        last_row_index <- i
      }
    } else {
      empty_votes$total_points[i] <- 0
    }
  }
}

# Check if all possible votes (1, 2, and 3) have been assigned
if (!is.na(last_row_index) && sum(remaining_votes) > 0) {
  remaining_votes <- remaining_votes[remaining_votes > 0]  # Remove already assigned votes
  for (i in seq_along(remaining_votes)) {
    # Assign remaining votes to the last records
    empty_votes$total_points[last_row_index] <- empty_votes$total_points[last_row_index] + remaining_votes[i]
    last_row_index <- last_row_index - 1
  }
}

# Print the updated empty_votes dataframe for the year 1962
print(empty_votes[empty_votes$year == 1963, ])

write.csv(empty_votes, "outputs/random_votes.csv", row.names = FALSE)


"
  1964-1966
  Since no jury ever gave 9 points to a single song, this option was completely
  ignored.
"

# Function to generate random number with a higher chance of being 0
generate_random_number <- function() {
  if (runif(1) < 0.5) {  # Adjust the probability for 0, e.g., 0.5 for 50% chance
    return(0)
  } else {
    return(sample(c(1, 3, 5, 6), 1))  # Randomly choose from available points
  }
}

# Initialize counters
current_country <- ""
available_votes <- c(0, 1, 3, 5, 6)  # Available votes for each from_country
last_row_index <- NULL
votes_given <- list()

# Iterate over each row in empty_votes
for (i in 1:nrow(empty_votes)) {
  # Check if year is between 1964 and 1966
  if (empty_votes$year[i] >= 1964 && empty_votes$year[i] <= 1966) {
    # Check if from_country is different from the previous record's from_country
    if (empty_votes$from_country[i] != current_country) {
      current_country <- empty_votes$from_country[i]
      if (runif(1) < 0.6) {  # Higher chance of giving 6 and 3 points
        available_votes <- c(0, 3, 6)
      } else {  # Higher chance of giving 5, 3, and 1 points
        available_votes <- c(0, 1, 3, 5)
      }
      votes_given[[current_country]] <- c()
    }
    
    # Set total_points to random number
    if (length(available_votes) > 0) {
      random_points <- generate_random_number()
      # Ensure random_points is not exceeding remaining votes
      while (random_points > sum(available_votes)) {
        random_points <- generate_random_number()
      }
      if (random_points > 0) {
        # Assign random_points to one of the available options
        vote_index <- match(random_points, available_votes)
        empty_votes$total_points[i] <- available_votes[vote_index]
        if (!is.na(empty_votes$total_points[i])) {  # Check if total_points is not NA
          if (empty_votes$total_points[i] %in% votes_given[[current_country]]) {  # Check if vote already given
            empty_votes$total_points[i] <- 0  # If vote already given, set to 0
          } else {
            votes_given[[current_country]] <- c(votes_given[[current_country]], empty_votes$total_points[i])
          }
          if (empty_votes$total_points[i] == 6) {  # If 6 is picked, 5 and 1 cannot be picked
            available_votes <- available_votes[available_votes != 5 & available_votes != 1]
          } else if (empty_votes$total_points[i] %in% c(5, 1)) {  # If 5 or 1 is picked, 6 cannot be picked
            available_votes <- available_votes[available_votes != 6]
          } else {  # If any other number is picked, remove it from available options
            available_votes <- available_votes[-vote_index]
          }
        }
      } else {
        empty_votes$total_points[i] <- 0
      }
      if (sum(available_votes) == 0) {
        last_row_index <- i
      }
    } else {
      empty_votes$total_points[i] <- 0
    }
  }
}

# Check if all possible votes (1, 3, 5, and 6) have been assigned
if (!is.null(last_row_index) && sum(available_votes) > 0) {
  available_votes <- available_votes[available_votes > 0]  # Remove already assigned votes
  for (i in seq_along(available_votes)) {
    # Assign remaining votes to the last records
    empty_votes$total_points[last_row_index] <- empty_votes$total_points[last_row_index] + available_votes[i]
    last_row_index <- last_row_index - 1
  }
}

# Replace NA with 0 in empty_votes$total_points
empty_votes$total_points[is.na(empty_votes$total_points)] <- 0

# Print the updated empty_votes dataframe for the years 1964 to 1966
print(empty_votes[empty_votes$year >= 1964 & empty_votes$year <= 1966, ])

write.csv(empty_votes, "outputs/random_votes.csv", row.names = FALSE)


"
  1971-1973 - Rating each song between 1 and 5 points
"

# Function to generate random number with higher chance of being less than 5
generate_random_number <- function() {
  if (runif(1) < 0.9) {
    return(sample(0:4, 1))
  } else {
    return(sample(5:10, 1))
  }
}

# Initialize counters
current_country <- ""
remaining_votes <- 0

# Iterate over each row in empty_votes
for (i in 1:nrow(empty_votes)) {
  # Check if year is between 1971 and 1973
  if (empty_votes$year[i] >= 1971 && empty_votes$year[i] <= 1973) {
    # Check if from_country is different from previous record's from_country
    if (empty_votes$from_country[i] != current_country) {
      current_country <- empty_votes$from_country[i]
      remaining_votes <- 20
    }
    
    # Set total_points to random number
    if (remaining_votes > 0) {
      random_points <- generate_random_number()
      # Ensure remaining_votes does not go negative
      if (remaining_votes - random_points < 0) {
        random_points <- remaining_votes
      }
      empty_votes$total_points[i] <- random_points
      remaining_votes <- remaining_votes - random_points
    } else {
      empty_votes$total_points[i] <- 0
    }
  }
}

# Print the updated empty_votes dataframe
print(empty_votes[empty_votes$year >= 1971 & empty_votes$year <= 1973, ])

write.csv(empty_votes, "outputs/random_votes.csv", row.names = FALSE)


"
  1975-2015
  Votes available: 1-8, 10, 12
"

# Function to generate random number with a higher chance of being 0
generate_random_number <- function() {
  if (runif(1) < 0.4) {  # Adjust the probability for 0
    return(0)
  } else {
    return(sample(c(1:8, 10, 12), 1))  # Skipping 9 and 11
  }
}

# Initialize counters
current_country <- ""
available_votes <- list(0, 1:8, 10, 12)  # Available votes for each from_country

# Iterate over each row in empty_votes
for (i in 1:nrow(empty_votes)) {
  # Check if year is between 1975 and 2015
  if (empty_votes$year[i] >= 1975 && empty_votes$year[i] <= 2015) {
    # Check if from_country is different from the previous record's from_country
    if (empty_votes$from_country[i] != current_country) {
      current_country <- empty_votes$from_country[i]
      available_votes[[current_country]] <- c(0, 1:8, 10, 12)  # Reset available votes for the current country
    }
    
    # Set total_points to random number
    if (length(available_votes[[current_country]]) > 0) {
      random_points <- generate_random_number()
      # Check if the selected random number is available for the current country
      if (random_points %in% available_votes[[current_country]]) {
        # Remove the selected random number from available votes
        available_votes[[current_country]] <- available_votes[[current_country]][available_votes[[current_country]] != random_points]
        empty_votes$total_points[i] <- random_points
      } else {
        # If the selected random number is not available, assign 0
        empty_votes$total_points[i] <- 0
      }
    } else {
      empty_votes$total_points[i] <- 0  # If no more available votes, assign 0
    }
  }
}

# Printing the updated section of empty_votes
print(empty_votes[empty_votes$year >= 1975 & empty_votes$year <= 2015, ])

write.csv(empty_votes, "outputs/random_votes.csv", row.names = FALSE)


"
  2016-2023
"

# Function to generate random number with a higher chance of being 0
generate_random_number <- function() {
  if (runif(1) < 0.4) {  # Adjust the probability for 0
    return(0)
  } else {
    return(sample(c(1:8, 10, 12), 1))  # Skipping 9 and 11
  }
}

# Initialize counters
current_country <- ""
available_votes <- list(0, 1:8, 10, 12)  # Available votes for each from_country

# Iterate over each row in empty_votes
for (i in 1:nrow(empty_votes)) {
  # Check if year is between 2016 and 2023
  if (empty_votes$year[i] >= 2016 && empty_votes$year[i] <= 2023) {
    # Check if from_country is different from the previous record's from_country
    if (empty_votes$from_country[i] != current_country) {
      current_country <- empty_votes$from_country[i]
      available_votes[[current_country]] <- c(0, 1:8, 10, 12)  # Reset available votes for the current country
    }
    
    # Set total_points, tele_points, and jury_points to random numbers
    if (length(available_votes[[current_country]]) > 0) {
      random_points <- generate_random_number()
      # Check if the selected random number is available for the current country
      if (random_points %in% available_votes[[current_country]]) {
        # Remove the selected random number from available votes
        available_votes[[current_country]] <- available_votes[[current_country]][available_votes[[current_country]] != random_points]
        #todo: check to remove this:
        # empty_votes$total_points[i] <- random_points
        # Assign random numbers to tele_points and jury_points
        empty_votes$tele_points[i] <- generate_random_number()
        empty_votes$jury_points[i] <- generate_random_number()
      } else {
        # If the selected random number is not available, assign 0 to total_points, tele_points, and jury_points
        empty_votes$total_points[i] <- 0
        empty_votes$tele_points[i] <- 0
        empty_votes$jury_points[i] <- 0
      }
    } else {
      # If no more available votes, assign 0 to total_points, tele_points, and jury_points
      empty_votes$total_points[i] <- 0
      empty_votes$tele_points[i] <- 0
      empty_votes$jury_points[i] <- 0
    }
  }
}

# Convert relevant columns to numeric
empty_votes$total_points <- as.numeric(empty_votes$total_points)
empty_votes$tele_points <- as.numeric(empty_votes$tele_points)
empty_votes$jury_points <- as.numeric(empty_votes$jury_points)

# Summing up tele_points and jury_points to total_points only for records between 2016 and 2023
empty_votes$total_points[empty_votes$year >= 2016 & empty_votes$year <= 2023] <- 
  empty_votes$tele_points[empty_votes$year >= 2016 & empty_votes$year <= 2023] +
  empty_votes$jury_points[empty_votes$year >= 2016 & empty_votes$year <= 2023]

# Printing the updated section of empty_votes
print(empty_votes[empty_votes$year >= 2016 & empty_votes$year <= 2023, ])

write.csv(empty_votes, "outputs/random_votes.csv", row.names = FALSE)

