# Description ------------------------------------------------------------------
# R script to process uploaded raw data into a tidy, analysis-ready data frame
# Load packages ----------------------------------------------------------------
## Run the following code in console if you don't have the packages
## install.packages(c("usethis", "fs", "here", "readr", "readxl", "openxlsx"))
library(usethis)
library(fs)
library(here)
library(readr)
library(dplyr)
library(readxl)
library(openxlsx)
library(lubridate)

# Load Data --------------------------------------------------------------------
# Load the necessary data from a CSV file
data_in <- readr::read_csv("data-raw/flood response waterpoint rehabilitation and chrolination survey.csv")

# (Optional) Read and clean the codebook if needed (commented out for now)
# codebook <- readxl::read_excel("data-raw/codebook.xlsx") %>%
#   clean_names()

# Tidy data --------------------------------------------------------------------
# Remove rows where the 'latitude' column contains NULL (NA) values
data_in <- data_in %>%
  filter(!is.na(latitude))

# Convert date columns to Date class
# These dates are in DD/MM/YYYY format
data_in <- data_in %>%
  mutate(
    submitted_on = dmy(submitted_on),
    chlorination_rehab_date = dmy(chlorination_rehab_date)
  )

# Standardize rating variables
# Convert all rating columns from character to numeric
# Replace "Missing" text with NA
rating_cols <- grep("^rating_", names(data_in), value = TRUE)
data_in[rating_cols] <- lapply(data_in[rating_cols], function(x) {
  x[x == "Missing"] <- NA
  as.numeric(x)
})


# Function to check for non-UTF-8 characters in character columns
check_utf8 <- function(df) {
  # Identify columns with invalid UTF-8 characters
  invalid_cols <- sapply(df, function(column) {
    if (!is.character(column)) return(FALSE) # Skip non-character columns
    any(sapply(column, function(x) {
      if (is.na(x)) return(FALSE) # Ignore NA values
      !identical(iconv(x, from = "UTF-8", to = "UTF-8"), x)
    }))
  })

  # Extract the column names with invalid characters
  bad_cols <- names(df)[invalid_cols]

  # Output a message depending on whether non-UTF-8 characters were found
  if (length(bad_cols) > 0) {
    message("Non-UTF-8 characters detected in columns: ",
            paste(bad_cols, collapse = ", "))
  } else {
    message("No non-UTF-8 characters found.")
  }
}

# Convert character columns from Latin1 encoding to UTF-8, removing problematic
#   characters
data_in[] <- lapply(data_in, function(x) {
  if (is.character(x)) {
    # Convert to UTF-8 and remove problematic characters
    iconv(x, from = "latin1", to = "UTF-8", sub = "")
  } else {
    x
  }
})

# Re-check the data for non-UTF-8 characters after the conversion
check_utf8(data_in)

# Data validation checks
# Check coordinate ranges
invalid_coords <- data_in %>%
  filter(!between(latitude, -17.5, -9.0) | !between(longitude, 32.0, 36.0))

if (nrow(invalid_coords) > 0) {
  warning(paste("Found", nrow(invalid_coords), "rows with coordinates outside Malawi bounds"))
}

# Check rating values are within 1-5 range
for (col in rating_cols) {
  invalid_ratings <- data_in[[col]][!is.na(data_in[[col]]) & 
                                    (data_in[[col]] < 1 | data_in[[col]] > 5)]
  if (length(invalid_ratings) > 0) {
    warning(paste("Column", col, "has", length(invalid_ratings), 
                  "values outside 1-5 range"))
  }
}

# Note on missing data patterns:
# - Chlorination-related variables have ~53% missing values because not all 
#   waterpoints underwent chlorination treatment
# - Replacement part variables have high missing rates (30-90%) because 
#   only damaged parts needed replacement
# - parts_chlorinated_unknown is 100% missing, likely a data collection issue

floodchlorinationsurvey <- data_in

# Export Data ------------------------------------------------------------------
usethis::use_data(floodchlorinationsurvey, overwrite = TRUE)
fs::dir_create(here::here("inst", "extdata"))
readr::write_csv(floodchlorinationsurvey,
                 here::here("inst", "extdata", paste0("floodchlorinationsurvey", ".csv")))
openxlsx::write.xlsx(floodchlorinationsurvey,
                     here::here("inst", "extdata", paste0("floodchlorinationsurvey",
                                                          ".xlsx")))
