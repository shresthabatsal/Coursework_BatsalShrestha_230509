library(tidyverse)

# Set file paths
input_base = "../Obtained Data/Crime"
output_dir = "../Cleaned Data/Crime"
output_file = file.path(output_dir, "cleaned_crime_data.csv")
postcode_lsoa_file = "../Cleaned Data/cleaned_postcode_to_lsoa.csv"

# Ensure output directory exists
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Get all monthly folders
month_folders = list.dirs(input_base, recursive = FALSE, full.names = TRUE)

# Load postcode-to-LSOA mapping and get one postcode per LSOA
postcode_lsoa = read_csv(postcode_lsoa_file, show_col_types = FALSE) %>%
  distinct(LSOA, .keep_all = TRUE) %>%
  select(LSOA, Postcode)

# Function to clean crime data for a specific county
clean_crime_data = function(county_name, filename_pattern) {
  all_data = list()
  
  for (folder in month_folders) {
    files = list.files(folder, pattern = filename_pattern, full.names = TRUE)
    
    for (file in files) {
      df = read_csv(file, show_col_types = FALSE)
      
      # Fix column names
      colnames(df) = gsub(" ", ".", colnames(df))
      
      # Rename Crime.ID to CrimeID if present
      if ("Crime.ID" %in% colnames(df)) {
        df = df %>% rename(CrimeID = Crime.ID)
      } else {
        stop(paste("Crime ID column not found in file:", file))
      }
      
      # Clean and transform
      df_cleaned = df %>%
        select(CrimeID, Month, LSOA.name, Crime.type) %>%
        mutate(
          County = county_name,
          District = word(LSOA.name, 1),
          LSOA = LSOA.name,
          Year = as.integer(substr(Month, 1, 4)),
          Month = as.integer(substr(Month, 6, 7))
        ) %>%
        select(CrimeID, Year, Month, County, District, LSOA, CrimeType = Crime.type) %>%
        filter(
          !is.na(CrimeID), CrimeID != "",
          !is.na(CrimeType), CrimeType != "",
          !is.na(District), District != ""
        ) %>%
        left_join(postcode_lsoa, by = c("LSOA" = "LSOA")) %>%
        filter(!is.na(Postcode))  # Remove rows without postcode
      
      all_data[[length(all_data) + 1]] = df_cleaned
    }
  }
  
  combined = bind_rows(all_data) %>% distinct()
  return(combined)
}

# Clean and combine data
south_yorkshire = clean_crime_data("South Yorkshire", "-south-yorkshire-street.csv")
west_yorkshire = clean_crime_data("West Yorkshire", "-west-yorkshire-street.csv")
combined_data = bind_rows(south_yorkshire, west_yorkshire)

# Save cleaned data
write_csv(combined_data, output_file)

# View cleaned data
View(combined_data)