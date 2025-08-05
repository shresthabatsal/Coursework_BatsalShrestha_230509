library(tidyverse)

# Set file paths
input_file = "../Obtained Data/Postcode to LSOA.csv"
output_file = "../Cleaned Data/cleaned_postcode_to_lsoa.csv"

# Mapping of Districts to Counties
district_to_county = tibble(
  District = c(
    "Sheffield", "Doncaster", "Rotherham", "Barnsley",     # South Yorkshire districts
    "Leeds", "Bradford", "Wakefield", "Kirklees", "Calderdale"  # West Yorkshire districts
  ),
  County = c(
    rep("South Yorkshire", 4),
    rep("West Yorkshire", 5)
  )
)

# Function to clean postcode to LSOA data
clean_postcode_lsoa_data = function(file_path) {
  df = read_csv(file_path, show_col_types = FALSE)
  
  # Select relevant columns and rename
  df = df %>%
    select(
      Postcode = pcds,     # Standardized postcode
      LSOA = lsoa11nm,     # LSOA name
      District = ladnm     # District name
    ) %>%
    inner_join(district_to_county, by = "District") %>%  # Filter for South & West Yorkshire
    distinct()  # Remove duplicate rows if any
  
  return(df)
}

# Clean the data
postcode_lsoa_cleaned = clean_postcode_lsoa_data(input_file)

# Write cleaned data to CSV
write_csv(postcode_lsoa_cleaned, output_file)

# View cleaned data
View(postcode_lsoa_cleaned)