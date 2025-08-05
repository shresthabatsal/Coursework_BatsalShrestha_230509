library(tidyverse)
library(lubridate)

input_dir = "../Obtained Data/HousePrices/"
output_file = "../Cleaned Data/HousePrices/cleaned_house_prices.csv"
postcode_lsoa_file = "../Cleaned Data/cleaned_postcode_to_lsoa.csv"
years = 2021:2024

# Load postcode to LSOA mapping
postcode_lsoa = read_csv(postcode_lsoa_file, show_col_types = FALSE) %>%
  mutate(
    Postcode_clean = toupper(Postcode),  # Create clean version for matching
    shortPostcode = str_sub(str_replace_all(Postcode, " ", ""), 1, 4)
  )

clean_house_price_data = function(file_path) {
  df = read_csv(file_path, show_col_types = FALSE)
  
  # Rename columns
  colnames(df)[1:16] = c("ID", "Price", "Date", "Postcode", "Property_Type", 
                         "New_Build_Flag", "Tenure_Type", "PAON", "SAON", "Street",
                         "Locality", "Town", "District", "County", "B", "A")
  
  # Select and clean columns
  df = df %>% 
    select(Price, Date, Postcode, Town, District, County, Property_Type) %>%
    mutate(
      Price = as.numeric(Price),
      Date = as.Date(Date),
      Postcode = toupper(Postcode),  # Standardize to uppercase but keep spaces
      County = str_to_title(County),  # Convert to Proper Case
      District = str_to_title(District),
      Town = str_to_title(Town)
    ) %>%
    drop_na() %>%
    filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
    mutate(
      Year = year(Date),
      shortPostcode = str_sub(str_replace_all(Postcode, " ", ""), 1, 4)  # For grouping
    )
  
  # Join with LSOA data using cleaned postcodes
  df = df %>%
    left_join(
      postcode_lsoa %>% 
        select(Postcode_clean, LSOA),
      by = c("Postcode" = "Postcode_clean")
    )
  
  return(df)
}

# Process all years and combine
all_data = map_dfr(years, function(y) {
  file_name = paste0("pp-", y, ".csv")
  file_path = paste0(input_dir, file_name)
  clean_house_price_data(file_path)
})

# Save cleaned data
write_csv(all_data, output_file)

# View clenaed data
View(all_data)