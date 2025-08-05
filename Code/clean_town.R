library(tidyverse)
library(lubridate)

# Load population data
Population2011_1656567141570 = read_csv("../Obtained Data/Population2011.csv", show_col_types = FALSE)

# Clean and predict population 2020-2024
pop_data_clean = Population2011_1656567141570 %>%
  mutate(
    Population = as.numeric(gsub(",", "", Population)), # Remove commas & convert to numeric
    shortPostcode = str_trim(substr(Postcode, 1, 4))
  ) %>%
  drop_na(Population, shortPostcode) %>% # Remove NA
  group_by(shortPostcode) %>%
  summarise(Population2011 = sum(Population, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Population2012 = 1.00695353132322269 * Population2011,
    Population2013 = 1.00669740535540783 * Population2012,
    Population2014 = 1.00736463978721671 * Population2013,
    Population2015 = 1.00792367505802859 * Population2014,
    Population2016 = 1.00757874492811929 * Population2015,
    Population2017 = 1.00679374473924223 * Population2016,
    Population2018 = 1.00605929132212552 * Population2017,
    Population2019 = 1.00561255390388033 * Population2018,
    Population2020 = 1.00561255390388033 * Population2019,
    Population2021 = 1.005425 * Population2020,
    Population2022 = 1.004920 * Population2021,
    Population2023 = 1.004510 * Population2022,
    Population2024 = 1.004220 * Population2023
  ) %>%
  select(shortPostcode, Population2020, Population2021, Population2022, Population2023, Population2024) %>%
  drop_na()

# Load raw house price data for 2021–2024 and extract location info
input_dir = "../Obtained Data/HousePrices/"
years = 2021:2024

clean_house_price_data = function(file_path) {
  df = read_csv(file_path, show_col_types = FALSE)
  
  colnames(df)[1:16] = c("ID", "Price", "Date", "Postcode", "Property_Type", 
                         "New_Build_Flag", "Tenure_Type", "PAON", "SAON", "Street",
                         "Locality", "Town", "District", "County", "B", "A")
  
  df %>% 
    select(Postcode, Town, District, County) %>%
    mutate(
      shortPostcode = str_trim(substr(Postcode, 1, 4)),
      Town = str_to_title(Town),
      District = str_to_title(District),
      County = str_to_title(County)
    ) %>%
    filter(County %in% c("West Yorkshire", "South Yorkshire")) %>%
    drop_na(shortPostcode, Town, District, County)
}

# Combine house data from all years
all_house_data = map_dfr(years, function(y) {
  file_path = paste0(input_dir, "pp-", y, ".csv")
  clean_house_price_data(file_path)
})

# Remove duplicates, pick first location info per shortPostcode
postcode_location = all_house_data %>%
  distinct(shortPostcode, Town, District, County) %>%
  group_by(shortPostcode) %>%
  summarise(
    Town = first(Town),
    District = first(District),
    County = first(County)
  ) %>%
  ungroup()

# Join population and location data
population_with_location = pop_data_clean %>%
  left_join(postcode_location, by = "shortPostcode") %>%
  drop_na() %>%
  filter(County %in% c("West Yorkshire", "South Yorkshire")) %>%
  select(shortPostcode, Town, District, County, Population2020, Population2021, Population2022, Population2023, Population2024)

# Save to CSV
write_csv(population_with_location, "../Cleaned Data/towns.csv")

# View cleaned data
View(population_with_location)