library(tidyverse)

# Set file paths
performance_file = "../Obtained Data/BroadbandSpeed/201805_fixed_pc_performance_r03.csv"
coverage_file = "../Obtained Data/BroadbandSpeed/201809_fixed_pc_coverage_r01.csv"
postcode_map_file = "../Cleaned Data/cleaned_postcode_to_lsoa.csv"

output_perf_file = "../Cleaned Data/BroadbandSpeed/cleaned_broadband_speed.csv"

# Function to normalize postcodes
normalize_postcode = function(df, column) {
  df %>%
    mutate(Postcode_clean = toupper(.data[[column]]))
}

# Function to clean broadband performance data
clean_broadband_performance = function(perf_file, postcode_map) {
  df = read_csv(perf_file, show_col_types = FALSE)
  # Use the postcode_space column which contains spaces
  df = normalize_postcode(df, "postcode_space")
  merged = df %>%
    inner_join(postcode_map, by = "Postcode_clean") %>%
    filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
    select(
      Postcode = postcode_space,
      LSOA,
      District,
      County,
      AverageDownloadSpeed_Mbps = `Average download speed (Mbit/s)`
    )
  return(merged)
}

# Function to clean broadband coverage data
clean_broadband_coverage = function(coverage_file, postcode_map) {
  df = read_csv(coverage_file, show_col_types = FALSE)
  # Use the postcodes column which contains spaces
  df = normalize_postcode(df, "pcds")
  merged = df %>%
    inner_join(postcode_map, by = "Postcode_clean") %>%
    filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
    select(
      Postcode = pcds,
      District,
      County,
      SFBB_Availability = `SFBB availability (% premises)`,
      FTTP_Availability = `FTTP availability (% premises)`,
      Below_10Mbps = `% of premises unable to receive 10Mbit/s`
    )
  return(merged)
}

# Read postcode mapping and normalize
postcode_map = read_csv(postcode_map_file, show_col_types = FALSE) %>%
  mutate(Postcode_clean = toupper(Postcode))

# Clean performance data
cleaned_perf = clean_broadband_performance(performance_file, postcode_map)

# Write cleaned data to CSV
write_csv(cleaned_perf, output_perf_file)

# View cleaned data
View(cleaned_perf)