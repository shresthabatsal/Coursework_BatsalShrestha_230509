library(dplyr)
library(tidyr)
library(stringr)

# Helper function to extract short postcode
extract_short_postcode = function(postcode) {
  if (is.na(postcode)) return(NA)
  clean_pc = gsub(" ", "", toupper(postcode))
  short_pc = str_extract(clean_pc, "^[A-Z]{1,2}[0-9]{1,2}")
  return(short_pc)
}

# Load and prepare individual datasets
# Population data
population_data = read.csv("../Cleaned Data/towns.csv") %>%
  mutate(
    Town = str_to_title(Town),
    County = str_to_title(County)
  ) %>%
  group_by(shortPostcode, Town, County) %>%
  summarise(Population = mean(Population2024, na.rm = TRUE)) %>%
  ungroup()

# House price data
house_price_data = read.csv("../Cleaned Data/HousePrices/cleaned_house_prices.csv") %>%
  mutate(
    shortPostcode = sapply(Postcode, extract_short_postcode),
    Town = str_to_title(Town),
    County = str_to_title(County)
  ) %>%
  group_by(shortPostcode, Town, County) %>%
  summarise(AvgPrice = mean(Price, na.rm = TRUE)) %>%
  ungroup()

# Broadband speed data
broadband_data = read.csv("../Cleaned Data/BroadbandSpeed/cleaned_broadband_speed.csv") %>%
  mutate(shortPostcode = sapply(Postcode, extract_short_postcode)) %>%
  group_by(shortPostcode) %>%
  summarise(AvgDownloadSpeed = mean(AverageDownloadSpeed_Mbps, na.rm = TRUE)) %>%
  ungroup()

# Crime data
crime_data = read.csv("../Cleaned Data/Crime/cleaned_crime_data.csv") %>%
  mutate(shortPostcode = sapply(Postcode, extract_short_postcode)) %>%
  group_by(shortPostcode) %>%
  summarise(TotalCrimes = n()) %>%
  ungroup()

# School performance data
school_data = read.csv("../Cleaned Data/School/cleaned_ks4_final.csv") %>%
  mutate(shortPostcode = sapply(Postcode, extract_short_postcode)) %>%
  group_by(shortPostcode) %>%
  summarise(AvgAttainment = mean(Attainment_8_Score, na.rm = TRUE)) %>%
  ungroup()

# Combine all datasets
town_scores_data = population_data %>%
  left_join(house_price_data, by = c("shortPostcode", "Town", "County")) %>%
  left_join(broadband_data, by = "shortPostcode") %>%
  left_join(crime_data, by = "shortPostcode") %>%
  left_join(school_data, by = "shortPostcode") %>%
  filter(
    !is.na(AvgPrice),
    !is.na(AvgDownloadSpeed),
    !is.na(TotalCrimes),
    !is.na(AvgAttainment)
  ) %>%
  group_by(shortPostcode, Town, County) %>%
  summarise(
    AvgPrice = mean(AvgPrice, na.rm = TRUE),
    AvgDownloadSpeed = mean(AvgDownloadSpeed, na.rm = TRUE),
    CrimeRate = (sum(TotalCrimes) / sum(Population)) * 1000,
    AvgAttainment = mean(AvgAttainment, na.rm = TRUE)
  ) %>%
  ungroup()

# Calculate scores (0–10 scale)
scored_towns = town_scores_data %>%
  mutate(
    PriceScore = 10 * (1 - (rank(AvgPrice) / n())),
    BroadbandScore = 10 * (rank(AvgDownloadSpeed) / n()),
    CrimeScore = 10 * (1 - (rank(CrimeRate) / n())),
    SchoolScore = 10 * (rank(AvgAttainment) / n()),
    OverallScore = round((PriceScore + BroadbandScore + CrimeScore + SchoolScore) / 4, 2)
  )

# Aggregate and rank recommended towns
recommended_towns = scored_towns %>%
  group_by(Town, County) %>%
  summarise(
    AffordabilityScore = mean(PriceScore, na.rm = TRUE),
    BroadbandScore = mean(BroadbandScore, na.rm = TRUE),
    SafetyScore = mean(CrimeScore, na.rm = TRUE),
    SchoolQualityScore = mean(SchoolScore, na.rm = TRUE),
    OverallScore = mean(OverallScore, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(desc(OverallScore)) %>%
  mutate(
    Rank = row_number(),
    AffordabilityScore = round(AffordabilityScore, 2),
    BroadbandScore = round(BroadbandScore, 2),
    SafetyScore = round(SafetyScore, 2),
    SchoolQualityScore = round(SchoolQualityScore, 2),
    OverallScore = round(OverallScore, 2)
  ) %>%
  select(
    Rank,
    Town,
    County,
    AffordabilityScore,
    SafetyScore,
    BroadbandScore,
    SchoolQualityScore,
    OverallScore
  )

# View top recommendations
top_10_towns = recommended_towns %>%
  slice_head(n = 10)
top_10_towns
View(top_10_towns)

# View all recommended towns
View(recommended_towns)

# Export top 10 recommended towns
write.csv(top_10_towns, "../Recommendation System/top_10_towns.csv", row.names = FALSE)