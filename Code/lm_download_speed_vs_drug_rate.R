library(tidyverse)
library(ggplot2)
library(broom)

# File paths
broadband_speed_path = "../Cleaned Data/BroadbandSpeed/cleaned_broadband_speed.csv"
crime_path = "../Cleaned Data/Crime/cleaned_crime_data.csv"
towns_path = "../Cleaned Data/towns.csv"

# Load data
broadband_speed = read_csv(broadband_speed_path)
crime_data = read_csv(crime_path)
towns = read_csv(towns_path)

# Counties to analyse
counties_of_interest = c("South Yorkshire", "West Yorkshire")

# Prepare broadband data
broadband_short = broadband_speed %>%
  filter(County %in% counties_of_interest) %>%
  mutate(
    shortPostcode = toupper(str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}")),
    County = str_to_title(County)
  ) %>%
  group_by(shortPostcode, County) %>%
  summarise(AvgDownloadSpeed = mean(AverageDownloadSpeed_Mbps, na.rm = TRUE), .groups = "drop")

# Prepare crime data (drug offenses)
crime_short = crime_data %>%
  filter(County %in% counties_of_interest, CrimeType == "Drugs") %>%
  mutate(
    shortPostcode = toupper(str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}")),
    County = str_to_title(County)
  ) %>%
  group_by(shortPostcode, County) %>%
  summarise(DrugOffenses = n(), .groups = "drop")

# Prepare population data
pop_short = towns %>%
  mutate(
    shortPostcode = toupper(shortPostcode),
    County = str_to_title(County)
  ) %>%
  filter(County %in% counties_of_interest) %>%
  select(shortPostcode, County, Population2022)

# Join crime and population data to calculate drug rate per 10,000
drug_rate = crime_short %>%
  left_join(pop_short, by = c("shortPostcode", "County")) %>%
  filter(!is.na(Population2022), Population2022 > 0) %>%
  mutate(DrugRatePer10000 = (DrugOffenses / Population2022) * 10000)

# Combine with broadband data
combined_data = broadband_short %>%
  inner_join(drug_rate, by = c("shortPostcode", "County"))

# Summary of Combined Data
cat("\nCombined Data Summary:\n")
print(summary(combined_data))
print(table(combined_data$County))

# Create output directory if it doesn't exist
dir.create("../Graphs/LinearModel", recursive = TRUE, showWarnings = FALSE)

# Plot: Download Speed vs Drug Offense Rate with confidence intervals
plot = ggplot(combined_data, aes(x = AvgDownloadSpeed, y = DrugRatePer10000, color = County, fill = County)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  labs(
    title = "Drug Offense Rate vs Average Download Speed",
    x = "Average Download Speed (Mbps)",
    y = "Drug Offense Rate per 10,000 People",
    color = "County",
    fill = "County"
  ) +
  scale_y_continuous(limits = c(0, 300)) +
  theme_minimal()

print(plot)
ggsave("../Graphs/LinearModel/download_speed_vs_drug_rate.png", plot = plot, width = 10, height = 6)

# Linear model for South Yorkshire
model_south = lm(DrugRatePer10000 ~ AvgDownloadSpeed,
                 data = filter(combined_data, County == "South Yorkshire"))

cat("\nSouth Yorkshire Linear Model Summary:\n")
print(summary(model_south))

# Linear model for West Yorkshire
model_west = lm(DrugRatePer10000 ~ AvgDownloadSpeed,
                data = filter(combined_data, County == "West Yorkshire"))

cat("\nWest Yorkshire Linear Model Summary:\n")
print(summary(model_west))

# Correlation coefficients
cor_south = cor(
  filter(combined_data, County == "South Yorkshire")$AvgDownloadSpeed,
  filter(combined_data, County == "South Yorkshire")$DrugRatePer10000,
  use = "complete.obs"
)

cor_west = cor(
  filter(combined_data, County == "West Yorkshire")$AvgDownloadSpeed,
  filter(combined_data, County == "West Yorkshire")$DrugRatePer10000,
  use = "complete.obs"
)

cat("\nCorrelation Coefficients:\n")
cat("South Yorkshire:", round(cor_south, 3), "\n")
cat("West Yorkshire:", round(cor_west, 3), "\n")