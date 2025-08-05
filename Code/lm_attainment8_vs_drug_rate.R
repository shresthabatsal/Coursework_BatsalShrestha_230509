library(tidyverse)
library(ggplot2)

# Load datasets
crime_data = read_csv("../Cleaned Data/Crime/cleaned_crime_data.csv")
school_data = read_csv("../Cleaned Data/School/cleaned_ks4_final.csv")
population_data = read_csv("../Cleaned Data/towns.csv")

# Prepare crime data for 2023
crime_short = crime_data %>%
  filter(Year == 2023, CrimeType %in% c("Drugs", "Drug offences")) %>%
  mutate(
    County = str_to_title(str_trim(County)),
    shortPostcode = toupper(str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}"))
  ) %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
  group_by(shortPostcode, County) %>%
  summarise(drug_crimes = n(), .groups = "drop")

# Prepare school data for 2022
school_short = school_data %>%
  mutate(
    County = str_to_title(str_trim(County)),
    shortPostcode = toupper(str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}")),
    Year = str_sub(Year, 1, 4)
  ) %>%
  filter(Year == "2022", County %in% c("South Yorkshire", "West Yorkshire")) %>%
  group_by(shortPostcode, County) %>%
  summarise(mean_attainment8 = mean(Attainment_8_Score, na.rm = TRUE), .groups = "drop")

# Prepare population data
pop_short = population_data %>%
  mutate(
    shortPostcode = toupper(str_trim(shortPostcode)),
    County = str_to_title(str_trim(County))
  ) %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
  select(shortPostcode, Population2023, County)

# Combine datasets
combined_data = crime_short %>%
  left_join(pop_short, by = c("shortPostcode", "County")) %>%
  left_join(school_short, by = c("shortPostcode", "County")) %>%
  filter(!is.na(Population2023), Population2023 > 0, !is.na(mean_attainment8))

# Calculate drug offense rate per 10,000 people
combined_data = combined_data %>%
  mutate(drug_rate_per_10k = (drug_crimes / Population2023) * 10000)

# Explore the combined data
print(summary(combined_data))
print(table(combined_data$County))

# Create output directory for plots
dir.create("../Graphs/LinearModel", recursive = TRUE, showWarnings = FALSE)

# Create and save plot with customized confidence intervals
plot = ggplot(combined_data, aes(x = mean_attainment8, y = drug_rate_per_10k)) +
  geom_point(aes(color = County), alpha = 0.6) +
  geom_smooth(
    method = "lm",
    aes(color = County, fill = County),
    se = TRUE,
    alpha = 0.2
  ) +
  labs(
    title = "Attainment 8 vs Drug Offense Rate per 10,000 (2023)",
    x = "Mean Attainment 8 Score",
    y = "Drug Offense Rate per 10,000 People"
  ) +
  coord_cartesian(ylim = c(0, 100)) +
  theme_minimal()

print(plot)
ggsave("../Graphs/LinearModel/attainment8_vs_drug_rate.png", plot = plot, width = 10, height = 6)

# Linear model for South Yorkshire
model_south = lm(drug_rate_per_10k ~ mean_attainment8,
                 data = filter(combined_data, County == "South Yorkshire"))

cat("\nSouth Yorkshire Linear Model Summary:\n")
print(summary(model_south))

# Linear model for West Yorkshire
model_west = lm(drug_rate_per_10k ~ mean_attainment8,
                data = filter(combined_data, County == "West Yorkshire"))

cat("\nWest Yorkshire Linear Model Summary:\n")
print(summary(model_west))

# Correlation coefficients
cor_south = cor(
  filter(combined_data, County == "South Yorkshire")$mean_attainment8,
  filter(combined_data, County == "South Yorkshire")$drug_rate_per_10k,
  use = "complete.obs"
)

cor_west = cor(
  filter(combined_data, County == "West Yorkshire")$mean_attainment8,
  filter(combined_data, County == "West Yorkshire")$drug_rate_per_10k,
  use = "complete.obs"
)

cat("\nCorrelation coefficients:\n")
cat("South Yorkshire:", round(cor_south, 3), "\n")
cat("West Yorkshire:", round(cor_west, 3), "\n")