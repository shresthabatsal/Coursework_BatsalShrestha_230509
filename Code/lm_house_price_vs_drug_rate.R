library(tidyverse)
library(scales)
library(broom)
library(stringr)

# Load and prepare crime data (2023, Drug crimes only)
crime_data = read_csv("../Cleaned Data/Crime/cleaned_crime_data.csv") %>%
  filter(Year == 2023, CrimeType == "Drugs") %>%
  mutate(
    County = str_to_title(tolower(County)),
    Postcode = toupper(gsub(" ", "", Postcode)),
    ShortPostcode = str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}")
  ) %>%
  group_by(County, ShortPostcode) %>%
  summarise(Drug_Crimes = n(), .groups = "drop")

# Load and prepare house price data (2023)
house_prices = read_csv("../Cleaned Data/HousePrices/cleaned_house_prices.csv") %>%
  filter(Year == 2023) %>%
  mutate(
    County = str_to_title(tolower(County)),
    Postcode = toupper(gsub(" ", "", Postcode)),
    ShortPostcode = str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}")
  ) %>%
  group_by(County, ShortPostcode) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = "drop")

# Load population data for towns (ShortPostcode-level)
town_population = read_csv("../Cleaned Data/towns.csv") %>%
  mutate(
    shortPostcode = toupper(shortPostcode)
  )

# Join population to crime data
crime_pop = crime_data %>%
  left_join(town_population %>% select(shortPostcode, Population2023), 
            by = c("ShortPostcode" = "shortPostcode")) %>%
  filter(!is.na(Population2023), Population2023 > 0) %>%
  mutate(Drug_Rate_per_10000 = (Drug_Crimes / Population2023) * 10000)

# Merge with house price data on County + ShortPostcode
merged_data = inner_join(crime_pop, house_prices, by = c("County", "ShortPostcode")) %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire"))

# Summary
cat("\n--- Merged Data Overview ---\n")
print(summary(merged_data))
print(table(merged_data$County))

# Create output directory for plot
dir.create("../Graphs/LinearModel", recursive = TRUE, showWarnings = FALSE)

# Plot: Drug Rate vs House Price with confidence intervals using same color but lower alpha
plot = ggplot(merged_data, aes(x = Drug_Rate_per_10000, y = Avg_Price)) +
  geom_point(aes(color = County), alpha = 0.4, size = 1.5) +
  geom_smooth(aes(fill = County), method = "lm", se = TRUE, alpha = 0.2, size = 0, color = NA) +  # confidence intervals
  geom_smooth(aes(color = County), method = "lm", se = FALSE, size = 1.2) +  # regression lines
  labs(
    title = "House Price vs Drug Offense Rate (2023)",
    x = "Drug Offense Rate per 10,000 People",
    y = "Average House Price (£)"
  ) +
  scale_y_continuous(labels = comma) +
  theme_minimal() +
  coord_cartesian(xlim = range(merged_data$Drug_Rate_per_10000))

print(plot)

# Save the plot to file
ggsave("../Graphs/LinearModel/house_price_vs_drug_rate.png", plot = plot, width = 10, height = 6)

# Linear model: South Yorkshire
lm_south = lm(Avg_Price ~ Drug_Rate_per_10000, 
              data = filter(merged_data, County == "South Yorkshire"))
cat("\n--- Linear Model Summary: South Yorkshire ---\n")
print(summary(lm_south))

# Linear model: West Yorkshire
lm_west = lm(Avg_Price ~ Drug_Rate_per_10000, 
             data = filter(merged_data, County == "West Yorkshire"))
cat("\n--- Linear Model Summary: West Yorkshire ---\n")
print(summary(lm_west))

# Correlations
cor_south = cor(
  filter(merged_data, County == "South Yorkshire")$Drug_Rate_per_10000,
  filter(merged_data, County == "South Yorkshire")$Avg_Price,
  use = "complete.obs"
)

cor_west = cor(
  filter(merged_data, County == "West Yorkshire")$Drug_Rate_per_10000,
  filter(merged_data, County == "West Yorkshire")$Avg_Price,
  use = "complete.obs"
)

cat("\n--- Correlations ---\n")
cat("South Yorkshire Correlation:", round(cor_south, 3), "\n")
cat("West Yorkshire Correlation:", round(cor_west, 3), "\n")