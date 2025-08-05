library(tidyverse)
library(ggplot2)
library(scales)
library(broom)

# Load and prepare house price data
house_prices = read_csv("../Cleaned Data/HousePrices/cleaned_house_prices.csv") %>%
  mutate(
    Year = as.character(Year),
    County = str_to_title(tolower(County)),
    shortPostcode = toupper(shortPostcode)
  ) %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
  drop_na(shortPostcode)

# Load and prepare Attainment 8 school performance data
ks4 = read_csv("../Cleaned Data/School/cleaned_ks4_final.csv") %>%
  mutate(
    Year = str_sub(Year, 1, 4),
    County = str_to_title(tolower(County)),
    Postcode = gsub(" ", "", toupper(Postcode)),
    shortPostcode = str_extract(Postcode, "^[A-Z]{1,2}[0-9]{1,2}")
  ) %>%
  drop_na(Attainment_8_Score, shortPostcode, County)

# Aggregate attainment data by shortPostcode, County, and Year
attainment_data = ks4 %>%
  group_by(shortPostcode, County, Year) %>%
  summarise(Avg_Attainment8 = mean(Attainment_8_Score, na.rm = TRUE), .groups = "drop")

# Aggregate house price data by shortPostcode, County, and Year
avg_prices = house_prices %>%
  group_by(shortPostcode, County, Year) %>%
  summarise(Avg_Price = mean(Price, na.rm = TRUE), .groups = "drop")

# Merge attainment and house price data
merged_data = inner_join(avg_prices, attainment_data, by = c("shortPostcode", "County", "Year")) %>%
  filter(!is.na(Avg_Attainment8), !is.na(Avg_Price))

# Downsample West Yorkshire data
set.seed(123)

south_data = filter(merged_data, County == "South Yorkshire")
west_data = filter(merged_data, County == "West Yorkshire")

west_sampled = sample_n(west_data, size = round(0.8 * nrow(west_data)))

# Combine sampled data
merged_sampled = bind_rows(south_data, west_sampled)

# Print summary
cat("\n--- Merged Education & House Price Data Overview ---\n")
print(summary(merged_sampled))
print(table(merged_sampled$County))

# Create output directory
dir.create("../Graphs/LinearModel", recursive = TRUE, showWarnings = FALSE)

# Create scatter plot with confidence intervals + regression lines
plot = ggplot(merged_sampled, aes(x = Avg_Attainment8, y = Avg_Price)) +
  geom_point(aes(color = County), alpha = 0.5, size = 1.5) +
  geom_smooth(aes(fill = County), method = "lm", se = TRUE, alpha = 0.2, size = 0, color = NA) +  # confidence interval
  geom_smooth(aes(color = County), method = "lm", se = FALSE, size = 1.2) +  # regression line
  scale_y_continuous(labels = comma_format(), limits = c(0, 800000)) +
  labs(
    title = "House Price vs Attainment 8 Score by Short Postcode",
    x = "Average Attainment 8 Score",
    y = "Average House Price (£)"
  ) +
  theme_minimal()

# Show plot
print(plot)

# Save plot
ggsave("../Graphs/LinearModel/house_price_vs_attainment8.png", plot = plot, width = 10, height = 6)

# Linear model for South Yorkshire
lm_south = lm(Avg_Price ~ Avg_Attainment8, data = filter(merged_sampled, County == "South Yorkshire"))
cat("\n--- Linear Model Summary: South Yorkshire ---\n")
print(summary(lm_south))

# Linear model for West Yorkshire
lm_west = lm(Avg_Price ~ Avg_Attainment8, data = filter(merged_sampled, County == "West Yorkshire"))
cat("\n--- Linear Model Summary: West Yorkshire ---\n")
print(summary(lm_west))

# Correlations
cor_south = cor(
  filter(merged_sampled, County == "South Yorkshire")$Avg_Attainment8,
  filter(merged_sampled, County == "South Yorkshire")$Avg_Price,
  use = "complete.obs"
)

cor_west = cor(
  filter(merged_sampled, County == "West Yorkshire")$Avg_Attainment8,
  filter(merged_sampled, County == "West Yorkshire")$Avg_Price,
  use = "complete.obs"
)

cat("\n--- Correlations ---\n")
cat("South Yorkshire Correlation:", round(cor_south, 3), "\n")
cat("West Yorkshire Correlation:", round(cor_west, 3), "\n")