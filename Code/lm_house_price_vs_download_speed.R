library(tidyverse)
library(ggplot2)
library(scales)

# Load and prepare house price data
house_prices = read_csv("../Cleaned Data/HousePrices/cleaned_house_prices.csv") %>%
  mutate(
    County = str_to_title(tolower(County)),
    Postcode = toupper(gsub(" ", "", Postcode)),
    ShortPostcode = str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}")
  ) %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
  group_by(County, ShortPostcode) %>%
  summarise(Avg_House_Price = mean(Price, na.rm = TRUE), .groups = "drop")

# Load and prepare broadband speed data
broadband_speed = read_csv("../Cleaned Data/BroadbandSpeed/cleaned_broadband_speed.csv") %>%
  mutate(
    County = str_to_title(tolower(County)),
    Postcode = toupper(gsub(" ", "", Postcode)),
    ShortPostcode = str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}")
  ) %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
  group_by(County, ShortPostcode) %>%
  summarise(Avg_Download_Speed = mean(AverageDownloadSpeed_Mbps, na.rm = TRUE), .groups = "drop")

# Merge the datasets
merged_data = inner_join(house_prices, broadband_speed, by = c("County", "ShortPostcode"))

# Print summary of merged data
cat("\n--- Merged Data Overview ---\n")
print(summary(merged_data))
print(table(merged_data$County))

# Create output directory for saving plot
dir.create("../Graphs/LinearModel", recursive = TRUE, showWarnings = FALSE)

# Create scatter plot with linear model lines and confidence intervals
plot = ggplot(merged_data, aes(x = Avg_Download_Speed, y = Avg_House_Price)) +
  geom_point(aes(color = County), alpha = 0.4, size = 1.5) +
  geom_smooth(aes(fill = County), method = "lm", se = TRUE, alpha = 0.2, size = 0, color = NA) +  # confidence intervals
  geom_smooth(aes(color = County), method = "lm", se = FALSE, size = 1.2) +  # regression lines
  labs(
    title = "Average Download Speed vs Average House Price",
    x = "Average Download Speed (Mbps)",
    y = "Average House Price (£)"
  ) +
  scale_y_continuous(labels = comma, limits = c(0, 1000000)) +
  theme_minimal()

# Preview the plot in RStudio
print(plot)

# Save the plot to file
ggsave("../Graphs/LinearModel/house_price_vs_download_speed.png", plot = plot, width = 10, height = 6)

# Create linear models for each county
lm_south = lm(Avg_House_Price ~ Avg_Download_Speed, data = filter(merged_data, County == "South Yorkshire"))
lm_west = lm(Avg_House_Price ~ Avg_Download_Speed, data = filter(merged_data, County == "West Yorkshire"))

# Print linear model summaries
cat("\n--- Linear Model Summary: South Yorkshire ---\n")
print(summary(lm_south))

cat("\n--- Linear Model Summary: West Yorkshire ---\n")
print(summary(lm_west))

# Calculate and print correlations
cor_south = cor(
  filter(merged_data, County == "South Yorkshire")$Avg_Download_Speed,
  filter(merged_data, County == "South Yorkshire")$Avg_House_Price,
  use = "complete.obs"
)

cor_west = cor(
  filter(merged_data, County == "West Yorkshire")$Avg_Download_Speed,
  filter(merged_data, County == "West Yorkshire")$Avg_House_Price,
  use = "complete.obs"
)

cat("\n--- Correlations ---\n")
cat("South Yorkshire Correlation:", round(cor_south, 3), "\n")
cat("West Yorkshire Correlation:", round(cor_west, 3), "\n")