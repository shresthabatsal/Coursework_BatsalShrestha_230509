library(tidyverse)
library(lubridate)
library(stringr)

options(scipen = 999)

# Create output directory for plots if it doesn't exist
output_dir = "../Graphs/HousePrices"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Load cleaned house price data
house_data = read_csv("../Cleaned Data/HousePrices/cleaned_house_prices.csv", show_col_types = FALSE) %>%
  mutate(
    County = str_to_title(County),
    District = str_to_title(District)
  )

# ====================================================
# 1. Line Graphs: Average Price Trends (2021–2024) by District
# ====================================================

# South Yorkshire line chart data
price_trend_south = house_data %>%
  filter(County == "South Yorkshire", Year %in% 2021:2024) %>%
  group_by(District, Year) %>%
  summarise(AvgPrice = mean(Price), .groups = "drop")

line_chart_south = ggplot(price_trend_south, aes(x = as.factor(Year), y = AvgPrice, color = District, group = District)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Average House Price Trends (2021–2024) - South Yorkshire",
    x = "Year",
    y = "Average House Price (£)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(line_chart_south)
ggsave(file.path(output_dir, "avg_price_trends_south_yorkshire.png"), plot = line_chart_south, width = 10, height = 6)

# West Yorkshire line chart data
price_trend_west = house_data %>%
  filter(County == "West Yorkshire", Year %in% 2021:2024) %>%
  group_by(District, Year) %>%
  summarise(AvgPrice = mean(Price), .groups = "drop")

line_chart_west = ggplot(price_trend_west, aes(x = as.factor(Year), y = AvgPrice, color = District, group = District)) +
  geom_line(size = 1) +
  geom_point() +
  labs(
    title = "Average House Price Trends (2021–2024) - West Yorkshire",
    x = "Year",
    y = "Average House Price (£)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(line_chart_west)
ggsave(file.path(output_dir, "avg_price_trends_west_yorkshire.png"), plot = line_chart_west, width = 10, height = 6)

# ====================================================
# 2. Bar Charts: Average House Prices in 2023 by District
# ====================================================

# South Yorkshire bar chart data
bar_data_south = house_data %>%
  filter(County == "South Yorkshire", Year == 2023) %>%
  group_by(District) %>%
  summarise(AvgPrice = mean(Price), .groups = "drop")

bar_chart_south = ggplot(bar_data_south, aes(x = District, y = AvgPrice, fill = District)) +
  geom_col() +
  labs(
    title = "Average House Prices in 2023 - South Yorkshire",
    x = "District",
    y = "Average House Price (£)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(bar_chart_south)
ggsave(file.path(output_dir, "avg_price_bar_2023_south_yorkshire.png"), plot = bar_chart_south, width = 10, height = 6)

# West Yorkshire bar chart data
bar_data_west = house_data %>%
  filter(County == "West Yorkshire", Year == 2023) %>%
  group_by(District) %>%
  summarise(AvgPrice = mean(Price), .groups = "drop")

bar_chart_west = ggplot(bar_data_west, aes(x = District, y = AvgPrice, fill = District)) +
  geom_col() +
  labs(
    title = "Average House Prices in 2023 - West Yorkshire",
    x = "District",
    y = "Average House Price (£)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

print(bar_chart_west)
ggsave(file.path(output_dir, "avg_price_bar_2023_west_yorkshire.png"), plot = bar_chart_west, width = 10, height = 6)


# ====================================================
# 3. Boxplots: House Prices by District
# ====================================================

# Filter data for the two counties
house_prices_filtered = house_data %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire"))

# South Yorkshire subset
south_data = house_prices_filtered %>% filter(County == "South Yorkshire")

# West Yorkshire subset
west_data = house_prices_filtered %>% filter(County == "West Yorkshire")

# Calculate upper limits for y-axis with 50% increase over 95th percentile
south_upper_limit = quantile(south_data$Price, 0.95, na.rm = TRUE) * 1.5
west_upper_limit = quantile(west_data$Price, 0.95, na.rm = TRUE) * 1.5

# South Yorkshire boxplot
boxplot_south = ggplot(south_data, aes(x = District, y = Price)) +
  geom_boxplot(fill = "#F8766D", outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, south_upper_limit)) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "South Yorkshire House Prices by District",
    x = "District",
    y = "Price (£)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(boxplot_south)
ggsave(file.path(output_dir, "south_yorkshire_house_prices_boxplot.png"), plot = boxplot_south, width = 10, height = 6)

# West Yorkshire boxplot
boxplot_west = ggplot(west_data, aes(x = District, y = Price)) +
  geom_boxplot(fill = "#00BFC4", outlier.alpha = 0.2) +
  coord_cartesian(ylim = c(0, west_upper_limit)) +
  scale_y_continuous(labels = scales::comma_format()) +
  labs(
    title = "West Yorkshire House Prices by District",
    x = "District",
    y = "Price (£)"
  ) +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(boxplot_west)
ggsave(file.path(output_dir, "west_yorkshire_house_prices_boxplot.png"), plot = boxplot_west, width = 10, height = 6)