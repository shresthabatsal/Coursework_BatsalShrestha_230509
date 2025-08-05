library(tidyverse)

# Load datasets
speed_data = read_csv("../Cleaned Data/BroadbandSpeed/cleaned_broadband_speed.csv")

# Create output directory if it doesn't exist
output_dir = "../Graphs/BroadbandSpeed"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Filter for South and West Yorkshire
south_yorkshire = speed_data %>% filter(County == "South Yorkshire")
west_yorkshire = speed_data %>% filter(County == "West Yorkshire")

# ==========================
# 1. Boxplot – Download Speed per District
# ==========================

# South Yorkshire boxplot
ggplot(south_yorkshire, aes(x = District, y = AverageDownloadSpeed_Mbps)) +
  geom_boxplot(fill = "#F8766D", alpha = 1, outlier.alpha = 0.2) +
  labs(
    title = "Download Speed Distribution in South Yorkshire",
    x = "District",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "south_yorkshire_boxplot.png"), width = 8, height = 5)

# West Yorkshire boxplot
ggplot(west_yorkshire, aes(x = District, y = AverageDownloadSpeed_Mbps)) +
  geom_boxplot(fill = "#00BFC4", alpha = 1, outlier.alpha = 0.2) +
  labs(
    title = "Download Speed Distribution in West Yorkshire",
    x = "District",
    y = "Average Download Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(output_dir, "west_yorkshire_boxplot.png"), width = 8, height = 5)

# ==========================
# 2. Barchart – Average Download Speed per Town
# ==========================

# Load towns dataset
towns = read_csv("../Cleaned Data/towns.csv")

# Extract short postcode and clean County in speed_data
speed_data = speed_data %>%
  mutate(
    shortPostcode = str_extract(Postcode, "^[A-Za-z]{1,2}[0-9]{1,2}"),
    shortPostcode = toupper(trimws(shortPostcode)),
    County = toupper(trimws(County))
  )

# Clean towns data
towns = towns %>%
  mutate(
    shortPostcode = toupper(trimws(shortPostcode)),
    County = toupper(trimws(County)),
    Town = str_to_title(trimws(Town))
  )

# Join datasets by shortPostcode and County
speed_towns = speed_data %>%
  inner_join(towns, by = c("shortPostcode", "County"))

# Filter for South and West Yorkshire
speed_towns = speed_towns %>%
  filter(County %in% c("SOUTH YORKSHIRE", "WEST YORKSHIRE"))

# Calculate average download speed per town
summary_data = speed_towns %>%
  group_by(County, Town) %>%
  summarise(AvgSpeed = mean(AverageDownloadSpeed_Mbps, na.rm = TRUE), .groups = "drop")

# Separate data for each county
south_data = summary_data %>% filter(County == "SOUTH YORKSHIRE")
west_data_full = summary_data %>% filter(County == "WEST YORKSHIRE")

set.seed(42)  # For reproducibility

# Get the best-performing town
best_town = west_data_full %>%
  arrange(desc(AvgSpeed)) %>%
  slice(1)

# Sample 9 towns from the remaining data across quantiles
remaining_towns = west_data_full %>%
  filter(Town != best_town$Town) %>%
  arrange(desc(AvgSpeed)) %>%
  mutate(Quantile = ntile(AvgSpeed, 9)) %>%
  group_by(Quantile) %>%
  slice_sample(n = 1) %>%
  ungroup()

# Combine best town with sampled towns
west_data = bind_rows(best_town, remaining_towns)

# Plot South Yorkshire – Town-level bar chart
ggplot(south_data, aes(x = reorder(Town, AvgSpeed), y = AvgSpeed, fill = Town)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Download Speed per Town in South Yorkshire",
    x = "Town",
    y = "Average Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10)
  )

ggsave(file.path(output_dir, "south_yorkshire_town_barchart.png"), width = 8, height = 6)

# Plot West Yorkshire – Town-level bar chart (sampled + best)
ggplot(west_data, aes(x = reorder(Town, AvgSpeed), y = AvgSpeed, fill = Town)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Download Speed per Town in West Yorkshire",
    x = "Town",
    y = "Average Speed (Mbps)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.y = element_text(size = 10)
  )

ggsave(file.path(output_dir, "west_yorkshire_town_barchart.png"), width = 8, height = 6)