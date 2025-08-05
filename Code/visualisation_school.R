library(tidyverse)

# Create output directory if it doesn't exist
output_dir = "../Graphs/School"
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

# Read the cleaned KS4 final data
ks4_final = read_csv("../Cleaned Data/School/cleaned_ks4_final.csv")

# Filter for the year 2021-2022 only
ks4_2022 = ks4_final %>%
  filter(Year == "2021-2022")

# ===============================
# 1. Boxplot – Attainment 8 Scores (2022) by District - South Yorkshire
# ===============================
sy_boxplot = ks4_2022 %>%
  filter(County == "South Yorkshire") %>%
  ggplot(aes(x = District, y = Attainment_8_Score)) +
  geom_boxplot(fill = "#F8766D", alpha = 1, outlier.alpha = 0.5) +
  labs(
    title = "Attainment 8 Scores (2022) by District – South Yorkshire",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(sy_boxplot)

# Save the plot
ggsave(file.path(output_dir, "attainment8_boxplot_south_yorkshire.png"),
       plot = sy_boxplot, width = 10, height = 6)

# ===============================
# 2. Boxplot – Attainment 8 Scores (2022) by District - West Yorkshire
# ===============================
wy_boxplot = ks4_2022 %>%
  filter(County == "West Yorkshire") %>%
  ggplot(aes(x = District, y = Attainment_8_Score)) +
  geom_boxplot(fill = "#00BFC4", alpha = 1, outlier.alpha = 0.2) +
  labs(
    title = "Attainment 8 Scores (2022) by District – West Yorkshire",
    x = "District",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(wy_boxplot)

# Save the plot
ggsave(file.path(output_dir, "attainment8_boxplot_west_yorkshire.png"),
       plot = wy_boxplot, width = 10, height = 6)

# ===============================
# 3. Line Graph – Attainment 8 Scores Trends
# ===============================
# South Yorkshire
attainment_trend = ks4_final %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
  group_by(County, District, Year) %>%
  summarise(Avg_Score = mean(Attainment_8_Score, na.rm = TRUE), .groups = "drop")

line_graph_south = attainment_trend %>%
  filter(County == "South Yorkshire") %>%
  ggplot(aes(x = Year, y = Avg_Score, color = District, group = District)) +
  geom_line(size = 1) +
  labs(
    title = "Attainment 8 Score Trends (KS4) – South Yorkshire",
    x = "Academic Year",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(line_graph_south)

# Save the plot
ggsave(file.path(output_dir, "attainment8_trend_south_yorkshire.png"),
       plot = line_graph_south, width = 10, height = 6)

# West Yorkshire
line_graph_west = attainment_trend %>%
  filter(County == "West Yorkshire") %>%
  ggplot(aes(x = Year, y = Avg_Score, color = District, group = District)) +
  geom_line(size = 1) +
  labs(
    title = "Attainment 8 Score Trends (KS4) – West Yorkshire",
    x = "Academic Year",
    y = "Average Attainment 8 Score"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(line_graph_west)

# Save the plot
ggsave(file.path(output_dir, "attainment8_trend_west_yorkshire.png"),
       plot = line_graph_west, width = 10, height = 6)