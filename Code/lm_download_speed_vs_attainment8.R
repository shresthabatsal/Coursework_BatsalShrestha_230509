library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

# Read datasets
ks4_final = read_csv("../Cleaned Data/School/cleaned_ks4_final.csv")
broadband_speed = read_csv("../Cleaned Data/BroadbandSpeed/cleaned_broadband_speed.csv")

# Prepare school data: filter, clean, extract short postcode, aggregate
school_avg = ks4_final %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
  mutate(
    County = toupper(County),
    Postcode = toupper(gsub(" ", "", Postcode)),
    ShortPostcode = str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}")
  ) %>%
  group_by(County, ShortPostcode) %>%
  summarise(Avg_Attainment8 = mean(Attainment_8_Score, na.rm = TRUE), .groups = "drop")

# Prepare broadband data: filter, clean, extract short postcode, aggregate
broadband_avg = broadband_speed %>%
  filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
  mutate(
    County = toupper(County),
    Postcode = toupper(gsub(" ", "", Postcode)),
    ShortPostcode = str_extract(Postcode, "^[A-Z]{1,2}\\d{1,2}")
  ) %>%
  group_by(County, ShortPostcode) %>%
  summarise(Avg_Download_Speed = mean(AverageDownloadSpeed_Mbps, na.rm = TRUE), .groups = "drop")

# Join school and broadband data
combined_data = inner_join(school_avg, broadband_avg, by = c("County", "ShortPostcode")) %>%
  mutate(
    County = recode(County,
                    "SOUTH YORKSHIRE" = "South Yorkshire",
                    "WEST YORKSHIRE" = "West Yorkshire")
  )

# Explore Combined Data
cat("\nCombined Data Summary:\n")
print(summary(combined_data))
print(table(combined_data$County))

# Create output folder
dir.create("../Graphs/LinearModel", recursive = TRUE, showWarnings = FALSE)

# Create and Save Plot
plot = ggplot(combined_data, aes(x = Avg_Download_Speed, y = Avg_Attainment8, color = County, fill = County)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  labs(
    title = "Average Download Speed vs Attainment 8 Score",
    x = "Average Download Speed (Mbps)",
    y = "Average Attainment 8 Score",
    color = "County",
    fill = "County"
  ) +
  coord_cartesian(ylim = c(0, 80)) +
  theme_minimal() +
  theme(legend.position = "right")

print(plot)
ggsave("../Graphs/LinearModel/download_speed_vs_attainment8.png", plot, width = 10, height = 6)

# Linear Models

# South Yorkshire
model_south = lm(Avg_Attainment8 ~ Avg_Download_Speed,
                 data = filter(combined_data, County == "South Yorkshire"))
cat("\nSouth Yorkshire Linear Model Summary:\n")
print(summary(model_south))

# West Yorkshire
model_west = lm(Avg_Attainment8 ~ Avg_Download_Speed,
                data = filter(combined_data, County == "West Yorkshire"))
cat("\nWest Yorkshire Linear Model Summary:\n")
print(summary(model_west))

# Correlation Coefficients
cor_south = cor(
  filter(combined_data, County == "South Yorkshire")$Avg_Download_Speed,
  filter(combined_data, County == "South Yorkshire")$Avg_Attainment8,
  use = "complete.obs"
)

cor_west = cor(
  filter(combined_data, County == "West Yorkshire")$Avg_Download_Speed,
  filter(combined_data, County == "West Yorkshire")$Avg_Attainment8,
  use = "complete.obs"
)

cat("\nCorrelation Coefficients:\n")
cat("South Yorkshire:", round(cor_south, 3), "\n")
cat("West Yorkshire:", round(cor_west, 3), "\n")