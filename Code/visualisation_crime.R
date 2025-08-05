library(tidyverse)
library(scales)
library(fmsb)
library(stringr)

# Create output directory if it doesn't exist
dir.create("../Graphs/Crime", recursive = TRUE, showWarnings = FALSE)

# Load datasets
crime_data = read_csv("../Cleaned Data/Crime/cleaned_crime_data.csv", show_col_types = FALSE)
towns = read_csv("../Cleaned Data/towns.csv", show_col_types = FALSE)

# Prepare crime data: clean names, format date parts
crime_data = crime_data %>%
  mutate(
    County = str_to_title(County),
    District = str_to_title(District),
    Year = as.character(Year),
    Month = sprintf("%02d", Month),
    YearMonth = paste(Year, Month, sep = "-")
  )

# Prepare population data in long format by Year, County, District
pop_long = towns %>%
  select(County, District, starts_with("Population")) %>%
  pivot_longer(cols = starts_with("Population"),
               names_to = "Year",
               values_to = "Population") %>%
  mutate(
    County = str_to_title(County),
    District = str_to_title(District),
    Year = str_extract(Year, "\\d{4}")
  ) %>%
  group_by(County, District, Year) %>%
  summarise(Population = sum(Population), .groups = "drop")

# =============================
# 1. Boxplot: Drug Offense Rate per 10,000 by District
# =============================
drug_yearly = crime_data %>%
  filter(CrimeType == "Drugs") %>%
  group_by(County, District, Year) %>%
  summarise(DrugOffenseCount = n(), .groups = "drop") %>%
  left_join(pop_long, by = c("County", "District", "Year")) %>%
  mutate(DrugOffenseRate = (DrugOffenseCount / Population) * 10000) %>%
  filter(!is.na(DrugOffenseRate))

for (cty in c("South Yorkshire", "West Yorkshire")) {
  p = ggplot(drug_yearly %>% filter(County == cty),
             aes(x = District, y = DrugOffenseRate)) +
    geom_boxplot(fill = ifelse(cty == "South Yorkshire", "#F8766D", "#00BFC4")) +
    labs(
      title = paste("Annual Drug Offense Rate per 10,000 -", cty),
      x = "District",
      y = "Rate per 10,000"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(p)
  
  ggsave(paste0("../Graphs/Crime/drug_rate_boxplot_", tolower(gsub(" ", "_", cty)), ".png"),
         plot = p, width = 10, height = 6)
}

# =============================
# 2. Radar Chart: Vehicle Crime Rate (South Yorkshire, May 2022)
# =============================
vehicle_crime = crime_data %>%
  filter(CrimeType == "Vehicle crime", County == "South Yorkshire", YearMonth == "2022-05") %>%
  group_by(County, District) %>%
  summarise(VehicleCrimes = n(), .groups = "drop") %>%
  left_join(pop_long %>% filter(Year == "2022", County == "South Yorkshire"), by = c("County", "District")) %>%
  mutate(VehicleCrimeRate = (VehicleCrimes / Population) * 10000) %>%
  filter(!is.na(VehicleCrimeRate))

vehicle_values = vehicle_crime$VehicleCrimeRate
names(vehicle_values) = vehicle_crime$District
max_val = ceiling(max(vehicle_values) * 1.1)

vehicle_df = rbind(
  rep(max_val, length(vehicle_values)),
  rep(0, length(vehicle_values)),
  vehicle_values
)
colnames(vehicle_df) = names(vehicle_values)
vehicle_df = as.data.frame(vehicle_df)

# Calculate percentages relative to max_val (for labeling)
vehicle_percent = round(vehicle_values / max_val * 100, 1)

png("../Graphs/Crime/vehicle_radar_south_yorkshire_2022_05.png", width = 800, height = 600)

radarchart(vehicle_df,
           axistype = 1,
           # Add percentage labels for axis lines (0%, 25%, 50%, 75%, 100%)
           caxislabels = paste0(seq(0, 100, by = 25), "%"),
           pcol = "#F8766D",
           pfcol = scales::alpha("#F8766D", 0.4),
           plwd = 2,
           title = "Vehicle Crime Rate per 10,000 by District - South Yorkshire (May 2022)")

# Add percentage labels for each district
n_vars <- length(vehicle_values)
angles <- seq(0, 2 * pi, length.out = n_vars + 1)[- (n_vars + 1)]
label_pos <- max_val * 1.1
x_lab <- label_pos * sin(angles)
y_lab <- label_pos * cos(angles)

text(x = x_lab, y = y_lab, labels = paste0(vehicle_percent, "%"), cex = 0.8, col = "#F8766D", font = 2)

dev.off()

# =============================
# 3. Pie Chart: Robbery Rate (West Yorkshire, May 2022)
# =============================
robbery_data = crime_data %>%
  filter(CrimeType == "Robbery", County == "West Yorkshire", YearMonth == "2022-05") %>%
  group_by(County, District) %>%
  summarise(RobberyCount = n(), .groups = "drop") %>%
  left_join(pop_long %>% filter(Year == "2022", County == "West Yorkshire"), by = c("County", "District")) %>%
  mutate(RobberyRate = (RobberyCount / Population) * 10000) %>%
  filter(!is.na(RobberyRate))

# Calculate percentage for labels (only percentage)
robbery_data = robbery_data %>%
  mutate(Percent = RobberyRate / sum(RobberyRate) * 100,
         Label = sprintf("%.1f%%", Percent))

p_pie = ggplot(robbery_data, aes(x = "", y = RobberyRate, fill = District)) +
  geom_col(width = 1) +
  coord_polar(theta = "y") +
  geom_text(aes(label = Label),
            position = position_stack(vjust = 0.5), size = 3.5) +
  labs(title = "Robbery Rate per 10,000 by District - West Yorkshire (May 2022)") +
  theme_void() +
  theme(legend.position = "right")

print(p_pie)

ggsave("../Graphs/Crime/robbery_rate_pie_west_yorkshire_2022_05.png", p_pie, width = 8, height = 6)

# =============================
# 4. Line Chart: Drug Offense Rate Over Time by County (2022–2024)
# =============================
drug_county_year = crime_data %>%
  filter(CrimeType == "Drugs", County %in% c("West Yorkshire", "South Yorkshire")) %>%
  group_by(County, Year) %>%
  summarise(DrugCrimes = n(), .groups = "drop") %>%
  left_join(pop_long %>%
              group_by(County, Year) %>%
              summarise(Population = sum(Population), .groups = "drop"),
            by = c("County", "Year")) %>%
  mutate(RatePer10k = (DrugCrimes / Population) * 10000) %>%
  filter(!is.na(RatePer10k)) %>%
  mutate(Year = factor(Year, levels = sort(unique(Year))))

p_line = ggplot(drug_county_year,
                aes(x = Year, y = RatePer10k, color = County, group = County)) +
  geom_line(size = 1.2) +
  geom_point(size = 2.5) +
  scale_color_manual(values = c("West Yorkshire" = "#00BFC4", "South Yorkshire" = "#F8766D")) +
  labs(
    title = "Drug Offense Rate per 10,000 by County (2022–2024)",
    x = "Year",
    y = "Rate per 10,000"
  ) +
  theme_minimal()

print(p_line)

ggsave("../Graphs/Crime/drug_offense_trend_both_counties.png", p_line, width = 10, height = 6)