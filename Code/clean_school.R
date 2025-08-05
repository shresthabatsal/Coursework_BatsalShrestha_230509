library(tidyverse)

# Set paths
input_base = "../Obtained Data/School"
output_base = "../Cleaned Data/School"
postcode_map_file = "../Cleaned Data/cleaned_postcode_to_lsoa.csv"

# Create output directory if it doesn't exist
dir.create(output_base, recursive = TRUE, showWarnings = FALSE)

# Define years
years = c("2021-2022", "2022-2023", "2023-2024")
ks2_years = c("2022-2023", "2023-2024")

# Load postcode to LSOA mapping
postcode_map = read_csv(postcode_map_file, show_col_types = FALSE)

# Enhanced numeric cleaner
clean_numeric = function(x) {
  if (is.numeric(x)) return(x)
  x = as.character(x)
  x = gsub("%", "", trimws(x))
  x = ifelse(x %in% c("SUPP", "NP", "NE", "NA", "", "N/A"), NA, x)
  as.numeric(x)
}

# Function to clean KS4 data
clean_ks4_data = function(year_folder, year_label, file_name) {
  file_path = file.path(input_base, year_folder, file_name)
  school_info_path = file.path(input_base, year_folder, "england_school_information.csv")
  
  ks4_data = suppressWarnings(
    read_csv(file_path, show_col_types = FALSE, guess_max = 10000)
  ) %>%
    mutate(School_URN = as.integer(URN)) %>%
    select(
      School_URN,
      School_Name = SCHNAME,
      Postcode = PCODE,
      ATT8SCR
    ) %>%
    mutate(
      Attainment_8_Score = clean_numeric(ATT8SCR),
      Year = year_label
    ) %>%
    select(-ATT8SCR)
  
  school_info = suppressWarnings(
    read_csv(school_info_path, show_col_types = FALSE, guess_max = 10000)
  ) %>%
    mutate(School_URN = as.integer(URN)) %>%
    select(School_URN, School_Postcode = POSTCODE, School_Name_Info = SCHNAME)
  
  ks4_data = ks4_data %>%
    left_join(school_info, by = "School_URN") %>%
    mutate(
      Postcode = coalesce(Postcode, School_Postcode),
      School_Name = coalesce(School_Name, School_Name_Info)
    ) %>%
    select(-School_Postcode, -School_Name_Info) %>%
    left_join(postcode_map, by = c("Postcode" = "Postcode")) %>%
    filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
    drop_na(Attainment_8_Score, District, County, School_Name)
  
  return(ks4_data)
}

# Function to clean KS2 data
clean_ks2_data = function(year_folder, year_label) {
  ks2_path = file.path(input_base, year_folder, "england_ks2final.csv")
  school_info_path = file.path(input_base, year_folder, "england_school_information.csv")
  
  ks2_data = suppressWarnings(
    read_csv(ks2_path, show_col_types = FALSE, guess_max = 10000)
  ) %>%
    mutate(School_URN = as.integer(URN)) %>%
    select(
      School_URN,
      School_Name = SCHNAME,
      Postcode = PCODE,
      Percent_Meeting_Expected_Standard_RWM = PTRWM_EXP,
      Reading_Average_Score = READ_AVERAGE,
      Writing_Average_Score = GPS_AVERAGE,
      Maths_Average_Score = MAT_AVERAGE
    ) %>%
    mutate(
      Year = year_label
    ) %>%
    mutate(across(
      c(
        Percent_Meeting_Expected_Standard_RWM,
        Reading_Average_Score,
        Writing_Average_Score,
        Maths_Average_Score
      ),
      clean_numeric
    ))
  
  school_info = suppressWarnings(
    read_csv(school_info_path, show_col_types = FALSE, guess_max = 10000)
  ) %>%
    mutate(School_URN = as.integer(URN)) %>%
    select(School_URN, School_Postcode = POSTCODE, School_Name_Info = SCHNAME)
  
  ks2_data = ks2_data %>%
    left_join(school_info, by = "School_URN") %>%
    mutate(
      Postcode = coalesce(Postcode, School_Postcode),
      School_Name = coalesce(School_Name, School_Name_Info)
    ) %>%
    select(-School_Postcode, -School_Name_Info) %>%
    left_join(postcode_map, by = c("Postcode" = "Postcode")) %>%
    filter(County %in% c("South Yorkshire", "West Yorkshire")) %>%
    drop_na(Percent_Meeting_Expected_Standard_RWM, District, County, School_Name)
  
  return(ks2_data)
}

# Process and combine KS4 final data
ks4_final_data = map2_dfr(years, years, ~ clean_ks4_data(.x, .y, "england_ks4final.csv"))
ks4_provisional_data = map2_dfr(years, years, ~ clean_ks4_data(.x, .y, "england_ks4provisional.csv"))

# Process KS2 final data
ks2_data = map2_dfr(ks2_years, ks2_years, ~ clean_ks2_data(.x, .y))

# Save output to CSV
write_csv(ks4_final_data, file.path(output_base, "cleaned_ks4_final.csv"))
write_csv(ks4_provisional_data, file.path(output_base, "cleaned_ks4_provisional.csv"))
write_csv(ks2_data, file.path(output_base, "cleaned_ks2_final.csv"))

# View cleaned data
View(ks4_final_data)
View(ks4_provisional_data)
View(ks2_data)