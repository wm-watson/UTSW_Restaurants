#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# DataAxleCleaning.R
# eating places (5812) 
# drinking places (5813)
# local & suburban transit (4111)
# Local passenger transportation NEC (4119)
# Intercity & rural bus transportation (4131)
# gas stations (5541)
# gyms (7991)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## LIBRARIES ####
library(tidyverse)

## PATHS ####
dataaxle.path = "/Users/williamwatson/Library/CloudStorage/Box-Box/PhD/GIS/UTWS_Restaurants/UTSW_Restaurants.xlsx"
# BG4111.path = "output/BG_4111.csv"
# BG4119.path = "output/BG_4119.csv"
# BG4131.path = "output/BG_4131.csv"
# BG5541.path = "output/BG_5541.csv"
# BG5812.path = "output/BG_5812.csv"
# BG5813.path = "output/BG_5813b.csv"
# BG7991.path = "output/BG_7991b.csv"
# BGbuff.path = "output/BG_1kmbuff_SIC.csv"

## READ IN ####
dataaxle = read_csv(dataaxle.path)
# BG4111 = read_csv(BG4111.path)
# BG4119 = read_csv(BG4119.path)
# BG4131 = read_csv(BG4131.path)
# BG5541 = read_csv(BG5541.path)
# BG5812 = read_csv(BG5812.path)
# BG5813 = read_csv(BG5813.path)
# BG7991 = read_csv(BG7991.path)
# BGbuff = read_csv(BGbuff.path)

## CLEAN & COMBINE ####
# BG4111.clean = BG4111 %>%
#   mutate(GEOID = str_sub(Name, 1, 12)) %>%
#   rename(Near4111_Min = Total_TravelTime,
#          Near4111_KM = Total_Kilometers) %>%
#   select(GEOID, Near4111_Min, Near4111_KM)
# BG4119.clean = BG4119 %>%
#   mutate(GEOID = str_sub(Name, 1, 12)) %>%
#   rename(Near4119_Min = Total_TravelTime,
#          Near4119_KM = Total_Kilometers) %>%
#   select(-Name)
# BG4131.clean = BG4131 %>%
#   mutate(GEOID = str_sub(Name, 1, 12)) %>%
#   rename(Near4131_Min = Total_TravelTime,
#          Near4131_KM = Total_Kilometers) %>%
#   select(-Name)
# BG5541.clean = BG5541 %>%
#   mutate(GEOID = str_sub(Name, 1, 12)) %>%
#   rename(Near5541_Min = Total_TravelTime,
#          Near5541_KM = Total_Kilometers) %>%
#   select(-Name)
# BG5812.clean = BG5812 %>%
#   mutate(GEOID = str_sub(Name, 1, 12)) %>%
#   rename(Near5812_Min = Total_TravelTime,
#          Near5812_KM = Total_Kilometers) %>%
#   select(-Name)
# BG5813.clean = BG5813 %>%
#   mutate(GEOID = str_sub(Name, 1, 12)) %>%
#   rename(Near5813_Min = Total_TravelTime,
#          Near5813_KM = Total_Kilometers) %>%
#   select(-Name)
# BG7991.clean = BG7991 %>%
#   mutate(GEOID = str_sub(Name, 1, 12)) %>%
#   rename(Near7991_Min = Total_TravelTime,
#          Near7991_KM = Total_Kilometers) %>%
#   select(-Name)
# 
# BG_Travel = BG4111.clean %>%
#   left_join(BG4119.clean, by = "GEOID") %>%
#   left_join(BG4131.clean, by = "GEOID") %>%
#   left_join(BG5541.clean, by = "GEOID") %>%
#   left_join(BG5812.clean, by = "GEOID") %>%
#   left_join(BG5813.clean, by = "GEOID") %>%
#   left_join(BG7991.clean, by = "GEOID")
# 
# BG_SICcounts = BGbuff %>%
#   mutate(GEOID = as.character(GEOID)) %>%
#   rename(Dens1KM4111 = SIC4111,
#          Dens1KM4119 = SIC4119,
#          Dens1KM4131 = SIC4131,
#          Dens1KM5541 = SIC5541,
#          Dens1KM5812 = SIC5812,
#          Dens1KM5813 = SIC5813,
#          Dens1KM7991 = SIC7991)
# 
# BG_FoodEnv = BG_SICcounts %>%
#   left_join(BG_Travel, by = "GEOID")
# 
# write_csv(BG_FoodEnv, "output/BG_FoodEnv_060624.csv")

## CONSOLIDATE SICs? ####
# If I need to group SIC codes further down to less categories...
# # All 41## SICs can be combined into Transit/Transportation
# BG_FoodEnv_consolidated = BG_FoodEnv %>%
#   mutate(Dens1KM41 = Dens1KM4111 + Dens1KM4119 + Dens1KM4131,
#          Near41_Min = pmin(Near4111_Min, Near4119_Min, Near4131_Min),
#          Near41_KM = pmin(Near4111_KM, Near4119_KM, Near4131_KM)) %>%
#   select(-Dens1KM4111, -Dens1KM4119, -Dens1KM4131, 
#          -Near4111_Min, -Near4119_Min, -Near4131_Min,
#          -Near4111_KM, -Near4119_KM, -Near4131_KM) %>%
#   select(GEOID, Dens1KM5812, Near5812_Min, Near5812_KM,
#          Dens1KM5813, Near5813_Min, Near5813_KM,
#          Dens1KM41, Near41_Min, Near41_KM,
#          Dens1KM5541, Near5541_Min, Near5541_KM,
#          Dens1KM7991, Near7991_Min, Near7991_KM)
# 
# write_csv(BG_FoodEnv_consolidated, "output/BG_FoodEnv_060624.csv")

## GRANULARIZE EATING ESTABLISHMENTS WITH AI ####
da.food = dataaxle %>%
  select(`IUSA Number`, `Company Name`, Address, City, State, `ZIP Code`,
         `SIC Code 1`, `SIC Code 1 Description`, `SIC Code 2`, `SIC Code 2 Description`,
         `SIC Code 3`, `SIC Code 3 Description`, `SIC Code 4`, `SIC Code 4 Description`,
         `SIC Code 5`, `SIC Code 5 Description`, `SIC Code 6`, `SIC Code 6 Description`,
         `SIC Code 7`, `SIC Code 7 Description`, `NAICS 1`, `NAICS 1 Description`,
         `NAICS 2`, `NAICS 2 Description`, `NAICS 3`, `NAICS 3 Description`,
         `NAICS 4`, `NAICS 4 Description`, `NAICS 5`, `NAICS 5 Description`,
         `NAICS 6`, `NAICS 6 Description`, `NAICS 7`, `NAICS 7 Description`,
         `Location Type`) %>%
  distinct()

sic1.unique = da.food %>%
  select(SIC_Code = `SIC Code 1`, Code_Description = `SIC Code 1 Description`) %>%
  distinct()
sic2.unique = da.food %>%
  select(SIC_Code = `SIC Code 2`, Code_Description = `SIC Code 2 Description`) %>%
  distinct()
sic3.unique = da.food %>%
  select(SIC_Code = `SIC Code 3`, Code_Description = `SIC Code 3 Description`) %>%
  distinct()
sic4.unique = da.food %>%
  select(SIC_Code = `SIC Code 4`, Code_Description = `SIC Code 4 Description`) %>%
  distinct()
sic5.unique = da.food %>%
  select(SIC_Code = `SIC Code 5`, Code_Description = `SIC Code 5 Description`) %>%
  distinct()
sic6.unique = da.food %>%
  select(SIC_Code = `SIC Code 6`, Code_Description = `SIC Code 6 Description`) %>%
  distinct()
sic7.unique = da.food %>%
  select(SIC_Code = `SIC Code 7`, Code_Description = `SIC Code 7 Description`) %>%
  distinct()

# This combined all the relevant SIC codes and excludes those not related to the food environment
unique.sic.codes = sic1.unique %>%
  rbind(sic2.unique, sic3.unique, sic4.unique, sic5.unique, sic6.unique, sic7.unique) %>%
  distinct() %>%
  arrange(SIC_Code) %>%
  filter(SIC_Code %in% c(539902:549999, 581201:581209,
                         581211, 581213:581219, 581222:581305,
                         592101:592107, 596201, 596316:596318,
                         701107, 832240, 832268))

naics1 = da.food %>%
  select(NAICS_Code = `NAICS 1`, Code_Description = `NAICS 1 Description`) %>%
  distinct()
naics2 = da.food %>%
  select(NAICS_Code = `NAICS 2`, Code_Description = `NAICS 2 Description`) %>%
  distinct()
naics3 = da.food %>%
  select(NAICS_Code = `NAICS 3`, Code_Description = `NAICS 3 Description`) %>%
  distinct()
naics4 = da.food %>%
  select(NAICS_Code = `NAICS 4`, Code_Description = `NAICS 4 Description`) %>%
  distinct()
naics5 = da.food %>%
  select(NAICS_Code = `NAICS 5`, Code_Description = `NAICS 5 Description`) %>%
  distinct()
naics6 = da.food %>%
  select(NAICS_Code = `NAICS 6`, Code_Description = `NAICS 6 Description`) %>%
  distinct()
naics7 = da.food %>%
  select(NAICS_Code = `NAICS 7`, Code_Description = `NAICS 7 Description`) %>%
  distinct()

# This combined all the relevant NAICS codes and excludes those not related to the food environment
unique.naics.codes = naics1 %>%
  rbind(naics2, naics3, naics4, naics5, naics6, naics7) %>%
  distinct() %>%
  arrange(NAICS_Code) %>%
  filter(NAICS_Code %in% c(31181102:31181104, 44511001:44532007, 
                           45619101:45619110, 62421001:62421002,
                           72119101, 72233002:72251518))


# This filters the data axle dataset to only those records that have at least 
#   one of the relevant SIC or NAICS codes in any of the 14 code columns.
da.food.filtered = da.food %>%
  filter(if_any(matches("^SIC Code [1-7]$"), ~ . %in% unique.sic.codes$SIC_Code) |
           if_any(matches("^NAICS [1-7]$"), ~ . %in% unique.naics.codes$NAICS_Code))

# Used ChatGPT to start with grouping subcodes of 5812 and 5813
# sic_classification = tribble(~SIC1, ~classification,
#                              581248, "healthy", # Juice bars
#                              581203, "unhealthy",
#                              581222, "unhealthy")
# 
# dataaxle.class1 = dataaxle %>%
#   left_join(sic_classification, by = c(`SIC Code 1` = "SIC1"))
# dataaxle.class2 = dataaxle.class1 %>%
#   filter(is.na(classification))
# dataaxle.class1 = dataaxle.class1 %>%
#   filter(!is.na(classification))
# 
# classify.by.keyword = function(sic_description) {
#   desc_lower = tolower(as.character(sic_description))
#   
#   healthy_keywords = c("health", "juice", "salad", "vegetarian", "organic")
#   unhealthy_keywords = c("fast food", "burger", "pizza", "fried", "ice cream", "barbecue", "steak", "buffet", "donut")
#   
#   if (any(grepl(paste(healthy_keywords, collapse = "|"), desc_lower))) {
#     return("healthy")
#   } else if (any(grepl(paste(unhealthy_keywords, collapse = "|"), desc_lower))) {
#     return("unhealthy")
#   } else {
#     return(NA)
#   }
# }
# 
# dataaxle.class2 = dataaxle.class2 %>%
#   mutate(classification = sapply(`SIC Code 1 Description`, classify.by.keyword))
# dataaxle.class3 = dataaxle.class2 %>%
#   filter(is.na(classification))
# dataaxle.class2 = dataaxle.class2 %>%
#   filter(!is.na(classification))

#####################################################BREAK##################################################################################
# Begin Will's Code----
## Classification----

dataaxle <- readxl::read_excel(dataaxle.path)

food_classification <- dataaxle %>%
  # First identify unhealthy establishments using NAICS and SIC codes
  mutate(
    is_unhealthy = case_when(
      # NAICS codes for unhealthy establishments
      grepl("445120", `Primary NAICS`) ~ TRUE,  # 445120: Convenience Stores
      grepl("722410", `Primary NAICS`) ~ TRUE,  # 722410: Drinking Places (Alcoholic Beverages)
      
      # SIC codes for unhealthy establishments
      `Primary SIC Code` %in% c(541103) ~ TRUE, # 541103: Convenience Stores
      `Primary SIC Code` %in% c(581225, 581301, 581222, 581219) ~ TRUE, # 581225: Ice Cream/Frozen Yogurt Stands
      # 581301: Bars
      # 581222: Pizza Restaurants
      # 581219: BBQ Restaurants
      # Check for BBQ in names regardless of code
      grepl("bbq|barbecue|bar-b-q|bar-b-que|barbeque", tolower(`Company Name`)) ~ TRUE,
      
      TRUE ~ FALSE
    )
  ) %>%
  # Create limited service indicator
  mutate(
    is_limited_service = case_when(
      grepl("722513", `Primary NAICS`) ~ TRUE,  # 722513: Limited-Service Restaurants
      `Primary SIC Code` %in% c(581208) ~ TRUE,  # 581208: Fast-Food Restaurants
      TRUE ~ FALSE
    )
  ) %>%
  # Main classification with unhealthy taking precedence
  mutate(food_category = case_when(
    # Unhealthy categories take precedence
    is_unhealthy ~ "unhealthy",
    
    # Then check for healthy categories
    grepl("445210|445230|445110", `Primary NAICS`) ~ "healthy", 
    # 445210: Meat Markets
    # 445230: Fruit and Vegetable Markets
    # 445110: Supermarkets and Grocery Stores
    
    grepl("445299", `Primary NAICS`) ~ "healthy", # 445299: All Other Specialty Food Stores
    
    `Primary SIC Code` %in% c(541101, 541102, 541105) ~ "healthy",
    # 541101: Health Food Stores
    # 541102: Diet Food Centers
    # 541105: Grocery Stores
    
    `Primary SIC Code` %in% c(543101, 543102, 543103) ~ "healthy",
    # 543101: Fruit and Vegetable Markets
    # 543102: Produce Markets
    # 543103: Farmers Markets
    
    # Limited service evaluation combining codes AND names
    is_limited_service & grepl("salad|bowl|poke|mediterranean|sushi|vegetarian|vegan", 
                               tolower(`Company Name`)) ~ "healthy",
    is_limited_service & grepl("smoothie|juice|sandwich|subway|fresh|health", 
                               tolower(`Company Name`)) ~ "moderately_healthy",
    is_limited_service & grepl("pizza|burger|fries|wings|fried|chicken|taco|fast food|chinese buffet|ice cream|dessert|donut|candy", 
                               tolower(`Company Name`)) ~ "unhealthy",
    is_limited_service ~ "needs_detailed_scoring",
    
    TRUE ~ "needs_review"
  )) %>%
  # Detailed scoring for remaining limited service restaurants
  mutate(health_score = case_when(
    food_category == "healthy" ~ 2,
    food_category == "moderately_healthy" ~ 1,
    food_category == "unhealthy" ~ -2,
    food_category == "needs_detailed_scoring" ~ 0, # baseline for further scoring
    TRUE ~ 0
  )) %>%
  # Additional scoring layers for "needs_detailed_scoring" category
  mutate(
    detailed_score = case_when(
      food_category != "needs_detailed_scoring" ~ 0,
      # Menu type indicators in name
      grepl("grill|deli|cafe|bistro", tolower(`Company Name`)) ~ 0.5,
      grepl("express|drive|thru|drive-thru", tolower(`Company Name`)) ~ -0.5,
      
      # Cuisine type indicators
      grepl("asian|thai|vietnamese|indian|mediterranean", tolower(`Company Name`)) ~ 0.5,
      grepl("tex-mex|bbq|barbecue|bar-b-q", tolower(`Company Name`)) ~ -0.5,
      
      # Secondary business characteristics
      grepl("family|traditional|home", tolower(`Company Name`)) ~ 0.3,
      grepl("quick|fast|speedy", tolower(`Company Name`)) ~ -0.3,
      
      TRUE ~ 0
    ),
    
    # Consider secondary NAICS/SIC codes
    secondary_code_score = case_when(
      food_category != "needs_detailed_scoring" ~ 0,
      grepl("445|446", `NAICS 2`) ~ 0.5,  # Food/Health stores as secondary
      grepl("722513", `NAICS 2`) ~ -0.5,  # Additional limited-service indicators
      `SIC Code 2` %in% c(581208, 581222) ~ -0.5, # Additional fast food indicators
      TRUE ~ 0
    )
  ) %>%
  # Calculate final score for detailed scoring cases
  mutate(
    final_health_score = case_when(
      food_category == "needs_detailed_scoring" ~ 
        health_score + 
        (detailed_score * 0.6) +     # Higher weight on detailed name analysis
        (secondary_code_score * 0.4), # Lower weight on secondary codes
      food_category == "healthy" ~ 2,
      food_category == "moderately_healthy" ~ 1,
      food_category == "unhealthy" ~ -2,
      TRUE ~ 0
    )
  ) %>%
  # Final classification
  mutate(final_category = case_when(
    food_category %in% c("healthy", "moderately_healthy", "unhealthy") ~ food_category,
    final_health_score >= 1.5 ~ "healthy",
    final_health_score >= 0 & final_health_score < 1.5 ~ "moderately_healthy",
    final_health_score < 0 & final_health_score > -1.5 ~ "moderately_unhealthy",
    final_health_score <= -1.5 ~ "unhealthy",
    TRUE ~ "needs_review"
  ))

## Summaries----
summary_all <- food_classification %>%
  group_by(food_category, final_category) %>%
  summarise(
    count = n(),
    pct_total = round(n() / nrow(food_classification) * 100, 1),
    .groups = 'drop'
  ) %>%
  arrange(food_category, final_category)

# Focus on needs_review cases
review_cases_detailed <- food_classification %>%
  filter(food_category == "needs_review") %>%
  select(
    `Company Name`, 
    `Primary NAICS`, 
    `Primary NAICS Description`,
    `Primary SIC Code`, 
    `Primary SIC Description`,
    food_category,
    final_category,
    final_health_score
  ) %>%
  distinct() %>%
  arrange(`Company Name`)

# Get NAICS/SIC patterns in review cases
code_patterns_in_review <- food_classification %>%
  filter(food_category == "needs_review") %>%
  group_by(`Primary NAICS`, `Primary NAICS Description`) %>%
  summarise(
    count = n(),
    pct_of_review = round(n() / sum(food_category == "needs_review") * 100, 1),
    common_names = paste(head(unique(`Company Name`), 3), collapse = "; "),
    .groups = 'drop'
  ) %>%
  arrange(desc(count))

# How review cases were ultimately classified
review_resolution <- food_classification %>%
  filter(food_category == "needs_review") %>%
  group_by(final_category) %>%
  summarise(
    count = n(),
    pct_of_review = round(n() / sum(food_category == "needs_review") * 100, 1),
    example_businesses = paste(head(unique(`Company Name`), 3), collapse = "; "),
    .groups = 'drop'
  ) %>%
  arrange(desc(count))

# Print results
print("Overall Classification Summary:")
print(summary_all)

print("\nNAICS/SIC Patterns in Review Cases:")
print(code_patterns_in_review)

print("\nFinal Resolution of Review Cases:")
print(review_resolution)

print("\nDetailed List of Review Cases:")
print(review_cases_detailed)


refined_classification <- food_classification %>%
  # Remove NAs
  filter(!is.na(`Primary NAICS`), 
         !is.na(`Primary SIC Code`)) %>%
  # Filter out transportation/transit/ambulance services AND caterers
  filter(!grepl("Transit|Transportation|Ambulance|Limousine|Bus|Rail|Special Needs|Sightseeing|Caterer", 
                `Primary NAICS Description`)) %>%
  filter(!grepl("Transit|Transportation|Ambulance|Limousine|Bus|Rail|Special Needs|Sightseeing|Caterer", 
                `Primary SIC Description`)) %>%
  # Update food_category for gas stations
  mutate(
    food_category = case_when(
      # Add gas stations to unhealthy category
      grepl("Gasoline|Gas Station", `Primary NAICS Description`) | 
        grepl("Gasoline|Gas Station", `Primary SIC Description`) ~ "unhealthy",
      # Keep existing classifications
      TRUE ~ food_category
    )
  ) %>%
  # Recalculate health scores based on updated categories
  mutate(health_score = case_when(
    food_category == "healthy" ~ 2,
    food_category == "moderately_healthy" ~ 1,
    food_category == "unhealthy" ~ -2,
    food_category == "needs_detailed_scoring" ~ 0,
    TRUE ~ 0
  )) %>%
  # Keep existing detailed scoring
  mutate(
    detailed_score = case_when(
      food_category != "needs_detailed_scoring" ~ 0,
      grepl("grill|deli|cafe|bistro", tolower(`Company Name`)) ~ 0.5,
      grepl("express|drive|thru|drive-thru", tolower(`Company Name`)) ~ -0.5,
      grepl("asian|thai|vietnamese|indian|mediterranean", tolower(`Company Name`)) ~ 0.5,
      grepl("tex-mex|bbq|barbecue|bar-b-q", tolower(`Company Name`)) ~ -0.5,
      grepl("family|traditional|home", tolower(`Company Name`)) ~ 0.3,
      grepl("quick|fast|speedy", tolower(`Company Name`)) ~ -0.3,
      TRUE ~ 0
    ),
    secondary_code_score = secondary_code_score  # Keep existing secondary code scores
  ) %>%
  # Recalculate final health score
  mutate(
    final_health_score = case_when(
      food_category == "needs_detailed_scoring" ~ 
        health_score + 
        (detailed_score * 0.6) +
        (secondary_code_score * 0.4),
      food_category == "healthy" ~ 2,
      food_category == "moderately_healthy" ~ 1,
      food_category == "unhealthy" ~ -2,
      TRUE ~ 0
    )
  ) %>%
  # Recalculate final category
  mutate(
    final_category = case_when(
      food_category %in% c("healthy", "moderately_healthy", "unhealthy") ~ food_category,
      final_health_score >= 1.5 ~ "healthy",
      final_health_score >= 0 & final_health_score < 1.5 ~ "moderately_healthy",
      final_health_score < 0 & final_health_score > -1.5 ~ "moderately_unhealthy",
      final_health_score <= -1.5 ~ "unhealthy",
      TRUE ~ "needs_review"
    )
  )

review_cases <- refined_classification %>%
  filter(food_category %in% c("needs_review", "needs_detailed_scoring")) %>%
  select(Company_Name = `Company Name`, 
         NAICS_Desc = `Primary NAICS Description`,
         SIC_Desc = `Primary SIC Description`,
         food_category,
         final_category,
         health_score,
         detailed_score,
         secondary_code_score,
         final_health_score) %>%
  arrange(food_category, final_category)

# Summary of how these cases were ultimately categorized
resolution_summary <- review_cases %>%
  group_by(food_category, final_category) %>%
  summarise(count = n(), .groups = 'drop')

print("Resolution Summary:")
print(resolution_summary)

print("\nSample of review cases:")
print(head(review_cases, 20))

## More Refinement----

review_cases_updated <- refined_classification %>%
  filter(food_category %in% c("needs_review", "needs_detailed_scoring")) %>%
  mutate(
    new_classification = case_when(
      # Healthy terms
      grepl("mediterr?an(ean)?|green|fresh", tolower(`Company Name`)) ~ "healthy",
      
      # Unhealthy terms - breads and sweets
      grepl("bread|brown(ie)?s|cookie(s)?", tolower(`Company Name`)) ~ "unhealthy",
      
      # Unhealthy - regional cuisine indicators
      grepl("country|southern|the[- ]south", tolower(`Company Name`)) ~ "unhealthy",
      
      # Unhealthy - specific foods
      grepl("cheese[- ]?steak|cheez[- ]?steak", tolower(`Company Name`)) ~ "unhealthy",
      grepl("\\bcheese\\b|bacon", tolower(`Company Name`)) ~ "unhealthy", # changed to word boundary
      
      # Unhealthy - beverages
      grepl("soda[- ]?(fountain)?", tolower(`Company Name`)) ~ "unhealthy",
      
      # Unhealthy - wings variations
      grepl("hot[- ]?wings?|chicken[- ]?wings?|hot[- ]?wangs?|wings?", tolower(`Company Name`)) ~ "unhealthy",
      
      # Bars and delis
      grepl("\\bbar\\b|[^-]deli\\b", tolower(`Company Name`)) ~ "unhealthy",
      
      TRUE ~ NA_character_  # Keep existing classification if no new match
    )
  ) %>%
  mutate(
    # Update food_category and final_category only if we have a new classification
    food_category = if_else(!is.na(new_classification), new_classification, food_category),
    final_category = if_else(!is.na(new_classification), new_classification, final_category)
  ) %>%
  select(-new_classification)  # Remove temporary classification column

# Check the results
summary_changes <- review_cases_updated %>%
  group_by(food_category, final_category) %>%
  summarise(
    count = n(),
    example_names = paste(head(unique(`Company Name`), 3), collapse = "; "),
    .groups = 'drop'
  )

print("Updated Classification Summary:")
print(summary_changes)

# Show examples of newly classified establishments
print("\nNewly Classified Examples:")
newly_classified <- review_cases_updated %>%
  filter(food_category %in% c("healthy", "unhealthy")) %>%
  select(`Company Name`, food_category, final_category)
print(head(newly_classified, 20))

# Summary of changes
summary_changes <- review_cases_updated %>%
  group_by(food_category, final_category) %>%
  summarise(
    count = n(),
    example_names = paste(head(unique(`Company Name`), 3), collapse = "; "),
    .groups = 'drop'
  )

print("\nUpdated Classification Summary:")
print(summary_changes)


review_needed <- refined_classification %>%
  filter(food_category %in% c("needs_review", "needs_detailed_scoring") |
           final_category %in% c("healthy", "moderately_healthy")) %>%
  select(
    `Company Name`, 
    # Primary codes
    `Primary NAICS`, `Primary NAICS Description`,
    `Primary SIC Code`, `Primary SIC Description`,
    # Secondary codes
    `NAICS 2`, `NAICS 2 Description`,
    `SIC Code 2`, `SIC Code 2 Description`,
    # Tertiary codes
    `NAICS 3`, `NAICS 3 Description`,
    `SIC Code 3`, `SIC Code 3 Description`,
    food_category,
    final_category
  ) %>%
  arrange(`Company Name`)

# Summary stats
print("Distribution by food_category:")
print(table(review_needed$food_category))

print("\nDistribution by final_category:")
print(table(review_needed$final_category))

# Look at code patterns for healthy/moderately healthy places
healthy_patterns <- review_needed %>%
  filter(final_category %in% c("healthy", "moderately_healthy")) %>%
  group_by(`Primary NAICS Description`, `Primary SIC Description`) %>%
  summarise(
    count = n(),
    example_names = paste(head(unique(`Company Name`), 3), collapse = "; "),
    final_cats = paste(unique(final_category), collapse = ", "),
    .groups = 'drop'
  ) %>%
  arrange(desc(count))

print("\nMost common industry codes for healthy/moderately healthy establishments:")
print(head(healthy_patterns, 20))

# Sample of cases
print("\nSample of establishments:")
print(head(review_needed, 20))

refined_classification <- refined_classification %>%
  mutate(
    final_category = case_when(
      `Primary SIC Description` == "Ice Cream Parlors" ~ "unhealthy",
      `Primary SIC Description` %in% c(
        "Restaurant Management",
        "Personal Trainers-Fitness"
      ) ~ "not_food_service",
      TRUE ~ final_category
    ),
    
    food_category = if_else(
      final_category == "not_food_service",
      "not_food_service",
      food_category
    )
  ) %>%
  filter(final_category != "not_food_service")

## Tokenization and Bigrams----
library(tidytext)
library(stringr)
library(cld2)

# First update classifications
refined_classification <- refined_classification %>%
  mutate(
    detected_language = detect_language(`Company Name`),
    is_mexican = detected_language == "es" | 
      str_detect(tolower(`Company Name`), "mexican|mexico"),
    final_category = case_when(
      (`Primary NAICS Description` == "Full-Service Restaurants" |
         `Primary SIC Description` == "Restaurants") &
        (detected_language == "es" | 
           str_detect(tolower(`Company Name`), "mexican|mexico")) ~ "unhealthy",
      TRUE ~ final_category
    )
  )

# Then analyze the healthy/moderately healthy restaurants
health_status_restaurants <- refined_classification %>%
  filter(
    (`Primary NAICS Description` == "Full-Service Restaurants" |
       `Primary SIC Description` == "Restaurants") &
      final_category %in% c("moderately_healthy", "healthy")
  )

# Tokenize restaurant names
tokens <- health_status_restaurants %>%
  unnest_tokens(word, `Company Name`) %>%
  anti_join(stop_words) %>%
  filter(!str_detect(word, "^[0-9]+$")) %>%
  count(word, sort = TRUE)

# Create bigrams
bigrams <- health_status_restaurants %>%
  unnest_tokens(bigram, `Company Name`, token = "ngrams", n = 2) %>%
  filter(!str_detect(bigram, "^[0-9 ]+$")) %>%
  count(bigram, sort = TRUE)

print("Top 200 Single Words in Non-Mexican Restaurant Names:")
print(tokens, n=200)

print("\nTop 200 Two-Word Combinations in Non-Mexican Restaurant Names:")
print(bigrams, n=200)

# Summary of changes
print("\nFinal Category Distribution:")
print(table(refined_classification$final_category))

# Update restaurant classifications to mark unhealthy establishments
# Update restaurant classifications to mark unhealthy establishments
refined_classification <- refined_classification %>%
  mutate(
    final_category = case_when(
      
      # Previous patterns
      str_detect(tolower(`Company Name`), 
                 "subway|raising cane|grandy|schlotzsky|blimpie|potbelly|chick fil|sonic") ~ "unhealthy",
      
      str_detect(tolower(`Company Name`), 
                 "bbq|bar b q|barbeque|bbque|smokehouse|smoke house|dickey|rudy's") ~ "unhealthy",
      
      str_detect(tolower(`Company Name`), 
                 "bar|pub|lounge|tavern|sports|twin peaks|wing|beer") ~ "unhealthy",
      
      str_detect(tolower(`Company Name`), 
                 "buffet|all you can|golden corral|luby") ~ "unhealthy",
      
      # New patterns based on token/bigram analysis
      
      # Steak-related
      str_detect(tolower(`Company Name`), 
                 "steak|steaks|steakhouse|hoffbrau|kobe|prime|angus|frisco") ~ "unhealthy",
      
      # Seafood combinations that tend to be unhealthy
      str_detect(tolower(`Company Name`), 
                 "crab|shells|truluck|fish city|rockfish|fishbone|joe's crab|captain|capt") ~ "unhealthy",
      
      # Additional grill patterns
      str_detect(tolower(`Company Name`), 
                 "grill bar|bar grill|sports grill|mongolian grill|genghis|hibachi") ~ "unhealthy",
      
      # Tex-Mex and specific Mexican chains
      str_detect(tolower(`Company Name`), 
                 "tex mex|southwest grill|moe's southwest|chevys|pedro's|lupe's") ~ "unhealthy",
      
      # Additional chain restaurants
      str_detect(tolower(`Company Name`), 
                 "carrabba|bone daddy|bottlecap|bonefish|boomerjack|icehouse|buffalo|tillman's") ~ "unhealthy",
      
      # Specific food types
      str_detect(tolower(`Company Name`), 
                 "philly|cheesesteak|burger|shake|pizza|pasta|noodle|spaghetti") ~ "unhealthy",
      
      # Additional venue types
      str_detect(tolower(`Company Name`), 
                 "movie grill|alley|tap house|cantina|neighborhood grill") ~ "unhealthy",
      
      # Specific regional chains
      str_detect(tolower(`Company Name`), 
                 "nick & sam|del frisco|bob's steak|kenny's|saltgrass") ~ "unhealthy",
      
      # Additional food indicators
      str_detect(tolower(`Company Name`), 
                 "cheese|grilled cheese|fire|smoke|pit|mesquite") ~ "unhealthy",
      
      # Keep existing classification if no matches
      TRUE ~ final_category
    )
  )

# Verify changes
summary_stats <- refined_classification %>%
  group_by(final_category) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(refined_classification) * 100, 2)
  )

print("Updated Classification Summary:")
print(summary_stats)

## SIC Analysis----
# Update restaurant classifications with SIC consideration
refined_classification <- refined_classification %>%
  mutate(
    final_category = case_when(
      # SIC-based classifications
      `Primary SIC Description` %in% c("Pizza", "Bars", "Night Clubs", "Cocktail Lounges", 
                                       "Pubs", "Comedy Clubs", "Karaoke Clubs") ~ "unhealthy",
      
      # Previous name-based patterns
      str_detect(tolower(`Company Name`), 
                 "subway|raising cane|grandy|schlotzsky|blimpie|potbelly|chick fil|sonic") ~ "unhealthy",
      
      str_detect(tolower(`Company Name`), 
                 "bbq|bar b q|barbeque|bbque|smokehouse|smoke house|dickey|rudy's") ~ "unhealthy",
      
      str_detect(tolower(`Company Name`), 
                 "bar|pub|lounge|tavern|sports|twin peaks|wing|beer") ~ "unhealthy",
      
      str_detect(tolower(`Company Name`), 
                 "buffet|all you can|golden corral|luby") ~ "unhealthy",
      
      str_detect(tolower(`Company Name`), 
                 "steak|steaks|steakhouse|hoffbrau|kobe|prime|angus|frisco") ~ "unhealthy",
      
      str_detect(tolower(`Company Name`), 
                 "pizza|burger|sandwich|wings|taco|burrito|nachos") ~ "unhealthy",
      
      str_detect(tolower(`Company Name`), 
                 "pasta|noodle|spaghetti|fettuccine|linguine|ramen|udon") ~ "unhealthy",
      
      # Healthy SIC overrides
      `Primary SIC Description` %in% c("Health Clubs Studios & Gymnasiums", 
                                       "Health Spas", "Pilates", "Juice Bars",
                                       "Children's Fitness", "Medical Fitness Centers") 
      & !str_detect(tolower(`Company Name`), 
                    "bar|grill|burger|pizza|steak|wing|pub|lounge") ~ "healthy",
      
      # Keep existing classification if no matches
      TRUE ~ final_category
    )
  )

# Verify changes
summary_stats <- refined_classification %>%
  group_by(`Primary SIC Description`, final_category) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(refined_classification) * 100, 2)
  ) %>%
  arrange(desc(count))

print("Updated Classification Summary:")
print(summary_stats)

## Final Update----
# Comprehensive classification update with "needs_manual_review" default
refined_classification <- refined_classification %>%
  mutate(
    # Pre-process company names to handle special cases
    processed_name = case_when(
      str_detect(tolower(`Company Name`), "barre|barry's|body bar") ~ 
        str_replace_all(tolower(`Company Name`), "barre|barry's|body bar", "FITNESS_TEMP"),
      TRUE ~ tolower(`Company Name`)
    ),
    
    final_category = case_when(
      # Health/Fitness establishments
      `Primary SIC Description` %in% c(
        "Health Clubs Studios & Gymnasiums",
        "Health Spas", 
        "Pilates",
        "Children's Fitness",
        "Medical Fitness Centers",
        "Gymnasiums",
        "Sports & Recreation Facilities Program",
        "Senior Citizen Fitness"
      ) & !str_detect(processed_name, 
                      "grill|burger|pizza|steak|wing|pub|lounge") ~ "healthy",
      
      # Clear unhealthy categories by SIC
      `Primary SIC Description` %in% c(
        "Pizza", 
        "Bars", 
        "Ice Cream Parlors", 
        "Night Clubs",
        "Cocktail Lounges", 
        "Pubs", 
        "Comedy Clubs", 
        "Karaoke Clubs"
      ) ~ "unhealthy",
      
      # Service stations with food
      `Primary SIC Description` == "Service Stations-Gasoline & Oil" &
        str_detect(processed_name, 
                   "food|restaurant|cafe|deli|mart|store") ~ "unhealthy",
      
      # Coffee shops classification
      `Primary SIC Description` == "Coffee Shops" &
        str_detect(processed_name, 
                   "juice|smoothie|health|organic|vegan") ~ "healthy",
      `Primary SIC Description` == "Coffee Shops" ~ "moderately_healthy",
      
      # Carry-out classification
      `Primary SIC Description` == "Foods-Carry Out" &
        str_detect(processed_name, 
                   "fast|burger|pizza|wing|taco|fries|drive|thru") ~ "unhealthy",
      `Primary SIC Description` == "Foods-Carry Out" &
        str_detect(processed_name, 
                   "healthy|organic|vegan|salad|poke|mediterranean") ~ "healthy",
      `Primary SIC Description` == "Foods-Carry Out" ~ "moderately_unhealthy",
      
      # Virtual kitchens
      `Primary SIC Description` == "Virtual Kitchens" &
        str_detect(processed_name, 
                   "healthy|organic|vegan|salad") ~ "moderately_healthy",
      `Primary SIC Description` == "Virtual Kitchens" ~ "moderately_unhealthy",
      
      # Juice bars
      `Primary SIC Description` == "Juice Bars" &
        !str_detect(processed_name, 
                    "grill|pub|lounge") ~ "healthy",
      
      # Restaurant classification by name patterns - Unhealthy
      `Primary SIC Description` == "Restaurants" & 
        str_detect(processed_name, paste(
          "subway|raising cane|grandy|schlotzsky|blimpie|potbelly|chick fil|sonic",
          "mcdonalds|burger king|wendys|arbys|popeyes|kfc|taco bell|whataburger",
          "dairy queen|five guys|in-n-out|carl|jack in|white castle|domino|pizza hut",
          "little caesars|papa john|chuck e cheese|cicis|papa murphy",
          sep="|"
        )) ~ "unhealthy",
      
      `Primary SIC Description` == "Restaurants" &
        str_detect(processed_name, 
                   "bbq|bar b q|barbeque|bbque|smokehouse|smoke house|dickey|rudy") ~ "unhealthy",
      
      `Primary SIC Description` == "Restaurants" &
        str_detect(processed_name, 
                   "bar |pub|lounge|tavern|sports|twin peaks|wing|beer") ~ "unhealthy",
      
      `Primary SIC Description` == "Restaurants" &
        str_detect(processed_name, 
                   "buffet|all you can|golden corral|luby|hometown|ryan|furr") ~ "unhealthy",
      
      `Primary SIC Description` == "Restaurants" &
        str_detect(processed_name, 
                   "steak|steakhouse|hoffbrau|kobe|prime|angus|frisco|outback|longhorn|texas roadhouse") ~ "unhealthy",
      
      # Clearly healthy restaurants
      `Primary SIC Description` == "Restaurants" &
        str_detect(processed_name, paste(
          "vegan|vegetarian|organic|salad|poke|fresh|health|smoothie|juice",
          "clean|protein|nutri|wellness|plant.based|wholesome|farm.to.table",
          sep="|"
        )) ~ "healthy",
      
      # Moderately healthy restaurants
      `Primary SIC Description` == "Restaurants" &
        str_detect(processed_name, paste(
          "sushi|mediterranean|greek|vietnamese|pho|thai|indian|korean|japanese",
          "asian|chinese|dim sum|seafood|fish|grilled|wok|mongolian|teriyaki",
          "kebab|shawarma|falafel|hummus|halal|bowl|bistro|cafe",
          sep="|"
        )) ~ "moderately_healthy",
      
      # Additional unhealthy patterns
      `Primary SIC Description` == "Restaurants" &
        str_detect(processed_name, paste(
          "pizza|burger|sandwich|wings|taco|burrito|nachos|quesadilla",
          "pasta|noodle|spaghetti|fettuccine|linguine|ramen|udon",
          "ice cream|frozen yogurt|dessert|sweet|candy|cake|donut|cookie",
          "drive.thru|drive.in|fast|express|quick|24.hour|delivery",
          sep="|"
        )) ~ "unhealthy",
      
      # Default restaurant classification to needs_manual_review if no patterns match
      `Primary SIC Description` == "Restaurants" ~ "needs_manual_review",
      
      # Keep existing classification if nothing matches
      TRUE ~ final_category
    )
  ) %>%
  select(-processed_name)  # Remove temporary column

# Run validation and summary statistics
summary_stats <- refined_classification %>%
  group_by(`Primary SIC Description`, final_category) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(refined_classification) * 100, 2)
  ) %>%
  arrange(desc(count))

print("Updated Classification Summary:")
print(summary_stats)

# Print sample of establishments needing review
needs_review <- refined_classification %>%
  filter(final_category == "needs_manual_review") %>%
  select(`Company Name`, `Primary SIC Description`) %>%
  arrange(`Company Name`)

print("\nSample of Establishments Needing Review:")
print(head(needs_review, 100))

# Distribution by category
category_distribution <- refined_classification %>%
  group_by(final_category) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(refined_classification) * 100, 2)
  ) %>%
  arrange(desc(count))

print("\nDistribution by Category:")
print(category_distribution)

### More refinement----
# Additional refined classification for remaining review cases
refined_classification <- refined_classification %>%
  mutate(
    final_category = case_when(
      # Keep existing non-review classifications
      final_category != "needs_manual_review" ~ final_category,
      
      # New patterns for remaining reviews
      # Chinese/Asian restaurants (like "2 Go China")
      str_detect(tolower(`Company Name`), 
                 "china|chinese|wok|dragon|panda|golden|palace|dynasty|hong kong|
                lucky|peking|taipei|tokyo|seoul|saigon|hunan|szechuan") ~ "moderately_healthy",
      
      # Chicken places
      str_detect(tolower(`Company Name`), 
                 "chicken|chick|pollo|hen|rooster(?!.*grill)") ~ "unhealthy",
      
      # Local/Family restaurants
      str_detect(tolower(`Company Name`), 
                 "\\d+\\s+(st|street|ave|avenue|rd|road|hwy|highway|n|s|e|w)") ~ "moderately_healthy",
      
      # Additional Mexican/Latin patterns
      str_detect(tolower(`Company Name`), 
                 "taqueria|tortilla|jalisco|guadalajara|el\\s|la\\s|los\\s|las\\s|
                comida|cocina|restaurante") ~ "moderately_healthy",
      
      # Additional Mediterranean/Middle Eastern
      str_detect(tolower(`Company Name`), 
                 "kebab|gyro|shawarma|falafel|hummus|pita|mediterranean|
                turkish|persian|lebanese|arabic") ~ "moderately_healthy",
      
      # Additional healthy indicators
      str_detect(tolower(`Company Name`), 
                 "garden|harvest|natural|green|fresh|organic|farm|market") ~ "healthy",
      
      # Additional unhealthy indicators
      str_detect(tolower(`Company Name`), 
                 "grill(?!.*fish)|grille|diner|drive|thru|express|
                shack|joint|house") ~ "unhealthy",
      
      # Traditional restaurants
      str_detect(tolower(`Company Name`), 
                 "kitchen|cafe|bistro|eatery|dining|restaurant|home|country") ~ "moderately_healthy",
      
      # Numbers and street names (likely local establishments)
      str_detect(tolower(`Company Name`), 
                 "^\\d+\\s+[a-z]+|\\d+th|\\d+nd|\\d+rd|\\d+st") ~ "moderately_healthy",
      
      # Keep as needs_manual_review if still no match
      TRUE ~ "needs_manual_review"
    )
  )

# Run updated statistics to see impact
summary_stats <- refined_classification %>%
  group_by(`Primary SIC Description`, final_category) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(refined_classification) * 100, 2)
  ) %>%
  arrange(desc(count))

print("Updated Classification Summary:")
print(summary_stats)

# Check remaining review cases
remaining_review <- refined_classification %>%
  filter(final_category == "needs_manual_review") %>%
  select(`Company Name`, `Primary SIC Description`) %>%
  arrange(`Company Name`)

print("\nRemaining Cases Needing Review:")
print(head(remaining_review, 100))

###Token and Bigram analysis of needs review----
# Analyze tokens and bigrams from remaining review cases
review_establishments <- refined_classification %>%
  filter(final_category == "needs_manual_review") %>%
  # Preprocess company names
  mutate(
    processed_name = tolower(`Company Name`),
    # Remove digits and special characters for cleaner analysis
    processed_name = str_replace_all(processed_name, "[0-9]", ""),
    processed_name = str_replace_all(processed_name, "[^a-z\\s]", " "),
    processed_name = str_squish(processed_name)
  )

# Create tokens (single words)
review_tokens <- review_establishments %>%
  unnest_tokens(word, processed_name) %>%
  anti_join(stop_words) %>%  # Remove common stop words
  filter(
    !str_detect(word, "^[0-9]+$"),  # Remove pure numbers
    nchar(word) > 1  # Remove single characters
  ) %>%
  count(word, sort = TRUE)

# Create bigrams (two-word combinations)
review_bigrams <- review_establishments %>%
  unnest_tokens(bigram, processed_name, token = "ngrams", n = 2) %>%
  filter(!str_detect(bigram, "^[0-9 ]+$")) %>%  # Remove pure number combinations
  count(bigram, sort = TRUE)

# Create trigrams (three-word combinations) for additional context
review_trigrams <- review_establishments %>%
  unnest_tokens(trigram, processed_name, token = "ngrams", n = 3) %>%
  filter(!str_detect(trigram, "^[0-9 ]+$")) %>%
  count(trigram, sort = TRUE)

# Analyze common word patterns
print("Most Common Single Words in Review Cases:")
print(review_tokens %>% filter(n >= 5), n=100)

print("\nMost Common Two-Word Combinations in Review Cases:")
print(review_bigrams %>% filter(n >= 3), n=100)

print("\nMost Common Three-Word Combinations in Review Cases:")
print(review_trigrams %>% filter(n >= 2), n=50)

# Analyze patterns with SIC descriptions
pattern_analysis <- review_establishments %>%
  group_by(`Primary SIC Description`) %>%
  summarise(
    count = n(),
    sample_names = paste(head(unique(`Company Name`), 3), collapse = "; ")
  ) %>%
  arrange(desc(count))

print("\nSIC Description Distribution in Review Cases:")
print(pattern_analysis)

# Analyze specific word associations
specific_words <- c("grill", "restaurant", "cafe", "kitchen", "house", "express", "buffet")
for(word in specific_words) {
  print(paste("\nEstablishments containing '", word, "':", sep=""))
  print(review_establishments %>% 
          filter(str_detect(processed_name, word)) %>%
          select(`Company Name`) %>%
          head(5))
}

# Get distribution of name lengths
name_length_dist <- review_establishments %>%
  mutate(
    word_count = str_count(processed_name, "\\S+")
  ) %>%
  group_by(word_count) %>%
  summarise(
    count = n(),
    examples = paste(head(unique(`Company Name`), 3), collapse = "; ")
  ) %>%
  arrange(desc(count))

print("\nDistribution of Name Lengths (word count):")
print(name_length_dist)

# Suggest additional classification patterns based on analysis
print("\nPotential New Classification Patterns:")
print(review_tokens %>% 
        filter(n >= 10) %>%  # Focus on frequently occurring words
        mutate(
          suggested_category = case_when(
            word %in% c("grill", "express", "buffet", "house") ~ "likely_unhealthy",
            word %in% c("cafe", "bistro", "fresh") ~ "likely_moderately_healthy",
            word %in% c("organic", "natural", "vegan") ~ "likely_healthy",
            TRUE ~ "needs_further_review"
          )
        ) %>%
        arrange(suggested_category, desc(n)))

### Refine Classification----
# Update classification with new patterns from token/bigram analysis
refined_classification <- refined_classification %>%
  mutate(
    final_category = case_when(
      # Keep existing non-review classifications
      final_category != "needs_manual_review" ~ final_category,
      
      # Major Chain Restaurants - Clearly Unhealthy
      str_detect(tolower(`Company Name`), paste(
        "mc donald|wendy|arby|denny|applebee|tgi friday|hooters|fuddruckers",
        "quiznos|panera|murphy|uncle julio|dave buster|bone daddy",
        sep="|"
      )) ~ "unhealthy",
      
      # Deli/Subs - Mixed Classifications
      str_detect(tolower(`Company Name`), "jason.*deli|jimmy john|jersey mike") ~ "moderately_healthy",
      str_detect(tolower(`Company Name`), "sub.*shop|which wich|cousin.*deli") ~ "moderately_unhealthy",
      
      # Mexican/Tex-Mex Establishments
      str_detect(tolower(`Company Name`), paste(
        "tex mex|cantina laredo|pappasito|blue goose cantina|mexican food",
        "mexican cuisine|mexican rstrnt|qdoba|julio",
        sep="|"
      )) ~ "unhealthy",
      
      # Seafood Establishments
      str_detect(tolower(`Company Name`), paste(
        "long john silver|joe.*crab|red lobster|oyster bar|fish|crawfish",
        sep="|"
      )) ~ "unhealthy",
      
      # Italian Establishments
      str_detect(tolower(`Company Name`), paste(
        "carino|ristorante|pizzeria|buca di beppo|italian rstrnt",
        sep="|"
      )) ~ "unhealthy",
      
      # BBQ/Southern
      str_detect(tolower(`Company Name`), paste(
        "bar b|b q|bbq|bar.*que|southern|soul food|ribs|riscky",
        sep="|"
      )) ~ "unhealthy",
      
      # Asian Establishments
      str_detect(tolower(`Company Name`), paste(
        "chopstix|taste of asia|egg roll|royal|asian cuisine",
        sep="|"
      )) ~ "moderately_healthy",
      
      # Bars/Wine
      str_detect(tolower(`Company Name`), paste(
        "wine bar|bar.*grill|cantina|crab shack",
        sep="|"
      )) ~ "unhealthy",
      
      # Bakery/Dessert
      str_detect(tolower(`Company Name`), paste(
        "au bon pain|baker|bread|yogurt",
        sep="|"
      )) ~ "unhealthy",
      
      # Specific Regional Chains
      str_detect(tolower(`Company Name`), paste(
        "texas land cattle|bahama buck|matt.*rancho|gloria",
        sep="|"
      )) ~ "unhealthy",
      
      # Local Establishments
      str_detect(tolower(`Company Name`), paste(
        "downtown|uptown|fort worth|dallas|street|local",
        sep="|"
      )) ~ "moderately_healthy",
      
      # Keep as review if still no match
      TRUE ~ "needs_manual_review"
    )
  )

# Verify impact
updated_stats <- refined_classification %>%
  group_by(final_category) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(refined_classification) * 100, 2),
    sample_names = paste(head(unique(`Company Name`), 3), collapse = "; ")
  ) %>%
  arrange(desc(count))

print("Updated Classification Distribution:")
print(updated_stats)

# Check remaining review cases
remaining_review <- refined_classification %>%
  filter(final_category == "needs_manual_review") %>%
  select(`Company Name`, `Primary SIC Description`) %>%
  arrange(`Company Name`)

print("\nRemaining Cases Needing Review:")
print(head(remaining_review, 20))

### LLC, locations, special----
refined_classification <- refined_classification %>%
  mutate(
    # Pre-process names to handle special cases
    processed_name = tolower(`Company Name`),
    
    final_category = case_when(
      # Keep existing non-review classifications
      final_category != "needs_manual_review" ~ final_category,
      
      # Clear unhealthy patterns from remaining review
      str_detect(processed_name, "rib shack|ribs|buffalo('s)?|wing|buffalo") ~ "unhealthy",
      
      # Business entities that need verification
      str_detect(processed_name, paste(
        "concepts llc|solutions|svc|enterprises|corp|services",
        "management|restaurant group|food service|catering",
        sep="|"
      )) ~ "needs_verification",
      
      # Generic locations that need context
      str_detect(processed_name, paste(
        "on main|downtown|uptown|plaza|street|avenue|center",
        "shopping|mall|terminal|airport",
        sep="|"
      )) ~ "needs_location_context",
      
      # Franchise indicators
      str_detect(processed_name, "#\\d+|number \\d+|no \\d+") ~ "needs_franchise_check",
      
      # Additional unhealthy patterns
      str_detect(processed_name, paste(
        "oasis|shack|joint|hut|express|drive|thru|quick",
        "fast|snack|treats|sweets|candy|ice cream|frozen",
        sep="|"
      )) ~ "unhealthy",
      
      # Local establishment patterns
      str_detect(processed_name, paste(
        "^[a-z]'s|^[a-z] & [a-z]'s|^the [a-z]+|^old|^original",
        "family|kitchen|home cooking|house|cafe",
        sep="|"
      )) ~ "moderately_healthy",
      
      # Ethnic cuisine patterns
      str_detect(processed_name, paste(
        "tandoor|kebab|pho|wok|dragon|bamboo|sakura|sushi",
        "thai|vietnamese|korean|indian|mediterranean",
        sep="|"
      )) ~ "moderately_healthy",
      
      # Additional chain identifiers
      str_detect(processed_name, paste(
        "famous|original|world|king|queen|prince|royal",
        "golden|silver|diamond|premium|supreme|ultimate",
        sep="|"
      )) ~ "unhealthy",
      
      # Keep remaining cases for manual review
      TRUE ~ "needs_manual_review"
    )
  ) %>%
  # Create verification flags
  mutate(
    needs_verification = case_when(
      final_category == "needs_verification" ~ TRUE,
      final_category == "needs_location_context" ~ TRUE,
      final_category == "needs_franchise_check" ~ TRUE,
      TRUE ~ FALSE
    ),
    # Update final category for verification cases
    final_category = case_when(
      final_category %in% c("needs_verification", 
                            "needs_location_context",
                            "needs_franchise_check") ~ "needs_manual_review",
      TRUE ~ final_category
    )
  ) %>%
  select(-processed_name)  # Remove temporary column

# Generate updated statistics
updated_stats <- refined_classification %>%
  group_by(final_category) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(refined_classification) * 100, 2),
    sample_names = paste(head(unique(`Company Name`), 3), collapse = "; ")
  ) %>%
  arrange(desc(count))

print("Updated Classification Distribution:")
print(updated_stats)

# Check remaining review cases with verification flags
remaining_review <- refined_classification %>%
  filter(final_category == "needs_manual_review") %>%
  mutate(
    review_type = case_when(
      needs_verification ~ "Needs Business Verification",
      TRUE ~ "Needs Name Review"
    )
  ) %>%
  select(`Company Name`, `Primary SIC Description`, review_type) %>%
  arrange(review_type, `Company Name`)

print("\nRemaining Cases by Review Type:")
print(table(remaining_review$review_type))

print("\nSample of Remaining Cases:")
print(head(remaining_review, 20))

### Tokens, Bigrams, and Trigrams of Remaining Review----
# Analyze tokens and bigrams from the remaining manual review cases
remaining_analysis <- remaining_review %>%
  # Preprocess company names
  mutate(
    processed_name = tolower(`Company Name`),
    processed_name = str_replace_all(processed_name, "[^a-z\\s]", " "),
    processed_name = str_squish(processed_name)
  )

# Create tokens (single words)
final_review_tokens <- remaining_analysis %>%
  unnest_tokens(word, processed_name) %>%
  anti_join(stop_words) %>%  # Remove common stop words
  filter(
    !str_detect(word, "^[0-9]+$"),  # Remove pure numbers
    nchar(word) > 1  # Remove single characters
  ) %>%
  count(word, sort = TRUE)

# Create bigrams (two-word combinations)
final_review_bigrams <- remaining_analysis %>%
  unnest_tokens(bigram, processed_name, token = "ngrams", n = 2) %>%
  filter(!str_detect(bigram, "^[0-9 ]+$")) %>%  # Remove pure number combinations
  count(bigram, sort = TRUE)

# Create trigrams (three-word combinations)
final_review_trigrams <- remaining_analysis %>%
  unnest_tokens(trigram, processed_name, token = "ngrams", n = 3) %>%
  filter(!str_detect(trigram, "^[0-9 ]+$")) %>%
  count(trigram, sort = TRUE)

# Analyze review types
review_type_analysis <- remaining_analysis %>%
  group_by(review_type) %>%
  summarise(
    count = n(),
    sample_names = paste(head(unique(`Company Name`), 3), collapse = "; ")
  )

# Print results
print("Most Common Single Words in Remaining Review Cases:")
print(final_review_tokens %>% filter(n >= 5), n=100)

print("\nMost Common Two-Word Combinations in Remaining Review Cases:")
print(final_review_bigrams %>% filter(n >= 3), n=100)

print("\nMost Common Three-Word Combinations in Remaining Review Cases:")
print(final_review_trigrams %>% filter(n >= 2), n=50)

print("\nDistribution by Review Type:")
print(review_type_analysis)

# Analyze patterns by review type
patterns_by_type <- remaining_analysis %>%
  group_by(review_type) %>%
  summarise(
    common_words = paste(head(final_review_tokens$word[final_review_tokens$n >= 10], 5), collapse=", "),
    common_bigrams = paste(head(final_review_bigrams$bigram[final_review_bigrams$n >= 5], 5), collapse=", ")
  )

print("\nCommon Patterns by Review Type:")
print(patterns_by_type)

# Additional analysis of specific patterns
business_indicators <- c("llc", "inc", "corp", "services", "enterprises", "group")
location_indicators <- c("main", "street", "ave", "plaza", "center", "mall")
franchise_indicators <- c("no", "number", "#", "loc", "location")

for(indicator in c(business_indicators, location_indicators, franchise_indicators)) {
  matches <- remaining_analysis %>%
    filter(str_detect(processed_name, indicator)) %>%
    select(`Company Name`) %>%
    head(5)
  
  print(paste("\nSample establishments with '", indicator, "':", sep=""))
  print(matches)
}

##Final Classification----
# Final classification of remaining review cases
refined_classification <- refined_classification %>%
  mutate(
    final_category = case_when(
      # Keep existing non-review classifications
      final_category != "needs_manual_review" ~ final_category,
      
      # Unhealthy Establishments
      
      # Bar and alcohol-related
      str_detect(tolower(`Company Name`), paste(
        "bar b q|bar b|b q|barbecue|red hot blue|dave buster|
        houlihan|redneck heaven|coal vines|snuffer",
        sep="|"
      )) ~ "unhealthy",
      
      # Specific chains and franchises
      str_detect(tolower(`Company Name`), paste(
        "mc alister|zaxby|freddy|culver|maggiano|tin star|
        paciugo|hook line sinker|medieval times",
        sep="|"
      )) ~ "unhealthy",
      
      # Specific cuisine types indicating unhealthy
      str_detect(tolower(`Company Name`), paste(
        "tex mex|fashioned hmbrgrs|short stop food|big daddy|
        cajun|gelato|melting pot",
        sep="|"
      )) ~ "unhealthy",
      
      # Deli and sub shops
      str_detect(tolower(`Company Name`), "deli|sub shop|york sub") ~ "moderately_unhealthy",
      
      # Moderately Healthy Establishments
      
      # Asian cuisine
      str_detect(tolower(`Company Name`), paste(
        "viet tofu|kuai dumplings|asian cuisine|pho|
        hibachi(?!.*grill)|sushi|thai",
        sep="|"
      )) ~ "moderately_healthy",
      
      # Mediterranean/Italian
      str_detect(tolower(`Company Name`), paste(
        "italian villa|penne pomodoro|pomodoro|
        mediterranean|greek|falafel",
        sep="|"
      )) ~ "moderately_healthy",
      
      # Health-focused establishments
      str_detect(tolower(`Company Name`), paste(
        "salata|first watch|natural|organic|
        wholesome|fresh|garden",
        sep="|"
      )) ~ "healthy",
      
      # Coffee shops and cafes
      str_detect(tolower(`Company Name`), 
                 "coffee shop|caffe|tea room") ~ "moderately_healthy",
      
      # Business entities and generic names
      str_detect(tolower(`Company Name`), paste(
        "llc$|inc$|corp$|services|enterprises|group|
        holdings|management|solutions",
        sep="|"
      )) ~ "needs_verification",
      
      # Additional unhealthy indicators
      str_detect(tolower(`Company Name`), paste(
        "burger|pizza|wings|fries|shack|
        drive|thru|express|fast|quick",
        sep="|"
      )) ~ "unhealthy",
      
      # Mexican/Latin American
      str_detect(tolower(`Company Name`), paste(
        "mariscos|pupuseria|tortilleria|antojitos|
        dos chiles|meso maya|anamia",
        sep="|"
      )) ~ "moderately_healthy",
      
      # Location-based that need verification
      str_detect(tolower(`Company Name`), paste(
        "on main|plaza|mall$|center|
        medical ctr|shopping|terminal",
        sep="|"
      )) ~ "needs_verification",
      
      # Keep remaining cases flagged for review
      TRUE ~ "needs_manual_review"
    )
  )

# Create separate lists for different types of review needs
verification_needed <- refined_classification %>%
  filter(final_category == "needs_verification") %>%
  select(`Company Name`, `Primary SIC Description`) %>%
  arrange(`Company Name`)

manual_review_needed <- refined_classification %>%
  filter(final_category == "needs_manual_review") %>%
  select(`Company Name`, `Primary SIC Description`) %>%
  arrange(`Company Name`)

# Generate final statistics
final_stats <- refined_classification %>%
  group_by(final_category) %>%
  summarise(
    count = n(),
    percentage = round(n() / nrow(refined_classification) * 100, 2),
    sample_names = paste(head(unique(`Company Name`), 3), collapse = "; ")
  ) %>%
  arrange(desc(count))

print("Final Classification Distribution:")
print(final_stats)

print("\nEstablishments Needing Verification:")
print(head(verification_needed, 20))

print("\nEstablishments Needing Manual Review:")
print(head(manual_review_needed, 20))

# Optional: Save review lists to files for manual processing
write.csv(verification_needed, "establishments_needing_verification.csv", row.names = FALSE)
write.csv(manual_review_needed, "establishments_needing_manual_review.csv", row.names = FALSE)


#NLP----
library(tidytext)
library(tidyverse)
library(quanteda)      # For text preprocessing and n-grams
library(quanteda.textstats) # For collocations
library(spacyr)        # For POS tagging
library(textrank)      # For keyword extraction

# Initialize spaCy
spacy_initialize()

# Function to preprocess text and create corpus
create_restaurant_corpus <- function(data, name_col) {
  # Create corpus with document IDs
  corp <- corpus(
    data %>% 
      mutate(doc_id = row_number()) %>% 
      select(doc_id, !!sym(name_col)),
    text_field = name_col
  )
  
  # Tokenize and clean
  toks <- tokens(corp,
                 remove_punct = TRUE,
                 remove_numbers = TRUE,
                 remove_symbols = TRUE) %>%
    tokens_tolower()
  
  return(list(corpus = corp, tokens = toks))
}

# Function to extract n-grams and collocations
analyze_ngrams <- function(tokens, label) {
  # Create dfm
  dfm <- dfm(tokens)
  
  # Get unigrams
  unigrams <- textstat_frequency(dfm) %>%
    select(feature, frequency) %>%
    rename(term = feature)
  
  # Get bigrams
  bigrams <- tokens_ngrams(tokens, n = 2) %>%
    dfm() %>%
    textstat_frequency() %>%
    select(feature, frequency) %>%
    rename(term = feature)
  
  # Get trigrams
  trigrams <- tokens_ngrams(tokens, n = 3) %>%
    dfm() %>%
    textstat_frequency() %>%
    select(feature, frequency) %>%
    rename(term = feature)
  
  # Get collocations
  collocs <- textstat_collocations(tokens, size = 2:3)
  
  # Print results
  print(paste("\n===", label, "Analysis ==="))
  
  print("\nTop Unigrams:")
  print(head(unigrams, 20))
  
  print("\nTop Bigrams:")
  print(head(bigrams, 20))
  
  print("\nTop Trigrams:")
  print(head(trigrams, 20))
  
  print("\nSignificant Collocations:")
  print(head(collocs, 20))
  
  return(list(
    unigrams = unigrams,
    bigrams = bigrams,
    trigrams = trigrams,
    collocations = collocs
  ))
}

# Function to get POS tags and dependencies
analyze_pos <- function(data, name_col) {
  # Parse with spaCy
  parsed <- spacy_parse(
    data %>% pull(!!sym(name_col)),
    pos = TRUE,
    tag = TRUE,
    dependency = TRUE
  )
  
  # Analyze POS patterns
  pos_patterns <- parsed %>%
    count(pos, token, sort = TRUE) %>%
    filter(!pos %in% c("PUNCT", "NUM", "SPACE"))
  
  # Analyze dependencies
  dep_patterns <- parsed %>%
    count(dep_rel, token, sort = TRUE)
  
  return(list(
    parsed = parsed,
    pos_patterns = pos_patterns,
    dep_patterns = dep_patterns
  ))
}

# Analyze verification needed establishments
verification_text <- create_restaurant_corpus(verification_needed, "Company Name")
verification_ngrams <- analyze_ngrams(verification_text$tokens, "Verification Needed")
verification_pos <- analyze_pos(verification_needed, "Company Name")

# Analyze manual review needed establishments
manual_review_text <- create_restaurant_corpus(manual_review_needed, "Company Name")
manual_review_ngrams <- analyze_ngrams(manual_review_text$tokens, "Manual Review Needed")
manual_review_pos <- analyze_pos(manual_review_needed, "Company Name")

# Analyze patterns across both datasets
print("\nCommon Patterns Analysis:")

# Function to identify common patterns
analyze_common_patterns <- function(ngrams, pos_data, label) {
  # Get most frequent meaningful combinations
  common_patterns <- ngrams$collocations %>%
    filter(lambda > 2) %>%  # Filter for significant collocations
    arrange(desc(count))
  
  # Get common POS sequences
  pos_sequences <- pos_data$parsed %>%
    group_by(doc_id) %>%
    summarise(pos_sequence = paste(pos, collapse = " ")) %>%
    count(pos_sequence, sort = TRUE)
  
  print(paste("\n===", label, "Common Patterns ==="))
  print("\nSignificant Word Combinations:")
  print(head(common_patterns, 20))
  
  print("\nCommon POS Sequences:")
  print(head(pos_sequences, 20))
  
  return(list(
    patterns = common_patterns,
    pos_sequences = pos_sequences
  ))
}

verification_patterns <- analyze_common_patterns(
  verification_ngrams, 
  verification_pos, 
  "Verification Needed"
)

manual_review_patterns <- analyze_common_patterns(
  manual_review_ngrams, 
  manual_review_pos, 
  "Manual Review Needed"
)

# Create classification rules based on discovered patterns
restaurant_patterns <- list(
  # Business/Corporate Entities (needs verification)
  business = list(
    unigrams = c("inc", "llc", "corp", "enterprises", "management", "holdings", "solutions", "industries"),
    bigrams = c("enterprises inc", "foods inc", "franchise inc", "holdings llc", "entertainment inc"),
    pos_sequences = c("PROPN PROPN PROPN", "PROPN PROPN", "NOUN PROPN PROPN")
  ),
  
  # Fast Food/Unhealthy
  unhealthy = list(
    # Direct indicators
    bigrams = c("hot blue", "red hot", "dave buster", "land cattle", "burger king",
                "fast food", "fried chicken", "ice cream"),
    # Cooking method indicators
    cooking = c("fried", "deep fried", "battered"),
    # Food type indicators
    food_types = c("burgers", "wings", "fries", "pizza", "hot dogs", "bbq", "bar b q"),
    pos_sequences = c("ADJ NOUN", "NOUN PART")
  ),
  
  # Healthy
  healthy = list(
    unigrams = c("salad", "organic", "vegan", "fresh", "natural", "garden"),
    bigrams = c("fresh market", "juice bar", "salad bar"),
    food_types = c("Mediterranean", "sushi", "poke", "vegetarian"),
    pos_sequences = c("ADJ NOUN", "NOUN ADP NOUN")
  ),
  
  # Moderate
  moderate = list(
    # Cultural cuisine indicators
    cuisine = c("taste of", "le peep", "rice xpress", "new orleans", "italian", "mexican", "thai"),
    # Restaurant types
    types = c("bistro", "cafe", "kitchen", "restaurant"),
    pos_sequences = c("PROPN PART NOUN", "NOUN ADP PROPN")
  ),
  
  # Location/Context Needed
  location = list(
    unigrams = c("texas", "dallas", "plano", "medical"),
    bigrams = c("medical ctr", "on main", "at the", "in the"),
    pos_sequences = c("PROPN ADP PROPN", "NOUN ADP PROPN")
  )
)

# Function to classify restaurants based on patterns
classify_restaurant <- function(name, pos_sequence) {
  name_lower <- tolower(name)
  
  # Check business entities first
  if(any(str_detect(name_lower, paste(restaurant_patterns$business$unigrams, collapse="|"))) ||
     any(str_detect(name_lower, paste(restaurant_patterns$business$bigrams, collapse="|"))) ||
     pos_sequence %in% restaurant_patterns$business$pos_sequences) {
    return("needs_verification")
  }
  
  # Check unhealthy indicators
  if(any(str_detect(name_lower, paste(restaurant_patterns$unhealthy$bigrams, collapse="|"))) ||
     any(str_detect(name_lower, paste(restaurant_patterns$unhealthy$cooking, collapse="|"))) ||
     any(str_detect(name_lower, paste(restaurant_patterns$unhealthy$food_types, collapse="|")))) {
    return("unhealthy")
  }
  
  # Check healthy indicators
  if(any(str_detect(name_lower, paste(restaurant_patterns$healthy$unigrams, collapse="|"))) ||
     any(str_detect(name_lower, paste(restaurant_patterns$healthy$bigrams, collapse="|"))) ||
     any(str_detect(name_lower, paste(restaurant_patterns$healthy$food_types, collapse="|")))) {
    return("healthy")
  }
  
  # Check moderate indicators
  if(any(str_detect(name_lower, paste(restaurant_patterns$moderate$cuisine, collapse="|"))) ||
     any(str_detect(name_lower, paste(restaurant_patterns$moderate$types, collapse="|")))) {
    return("moderately_healthy")
  }
  
  # Check location/context
  if(any(str_detect(name_lower, paste(restaurant_patterns$location$unigrams, collapse="|"))) ||
     any(str_detect(name_lower, paste(restaurant_patterns$location$bigrams, collapse="|"))) ||
     pos_sequence %in% restaurant_patterns$location$pos_sequences) {
    return("needs_location_context")
  }
  
  return("needs_manual_review")
}

# Apply classification
classify_establishments <- function(data, pos_data) {
  data %>%
    mutate(
      pos_sequence = pos_data$parsed %>%
        group_by(doc_id) %>%
        summarise(pos_seq = paste(pos, collapse = " ")) %>%
        pull(pos_seq),
      classification = map2_chr(`Company Name`, pos_sequence, classify_restaurant)
    )
}

# Test the classification
verification_classified <- classify_establishments(verification_needed, verification_pos)
manual_review_classified <- classify_establishments(manual_review_needed, manual_review_pos)

# Print results
print("\nClassification Results:")
print("Verification Needed:")
print(verification_classified %>% count(classification, sort = TRUE))
print("\nManual Review Needed:")
print(manual_review_classified %>% count(classification, sort = TRUE))

# Create refined classification patterns based on actual data
refined_patterns <- list(
  # Business entities requiring verification
  business_entities = list(
    tokens = c("inc", "llc", "corp", "enterprises", "group", "management", 
               "holdings", "solutions", "franchise", "entertainment", "industries"),
    bigrams = c("enterprises inc", "foods inc", "food inc", "yum brands",
                "brands inc", "butler franchise", "franchise inc", "holdings llc",
                "foods llc", "cec entertainment"),
    trigrams = c("yum brands inc", "butler franchise inc", "cec entertainment inc")
  ),
  
  # Restaurant types
  restaurant_types = list(
    casual_dining = c(
      "taste of", "bar grill", "kitchen", "cafe", "bistro", "diner",
      "restaurant", "eatery", "cantina", "dining"
    ),
    fast_food = c(
      "express", "fast", "quick", "drive", "thru", "to go"
    ),
    specialty = c(
      "bakery", "pizzeria", "steakhouse", "bbq", "bar b q", "barbecue",
      "sushi", "pho", "taqueria"
    )
  ),
  
  # Cuisine indicators
  cuisine_types = list(
    asian = c(
      "chinese", "japanese", "thai", "vietnamese", "korean", "asian",
      "sushi", "pho", "wok", "rice", "noodle"
    ),
    mexican = c(
      "mexican", "tex mex", "taqueria", "taco", "cantina", "casa",
      "dos", "tres", "el", "la", "los", "las"
    ),
    italian = c(
      "italian", "pizzeria", "pasta", "pizza", "ristorante"
    ),
    american = c(
      "grill", "bar", "burger", "steak", "bbq", "bar b q", "wings",
      "american", "diner"
    ),
    other = c(
      "mediterranean", "greek", "indian", "cajun", "brazilian",
      "european", "french", "german"
    )
  ),
  
  # Food type indicators
  food_types = list(
    proteins = c(
      "burger", "steak", "chicken", "beef", "fish", "seafood",
      "wings", "bbq", "bar b q"
    ),
    specialty_items = c(
      "pizza", "sushi", "pho", "tacos", "pasta", "noodles",
      "rice", "sandwich", "salad"
    )
  ),
  
  # Location/Context
  location = list(
    places = c("texas", "dallas", "plano", "fort worth", "richardson", "irving"),
    contexts = c("mall", "center", "plaza", "square", "street", "avenue", "medical")
  )
)

# Function to classify based on refined patterns
classify_restaurant_refined <- function(name, tokens, bigrams, trigrams) {
  name_lower <- tolower(name)
  
  # Helper function to check pattern matches
  check_patterns <- function(text, patterns) {
    any(sapply(patterns, function(p) str_detect(text, fixed(p))))
  }
  
  # Check business entities first
  if(check_patterns(name_lower, refined_patterns$business_entities$tokens) ||
     check_patterns(name_lower, refined_patterns$business_entities$bigrams) ||
     check_patterns(name_lower, refined_patterns$business_entities$trigrams)) {
    return("needs_verification")
  }
  
  # Check restaurant types
  restaurant_type <- case_when(
    check_patterns(name_lower, refined_patterns$restaurant_types$fast_food) ~ "fast_food",
    check_patterns(name_lower, refined_patterns$restaurant_types$casual_dining) ~ "casual_dining",
    check_patterns(name_lower, refined_patterns$restaurant_types$specialty) ~ "specialty",
    TRUE ~ NA_character_
  )
  
  # Check cuisine type
  cuisine_type <- case_when(
    check_patterns(name_lower, refined_patterns$cuisine_types$asian) ~ "asian",
    check_patterns(name_lower, refined_patterns$cuisine_types$mexican) ~ "mexican",
    check_patterns(name_lower, refined_patterns$cuisine_types$italian) ~ "italian",
    check_patterns(name_lower, refined_patterns$cuisine_types$american) ~ "american",
    check_patterns(name_lower, refined_patterns$cuisine_types$other) ~ "other",
    TRUE ~ NA_character_
  )
  
  # Determine health classification based on restaurant and cuisine type
  if(!is.na(restaurant_type) || !is.na(cuisine_type)) {
    return(case_when(
      restaurant_type == "fast_food" ~ "unhealthy",
      restaurant_type == "casual_dining" && cuisine_type %in% c("asian", "other") ~ "moderately_healthy",
      cuisine_type %in% c("asian", "other") ~ "moderately_healthy",
      cuisine_type %in% c("mexican", "italian", "american") ~ "moderately_unhealthy",
      TRUE ~ "needs_manual_review"
    ))
  }
  
  # Check location context
  if(check_patterns(name_lower, refined_patterns$location$places) ||
     check_patterns(name_lower, refined_patterns$location$contexts)) {
    return("needs_location_context")
  }
  
  return("needs_manual_review")
}

# Apply refined classification
apply_refined_classification <- function(data) {
  data %>%
    mutate(
      classification = map_chr(`Company Name`, 
                               ~classify_restaurant_refined(., tokens, bigrams, trigrams))
    )
}

# Test refined classification
verification_refined <- apply_refined_classification(verification_needed)
manual_review_refined <- apply_refined_classification(manual_review_needed)

# Print results
print("\nRefined Classification Results:")
print("Verification Needed:")
print(verification_refined %>% count(classification, sort = TRUE))
print("\nManual Review Needed:")
print(manual_review_refined %>% count(classification, sort = TRUE))

## Last NLP Classification----
# Combine NLP insights with our original patterns for a more comprehensive classification system
final_classification_patterns <- list(
  # Business entities identified through NLP analysis of verification cases
  business_entities = list(
    # Top frequent individual business-related words
    tokens = unique(c(
      verification_ngrams$unigrams$term[1:10],  # Most common business tokens from verification cases
      refined_patterns$business_entities$tokens  # Our original business tokens
    )),
    # Common two-word business combinations
    bigrams = unique(c(
      gsub("_", " ", verification_ngrams$bigrams$term[1:10]),  # Convert NLP bigrams to space-separated
      refined_patterns$business_entities$bigrams  # Our original business bigrams
    )),
    # Three-word business patterns
    trigrams = unique(c(
      gsub("_", " ", verification_ngrams$trigrams$term[1:10]),  # Convert NLP trigrams to space-separated
      refined_patterns$business_entities$trigrams  # Our original business trigrams
    ))
  ),
  
  # Restaurant and cuisine patterns from manual review analysis
  restaurant_patterns = list(
    # Individual restaurant-related words
    tokens = unique(c(
      manual_review_ngrams$unigrams$term[1:20],  # Most common restaurant terms
      unlist(refined_patterns$restaurant_types)   # Our original restaurant type patterns
    )),
    # Two-word restaurant/cuisine combinations
    bigrams = unique(c(
      gsub("_", " ", manual_review_ngrams$bigrams$term[1:20]),  # Common restaurant word pairs
      unlist(refined_patterns$cuisine_types)  # Our original cuisine patterns
    ))
  )
)

# Update refined_classification with NLP-enhanced classifications
refined_classification <- refined_classification %>%
  mutate(
    final_category = case_when(
      # Preserve existing classifications that aren't under review
      final_category != "needs_manual_review" & 
        final_category != "needs_verification" ~ final_category,
      
      # Check for business entities using NLP-derived patterns
      # Match against single words
      str_detect(tolower(`Company Name`), 
                 paste(final_classification_patterns$business_entities$tokens, collapse="|")) |
        # Match against two-word combinations
        str_detect(tolower(`Company Name`), 
                   paste(final_classification_patterns$business_entities$bigrams, collapse="|")) |
        # Match against three-word combinations
        str_detect(tolower(`Company Name`), 
                   paste(final_classification_patterns$business_entities$trigrams, collapse="|")) 
      ~ "needs_verification",
      
      # Apply restaurant type and cuisine patterns for health classification
      
      # Fast food indicators suggest unhealthy
      str_detect(tolower(`Company Name`), 
                 paste(refined_patterns$restaurant_types$fast_food, collapse="|")) ~ "unhealthy",
      
      # Asian and other international cuisines tend to be moderately healthy
      str_detect(tolower(`Company Name`), 
                 paste(refined_patterns$cuisine_types$asian, collapse="|")) |
        str_detect(tolower(`Company Name`), 
                   paste(refined_patterns$cuisine_types$other, collapse="|")) ~ "moderately_healthy",
      
      # Mexican, Italian, and American cuisines tend to be moderately unhealthy
      str_detect(tolower(`Company Name`), 
                 paste(refined_patterns$cuisine_types$mexican, collapse="|")) |
        str_detect(tolower(`Company Name`), 
                   paste(refined_patterns$cuisine_types$italian, collapse="|")) |
        str_detect(tolower(`Company Name`), 
                   paste(refined_patterns$cuisine_types$american, collapse="|")) ~ "moderately_unhealthy",
      
      # If no patterns match, keep the existing classification
      TRUE ~ final_category
    )
  )

# Generate summary statistics of the updated classifications
updated_stats <- refined_classification %>%
  group_by(final_category) %>%
  summarise(
    # Count occurrences of each category
    count = n(),
    # Calculate percentage of total
    percentage = round(n() / nrow(refined_classification) * 100, 2),
    # Show example establishments for each category
    sample_names = paste(head(unique(`Company Name`), 3), collapse = "; ")
  ) %>%
  arrange(desc(count))  # Sort by frequency

# Display results
print("Updated Classification Distribution:")
print(updated_stats)

# Show examples of how establishments were classified
print("\nSample of Updated Classifications:")
refined_classification %>%
  group_by(final_category) %>%
  slice_head(n = 3) %>%  # Take first 3 examples from each category
  select(`Company Name`, final_category) %>%
  arrange(final_category) %>%
  print(n = Inf)