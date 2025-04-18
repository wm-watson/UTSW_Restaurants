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
### NAICS/SIC Codes----
library(tidyverse)

dataaxle <- readxl::read_excel(dataaxle.path)

# First create vectors of unhealthy and healthy codes
unhealthy_naics <- c(
  722513,  # Limited-service restaurants
  722514,  # Cafeterias, buffets, grills
  445120,  # Convenience stores
  447110,  # Gas stations with convenience stores
  311520,  # Ice cream manufacturing
  445292,  # Confectionery and nut stores
  311330,  # Confectionery manufacturing
  311821,  # Cookie and cracker manufacturing
  722410,  # Drinking places
  454210   # Vending machine operators
)

unhealthy_sic <- c(
  5812,    # Fast-food restaurants and pizza places
  5411,    # Convenience stores
  5441,    # Candy and confectionery stores
  5813,    # Drinking places
  2024,    # Ice cream manufacturing
  5451,    # Dairy and ice cream stores
  5962,    # Automatic merchandising machine operators
  55401    # Service Stations - Gasoline and Oil
)

healthy_naics <- c(
  445230,  # Fruit and vegetable markets
  445210,  # Meat markets
  445220,  # Fish and seafood markets
  445110,  # Supermarkets and full-service grocery
  446191,  # Food supplement stores
  445299,  # All other specialty food stores
  311411,  # Frozen fruit/vegetable manufacturing
  311421   # Fruit and vegetable canning
)

healthy_sic <- c(
  5431,    # Fruit and vegetable markets
  5421,    # Meat and fish markets
  5499,    # Health food stores
  2033,    # Canned fruits and vegetables
  2034     # Dehydrated fruits, vegetables, soups
)

dataaxle <- dataaxle %>%
  mutate(
    # Check if any NAICS codes are unhealthy
    has_unhealthy_naics = if_any(
      starts_with("NAICS"), 
      ~. %in% unhealthy_naics
    ),
    
    # Check if any SIC codes are unhealthy
    has_unhealthy_sic = if_any(
      starts_with("SIC Code"), 
      ~. %in% unhealthy_sic
    ),
    
    # Check if any NAICS codes are healthy
    has_healthy_naics = if_any(
      starts_with("NAICS"), 
      ~. %in% healthy_naics
    ),
    
    # Check if any SIC codes are healthy
    has_healthy_sic = if_any(
      starts_with("SIC Code"), 
      ~. %in% healthy_sic
    ),
    
    # Final classification
    food_health_status = case_when(
      # If any code is unhealthy, classify as unhealthy
      has_unhealthy_naics | has_unhealthy_sic ~ "unhealthy",
      
      # If no unhealthy codes and has healthy codes, classify as healthy
      (has_healthy_naics | has_healthy_sic) ~ "healthy",
      
      # Otherwise keep as needs_review
      TRUE ~ "needs_review"
    )
  ) %>%
  # Clean up intermediate columns
  select(-has_unhealthy_naics, -has_unhealthy_sic, -has_healthy_naics, -has_healthy_sic)

## NLP: Unigrams, bigrams, and trigrams----
library(tidytext)
library(stringr)
library(janitor)

# Convert all column names to snake case
dataaxle <- dataaxle %>%
  clean_names()

# Custom stop words relevant to business names
business_stop_words <- c(
  "inc", "llc", "ltd", "corp", "corporation", "co", "company",
  "the", "and", "of", "&", "in", "at", "by", "for", "to", "a", "an"
)

# Process business names and create n-grams
name_analysis <- dataaxle %>%
  filter(food_health_status == "needs_review") %>%
  # Clean and standardize names
  mutate(
    clean_name = company_name %>%
      tolower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("[0-9]", " ") %>%
      str_squish()
  ) %>%
  # Create separate records for unigrams, bigrams, and trigrams
  unnest_tokens(ngram, clean_name, token = "ngrams", n = 1) %>%
  filter(!ngram %in% business_stop_words) %>%
  group_by(ngram) %>%
  summarise(
    unigram_count = n(),
    example_names = paste(head(company_name, 3), collapse = "; ")
  ) %>%
  arrange(desc(unigram_count))

bigram_analysis <- dataaxle %>%
  filter(food_health_status == "needs_review") %>%
  mutate(
    clean_name = company_name %>%
      tolower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("[0-9]", " ") %>%
      str_squish()
  ) %>%
  unnest_tokens(ngram, clean_name, token = "ngrams", n = 2) %>%
  # Remove bigrams containing stop words
  filter(!str_detect(ngram, paste(business_stop_words, collapse = "|"))) %>%
  group_by(ngram) %>%
  summarise(
    bigram_count = n(),
    example_names = paste(head(company_name, 3), collapse = "; ")
  ) %>%
  arrange(desc(bigram_count))

trigram_analysis <- dataaxle %>%
  filter(food_health_status == "needs_review") %>%
  mutate(
    clean_name = company_name %>%
      tolower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("[0-9]", " ") %>%
      str_squish()
  ) %>%
  unnest_tokens(ngram, clean_name, token = "ngrams", n = 3) %>%
  # Remove trigrams containing stop words
  filter(!str_detect(ngram, paste(business_stop_words, collapse = "|"))) %>%
  group_by(ngram) %>%
  summarise(
    trigram_count = n(),
    example_names = paste(head(company_name, 3), collapse = "; ")
  ) %>%
  arrange(desc(trigram_count))

# Print top results
print("Top 20 single words:")
print(head(name_analysis, 20))

print("\nTop 20 bigrams:")
print(head(bigram_analysis, 20))

print("\nTop 20 trigrams:")
print(head(trigram_analysis, 20))

# Create a summary of potential classification patterns
potential_patterns <- bind_rows(
  name_analysis %>% 
    head(50) %>% 
    mutate(type = "unigram"),
  bigram_analysis %>% 
    head(50) %>% 
    mutate(type = "bigram"),
  trigram_analysis %>% 
    head(50) %>% 
    mutate(type = "trigram")
)

# Define all classification terms
unhealthy_terms <- c(
  # Fast food and chains
  "sonic drive", "wendy", "chick fil", "church s chicken",
  "popeye", "chuck e cheese", "burger", "mcdonald", "subway",
  "pizza", "krispy krunchy", "golden chick",
  "hong kong express", "oriental express", "ice cream", "wing", "donut", "donuts",
  "fast food", "drive in", "drive thru",
  "dairy queen", "jack in the box", "whataburger", "carl s jr", "arby s",
  "raising cane", "five guys", "shake shack", "white castle", "braum s",
  "culver s", "hardee s", "krystal", "checkers", "rally s",
  
  # Casual dining chains
  "applebee s", "chili s", "tgi friday", "buffalo wild wings", "hooters",
  "twin peaks", "dave buster", "peter piper", "cicis", "little caesars",
  "olive garden", "red lobster", "outback", "texas roadhouse",
  "cracker barrel", "golden corral", "perkins", "red robin",
  "famous dave", "tony roma", "logan s", "longhorn",
  
  # BBQ variations
  "bbq", "bar b q", "bar-b-q", "barbecue", "barbeque", "smokehouse", "smoke house",
  "smoker", "pit bbq", "bar b que", "bar-b-que",
  
  # Mexican restaurant indicators
  "mexican", "taqueria", "taco", "cantina", "tex mex", "tex-mex",
  "jalisco", "guadalajara", "el rey",
  
  # Bar/pub variations
  "sports bar", "grill bar", "bar grill", "bar & grill",
  "pub", "tavern", "saloon", "lounge", "beer garden",
  "draft house", "draught house", "brew house", "brewing co",
  "brewery", "ale house", "tap house", "taphouse", "beer garden",
  "draft haus", "draught haus", "brauhaus", "hofbrau",
  "beer hall", "public house",
  "icehouse", "ice house", "cocktail", "cocktails", "drinks",
  "nightclub", "night club", "dancing", "karaoke", "cantina",
  
  # Restaurant/diner terms
  "diner", "cafe", "cafeteria", "grill", "steakhouse", "steak house",
  "pancake", "waffle house", "ihop", "dennys", "denny s",
  "buffet", "all you can eat", "ayce", "hibachi", "asian buffet",
  "chinese buffet", "sushi buffet", "mongolian",
  
  # Dessert/treat shops
  "baskin robbins", "dairy queen", "cold stone", "marble slab",
  "frozen yogurt", "yogurt shop", "froyo", "tcby", "gelato",
  "sweet shop", "candy shop", "candies", "confectionery",
  "cookie shop", "cookies", "bakery", "cinnabon", "dunkin",
  "dairy bar", "ice cream shop", "custard", "andy s frozen",
  "rita s ice", "carvel", "dippin dots", "haagen", "ben jerry",
  "cold stone", "braum s",
  
  # Snack shops
  "pretzel", "auntie anne", "wetzel pretzel", "snack bar", "snack shop",
  "popcorn shop", "nuts", "peanuts", "candy store",
  
  # Gas station/convenience store terms
  "quick shop", "quik shop", "kwik shop", "quick stop", "quick mart",
  "food mart", "mini mart", "ez mart", "ezy mart", "7 eleven",
  "7-eleven", "circle k", "corner store", "gas n go", "pak a sac",
  "stop n go", "stop & go", "food store", "quick stop", "quick way",
  "speedy stop", "super stop", "flash foods", "hop in",
  
  # Snack/beverage places
  "coffee shop", "coffee house", "coffee bar", "starbucks", "caribou coffee",
  "peet s coffee", "dunkin donuts", "krispy kreme", "smoothie king",
  "jamba juice", "tropical smoothie", "bubble tea", "boba tea",
  
  # Specifically fried seafood/chicken terms
  "fried fish", "fish fry", "fried seafood", "fried shrimp",
  "fried chicken", "chicken fried", "captain d", "long john silver",
  
  # Additional chains and restaurants from n-grams
  "schlotzsky", "which wich", "jersey mike", "firehouse subs",
  "pho que huong", "fish city grill", "red hot blue",
  "moe s southwest", "crispy chicken", "rice bowl express",
  "golden wok", "china wok", "china star", "china garden", "china buffet",
  "china king", "china one", "china house", "panda express",
  "great wall", "happy wok", "lucky wok", "new wok", "wok n roll",
  
  # Additional café/restaurant terms
  "bistro", "chophouse", "chop house", "eatery", "kitchen",
  "deli", "delicatessen", "sandwich shop", "sub shop", "sub house",
  "pizzeria", "trattoria", "osteria", "restaurant", "dining",
  "place to eat", "family restaurant",
  
  # Additional convenience store terms
  "shell food mart", "texaco food mart", "chevron food mart",
  "conoco food mart", "exxon food mart", "valero food mart",
  "phillips food mart", "citgo food mart",
  "shell station", "texaco station", "chevron station",
  "conoco station", "exxon station", "valero station",
  "phillips station", "citgo station",
  
  # Additional snack/beverage terms
  "tea house", "tea shop", "tea bar", "bubble tea", "boba tea",
  "snow ice", "shaved ice", "ice cream parlor", "creamery",
  "sweet treats", "sweet spot", "candy corner", "candy shop",
  "snacks", "treats",
  
  # Additional restaurant terms from remaining review
  "china inc", "china cafe", "china palace", "china taste",
  "golden china", "hong kong", "jade", "lucky china",
  "panda", "peking", "royal china", "szechuan", "wok",
  
  # Additional service station terms
  "stop n shop", "quick stop", "one stop", "pit stop",
  "food n fuel", "grab n go", "on the go", "convenience",
  "express mart", "star mart", "fuel mart", "star stop",
  "fuel stop", "petro stop", "travel stop", "truck stop",
  
  # Additional catering terms that typically indicate unhealthy food
  "bbq catering", "taco catering", "party catering",
  "event catering", "catering service", "catering services",
  "catering co", "catering company"
)

# Terms that indicate healthy establishments
healthy_terms <- c(
  # Healthy bar terms
  "juice bar", "smoothie bar", "salad bar", "acai bar", "health bar",
  
  # Healthy chicken/seafood terms
  "pollo asado", "pollo a la brasa", "grilled chicken",
  "rotisserie chicken", "seafood market", "fish market",
  "mariscos", "del mar", "pescado", "ceviche",
  
  # Health food stores/restaurants
  "vegan", "vegetarian", "health food", "organic", "natural foods",
  "fresh market", "farmers market", "produce market",
  
  # Additional healthy restaurant indicators
  "poke", "sushi", "salad", "mediterranean", "greek",
  
  # Additional healthy terms
  "farm to table", "farm to fork", "fresh kitchen", "clean eats",
  "healthy eats", "raw food", "whole food", "plant based",
  
  # Specific healthy chains/types
  "sweetgreen", "chopt", "tender greens", "true food kitchen",
  "seasons 52", "zoes kitchen", "cava", "panera",
  
  # Health food stores
  "vitamin shop", "nutrition store", "supplement shop",
  "whole foods", "trader joe", "sprouts", "natural grocers",
  
  # Additional healthy establishments
  "fish market", "farmers market", "produce market",
  "health foods", "health store", "nutrition center",
  "wellness center", "fitness center", "workout",
  "gym", "cross fit", "crossfit", "yoga studio",
  "pilates", "martial arts",
  
  # Additional healthy restaurant terms
  "raw bar", "oyster bar", "poke bowl", "acai bowl",
  "salad works", "salad factory", "green kitchen",
  "fresh kitchen", "fresh eats", "fresh foods",
  "natural cafe", "natural kitchen", "natural foods",
  "organic cafe", "organic kitchen", "organic foods",
  
  # Additional healthy catering terms
  "organic catering", "fresh catering", "healthy catering",
  "farm to table catering", "vegan catering", "vegetarian catering",
  "whole foods catering", "wellness catering"
)

# Update classification with more specific rules
dataaxle <- dataaxle %>%
  mutate(
    food_health_status = case_when(
      # Keep existing classifications
      food_health_status != "needs_review" ~ food_health_status,
      
      # First check for healthy exceptions
      str_detect(tolower(company_name), 
                 paste(healthy_terms, collapse = "|")) ~ "healthy",
      
      # Then check for unhealthy terms
      str_detect(tolower(company_name), 
                 paste(unhealthy_terms, collapse = "|")) ~ "unhealthy",
      
      # Generic "bar" term (not caught in healthy exceptions)
      str_detect(tolower(company_name), "bar|draft|draught") ~ "unhealthy",
      
      # Additional NAICS-based rules
      primary_naics_description == "Drinking Places (Alcoholic Beverages)" ~ "unhealthy",
      primary_naics_description == "Snack And Nonalcoholic Beverage Bars" & 
        !str_detect(tolower(company_name), paste(healthy_terms, collapse = "|")) ~ "unhealthy",
      
      # Remove transportation-related businesses
      primary_naics_description %in% c(
        "All Other Transit And Ground Passenger Transportation",
        "Other Urban Transit Systems",
        "Special Needs Transportation",
        "Commuter Rail Systems",
        "Scenic And Sightseeing Transportation, Land"
      ) ~ NA_character_,
      
      # Generic rules for remaining common categories
      primary_naics_description == "Limited-Service Restaurants" ~ "unhealthy",
      
      # Specific rules for catering
      primary_naics_description == "Caterers" & 
        !str_detect(tolower(company_name), paste(healthy_terms, collapse = "|")) ~ "unhealthy",
      
      # Keep others as needs_review
      TRUE ~ "needs_review"
    )
  )

# Get updated summaries
classification_summary <- dataaxle %>%
  group_by(food_health_status) %>%
  summarise(count = n())

remaining_review <- dataaxle %>%
  filter(food_health_status == "needs_review") %>%
  group_by(primary_naics_description) %>%
  summarise(
    count = n(),
    example_names = paste(head(company_name, 3), collapse = "; ")
  ) %>%
  arrange(desc(count))

# Update classification to flag the specific non-target businesses from the results
dataaxle <- dataaxle %>%
  mutate(
    food_health_status = case_when(
      primary_naics_description %in% c(
        "Limousine Service",
        "All Other Transit And Ground Passenger Transportat",
        "NA",
        "Food Service Contractors",
        "Ambulance Services",
        "Interurban And Rural Bus Transportation"
      ) ~ "flag_for_potential_removal",
      
      # Keep existing classifications
      TRUE ~ food_health_status
    )
  )

# Get updated summary
classification_summary <- dataaxle %>%
  group_by(food_health_status) %>%
  summarise(count = n())

# Get summary of remaining needs_review cases
remaining_review <- dataaxle %>%
  filter(food_health_status == "needs_review") %>%
  group_by(primary_naics_description) %>%
  summarise(
    count = n(),
    example_names = paste(head(company_name, 3), collapse = "; ")
  ) %>%
  arrange(desc(count))

print("Classification Summary:")
print(classification_summary)

print("\nRemaining categories needing review:")
print(remaining_review)

# Examine NA records
na_analysis <- dataaxle %>%
  filter(is.na(primary_naics_description)) %>%
  group_by(primary_naics, primary_sic_code, primary_sic_description) %>%
  summarise(
    count = n(),
    example_names = paste(head(company_name, 3), collapse = "; ")
  ) %>%
  arrange(desc(count))

print("Analysis of NA NAICS records:")
print(na_analysis)

# Let's also look at secondary NAICS codes for these records
na_secondary_naics <- dataaxle %>%
  filter(is.na(primary_naics_description)) %>%
  select(company_name, 
         naics_1, naics_1_description,
         naics_2, naics_2_description,
         naics_3, naics_3_description) %>%
  filter(!is.na(naics_1) | !is.na(naics_2) | !is.na(naics_3))

print("\nSecondary NAICS codes for NA records:")
print(na_secondary_naics)

dataaxle <- dataaxle %>%
  mutate(
    food_health_status = case_when(
      # Keep existing non-NA classifications
      !is.na(food_health_status) & food_health_status != "needs_review" ~ food_health_status,
      
      # For NA primary_naics_description records:
      is.na(primary_naics_description) & (
        # If it has a restaurant/food service NAICS code
        naics_1_description %in% c("Full-Service Restaurants") |
          naics_2_description %in% c("Full-Service Restaurants") |
          naics_3_description %in% c("Full-Service Restaurants") |
          # Or if it has a food service SIC code
          primary_sic_code %in% c(581209, 581224, 581219, 581214, 581223)
      ) ~ "needs_review",  # Keep these for further food classification
      
      # Flag non-food related NA records
      is.na(primary_naics_description) ~ "flag_for_potential_removal",
      
      # Keep other existing classifications
      TRUE ~ food_health_status
    )
  )

# Get updated summary
classification_summary <- dataaxle %>%
  group_by(food_health_status) %>%
  summarise(count = n())

print("Updated Classification Summary:")
print(classification_summary)

# Look at the NA records
na_records <- dataaxle %>%
  filter(is.na(food_health_status)) %>%
  select(company_name, 
         primary_naics, primary_naics_description,
         primary_sic_code, primary_sic_description,
         naics_1, naics_1_description,
         naics_2, naics_2_description) %>%
  arrange(company_name)

print("Records with NA food_health_status:")
print(na_records, n=Inf)

dataaxle <- dataaxle %>%
  mutate(
    food_health_status = case_when(
      # Update NA records to flag_for_potential_removal
      is.na(food_health_status) ~ "flag_for_potential_removal",
      # Keep all other existing classifications
      TRUE ~ food_health_status
    )
  )

# Verify the update
classification_summary <- dataaxle %>%
  group_by(food_health_status) %>%
  summarise(count = n())

print("Updated Classification Summary:")
print(classification_summary)

#Look at gas stations needing review
gas_stations_review <- dataaxle %>%
  filter(
    primary_naics_description == "Other Gasoline Stations",
    food_health_status == "needs_review"
  ) %>%
  select(
    company_name,
    primary_naics_description,
    primary_sic_code,
    primary_sic_description
  ) %>%
  arrange(company_name)

# Let's also get a word frequency analysis of these station names
gas_station_words <- dataaxle %>%
  filter(
    primary_naics_description == "Other Gasoline Stations",
    food_health_status == "needs_review"
  ) %>%
  mutate(
    clean_name = company_name %>%
      tolower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("[0-9]", " ") %>%
      str_squish()
  ) %>%
  unnest_tokens(word, clean_name) %>%
  count(word, sort = TRUE) %>%
  filter(!word %in% c("the", "and", "of", "a", "in", "at", "by", "on", "for"))

print("Sample of gas stations needing review:")
print(gas_stations_review)

print("\nMost common words in gas station names:")
print(head(gas_station_words, 20))

# Get unique SIC codes and their counts
gas_station_sic <- dataaxle %>%
  filter(
    primary_naics_description == "Other Gasoline Stations",
    food_health_status == "needs_review"
  ) %>%
  group_by(primary_sic_code, primary_sic_description) %>%
  summarise(
    count = n(),
    example_names = paste(head(company_name, 3), collapse = "; ")
  ) %>%
  arrange(desc(count))

print("SIC code distribution for gas stations needing review:")
print(gas_station_sic)


dataaxle <- dataaxle %>%
  mutate(
    food_health_status = case_when(
      # Update gas stations to unhealthy
      primary_naics_description == "Other Gasoline Stations" &
        primary_sic_code == 554101 &
        food_health_status == "needs_review" ~ "unhealthy",
      
      # Keep all other existing classifications
      TRUE ~ food_health_status
    )
  )

# RESTAURANTS----
# Update pizza places to unhealthy
dataaxle <- dataaxle %>%
  mutate(
    food_health_status = case_when(
      primary_naics_description == "Full-Service Restaurants" &
        primary_sic_code == 581222 &
        food_health_status == "needs_review" ~ "unhealthy",
      TRUE ~ food_health_status
    )
  )

# Now get bigrams for remaining restaurants
restaurant_bigrams <- dataaxle %>%
  filter(
    primary_naics_description == "Full-Service Restaurants",
    food_health_status == "needs_review"
  ) %>%
  mutate(
    clean_name = company_name %>%
      tolower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("[0-9]", " ") %>%
      str_squish()
  ) %>%
  unnest_tokens(bigram, clean_name, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  filter(!is.na(bigram))

# Verify the update
classification_summary <- dataaxle %>%
  group_by(food_health_status) %>%
  summarise(count = n())

print("Updated Classification Summary:")
print(classification_summary)

print("\nMost common bigrams in restaurant names:")
print(head(restaurant_bigrams, 20))

dataaxle <- dataaxle %>%
  mutate(
    food_health_status = case_when(
      primary_naics_description == "Full-Service Restaurants" &
        food_health_status == "needs_review" &
        str_detect(tolower(company_name), paste(
          "mc donald|mcdonald|chick fil|church s|arby s|denny s|applebee s|grandy s|boston market|freebirds",
          collapse = "|"
        )) ~ "unhealthy",
      TRUE ~ food_health_status
    )
  )

# Classify restaurants using grep patterns
dataaxle <- dataaxle %>%
  mutate(
    food_health_status = case_when(
      primary_naics_description == "Full-Service Restaurants" &
        food_health_status == "needs_review" &
        (
          grepl("mc.*donald|mcdonald", company_name, ignore.case=TRUE) |
            grepl("chick.*fil|chickfil", company_name, ignore.case=TRUE) |
            grepl("church.*s.*chicken|churchs.*chicken", company_name, ignore.case=TRUE) |
            grepl("arby.*s|arbys", company_name, ignore.case=TRUE) |
            grepl("denny.*s|dennys", company_name, ignore.case=TRUE) |
            grepl("applebee.*s|applebees", company_name, ignore.case=TRUE) |
            grepl("grandy.*s|grandys", company_name, ignore.case=TRUE) |
            grepl("luby.*s|lubys", company_name, ignore.case=TRUE) |
            grepl("boston.*market", company_name, ignore.case=TRUE) |
            grepl("freebirds", company_name, ignore.case=TRUE) |
            grepl("mi.*cocina", company_name, ignore.case=TRUE) |
            grepl("roadhouse", company_name, ignore.case=TRUE) |
            grepl("chicken.*rice", company_name, ignore.case=TRUE) |
            grepl("cowboy.*chicken", company_name, ignore.case=TRUE) |
            grepl("chicken.*express", company_name, ignore.case=TRUE) |
            grepl("pollo.*regio", company_name, ignore.case=TRUE) |
            grepl("la.*madeleine", company_name, ignore.case=TRUE) |
            grepl("italian.*restaurant|s.*italian", company_name, ignore.case=TRUE) |
            grepl("carl.*s.*jr|carls.*jr", company_name, ignore.case=TRUE) |
            grepl("steak.*n.*shake|steak.*and.*shake", company_name, ignore.case=TRUE) |
            grepl("captain.*d|capt.*d", company_name, ignore.case=TRUE) |
            grepl("crab.*shack", company_name, ignore.case=TRUE) |
            grepl("texas.*land.*cattle|land.*cattle", company_name, ignore.case=TRUE) |
            grepl("henderson.*chicken", company_name, ignore.case=TRUE) |
            grepl("chicken.*dinner.*house|dinner.*house", company_name, ignore.case=TRUE) |
            grepl("daddy.*s.*chicken", company_name, ignore.case=TRUE) |
            grepl("babe.*s.*chicken", company_name, ignore.case=TRUE)|
            grepl("chicken.*seafood", company_name, ignore.case=TRUE) |
            grepl("s.*catfish", company_name, ignore.case=TRUE) |
            grepl("s.*ribs", company_name, ignore.case=TRUE) |
            grepl("red.*hot.*blue", company_name, ignore.case=TRUE) |
            grepl("egg.*roll", company_name, ignore.case=TRUE) |
            grepl("pollo.*loco", company_name, ignore.case=TRUE) |
            grepl("royal.*chopstix", company_name, ignore.case=TRUE) |
            grepl("cheesecake.*factory", company_name, ignore.case=TRUE) |
            grepl("huddle.*house", company_name, ignore.case=TRUE) |
            grepl("soul.*food", company_name, ignore.case=TRUE) |
            grepl("kfc|kentucky fried chicken", company_name, ignore.case=TRUE) |
            grepl("quiznos", company_name, ignore.case=TRUE) |
            grepl("fuddruckers", company_name, ignore.case=TRUE) |
            grepl("dave.*buster", company_name, ignore.case=TRUE) |
            grepl("texadelphia", company_name, ignore.case=TRUE) |
            grepl("au bon pain", company_name, ignore.case=TRUE) |
            grepl("egg.*i", company_name, ignore.case=TRUE) |  # Breakfast chain, typically heavy foods
            grepl("le peep", company_name, ignore.case=TRUE) |  # Similar breakfast chain
            grepl("rice express", company_name, ignore.case=TRUE) |  # Fast food Asian
            grepl("buca di beppo", company_name, ignore.case=TRUE) |  # Italian chain, large portions
            grepl("china express", company_name, ignore.case=TRUE) |  # Fast food Chinese
            grepl("houlihan's", company_name, ignore.case=TRUE)  |
            grepl("lisa.*s chicken", company_name, ignore.case=TRUE) |
            grepl("williams chicken", company_name, ignore.case=TRUE) |
            grepl("zaxby.*s", company_name, ignore.case=TRUE) |
            grepl("italia express", company_name, ignore.case=TRUE) |
            grepl("bahama buck.*s", company_name, ignore.case=TRUE) |  # Shaved ice/desserts
            grepl("benihana", company_name, ignore.case=TRUE) |  # Hibachi, large portions
            grepl("freddy.*s", company_name, ignore.case=TRUE) | # Freddy's Frozen Custard & Steakburgers |
          grepl("taste of asia", company_name, ignore.case=TRUE) |  # Typically fast-food Asian
            grepl("culver.*s", company_name, ignore.case=TRUE) |  # Fast food burgers/custard
            grepl("tin star", company_name, ignore.case=TRUE) |  # Tex-Mex fast casual
            grepl("el regio", company_name, ignore.case=TRUE) |  # Similar to Pollo Regio
            grepl("penne pomodoro", company_name, ignore.case=TRUE) |  # Italian
            grepl("pollo tropical", company_name, ignore.case=TRUE) |  # Fast food chicken
            grepl("rice xpress", company_name, ignore.case=TRUE) | # Fast food Asian
            grepl("charley.*s philly|fred.*s.*downtown philly", company_name, ignore.case=TRUE) |  # Philly cheesesteak places
            grepl("melting pot", company_name, ignore.case=TRUE) |  # Fondue restaurant
            grepl("redneck heaven", company_name, ignore.case=TRUE) |  # Bar food
            grepl("gloria.*s latin", company_name, ignore.case=TRUE) |  # Tex-Mex
            grepl("las ranitas", company_name, ignore.case=TRUE) |  # Mexican
            grepl("el paisa|la paisanita", company_name, ignore.case=TRUE) |  # Mexican fast food
            grepl("la playa maya", company_name, ignore.case=TRUE) | # Mexican |
          grepl("texas.*de.*brazil", company_name, ignore.case=TRUE) |  # Brazilian steakhouse
            grepl("yogurt story", company_name, ignore.case=TRUE) |  # Frozen yogurt
            grepl("keller.*s drive|kellers drive", company_name, ignore.case=TRUE) |  # Drive-in burger joint
            grepl("maggiano.*s", company_name, ignore.case=TRUE) |  # Italian chain
            grepl("matt.*s rancho", company_name, ignore.case=TRUE) |  # Tex-Mex
            grepl("burrito jimmy", company_name, ignore.case=TRUE) |  # Fast food Mexican
            grepl("china harbor", company_name, ignore.case=TRUE) |  # Chinese
            grepl("pollo campero", company_name, ignore.case=TRUE) |  # Fried chicken chain
            grepl("wienerschnitzel", company_name, ignore.case=TRUE) |  # Hot dog chain
            grepl("bone daddy", company_name, ignore.case=TRUE) |  # BBQ/sports bar
            grepl("el fenix", company_name, ignore.case=TRUE) |  # Tex-Mex
            grepl("hacienda ranch", company_name, ignore.case=TRUE) |  # Mexican
            grepl("s ristorante", company_name, ignore.case=TRUE) | # Italian restaurants |
          grepl("benito.*s", company_name, ignore.case=TRUE) |  # Mexican
            grepl("birrieria", company_name, ignore.case=TRUE) |  # Mexican birria
            grepl("los vaqueros", company_name, ignore.case=TRUE) |  # Mexican
            grepl("new china", company_name, ignore.case=TRUE) |  # Chinese takeout
            grepl("tortilleria", company_name, ignore.case=TRUE) |  # Mexican
            grepl("chef chen", company_name, ignore.case=TRUE) |  # Chinese
            grepl("del frisco.*s", company_name, ignore.case=TRUE) |  # Steakhouse
            grepl("el tacaso", company_name, ignore.case=TRUE) |  # Taco place
            grepl("b.*q", company_name, ignore.case=TRUE) |  # BBQ places
            grepl("seafood.*chicken|chicken.*fish", company_name, ignore.case=TRUE) |  # Fried seafood/chicken combos
            grepl("julio.*s", company_name, ignore.case=TRUE) | # Mexican
            grepl("el tizoncito", company_name, ignore.case=TRUE) |  # Mexican
            grepl("hoffbrau steaks", company_name, ignore.case=TRUE) |  # Steakhouse
            grepl("italian villa", company_name, ignore.case=TRUE) |  # Italian
            grepl("londoner", company_name, ignore.case=TRUE) |  # British pub
            grepl("rio mambo", company_name, ignore.case=TRUE) |  # Mexican
            grepl("salsa limon", company_name, ignore.case=TRUE) |  # Mexican
            grepl("cajun corner", company_name, ignore.case=TRUE) |  # Cajun
            grepl("los lupes", company_name, ignore.case=TRUE) |  # Mexican
            grepl("rice pot", company_name, ignore.case=TRUE) |  # Asian fast food
            grepl("fish.*chicken|chicken.*fish", company_name, ignore.case=TRUE) |  # Fried combos
            grepl("old fashioned", company_name, ignore.case=TRUE) | # Usually burger/ice cream places
            grepl("catfish house", company_name, ignore.case=TRUE) |  # Fried catfish
            grepl("chopstick express", company_name, ignore.case=TRUE) |  # Fast food Asian
            grepl("dos chiles grandes", company_name, ignore.case=TRUE) |  # Mexican
            grepl("dragon express", company_name, ignore.case=TRUE) |  # Fast food Asian
            grepl("el polio regio", company_name, ignore.case=TRUE) |  # Mexican chicken
            grepl("el rinconcito", company_name, ignore.case=TRUE) |  # Mexican
            grepl("gourmet china", company_name, ignore.case=TRUE) |  # Chinese
            grepl("grumps", company_name, ignore.case=TRUE) |  # Burgers
            grepl("hook line.*sinker", company_name, ignore.case=TRUE) |  # Fried seafood
            grepl("eddie v.*s", company_name, ignore.case=TRUE) |  # Steakhouse/seafood chain
            grepl("new orleans", company_name, ignore.case=TRUE) |  # Cajun/creole
            grepl("italian rstrnt", company_name, ignore.case=TRUE) | # Italian restaurants
            grepl("king.*s noodle", company_name, ignore.case=TRUE) |  # Chinese
            grepl("la picosa", company_name, ignore.case=TRUE) |  # Mexican
            grepl("la playita", company_name, ignore.case=TRUE) |  # Mexican
            grepl("mattito.*s", company_name, ignore.case=TRUE) |  # Tex-Mex
            grepl("plaza del sol", company_name, ignore.case=TRUE) |  # Mexican
            grepl("pollo salsa", company_name, ignore.case=TRUE) |  # Chicken
            grepl("riscky.*s", company_name, ignore.case=TRUE) |  # BBQ
            grepl("york sub", company_name, ignore.case=TRUE) |  # Sub sandwiches
            grepl("empress of", company_name, ignore.case=TRUE) |  # Usually Chinese
            grepl("kuai.*dumpling", company_name, ignore.case=TRUE) | # Dumplings
            grepl("rise.*shine", company_name, ignore.case=TRUE) |  # Breakfast
            grepl("sarku japan", company_name, ignore.case=TRUE) |  # Mall food court Japanese
            grepl("short stop food", company_name, ignore.case=TRUE) |  # Fast food
            grepl("spaghetti warehouse", company_name, ignore.case=TRUE) |  # Italian
            grepl("urban crust", company_name, ignore.case=TRUE) |  # Pizza
            grepl("wild turkey", company_name, ignore.case=TRUE) |  # Bar food
            grepl("yesterdays texas", company_name, ignore.case=TRUE) |  # American/Texas
            grepl("becks prime", company_name, ignore.case=TRUE) |  # Burgers
            grepl("big shucks", company_name, ignore.case=TRUE) |  # Seafood
            grepl("shell shack", company_name, ignore.case=TRUE) |  # Seafood
            grepl("harry.*s", company_name, ignore.case=TRUE) | # Usually steakhouse/American
            grepl("el atoron", company_name, ignore.case=TRUE) |  # Mexican
            grepl("ernesto.*s", company_name, ignore.case=TRUE) |  # Mexican
            grepl("holy frijoles", company_name, ignore.case=TRUE) |  # Mexican
            grepl("johnny carino.*s", company_name, ignore.case=TRUE) |  # Italian chain
            grepl("johnny rockets", company_name, ignore.case=TRUE) |  # Burger chain
            grepl("kincaid.*s", company_name, ignore.case=TRUE) |  # Burgers
            grepl("la potosina", company_name, ignore.case=TRUE) |  # Mexican
            grepl("medieval times", company_name, ignore.case=TRUE) |  # Theme restaurant
            grepl("home cooking", company_name, ignore.case=TRUE) |  # Southern/comfort food
            grepl("chinese food", company_name, ignore.case=TRUE) |  # Generic Chinese
            grepl("mac.*s", company_name, ignore.case=TRUE) |  # Usually burgers/American
            grepl("mi pueblo", company_name, ignore.case=TRUE) |  # Mexican
            grepl("my estrella", company_name, ignore.case=TRUE) |  # Mexican
            grepl("noodle wave", company_name, ignore.case=TRUE) |  # Asian noodles
            grepl("pdq", company_name, ignore.case=TRUE) |  # Fast food chicken
            grepl("rice garden", company_name, ignore.case=TRUE) |  # Asian
            grepl("slim chickens", company_name, ignore.case=TRUE) |  # Fried chicken
            grepl("torteria", company_name, ignore.case=TRUE) |  # Mexican
            grepl("yogurtville", company_name, ignore.case=TRUE) |  # Frozen yogurt
            grepl("anamia.*s", company_name, ignore.case=TRUE) |  # Mexican
            grepl("coal vines", company_name, ignore.case=TRUE) |  # Pizza
            grepl("street food", company_name, ignore.case=TRUE) |  # Usually fast food
            grepl("the box", company_name, ignore.case=TRUE) | # Usually quick service
            grepl("asian bowl", company_name, ignore.case=TRUE) |  # Fast casual Asian
            grepl("blue goose", company_name, ignore.case=TRUE) |  # Mexican cantina
            grepl("buzzbrews", company_name, ignore.case=TRUE) |  # Diner
            grepl("china dragon", company_name, ignore.case=TRUE) |  # Chinese
            grepl("chubby.*s", company_name, ignore.case=TRUE) |  # Usually burgers/wings
            grepl("go loco", company_name, ignore.case=TRUE) |  # Mexican
            grepl("italian express", company_name, ignore.case=TRUE) |  # Fast Italian
            grepl("el pollo", company_name, ignore.case=TRUE) |  # Mexican chicken
            grepl("la nueva", company_name, ignore.case=TRUE) |  # Mexican
            grepl("s steak", company_name, ignore.case=TRUE) |  # Steakhouse
            grepl("de noche", company_name, ignore.case=TRUE) |  # Mexican
            grepl("daddy.*s", company_name, ignore.case=TRUE)  # Usually BBQ/comfort food
        ) ~ "unhealthy",
      
      # Previous healthy classifications remain the same
      primary_naics_description == "Full-Service Restaurants" &
        food_health_status == "needs_review" &
        (
          grepl("thai.*cuisine", company_name, ignore.case=TRUE) |
            grepl("jimmy.*john|jimmyjohn", company_name, ignore.case=TRUE) |
            grepl("indian.*cuisine", company_name, ignore.case=TRUE) |
            grepl("asian.*cuisine", company_name, ignore.case=TRUE) |
            grepl("chinese.*rstrnt|chinese.*restaurant", company_name, ignore.case=TRUE) |
            grepl("salata", company_name, ignore.case=TRUE) |
            grepl("flying fish", company_name, ignore.case=TRUE) |  # Seafood restaurant
            grepl("blue fish", company_name, ignore.case=TRUE) | # Usually sushi/seafood 
            grepl("best thai", company_name, ignore.case=TRUE) |
            grepl("sweet tomatoes", company_name, ignore.case=TRUE) | # Salad buffet
            grepl("modern market", company_name, ignore.case=TRUE) | # Farm-to-table, health-focused 
            grepl("pho.*is.*for.*lovers", company_name, ignore.case=TRUE) |  # Vietnamese pho
            grepl("garden.*tea", company_name, ignore.case=TRUE) | # Tea rooms often have healthier options
            grepl("pho 95", company_name, ignore.case=TRUE) |  # Vietnamese pho
            grepl("viet tofu", company_name, ignore.case=TRUE) | # Vietnamese/vegetarian
            grepl("pho pasteur", company_name, ignore.case=TRUE) |  # Vietnamese pho
            grepl("que huong", company_name, ignore.case=TRUE) | # Vietnamese
            grepl("urban eatz", company_name, ignore.case=TRUE) |  # Health-focused
            grepl("chilli thai", company_name, ignore.case=TRUE) |  # Thai
            grepl("little katana", company_name, ignore.case=TRUE) |  # Sushi
            grepl("first watch", company_name, ignore.case=TRUE) |  # Health-focused breakfast
            grepl("japanese cuisine", company_name, ignore.case=TRUE) | # Japanese
            grepl("bamboo garden", company_name, ignore.case=TRUE) |  # Usually vegetarian Asian
            grepl("japanese rstrnt", company_name, ignore.case=TRUE) | # Japanese
            grepl("east hampton sandwich", company_name, ignore.case=TRUE) |  # Health-focused sandwiches
            grepl("chef.*s gallery", company_name, ignore.case=TRUE)  # Usually upscale dining
        ) ~ "healthy",
      
      # Remove medical center - not a restaurant
      primary_naics_description == "Full-Service Restaurants" &
        food_health_status == "needs_review" &
        grepl("children.*s medical|medical c", company_name, ignore.case=TRUE) |
        grepl("mark of excellence|enterprises inc", company_name, ignore.case=TRUE) |
        grepl("foods inc", company_name, ignore.case=TRUE) |
        grepl("food inc", company_name, ignore.case=TRUE) |
        grepl("yum brands", company_name, ignore.case=TRUE) |
        grepl("butler franchise", company_name, ignore.case=TRUE)
      ~ "flag_for_potential_removal",
      
      TRUE ~ food_health_status
    )
  )


# Look at remaining bigrams
remaining_bigrams <- dataaxle %>%
  filter(
    primary_naics_description == "Full-Service Restaurants",
    food_health_status == "needs_review"
  ) %>%
  mutate(
    clean_name = company_name %>%
      tolower() %>%
      str_replace_all("[[:punct:]]", " ") %>%
      str_replace_all("[0-9]", " ") %>%
      str_squish()
  ) %>%
  unnest_tokens(bigram, clean_name, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  filter(!is.na(bigram))

print("\nRemaining most common bigrams:")
print(head(remaining_bigrams, 20))

# Get unique company names and their counts for restaurants still needing review
remaining_restaurants <- dataaxle %>%
  filter(
    primary_naics_description == "Full-Service Restaurants",
    food_health_status == "needs_review"
  ) %>%
  count(company_name, sort = TRUE) %>%
  filter(!is.na(company_name))

print("Most common restaurant names still needing review:")
print(head(remaining_restaurants, 50))



# Get summary of current classifications
classification_summary <- dataaxle %>%
  group_by(food_health_status) %>%
  summarise(count = n()) %>%
  arrange(desc(count))

print("Current Classification Summary:")
print(classification_summary)