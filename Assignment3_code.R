# PART 1 ----
# Load necessary libraries
library(tidyverse)

# Read the dataset
myData <- read_csv("Assignment3_data.csv")

# View structure of the dataset
glimpse(myData)

# Check the first few rows
head(myData)

# Check for missing or unusual values
summary(myData)


# Check age distribution
summary(myData$Age)
hist(myData$Age)

hist(myData$Age,
     main = "Histogram of Participant Age",
     xlab = "Age (years)",
     ylab = "Frequency")

# Identify problematic entries
problematic_ages <- myData %>%
  filter(Age > 110.65 | Age < 75)

# Print problematic entries
print(problematic_ages)


# Investigate Smoking Inconsistencies
# Check current smokers who claim never smoking
inconsistent_smokers <- myData %>%
  filter(Current.smoke == "Yes" & Ever.smoked == "No")

# Print details of these inconsistent records
print(inconsistent_smokers)

# Count of such inconsistencies
nrow(inconsistent_smokers)


# Check Smoking Details for Never Smokers:
# Find never smokers with smoking details
never_smokers_with_smoke_details <- myData %>%
  filter(Ever.smoked == "No" & !is.na(How.much.smoke))

# Print details of these inconsistent records
print(never_smokers_with_smoke_details)

# Count of such inconsistencies
nrow(never_smokers_with_smoke_details)
  

#Data Cleaning Checklist ---- 
#1. Categorical Variables Transformation ----

cleaned_data <- myData %>%
  mutate(
    # Convert Dementia to numeric
    Dementia = case_when(
      Dementia == "No" ~ 0,
      Dementia == "Yes" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Convert Sex to factor
    Sex = factor(Sex, levels = c(0, 1), labels = c("Male", "Female")),
    
    # Current Smoking
    Current.smoke = case_when(
      Current.smoke == "No" ~ 0,
      Current.smoke == "Yes" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Ever Smoked
    Ever.smoked = case_when(
      Ever.smoked == "No" ~ 0,
      Ever.smoked == "Yes" ~ 1,
      Ever.smoked == "Prefer not to answer" ~ 2,
      TRUE ~ NA_real_
    ),
    
    # How Much Smoke
    How.much.smoke = case_when(
      How.much.smoke == "less than 5 a day" ~ 0,
      How.much.smoke == "5+ a day" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Family History
    Fam.hist = case_when(
      Fam.hist == "No fam hist" ~ 0,
      Fam.hist == "Fam hist" ~ 1,
      TRUE ~ NA_real_
    ),
    
    # Education
    Education = case_when(
      Education == "7 or less" ~ 0,
      Education == ">7" ~ 1,
      TRUE ~ NA_real_
    )
  )

#2. Numeric Variables Cleaning ----
cleaned_data <- cleaned_data %>%
  mutate(
    # Blood Pressure Validation
    Diastolic = case_when(
      Diastolic > 110 ~ NA_real_,
      TRUE ~ Diastolic
    ),
    
    Systolic = case_when(
      Systolic < 80 ~ NA_real_,
      TRUE ~ Systolic
    ),
    
    # Smoking Variables Cleaning
    Stop.smoke.age = case_when(
      Stop.smoke.age < 0 | Stop.smoke.age > Age ~ NA_real_,
      TRUE ~ Stop.smoke.age
    ),
    
    How.long.smoked = case_when(
      How.long.smoked == -9999.0 ~ NA_real_,
      How.long.smoked < 0 | How.long.smoked > 70 ~ NA_real_,
      TRUE ~ How.long.smoked
    ),
    
    # Clear Smoking Details for Never Smokers
    # Before: Ever.smoked = "No", How.much.smoke = "5+ a day"
    # After: Ever.smoked = "No", How.much.smoke = NA
    How.much.smoke = case_when(
      Ever.smoked == "No" ~ NA_character_,
      TRUE ~ How.much.smoke
    ),
    
    # Age Validation
    Age = case_when(
      Age > 110.65 | Age < 75 ~ NA_real_,
      TRUE ~ Age
    )
  )

#3. Missing Value Analysis ----
# Check missing values
missing_summary <- cleaned_data %>%
  summarise(across(everything(), ~ sum(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Missing_Count")

# Print missing value summary
print(missing_summary)

#4. Descriptive Statistics ----
# Generate descriptive statistics
desc_stats <- cleaned_data %>%
  summarise(
    across(
      c(Diastolic, Systolic, Stop.smoke.age, How.long.smoked, Age),
      list(
        mean = ~mean(., na.rm = TRUE),
        median = ~median(., na.rm = TRUE),
        min = ~min(., na.rm = TRUE),
        max = ~max(., na.rm = TRUE)
      )
    )
  )

print(desc_stats)


# PART 2 ----

# Load required library
library(tidyverse)

# 1. Print first 10 rows
# Assuming your cleaned dataset is named 'cleaned_data'
cleaned_data %>% 
  head(10)

# Transposed view of first 10 rows
t(head(cleaned_data, 10))


# 2. Filter rows where participants responded
filtered_data <- cleaned_data %>%
  filter(Ever.smoked != 2) %>%       # remove "Prefer not to answer"
  select(-ID) %>%                    # remove ID
  select_if(~n_distinct(.) > 1)      # remove constant variables

filtered_data %>% 
  head(10)

# Transposed view of first 10 rows
t(head(filtered_data, 10))


# 3. Create MAP (Mean Arterial Pressure) variable
cleaned_data <- cleaned_data %>%
  mutate(MAP = (Systolic + 2 * Diastolic) / 3)

# Check MAP calculation
summary(cleaned_data$MAP)

# Verify no missing values
sum(is.na(cleaned_data$MAP))

cleaned_data %>% 
  head(10)

# Transposed view of first 10 rows
t(head(cleaned_data, 10))


# 4. Create smoker variable
cleaned_data <- cleaned_data %>%
  mutate(smoker = case_when(
    Current.smoke == 1 | How.long.smoked > 5 ~ "1",
    TRUE ~ "0"
  ))

# Frequency table for smoker
smoker_freq <- cleaned_data %>% 
  count(smoker) %>% 
  mutate(percentage = n / sum(n) * 100)

smoker_freq


# 5. Summary statistics for MAP by Sex
summary_by_sex <- cleaned_data %>%
  group_by(Sex) %>%
  summarise(
    mean = mean(MAP, na.rm = TRUE),
    median = median(MAP, na.rm = TRUE),
    sd = sd(MAP, na.rm = TRUE),
    min = min(MAP, na.rm = TRUE),
    max = max(MAP, na.rm = TRUE)
  )

summary_by_sex


# 6. Pivot to Long Format
summary_long <- summary_by_sex %>%
  pivot_longer(
    cols = -Sex, 
    names_to = "Statistic", 
    values_to = "Value"
  )

summary_long

# 7. Pivot to Wide Format
summary_wide <- summary_long %>%
  pivot_wider(
    names_from = Sex, 
    values_from = Value
  )

summary_wide