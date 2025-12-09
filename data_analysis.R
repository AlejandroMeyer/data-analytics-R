# /////////////////////////////////////////////////////////////////
#
#        PROJECT ENVIRONMENT MANAGEMENT (renv)
#
# /////////////////////////////////////////////////////////////////

# This project uses the 'renv' package to manage dependencies and 
# ensure reproducibility. All collaborators must use the same package 
# versions defined in the 'renv.lock' file.

# 1. INITIAL SETUP FOR NEW USERS:
#    If this is the first time working on the project, the environment 
#    should activate automatically when you open the 'Project.Rproj' file. 
#    If prompted, confirm that you wish to restore the library.

# 2. MANUAL RESTORE (If activation fails):
#    If the environment doesn't load, run this command in the R Console 
#    to install/restore the required packages and versions. This ensures 
#    your setup matches the committed environment.
#
# renv::restore() 

# 3. UPDATING PACKAGES:
#    If you install or update packages necessary for the project, run:
#    renv::snapshot() 
#    Then, COMMIT the updated 'renv.lock' file to GitHub.
#
# /////////////////////////////////////////////////////////////////


# The file has been loaded
load("Group_3.RData")


# Changing the name of my variable
View(dat) # As table
raw_data <- dat
head(raw_data)

#Count how many times each educational level code appears
#table(raw_data[["isced11_20"]])


# ---------------------------------------------------------------- #
#                       DATA CLEANING
# ---------------------------------------------------------------- #

# Load the necessary library for data manipulation
library(dplyr)

# Rename the columns in the 'new_data' data frame from 'raw_data'
new_data <- raw_data %>%
  rename(
    # New_Name = Actual_Name
    
    # Key Dependent Variable
    personal_achievement_deserved = bkp_05_02, 
    
    # Demographics and Background
    education_level             = isced11_20,
    marital_status              = bkfamstd,
    birth_year                  = bkpbirthy,
    gender                      = sex,
    migration_background        = migback,
    total_children              = sumkids,
    household_id                = cid,
    
    # Financial and Employment
    gross_labor_income          = labgro20,
    exp_fulltime_years          = expft20,
    exp_unemployment_years      = expue20,
    exp_parttime_years          = exppt20,
    labor_force_status          = lfs20,
    job_prestige_siops          = siops08_20,
    
    # Satisfaction and Attitudes
    satisfaction_income         = bkp_01_06,
    satisfaction_job            = bkp_01_03,
    feeling_worried             = bkp_02_02,
    feeling_happy               = bkp_02_03,
    feeling_sad                 = bkp_02_04,
    life_value_usefulness       = bkp_03,
    positive_attitude           = bkp_06_11,
    concern_economic_situation  = bkp_168_02,
    life_satisfaction_general   = bkp_204,
    health_status               = bkp_123,
    political_interest          = bkp_169,
    concern_social_cohesion     = bkp_168_09,
    number_close_friends        = bkp_07_01
    
    # Note: bkp_02_01 (angriness) was missing from the names() list, 
    # so we exclude it here.
  )

head(new_data)

# ---------------------------------------------------------------- #

# GENDER!

# Use the pipe (|>) without assignment (<-) to inspect the result.
new_data |>
  # Select only the relevant columns to make the comparison clear
  select(gender) |>
  
  # Apply the transformation and calculate the new column
  mutate(
    # Create the numeric dummy variable 'gender_female'
    gender_female = case_when(
      gender == 2 ~ 1,      # Female (code 2) becomes 1
      gender == 1 ~ 0,      # Male (code 1) becomes 0 (REFERENCE) !!
      TRUE ~ NA_real_       # Invalid/Non-response codes become NA
    )
  ) |>
  # Display the first few rows for confirmation (equivalent to SELECT TOP N)
  head()


# Update the 'new_data' DataFrame with the new column 'gender_female'

# new_data <- new_data |>
#   mutate(
#     gender_female = case_when(
#       gender == 2 ~ 1,
#       gender == 1 ~ 0,
#       TRUE ~ NA_real_
#     )
#   )



# EDUCATION LEVEL
# Doctoral as REFERENCE!!

new_data |>
  # Select the original variable for comparison
  select(education_level) |>
  
  # Apply the transformation and calculate the seven new dummy columns
  mutate(
    # NOTE: The reference group (0) is Doctoral (8).
    
    # Define the set of ALL valid levels (1 through 8)
    ALL_VALID_LEVELS = list(c(1, 2, 3, 4, 5, 6, 7, 8)), # Constant
    
    # --- DUMMY 1: Primary Education (1) ---
    edu_primary_dummy = case_when(
      education_level == 1 ~ 1,
      education_level %in% ALL_VALID_LEVELS[[1]] ~ 0, # All other valid levels are 0
      TRUE ~ NA_real_ # Codes 0 and all negatives become NA
    ),
    
    # --- DUMMY 2: Lower Secondary Education (2) ---
    edu_lower_secondary_dummy = case_when(
      education_level == 2 ~ 1,
      education_level %in% ALL_VALID_LEVELS[[1]] ~ 0, # All other valid levels are 0
      TRUE ~ NA_real_ 
    ),
    
    # --- DUMMY 3: Upper Secondary Education (3) ---
    edu_upper_secondary_dummy = case_when(
      education_level == 3 ~ 1,
      education_level %in% ALL_VALID_LEVELS[[1]] ~ 0, # All other valid levels are 0
      TRUE ~ NA_real_
    ),
    
    # --- DUMMY 4: Post-secondary non-tertiary (4) ---
    edu_post_secondary_dummy = case_when(
      education_level == 4 ~ 1,
      education_level %in% ALL_VALID_LEVELS[[1]] ~ 0, # All other valid levels are 0
      TRUE ~ NA_real_
    ),
    
    # --- DUMMY 5: Short cycle tertiary education (5) ---
    edu_short_cycle_dummy = case_when(
      education_level == 5 ~ 1,
      education_level %in% ALL_VALID_LEVELS[[1]] ~ 0, # All other valid levels are 0
      TRUE ~ NA_real_
    ),
    
    # --- DUMMY 6: Bachelor’s or equivalent level (6) ---
    edu_bachelor_dummy = case_when(
      education_level == 6 ~ 1,
      education_level %in% ALL_VALID_LEVELS[[1]] ~ 0, # All other valid levels are 0
      TRUE ~ NA_real_
    ),
    
    # --- DUMMY 7: Master’s or equivalent level (7) ---
    edu_master_dummy = case_when(
      education_level == 7 ~ 1,
      education_level %in% ALL_VALID_LEVELS[[1]] ~ 0, # All other valid levels are 0
      TRUE ~ NA_real_
    )
    # The 'ALL_VALID_LEVELS' list simplifies the '0' condition:
    # If the level is valid (1-8) BUT NOT the '1' level for the current dummy, it's 0.
  ) |>
  # Exclude the temporary list variable from the output
  select(-ALL_VALID_LEVELS) |>
  
  # Display the first 10 rows for confirmation
  head(10)






# this is s new comment

# ---------------------------------------------------------------- #






# ---------------------------------------------------------------- #
#                   EXPLORATORY DATA ANALYSIS (EDA)
# ---------------------------------------------------------------- #

# Sorts the frequency table of the 'personal_achievement_deserved' variable
# in descending order (from largest count to smallest count).
sort(table(new_data[["personal_achievement_deserved"]]), decreasing = TRUE)


#How to convert dummy reference?
#table(raw_data[["education_level"]])



