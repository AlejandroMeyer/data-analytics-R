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
#                   EXPLORATORY DATA ANALYSIS (EDA)
# ---------------------------------------------------------------- #

# Sorts the frequency table of the 'personal_achievement_deserved' variable
# in descending order (from largest count to smallest count).
sort(table(new_data[["personal_achievement_deserved"]]), decreasing = TRUE)



