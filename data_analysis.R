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
    feeling_angry               = bkp_02_01,
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
  )

head(new_data)

# ---------------------------------------------------------------- #
# ---------------------------------------------------------------- #
#                       DUMMY VARIABLES
# ---------------------------------------------------------------- #
# ---------------------------------------------------------------- #

# We add all dummy variables to the DF
new_data <- new_data|>
  mutate(
    
    # -----------------------------------------
    # ------ Demographics and Background ------
    # -----------------------------------------
    
    # -- Education_level --
    
    # We make 3 dummy variables for low, average (reference), and high education as prof suggested
    education_low = case_when(
      education_level %in% c(0, 1, 2) ~ 1,
      education_level < 0 ~ NA_real_,
      TRUE ~ 0, 
    ),
    
    education_high = case_when(
      education_level %in% c(6, 7, 8) ~ 1,
      education_level < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- Marital_status --
    
    # Marital status with married living together, married living separate and single/unmarried (reference)
    #Reference: single/unmarried
    married_together = case_when(
      marital_status %in% c(1, 7) ~ 1,
      marital_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    married_separate = case_when(
      marital_status %in% c(2, 6, 8) ~ 1,
      marital_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- Birth_year --    
    
    # We use conditional logic to handle missing birth years (-1, -2, etc.)
    # We use 2020 as the reference year to match the survey wave (labgro20)
    age = case_when(
      birth_year < 0 ~ NA_real_,  # If birth_year is negative (missing code), set Age to NA
      birth_year > 2020 ~ NA_real_, # Safety check: Cannot be born after survey
      TRUE ~ 2020 - birth_year    # Valid calculation
    ),
    
    # -- Gender --
    
    # Reference: Male(0)
    gender_female = case_when(
      gender == 2 ~ 1,      # Female (code 2) becomes 1
      gender == 1 ~ 0,      # Male (code 1) becomes 0 (REFERENCE) !!
      TRUE ~ NA_real_       # Invalid/Non-response codes become NA
    ),
    
    # -- Migration_background --
    
    # Reference: No background 
    migration_direct = case_when(
      migration_background == 2 ~ 1,
      migration_background < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    migration_indirect = case_when(
      migration_background == 3 ~ 1,
      migration_background < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- Total_children --
    
    # Reference: having no kids
    one_or_two_children = case_when(
      total_children %in% c(1,2) ~ 1,
      labor_force_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    more_than_two_children = case_when(
      total_children > 2 ~ 1,
      labor_force_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- Household_id --       
    
    
    # -----------------------------------------
    # ------ Financial and Employment ------
    # -----------------------------------------
    
    # -- gross_labor_income --
    
    # transforms income into 500 unit steps (could also use 1,000 unit steps or income classes instead)
    income_per_500 = (gross_labor_income/500
    ),
    
    
    # -- exp_fulltime_years --
    
    
    # -- exp_unemployment_years --
    
    
    # -- exp_parttime_years --
    
    
    # -- labor_force_status --
    
    # dummy variables for labor force status, with "working" as reference
    # unemployed or only secondary job
    unemployed_or_minimal = case_when(
      labor_force_status %in% c(1, 6, 8, 9, 10) ~ 1,
      labor_force_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    # non working due to reasons: education, pension, military, parental leave
    non_working = case_when(
      labor_force_status %in% c(1, 6, 8, 9, 10) ~ 1,
      labor_force_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- job_prestige_siops --       
    
    
    # -----------------------------------------
    # ------ Satisfaction and Attitudes ------
    # -----------------------------------------
    
    # -- satisfaction_income --
    
    # -- satisfaction_job --     
    
    # -- feeling_angry --    
    
    # Reference: Rarely/Never Angry
    angry_often = case_when(
      feeling_angry %in% c(1, 2) ~ 1,   # Very often (1) or Often (2) -> 1
      feeling_angry < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- feeling_worried -- 
    
    # Reference: Rarely/Never Worried
    worried_often = case_when(
      feeling_worried %in% c(1, 2) ~ 1, # Very often (1) or Often (2) -> 1
      feeling_worried < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- feeling_happy -- 
    
    # Reference: Rarely/Never Happy
    happy_often = case_when(
      feeling_happy %in% c(1, 2) ~ 1,   # Very often (1) or Often (2) -> 1
      feeling_happy < 0 ~ NA_real_,
      TRUE ~ 0                          # Sometimes/Seldom/Never -> 0
    ),
    
    # -- feeling_sad --  
    
    # Reference: Rarely/Never Sad
    sad_often = case_when(
      feeling_sad %in% c(1, 2) ~ 1,     # Very often (1) or Often (2) -> 1
      feeling_sad < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- life_value_usefulness --
    
    # -- positive_attitude --    
    
    # -- concern_economic_situation --
    
    # Reference: No concerns
    concern_econ_high = case_when(
      concern_economic_situation %in% c(1, 2) ~ 1, # Great (1) or Some (2) concern -> 1
      concern_economic_situation < 0 ~ NA_real_,
      TRUE ~ 0                                     # No concern (3) -> 0
    ),
    
    # -- life_satisfaction_general --
    
    # -- health_status --
    
    # Reference: Average/Bad Health
    health_good = case_when(
      health_status %in% c(1, 2) ~ 1,   # Very good (1) or Good (2) -> 1
      health_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- political_interest -- 
    
    # Reference: Little/No Interest
    political_interest_high = case_when(
      political_interest %in% c(1, 2) ~ 1, # Very strong (1) or Strong (2) -> 1
      political_interest < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- concern_social_cohesion --
    
    # Reference: No concerns (3)
    concern_social_high = case_when(
      concern_social_cohesion %in% c(1, 2) ~ 1,    # Great (1) or Some (2) concern -> 1
      concern_social_cohesion < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- number_close_friends --          
    
    # Reference is average number of close friends (2 - 4)
    low_friends = case_when(
      number_close_friends %in% c(0, 1) ~ 1,
      labor_force_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    high_friends = case_when(
      number_close_friends %in% c(5,6,7,8) ~ 1,
      labor_force_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    very_high_friends = case_when(
      number_close_friends > 8 ~ 1,
      labor_force_status < 0 ~ NA_real_,
      TRUE ~ 0
    )
    
  )

# replace all other negative values we haven't replaces yet also with NA
new_data[new_data < 0] <- NA


# ---------------------------------------------------------------- #
#                       END DUMMY VARIABLES
# ---------------------------------------------------------------- #



# Check the result
head(new_data)
summary(new_data$age)


# ---------------------------------------------------------------- #
# ---------------------------------------------------------------- #
#               EXPERIMENTATION WITH MODELLING lm()
# ---------------------------------------------------------------- #
# ---------------------------------------------------------------- #


mod1 <- lm(personal_achievement_deserved ~ gender_female + age, data = new_data)
summary(mod1)
# we see no correlation between age and main variable, there is an effect due to gender p value is < 0.05, but the effect is quite small


mod2 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high, data = new_data)
summary(mod2)
# p values are all significant, the effects from gender and age are negligible, we can see a very significant effect due to high or low education though


mod3 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + married_together 
           + married_separate
           , data = new_data
           )
summary(mod3)
# we add marriage and see bad p values for it, meaning no effect, this could be due to the data not capturing regular relationships but just marriages


mod4 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + unemployed_or_minimal 
           + non_working
           , data = new_data
           )
summary(mod4)
# 


mod5 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + income_per_500 
           + I(income_per_500^2)
           , data = new_data
           )
summary(mod5)
#incomes effect seems to be very small too, until no education prevails especially the effect of low education, 
# additionally the gender_female variable seems to get traction


mod6 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + income_per_500 
           + I(income_per_500^2) 
           + satisfaction_job + life_satisfaction_general
           , data = new_data
           )
summary(mod6)
# for life and job satisfaction we can see good effects with very good p values
# for every 5 points in life satisfaction (scale is 1-10), the main variable decreases by a whole point (the think they achieved what they deserved)


mod7 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + income_per_500 
           + I(income_per_500^2) 
           + satisfaction_job 
           + life_satisfaction_general + life_value_usefulness 
           + positive_attitude + number_close_friends
           , data = new_data
           )
summary(mod7)
# number of close friends is completely insignificant, life_value_usefulness has a moderate effect


mod8 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job 
           + life_satisfaction_general + life_value_usefulness 
           + positive_attitude + number_close_friends 
           + feeling_happy + migration_direct 
           + migration_indirect
           , data = new_data
           )
summary(mod8)
# next we can see that the feeling of being happy is a good predictor with a very good p value, same with being a direct migrant
# conclusion for now: good predictors: education, life satisfaction in general, feeling of happiness, direct migration
# no correlation: number of close friends, age, gender (only very minimal)

summary(new_data$age)

# ---------------------------------------------------------------- #
#                       END EXPERIMENTATION MODELLING
# ---------------------------------------------------------------- #


# ---------------------------------------------------------------- #
# ---------------------------------------------------------------- #
#                   VISUALIZATIONS AND GRAPHS
# ---------------------------------------------------------------- #
# ---------------------------------------------------------------- #


# BELOW HERE ARE SOME EXPERIMENTATIONS TO VISUALIZE AND GET MORE FAMILIARIZED WITH THE DATA
#
mean(new_data$personal_achievement_deserved)

hist(new_data$personal_achievement_deserved, 
     main = "Distribution of main variable", 
     xlab = "achieved    <>     not achieved", 
     col = "lightblue", 
     border = "white")

hist(new_data$number_close_friends, main = "number of friends", xlab = "friends"
     , col = "lightblue", breaks = seq(0 , 100, by = 1)
     , xlim = c(0,20), xaxt = "n"
     )
axis(1, at = seq(0, 20, by = 1))
median(new_data$number_close_friends, na.rm = TRUE)

hist(new_data$feeling_happy, main = "happy", xlab = "happiness", col = "lightblue", breaks = seq(0.5, 5.5, by = 1))
axis(1, at = seq(1,6, by = 1))

hist(new_data$total_children, main = "happy", xlab = "happiness", col = "lightblue", breaks = seq(0, max(new_data$total_children), by = 1))

