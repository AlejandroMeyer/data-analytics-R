#' ---
#' title: ""
#' author: ""
#' date: ""
#' ---



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


# ================================================================ #
# ================================================================ #
#                        DATA CLEANING
# ================================================================ #
# ================================================================ #

# Load the necessary library for data manipulation
library(dplyr)

# Rename the columns in the 'new_data' data frame from 'raw_data'
new_data <- raw_data %>%
  rename(
    # New_Name = Actual_Name
    
    # Key Dependent Variable
    personal_achievement_deserved = bkp_05_02, 
    
    # Demographics and Background #Ale
    education_level             = isced11_20,
    marital_status              = bkfamstd,
    birth_year                  = bkpbirthy,
    gender                      = sex,
    migration_background        = migback,
    total_children              = sumkids,
    household_id                = cid,
    
    # Financial and Employment  # Ale Meyer
    gross_labor_income          = labgro20,
    
    # to test for fulltime experience with 10 year steps 
    # instead of single years
    exp_fulltime_years          = expft20 / 10, 
    
    exp_unemployment_years      = expue20,
    exp_parttime_years          = exppt20,
    labor_force_status          = lfs20,
    
    # to test for jumps in prestige by 10 points 
    # instead of just 1.0
    job_prestige_siops          = siops08_20 / 10, 
    
    # Satisfaction and Attitudes #Deniz and Filippo
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

# ================================================================ #
#                        END DATA CLEANING
# ================================================================ #


# ================================================================ #
# ================================================================ #
#                        DUMMY VARIABLES
# ================================================================ #
# ================================================================ #

# We add all dummy variables to the dataframe
new_data <- new_data |>
  mutate(
    
    # ========================================-
    # ====-- Demographics and Background ====--
    # ========================================-
    
    # -- Education_level --
    
    # We make 3 education classes for low, average (reference), 
    # and high education as professor suggested
    education_low = case_when(
      education_level %in% c(1, 2) ~ 1,
      education_level == 0 ~ NA_real_,
      education_level < 0 ~ NA_real_,
      TRUE ~ 0, 
    ),
    
    education_high = case_when(
      education_level %in% c(6, 7, 8) ~ 1,
      education_level == 0 ~ NA_real_,
      education_level < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- Marital status --
    # with married living together, married living separate 
    # and single/unmarried (reference)
    # Reference: single/unmarried
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
      birth_year < 0 ~ NA_real_, # If negative (missing code), set Age to NA
      # age calculation, we look at the age variable in 10 year steps (decades)
      TRUE ~ (2020 - birth_year)/10    
    ),
    
    # -- Gender --
    
    # Reference: Male(0)
    gender_female = case_when(
      gender == 2 ~ 1,      # Female (code 2) becomes 1
      gender == 1 ~ 0,      # Male (code 1) becomes 0 (reference)
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
      total_children < 0 ~ NA_real_, #Error!
      TRUE ~ 0
    ),
    
    more_than_two_children = case_when(
      total_children > 2 ~ 1,
      total_children < 0 ~ NA_real_, #Error fix
      TRUE ~ 0
    ),
    
    # -- Household_id --        
    
    
    # ========================================-
    # ====-- Financial and Employment ====--
    # ========================================-
    
    # -- gross_labor_income --
    
    # transforms income into 1,000 unit steps 
    # (could also use 500 unit steps or income classes instead)
    #income_per_1000 = (gross_labor_income/1000),
    #Delete this
    
    
    # -- exp_fulltime_years --
    
    
    # -- exp_unemployment_years --
    
    
    # -- exp_parttime_years --
    
    
    # -- labor_force_status --
    
    # dummy variables for labor force status, with "working" as reference
    # unemployed or only secondary job
    unemployed_or_minimal = case_when(
      labor_force_status %in% c(1, 6, 8, 9, 10, 13) ~ 1,
      labor_force_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    # non working due to reasons: education, pension, military, parental leave
    non_working = case_when(
      labor_force_status %in% c(2, 3, 4, 5) ~ 1,
      labor_force_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- job_prestige_siops --        
    
    
    # ========================================-
    # ====-- Satisfaction and Attitudes ====--
    # ========================================-
    
    # -- satisfaction_income --
    
    # -- satisfaction_job --      
    
    # -- feeling_angry --     
    
    # Reference: very rarely or rarely angry
    # Because want to test the effect of having the negative feelings: 
    # angriness, worriedness and sadness
    angry_often = case_when(
      feeling_angry %in% c(3, 4, 5) ~ 1, # Very often, Often, somewhat = 1
      feeling_angry < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- feeling_worried -- 
    
    # Reference: very rarely or rarely worried
    worried_often = case_when(
      feeling_worried %in% c(3, 4, 5) ~ 1, # Very often, Often, somewhat = 1
      feeling_worried < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- feeling_sad --  
    
    # Reference: very rarely or rarely sad
    sad_often = case_when(
      feeling_sad %in% c(3, 4, 5) ~ 1,     # Very often, Often, somewhat = 1
      feeling_sad < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- feeling_happy -- 
    
    # Reference: often or very often happy, because we assume that 
    # being happy is the default.
    # > we want to test the effect of NOT being happy
    not_happy = case_when(
      feeling_happy %in% c(1, 2) ~ 1,   # very rarely (1) or rarely (2) = 1
      feeling_happy < 0 ~ NA_real_,
      TRUE ~ 0                          
    ),
    
    # -- life_value_usefulness --
    # likert scale is from 0-10 > large enough to assume metric data 
    # and take the raw values
    
    # -- positive_attitude --    
    # likert scale is from 1-7 > large enough to assume metric data 
    # and take the raw values
    
    # -- concern_economic_situation --
    
    # dummy for having no concerns about own economic situation 
    # with having some worries (concern_economic_situation = 2) as reference
    economy_not_worried = case_when(
      concern_economic_situation == 3 ~ 1,
      concern_economic_situation < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy for having a lot of concerns about own economic situation
    economy_worried = case_when(
      concern_economic_situation == 1 ~ 1,
      concern_economic_situation < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- life_satisfaction_general --
    
    # -- health_status --
    
    # Reference: good health
    # dummy for the health status with very good=1 and good=2 as reference 
    # (this is the reference because the largest group is health being good)
    # dummy variable = 1 means their health status is only satisfactory or below
    health_not_good = case_when(
      health_status %in% c(3,4,5) ~ 1,
      health_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- political_interest -- 
    
    # Reference: political interest being not so strong (largest group)
    # dummy for having strong political interest
    strong_political_interest = case_when(
      political_interest %in% c(1,2) ~ 1,
      political_interest < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy for having strong political interest at all
    no_political_interest = case_when(
      political_interest == 4 ~ 1,
      political_interest < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # -- concern_social_cohesion --
    
    # Reference: Having some worries (concern_social_cohesion = 2)
    # dummy for having a lot of concerns about social cohesion
    social_great_concern = case_when(
      concern_social_cohesion == 1 ~ 1,
      concern_social_cohesion < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy for having no concerns about social cohesion
    social_no_concern = case_when(
      concern_social_cohesion == 3 ~ 1,
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
    ),
    
    # --- Further dummy variables for grouping multiple variables
    
    # dummy for grouping bad feelings in general
    # Reference: not experiencing bad feelings across all 3 categories
    # bad_feeling_overall = 1 when person experiences angriness, sadness 
    # and lack of happiness all at the same time
    # worriedness is not included because it has been identified as a 
    # different type of bad feeling and doesn't correlate well with others
    bad_feeling_overall = case_when(
      feeling_angry %in% c(3,4,5) & 
        feeling_sad %in% c(3,4,5) & 
        feeling_happy %in% c(1,2) ~ 1,
      TRUE ~ 0
    ),
    
    # Dummy for checking extremely high life satisfaction
    # Reference: values from 1-8 on the likert scale
    high_life_satisfaction = case_when(
      life_satisfaction_general %in% c(9,10) ~ 1,
      life_satisfaction_general < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # Dummy for experiencing high satisfaction and psychological values 
    # across 3 categories
    # Reference: not experiencing high satisfaction (> 7) in all 3 categories
    high_satisfaction_values = case_when(
      satisfaction_income > 7 &
        satisfaction_job > 7 &
        life_value_usefulness > 7 ~ 1,
      TRUE ~ 0
    ),
    
    # lOGTEST
    # Dummy for optional log tests, please do NOT delete
    achievement_deserved = case_when(
      personal_achievement_deserved %in% c(1,2,3) ~ 1,
      personal_achievement_deserved < 0 ~ NA_real_,
      TRUE ~ 0
    )
    
  )

# replace all other negative values we haven't replaced yet also with NA
new_data[new_data < 0] <- NA

sum(new_data$gross_labor_income == 0, na.rm = TRUE)
# Because we use log() in the modelling phase for gross labor income
# We can not have income values that equal 0
# (there are 50 gross labor income values in the data set with the value 0)
new_data$gross_labor_income[new_data$gross_labor_income == 0] <- NA

# ================================================================ #
#                        END DUMMY VARIABLES
# ================================================================ #


# Check the result
head(new_data)
summary(new_data$age)


# ================================================================ #
# ================================================================ #
#                EXPERIMENTATION WITH MODELLING lm()
# ================================================================ #
# ================================================================ #


mod1 <- lm(personal_achievement_deserved ~ gender_female + age, 
           data = new_data)
summary(mod1)
# we see no correlation between age and main variable, there is an effect 
# due to gender p value is < 0.05, but the effect is quite small


mod2 <- lm(personal_achievement_deserved ~ gender_female + age + education_low 
           + education_high, 
           data = new_data)
summary(mod2)
# p values are all significant, the effects from gender and age are negligible, 
# we can see a very significant effect due to high or low education though


mod3 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + married_together 
  + married_separate, 
  data = new_data
)
summary(mod3)
# we add marriage and see bad p values for it, meaning no effect, 
# this could be due to the data not capturing regular relationships 
# but just marriages


mod4 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + unemployed_or_minimal 
  + non_working, 
  data = new_data
)
summary(mod4)
# being unemployed or only having a minimal job > great effect of 0.54


mod5 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2), 
  data = new_data
)
summary(mod5)
# incomes effect seems to be very small too, until now education prevails 
# especially the effect of low education, additionally the gender_female 
# variable seems to get traction


mod6 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general, 
  data = new_data
)
summary(mod6)
# for life and job satisfaction we can see good effects with very good p values
# for every 5 points in life satisfaction (scale is 1-10), the main variable 
# decreases by a whole point (they think they achieved what they deserved)


mod7 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job 
  + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends, 
  data = new_data
)
summary(mod7)
# number of close friends is completely insignificant, life_value_usefulness 
# has a moderate effect


mod8 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) + satisfaction_job 
  + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends 
  + migration_direct 
  + migration_indirect, 
  data = new_data
)
summary(mod8)
# next we can see that the feeling of being happy is a good predictor with a 
# very good p value, same with being a direct migrant
# conclusion for now: good predictors: education, life satisfaction in general, 
# feeling of happiness, direct migration
# no correlation: number of close friends, age, gender (only very minimal)
# > But we still have to keep age and gender in tests as controlling variables, 
# same with income


# new tests added start here
# Starting from here, we use the previous model as a base 
# and gradually tested some other variables one by one using this base

# testing of all the feelings variables
mod9 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + not_happy 
  + migration_direct + migration_indirect, 
  data = new_data
)
summary(mod9)
# added not_happy


mod10 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + not_happy 
  + angry_often + migration_direct + migration_indirect, 
  data = new_data
)
summary(mod10)
# added angriness


mod11 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + worried_often 
  + sad_often + migration_direct + migration_indirect, 
  data = new_data
)
summary(mod11)
# tested worriedness and sadness instead of happy and angry


mod12 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + migration_direct 
  + migration_indirect + health_not_good, 
  data = new_data
)
summary(mod12)
# testing of the health status instead of the feelings variables


mod13 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + migration_direct 
  + migration_indirect + one_or_two_children + more_than_two_children, 
  data = new_data
)
summary(mod13)
# testing of number of children instead of health status


mod14 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + migration_direct 
  + migration_indirect + strong_political_interest + no_political_interest, 
  data = new_data
)
summary(mod14)
# testing of political interest instead of number of children


mod15 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + migration_direct 
  + migration_indirect + economy_worried + economy_not_worried 
  + social_great_concern + social_no_concern, 
  data = new_data
)
summary(mod15)
# testing of concern for own economic situation and social cohesion concern 
# instead of political interest. Concern for economic situation seems important 
# but for social cohesion not.


# Test for remaining independent variables relating to Job and Income
mod16 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + migration_direct 
  + migration_indirect + job_prestige_siops, 
  data = new_data
)
summary(mod16)
# Testing the values for job prestige


mod17 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + migration_direct 
  + migration_indirect + satisfaction_income, 
  data = new_data
)
summary(mod17)
# Testing the variables for income satisfaction 


mod18 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + migration_direct 
  + migration_indirect + exp_fulltime_years + exp_parttime_years 
  + exp_unemployment_years, 
  data = new_data
)
summary(mod18)
# Testing the experience of years spent in full time, part time or unemployment


mod19 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + satisfaction_job + life_satisfaction_general + life_value_usefulness 
  + positive_attitude + low_friends + high_friends + migration_direct 
  + migration_indirect + bad_feeling_overall, 
  data = new_data
)
summary(mod19)
# testing for a new dummy variable called "bad_feeling_overall"
# this tests the effect of experiencing bad feelings overall
# (being sad, angry and not happy at the same time)


mod20 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + high_life_satisfaction + migration_direct + migration_indirect 
  + bad_feeling_overall + economy_worried 
  + economy_not_worried + one_or_two_children + more_than_two_children, 
  data = new_data
)
summary(mod20)
# using the new dummy variable for extremely high life satisfaction 
# instead raw values (high_life_satisfaction)


mod21 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) 
  + high_life_satisfaction + migration_direct + migration_indirect 
  + bad_feeling_overall + economy_worried 
  + economy_not_worried + one_or_two_children + more_than_two_children 
  + satisfaction_income + satisfaction_job + life_value_usefulness 
  + log(gross_labor_income), 
  data = new_data
)
summary(mod21)
# using the new dummy variable for extremely high life satisfaction 
# instead raw values (high_life_satisfaction)
# testing for extremely high life_satisfaction and comparing the effect 
# of other satisfaction variables

# checking for multicollinearity
# -> by this test we can see that we cannot test for income and labor 
# force status at the same time because they are multicollinear
sum(new_data$income_per_1000[new_data$non_working == 1], na.rm = TRUE)


mod22 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) + high_life_satisfaction 
  + migration_direct + migration_indirect + bad_feeling_overall 
  + economy_worried + economy_not_worried + one_or_two_children 
  + more_than_two_children + log(gross_labor_income) 
  + satisfaction_income + satisfaction_job + job_prestige_siops
  + positive_attitude + life_value_usefulness + exp_fulltime_years 
  + exp_unemployment_years + married_together + married_separate 
  + worried_often + no_political_interest + strong_political_interest, 
  data = new_data
)
summary(mod22)
# For this one I just wanted test as many variables as possible
# to see if there are any effects on the model if I try to include anything


mod23 <- lm(
  personal_achievement_deserved ~ gender_female + age + education_low 
  + education_high + high_life_satisfaction + migration_direct 
  + migration_indirect + bad_feeling_overall  
  + economy_worried + economy_not_worried + one_or_two_children 
  + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) + satisfaction_income + satisfaction_job
  + positive_attitude + life_value_usefulness + exp_unemployment_years 
  + worried_often + no_political_interest + strong_political_interest, 
  data = new_data
)
summary(mod23)
# This is a test where I tried to include every variable that seemed 
# SOMEWHAT important so far


robustnesstest <- glm(
  achievement_deserved ~ gender_female + age + education_low 
  + education_high + high_life_satisfaction + migration_direct 
  + migration_indirect + bad_feeling_overall 
  + economy_worried + economy_not_worried + one_or_two_children 
  + more_than_two_children + log(gross_labor_income) 
  + log(gross_labor_income) + I(log(gross_labor_income)^2) 
  + satisfaction_income + satisfaction_job + job_prestige_siops
  + positive_attitude + life_value_usefulness + exp_fulltime_years 
  + exp_unemployment_years + married_together + married_separate 
  + worried_often + no_political_interest + strong_political_interest, 
  family = "binomial", 
  data = new_data
)
summary(robustnesstest)
exp(coef(robustnesstest))
# This is a robustness test with logistic regression
# It tests if the variables would also be relevant if we used logistic 
# regression instead.
# It seems like the chosen variables stay relevant / significant (robust)



# ================================================================ #
#                  END EXPERIMENTATION MODELLING
# ================================================================ #


# ================================================================ #
# ================================================================ #
#                  FINAL MODEL (MOD 20) - STRUCTURED
# ================================================================ #
# ================================================================ #

mod29 <- lm(
  formula = personal_achievement_deserved ~ 
    
    # --- Demographics and Background ---
    gender_female 
  + age 
  + education_low 
  + education_high 
  + migration_direct 
  + migration_indirect 
  + one_or_two_children 
  + more_than_two_children  #New
  
  # --- Financial and Employment ---
  + log(gross_labor_income) 
  + I(log(gross_labor_income)^2) #New
  + exp_unemployment_years 
  
  
  # --- Satisfaction and Attitudes ---
  + satisfaction_income 
  + satisfaction_job 
  + high_life_satisfaction 
  + life_value_usefulness 
  + positive_attitude 
  + bad_feeling_overall 
  + worried_often 
  
  # --- Worries and Interests ---
  + economy_worried 
  + economy_not_worried 
  + no_political_interest 
  + strong_political_interest,
  
  data = new_data
)

summary(mod29)


mod30 <- lm(
  formula = personal_achievement_deserved ~ 
    
    # --- Demographics and Background ---
    #gender_female 
    #+ age 
    + education_low 
  #+ education_high 
  #+ migration_direct 
  #+ migration_indirect 
  + one_or_two_children 
  
  # --- Financial and Employment ---
  #+ log(gross_labor_income) 
  + exp_unemployment_years 
  
  # --- Satisfaction and Attitudes ---
  + satisfaction_income 
  + satisfaction_job 
  #+ high_life_satisfaction 
  + life_value_usefulness 
  #+ positive_attitude 
  #+ bad_feeling_overall 
  #+ worried_often 
  
  # --- Worries and Interests ---
  #+ economy_worried 
  + economy_not_worried 
  + no_political_interest 
  + strong_political_interest,
  
  data = new_data
)

summary(mod30)


# testing of multicollinearity of models
# test any model by replacing mod_x
# might need to install packages when using these functions
library(car)

# We removed vif(mod20) to focus on the final model validation

vif(mod29) #Final Model


# ================================================================ #
#                  END FINAL MODEL STRUCTURED
# ================================================================ #


# ================================================================ #
# ================================================================ #
#                     VISUALIZATIONS AND GRAPHS
# ================================================================ #
# ================================================================ #

# BELOW HERE ARE SOME EXPERIMENTATIONS TO VISUALIZE AND GET MORE 
# FAMILIARIZED WITH THE DATA

mean(new_data$personal_achievement_deserved)
hist(new_data$job_prestige_siops)

# visualization of main variable
hist(new_data$personal_achievement_deserved, 
     main = "Distribution of main variable", 
     xlab = "achieved    <>     not achieved", 
     col = "lightblue", 
     border = "white")

hist(new_data$number_close_friends, main = "number of friends", 
     xlab = "friends"
     , col = "lightblue", breaks = seq(0 , 100, by = 1)
     , xlim = c(0,20), xaxt = "n"
)
axis(1, at = seq(0, 20, by = 1))
median(new_data$number_close_friends, na.rm = TRUE)

hist(new_data$feeling_happy, main = "happy", xlab = "happiness", 
     col = "lightblue", breaks = seq(0.5, 5.5, by = 1))
axis(1, at = seq(1,6, by = 1))

hist(new_data$total_children, main = "Children", xlab = "amount of children", 
     col = "lightblue", breaks = seq(0, max(new_data$total_children), by = 1))

# This checks how the gross labor income behaves (in this case from 
# 2,000 to 6,000 units, an increase of 200%)
old <- log(2000) 
new <- log(6000) 
effect <- 0.754485 * (new - old) + -0.058797 * (new^2 - old^2) 
effect

# ================================================================ #
#                     END VISUALIZATIONS AND GRAPHS
# ================================================================ #


# ================================================================ #
# ================================================================ #
#                        COMPARING MODELS
# ================================================================ #
# ================================================================ #

install.packages("stargazer")

library(stargazer)

#+ results='asis'
stargazer(mod23, mod29, mod30, 
          type = "latex", 
          header = FALSE,
          font.size = "footnotesize",
          title = "Evolution of Models",
          dep.var.labels = "Achievement Gap (High = Worse)",
          omit = c("Constant") # We remove the renaming list to avoid errors
)


#================-

# 1. Frequency Table (How many Men vs Women)
gender_distribution <- new_data %>%
  count(gender_code = gender) %>%
  mutate(
    label = case_when(
      gender_code == 1 ~ "Male",
      gender_code == 2 ~ "Female",
      TRUE ~ "Unknown/Missing"
    )
  )

# View the result in a table
View(gender_distribution)

# 2. Base R Table (Quick check)
table(new_data$gender, useNA = "ifany")


# ================================================================ #
#                        TEST TO VALIDATE DUMMIES
# ================================================================ #


table(Original = new_data$education_level, 
      Dummy = new_data$education_low, useNA = "ifany")

# New table
validation_education <- new_data %>%
  count(original = education_level, 
        nuevo_dummy = education_low)

View(validation_education)

# ================================================================ #
#                        END PROJECT
# ================================================================ #