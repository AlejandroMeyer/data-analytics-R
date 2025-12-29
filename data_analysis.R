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
    exp_fulltime_years          = expft20 / 10,
    exp_unemployment_years      = expue20,
    exp_parttime_years          = exppt20,
    labor_force_status          = lfs20,
    job_prestige_siops          = siops08_20 / 10,
    
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

# GENDER!

# Use the pipe (|>) without assignment (<-) to inspect the result.
new_data <- new_data |>
  # Apply the transformation and calculate the new column
  mutate(
    # Create the numeric dummy variable 'gender_female'
    gender_female = case_when(
      gender == 2 ~ 1,      # Female (code 2) becomes 1
      gender == 1 ~ 0,      # Male (code 1) becomes 0 (REFERENCE) !!
      TRUE ~ NA_real_       # Invalid/Non-response codes become NA
    ), 
    
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
    
    age = (2020 - birth_year)/10,
    
    # dummy variables for marital status with married living together, married living separate and single/unmarried (reference)
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
    
    # dummy variables for migration background, with no background as reference
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
    
    # dummy variables for number of friends, reference is average number of close friends (2 - 4)
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
    
    # dummy variables for the total number of kids, reference is having no kids
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
    
    # transforms income into 500 unit steps (could also use 1,000 unit steps or income classes instead)
    income_per_500 = gross_labor_income/1000,
    
    # sATISFACTION AND ATTITUDES
    
    # dummy variable for feeling of happiness with feeling good=4 and very good=5 as reference category
    not_or_somewhat_happy = case_when(
      feeling_happy %in% c(1,2) ~ 1,
      feeling_happy < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy variable for feeling of angriness with very rarely=1 and rarely=2 as reference category
    angry = case_when(
      feeling_angry %in% c(3,4,5) ~ 1,
      feeling_angry < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy variable for feeling of sadness with very rarely=1 and rarely=2 as reference category
    sad = case_when(
      feeling_sad %in% c(3,4,5) ~ 1,
      feeling_sad < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy variable for feeling of woriedness with very rarely=1 and rarely=2 as reference category
    worried = case_when(
      feeling_worried %in% c(3,4,5) ~ 1,
      feeling_worried < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    bad_feeling_overall = case_when(
      feeling_angry %in% c(3,4,5) & 
      feeling_sad %in% c(3,4,5) & 
      feeling_happy %in% c(1,2) ~ 1,
      TRUE ~ 0
    ),
    
    # dummy variable for the health status with very good=1 and good=2 as reference category 
    # (dummy variable = 1 means health status is only satisfactory or even below)
    only_satisfactory = case_when(
      health_status %in% c(3,4,5) ~ 1,
      health_status < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy variable for having strong political interest with not so strong (political_interest = 3)as reference category
    strong_political_interest = case_when(
      political_interest %in% c(1,2) ~ 1,
      political_interest < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy variable for having strong political interest at all
    no_political_interest = case_when(
      political_interest == 4 ~ 1,
      political_interest < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy variable for no concerns about own economic situation with having some worries (concern_economic_situation = 2) as reference category
    economy_not_worried = case_when(
      concern_economic_situation == 3 ~ 1,
      concern_economic_situation < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy variable for a lot of concerns about own economic situation
    economy_worried = case_when(
      concern_economic_situation == 1 ~ 1,
      concern_economic_situation < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy variable for a lot of concerns about social cohesion with having some worries (concern_social_cohesion = 2) as reference category
    social_great_concern = case_when(
      concern_social_cohesion == 1 ~ 1,
      concern_social_cohesion < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    # dummy variable for no concerns about social cohesion
    social_no_concern = case_when(
      concern_social_cohesion == 3 ~ 1,
      concern_social_cohesion < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    high_life = case_when(
      life_satisfaction_general %in% c(9,10) ~ 1,
      life_satisfaction_general < 0 ~ NA_real_,
      TRUE ~ 0
    ),
    
    high_satisfaction_values = case_when(
      satisfaction_income > 7 &
      satisfaction_job > 7 &
      life_value_usefulness > 7 ~ 1,
      TRUE ~ 0
    ),
    
    # LOGTEST
    achievement_deserved = case_when(
      personal_achievement_deserved %in% c(1,2,3) ~ 1,
      personal_achievement_deserved < 0 ~ NA_real_,
      TRUE ~ 0
    )
    
  )

# replace all other negative values we haven't replaces yet also with NA
new_data[new_data < 0] <- NA

# First experimentations with linear modelling lm()

mod1 <- lm(personal_achievement_deserved ~ gender_female + age, data = new_data)
summary(mod1)
# we see no correlation between age and main variable, there is an effect due to gender p value is < 0.05, but the effect is quite small


mod2 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high, data = new_data)
summary(mod2)
# p values are all significant, the effects from gender and age are negligible, we can see a very significant effect due to high or low education though

mod3 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + married_together + married_separate, data = new_data)
summary(mod3)
# we add marriage and see bad p values for it, meaning no effect, this could be due to the data not capturing regular relationships but just marriages

mod4 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + unemployed_or_minimal + non_working, data = new_data)
summary(mod4)
# being unemployed or only having a minimal job > great affect of 0.54

mod5 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + income_per_500 + I(income_per_500^2), data = new_data)
summary(mod5)
#incomes effect seems to be very small too, until no education prevails especially the effect of low education, additionally the gender_female variable seems to get traction

mod6 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + income_per_500 + I(income_per_500^2) + satisfaction_job + life_satisfaction_general, data = new_data)
summary(mod6)
# for life and job satisfaction we can see good effects with very good p values
# for every 5 points in life satisfaction (scale is 1-10), the main variable decreases by a whole point (they think they achieved what they deserved)

mod7 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + income_per_500 + I(income_per_500^2) + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + number_close_friends, data = new_data)
summary(mod7)
# number of close friends is completely insignificant, life_value_usefulness has a moderate effect

mod8 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high +  satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + number_close_friends + feeling_happy + migration_direct + migration_indirect, data = new_data)
summary(mod8)
# next we can see that the feeling of being happy is a good predictor with a very good p value, same with being a direct migrant
# conclusion for now: good predictors: education, life satisfaction in general, feeling of happiness, direct migration
# no correlation: number of close friends, age, gender (only very minimal)


mod9 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high +  satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + feeling_happy + migration_direct + migration_indirect, data = new_data)
summary(mod9)

mod9 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high +  satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + not_or_somewhat_happy + angry + migration_direct + migration_indirect, data = new_data)
summary(mod9)

mod10 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high +  satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + worried + sad + migration_direct + migration_indirect, data = new_data)
summary(mod10)

mod11 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high +  satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + only_satisfactory, data = new_data)
summary(mod11)

mod12 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high +  satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + one_or_two_children + more_than_two_children, data = new_data)
summary(mod12)

mod13 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high +  satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + strong_political_interest + no_political_interest, data = new_data)
summary(mod13)

mod14 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high +  satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + economy_worried + economy_not_worried + social_great_concern + social_no_concern, data = new_data)
summary(mod14)

mod15 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high +  satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + bad_feeling_overall, data = new_data)
summary(mod15)

mod16 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + high_life + migration_direct + migration_indirect + bad_feeling_overall + economy_worried + economy_not_worried + one_or_two_children + more_than_two_children + unemployed_or_minimal + non_working, data = new_data)
summary(mod16)

mod16 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + high_life + migration_direct + migration_indirect + bad_feeling_overall + economy_worried + economy_not_worried + one_or_two_children + more_than_two_children + satisfaction_income + satisfaction_job + life_value_usefulness + income_per_500, data = new_data)
summary(mod16)

# checking for multicolinearity
sum(new_data$income_per_500[new_data$non_working == 1], na.rm = TRUE)

mod16 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + high_life + migration_direct + migration_indirect + bad_feeling_overall 
            + economy_worried + economy_not_worried + one_or_two_children + more_than_two_children + income_per_500 + satisfaction_income + satisfaction_job + job_prestige_siops
            + positive_attitude + life_value_usefulness + exp_fulltime_years + exp_unemployment_years + married_together + married_separate + worried + no_political_interest + strong_political_interest, data = new_data)
summary(mod16)

mod16 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + high_life + migration_direct + migration_indirect + bad_feeling_overall  
            + economy_worried + economy_not_worried + one_or_two_children + income_per_500 + satisfaction_income + satisfaction_job
            + positive_attitude + life_value_usefulness + exp_unemployment_years + worried + no_political_interest + strong_political_interest, data = new_data)
summary(mod16)

robustnesstest <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + high_life + migration_direct + migration_indirect + bad_feeling_overall 
            + economy_worried + economy_not_worried + one_or_two_children + more_than_two_children + income_per_500 + satisfaction_income + satisfaction_job + job_prestige_siops
            + positive_attitude + life_value_usefulness + exp_fulltime_years + exp_unemployment_years + married_together + married_separate + worried + no_political_interest + strong_political_interest, family = "binomial", data = new_data)
summary(robustnesstest)
exp(coef(robustnesstest))
lrtest(robustnesstest, mod17)

#mod16 <- lm(personal_achievement_deserved ~ gender_female + age + education_low + education_high + high_life + migration_direct + migration_indirect + bad_feeling_overall + economy_worried + economy_not_worried + one_or_two_children + more_than_two_children + unemployed_or_minimal + non_working, data = new_data)
summary(mod16)

# LOGTEST VERSIONS
mod17 <- glm(achievement_deserved ~ gender_female + age,
             data = new_data, family = binomial)
summary(mod17)

mod18 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high,
             data = new_data, family = binomial)
summary(mod18)

mod19 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + married_together + married_separate,
             data = new_data, family = binomial)
summary(mod19)

mod20 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + unemployed_or_minimal + non_working,
             data = new_data, family = binomial)
summary(mod20)

mod21 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + income_per_500 + I(income_per_500^2),
             data = new_data, family = binomial)
summary(mod21)

mod22 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + income_per_500 + I(income_per_500^2) + satisfaction_job + life_satisfaction_general,
             data = new_data, family = binomial)
summary(mod22)

mod23 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + income_per_500 + I(income_per_500^2) + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + number_close_friends,
             data = new_data, family = binomial)
summary(mod23)

mod24 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + number_close_friends + feeling_happy + migration_direct + migration_indirect,
             data = new_data, family = binomial)
summary(mod24)

mod25 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + feeling_happy + migration_direct + migration_indirect,
             data = new_data, family = binomial)
summary(mod25)

mod26 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + not_or_somewhat_happy + angry + migration_direct + migration_indirect,
             data = new_data, family = binomial)
summary(mod26)

mod27 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + worried + sad + migration_direct + migration_indirect,
             data = new_data, family = binomial)
summary(mod27)

mod28 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + only_satisfactory,
             data = new_data, family = binomial)
summary(mod28)

mod29 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + one_or_two_children + more_than_two_children,
             data = new_data, family = binomial)
summary(mod29)

mod30 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + strong_political_interest + no_political_interest,
             data = new_data, family = binomial)
summary(mod30)

mod31 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + economy_worried + economy_not_worried + social_great_concern + social_no_concern,
             data = new_data, family = binomial)
summary(mod31)

mod32 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + satisfaction_job + life_satisfaction_general + life_value_usefulness + positive_attitude + low_friends + high_friends + migration_direct + migration_indirect + bad_feeling_overall,
             data = new_data, family = binomial)
summary(mod32)

mod33 <- glm(achievement_deserved ~ gender_female + age + education_low + education_high + high_life + migration_direct + migration_indirect + bad_feeling_overall + economy_worried + economy_not_worried + one_or_two_children + more_than_two_children + unemployed_or_minimal + non_working,
             data = new_data, family = binomial)
summary(mod33)

residuals(mod16)
plot(mod16, which = 2)
hist(residuals(mod16))
summary(new_data$age)

library(car)
vif(mod16)
library(lmtest)
bptest(mod16)
library(sandwich)
coeftest(mod16, vcov = vcovHC(mod16, "HC3"))

# BELOW HERE ARE SOME EXPERIMENTATIONS TO VISUALIZE AND GET MORE FAMILIARIZED WITH THE DATA
#
mean(new_data$personal_achievement_deserved)
hist(new_data$job_prestige_siops)
hist(new_data$personal_achievement_deserved, 
     main = "Distribution of main variable", 
     xlab = "achieved    <>     not achieved", 
     col = "lightblue", 
     border = "white")

hist(new_data$number_close_friends, main = "number of friends", xlab = "friends", col = "lightblue", breaks = seq(0 , 100, by = 1), xlim = c(0,20), xaxt = "n")
axis(1, at = seq(0, 20, by = 1))
median(new_data$number_close_friends, na.rm = TRUE)

hist(new_data$feeling_happy, main = "happy", xlab = "happiness", col = "lightblue", breaks = seq(0.5, 5.5, by = 1))
axis(1, at = seq(1,6, by = 1))

hist(new_data$total_children, main = "happy", xlab = "happiness", col = "lightblue", breaks = seq(0, max(new_data$total_children), by = 1))

# ---------------------------------------------------------------- #
#                   EXPLORATORY DATA ANALYSIS (EDA)
# ---------------------------------------------------------------- #

# Sorts the frequency table of the 'personal_achievement_deserved' variable
# in descending order (from largest count to smallest count).
# sort(table(new_data[["personal_achievement_deserved"]]), decreasing = TRUE)


#How to convert dummy reference?
#table(raw_data[["education_level"]])

View(new_data)
