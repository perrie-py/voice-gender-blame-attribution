# ET323 Code


install.packages('readr')
library(readr)
install.packages('lme4')
library('lme4')
install.packages('ggplot2')
library(ggplot2)
library(tidyr)
library(dplyr)

driverdata <- read.csv('https://raw.githubusercontent.com/perrie-py/ET323-Female-Drivers/refs/heads/main/data.drivergender.csv')
View(driverdata)

# boxplot shoiwng rating of skill in driver 1, male and female condition
ggplot(driverdata, aes(x = Group %in% c('FA', 'FP'), y = D1.skill)) +
  geom_boxplot() +
  scale_x_discrete(labels = c('MA/MP', 'FA/FP')) +
  labs(
    x = 'Driver Group',
    y = 'Rating of Skill'
  )
# boxplot shoiwng rating of skill in driver 2, male and female condition
ggplot(driverdata, aes(x = Group %in% c('FA', 'FP'), y = D2.skill)) +
  geom_boxplot() +
  scale_x_discrete(labels = c('MA/MP', 'FA/FP')) +
  labs(
    x = 'Driver Group',
    y = 'Rating of Skill'
  )

# boxplot shoiwng rating of skill in driver 3, male and female condition
ggplot(driverdata, aes(x = Group %in% c('FA', 'FP'), y = D3.skill)) +
  geom_boxplot() +
  scale_x_discrete(labels = c('MA/MP', 'FA/FP')) +
  labs(
    x = 'Driver Group',
    y = 'Rating of Skill'
  )

# boxplot shoiwng rating of success in driver 1, male and female condition
ggplot(driverdata, aes(x = Group %in% c('FA', 'FP'), y = D1.success)) +
  geom_boxplot() +
  scale_x_discrete(labels = c('MA/MP', 'FA/FP')) +
  labs(
    x = 'Driver Group',
    y = 'Rating of Success'
  )

# boxplot shoiwng rating of success in driver 2, male and female condition
ggplot(driverdata, aes(x = Group %in% c('FA', 'FP'), y = D2.success)) +
  geom_boxplot() +
  scale_x_discrete(labels = c('MA/MP', 'FA/FP')) +
  labs(
    x = 'Driver Group',
    y = 'Rating of Success'
  )

# boxplot shoiwng rating of success in driver 3, male and female condition
ggplot(driverdata, aes(x = Group %in% c('FA', 'FP'), y = D3.success)) +
  geom_boxplot() +
  scale_x_discrete(labels = c('MA/MP', 'FA/FP')) +
  labs(
    x = 'Driver Group',
    y = 'Rating of Success'
  )


data_long <- pivot_longer(driverdata,
                          cols = c(D1.skill, D2.skill, D3.skill),
                          names_to = "Driver",
                          values_to = "Skill")

# comparison of all driver's skill in section 1, compared accross gender



ggplot(data_long,
       aes(x = Group %in% c("FA","FP"),
           y = Skill,
           fill = Group %in% c("FA","FP"))) +
  geom_boxplot() +
  facet_wrap(~ Driver,
             labeller = as_labeller(c(
               'D1.skill' = 'Driver One - Experienced Midfielder',
               'D2.skill' = 'Driver Two - Promising Young Driver',
               'D3.skill' = 'Driver Three - Established Top Tier'
             ))) +
  scale_fill_manual(values = c("lightblue", "lightpink"),
                    labels = c("Male", "Female"),
                    name = "Driver Gender") +
  labs(x = "Driver Gender Condition",
       y = "Skill Rating",
       title = "Skill Ratings of Drivers in Section 2 Accross Gender") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


data_long2 <- pivot_longer(driverdata, 
                           cols = c(D1.success, D2.success, D3.success), 
                           names_to = 'Driver', 
                           values_to = 'Success')


ggplot(data_long2,
       aes(x = Group %in% c("FA","FP"),
           y = Success,
           fill = Group %in% c("FA","FP"))) +
  geom_boxplot() +
  facet_wrap(~ Driver,
             labeller = as_labeller(c(
               'D1.success' = 'Driver One - Experienced Midfielder',
               'D2.success' = 'Driver Two - Promising Young Driver',
               'D3.success' = 'Driver Three - Established Top Tier'
             ))) +
  scale_fill_manual(values = c("lightblue", "lightpink"),
                    labels = c("Male", "Female"),
                    name = "Driver Gender") +
  labs(x = "Driver Gender Condition",
       y = "Success Rating", 
       title = "Success Ratings of Drivers in Section 2 Accross Gender") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


#statistics#
driverdata$Gender <- ifelse(driverdata$Group %in% c('FA', 'FP'), 'Female', 'Male')

# Calculate average skill and success
driverdata$Avg_Skill <- (driverdata$D1.skill + driverdata$D2.skill + driverdata$D3.skill) / 3
driverdata$Avg_Success <- (driverdata$D1.success + driverdata$D2.success + driverdata$D3.success) / 3

# T-TEST FOR SKILL
t.test(Avg_Skill ~ Gender, data = driverdata)

# T-TEST FOR SUCCESS
t.test(Avg_Success ~ Gender, data = driverdata)

# Get SDs
aggregate(Avg_Skill ~ Gender, data = driverdata, FUN = sd)
aggregate(Avg_Success ~ Gender, data = driverdata, FUN = sd)

# Get sample sizes
table(driverdata$Gender)


###### MAIN PART #####

# ratings of responsability 

ggplot(driverdata,
       aes(x = Group,
           y = Responsible)) +
  geom_boxplot() +
  scale_x_discrete(labels = c(
    "FA" = "Female Active",
    "FP" = "Female Passive",
    "MA" = "Male Active",
    "MP" = "Male Passive"
  )) +
  labs(x = "Driver Gender and Grammatical Voice Condition",
       y = "Perceived Responsibility", 
       title = "Ratings of Responsability Accross Gender and Condition") +
  theme_minimal()


# ratings of control

ggplot(driverdata,
       aes(x = Group,
           y = Control)) +
  geom_boxplot() +
  scale_x_discrete(labels = c(
    "FA" = "Female Active",
    "FP" = "Female Passive",
    "MA" = "Male Active",
    "MP" = "Male Passive"
  )) +
  labs(x = "Driver Gender and Grammatical Voice Condition",
       y = "Perceived Control", 
       title = "Ratings of Control Accross Gender and Condition") +
  theme_minimal()




# ratings of deliberateness

ggplot(driverdata,
       aes(x = Group,
           y = Deliberate)) +
  geom_boxplot() +
  scale_x_discrete(labels = c(
    "FA" = "Female Active",
    "FP" = "Female Passive",
    "MA" = "Male Active",
    "MP" = "Male Passive"
  )) +
  labs(x = "Driver Gender and Grammatical Voice Condition",
       y = "Perceived Responsibility", 
       title = "Ratings of Deliberateness Accross Gender and Condition") +
  theme_minimal()



# ratings of negligence 

ggplot(driverdata,
       aes(x = Group,
           y = Negligent)) +
  geom_boxplot() +
  scale_x_discrete(labels = c(
    "FA" = "Female Active",
    "FP" = "Female Passive",
    "MA" = "Male Active",
    "MP" = "Male Passive"
  )) +
  labs(x = "Driver Gender and Grammatical Voice Condition",
       y = "Perceived Responsibility", 
       title = "Ratings of Negligence Accross Gender and Condition") +
  theme_minimal()



#ratings of penalty 

ggplot(driverdata,
       aes(x = Group,
           y = Penalty)) +
  geom_boxplot() +
  scale_x_discrete(labels = c(
    "FA" = "Female Active",
    "FP" = "Female Passive",
    "MA" = "Male Active",
    "MP" = "Male Passive"
  )) +
  labs(x = "Driver Gender and Grammatical Voice Condition",
       y = "Perceived Responsibility", 
       title = "How Serious A Penalty Should Be Accross Gender and Condition") +
  theme_minimal()

############ STATISTICS FOR GENDER AND VOICE ###########

# ============================================
# SECTION 3: SIMPLE STATISTICAL ANALYSIS
# (Using same approach as Section 2)
# ============================================

# Make sure Voice and Gender variables exist
driverdata$Voice <- ifelse(driverdata$Group %in% c('FA', 'MA'), 'Active', 'Passive')
driverdata$Gender <- ifelse(driverdata$Group %in% c('FA', 'FP'), 'Female', 'Male')


# 1. RESPONSIBILITY


# Descriptive statistics
cat("Means by condition:\n")
aggregate(Responsible ~ Voice + Gender, data = driverdata, FUN = mean)

cat("\nStandard deviations:\n")
aggregate(Responsible ~ Voice + Gender, data = driverdata, FUN = sd)

cat("\nSample sizes:\n")
aggregate(Responsible ~ Voice + Gender, data = driverdata, FUN = length)

# ANOVA
cat("\n2×2 ANOVA:\n")
model_resp <- aov(Responsible ~ Voice * Gender, data = driverdata)
summary(model_resp)


# ============================================
# 2. CONTROL
# ============================================

cat("\n\n========== CONTROL ==========\n")

cat("Means by condition:\n")
aggregate(Control ~ Voice + Gender, data = driverdata, FUN = mean)

cat("\nStandard deviations:\n")
aggregate(Control ~ Voice + Gender, data = driverdata, FUN = sd)

cat("\n2×2 ANOVA:\n")
model_cont <- aov(Control ~ Voice * Gender, data = driverdata)
summary(model_cont)


# ============================================
# 3. DELIBERATENESS
# ============================================

cat("\n\n========== DELIBERATENESS ==========\n")

cat("Means by condition:\n")
aggregate(Deliberate ~ Voice + Gender, data = driverdata, FUN = mean)

cat("\nStandard deviations:\n")
aggregate(Deliberate ~ Voice + Gender, data = driverdata, FUN = sd)

cat("\n2×2 ANOVA:\n")
model_delib <- aov(Deliberate ~ Voice * Gender, data = driverdata)
summary(model_delib)


# ============================================
# 4. NEGLIGENCE
# ============================================

cat("\n\n========== NEGLIGENCE ==========\n")

cat("Means by condition:\n")
aggregate(Negligent ~ Voice + Gender, data = driverdata, FUN = mean)

cat("\nStandard deviations:\n")
aggregate(Negligent ~ Voice + Gender, data = driverdata, FUN = sd)

cat("\n2×2 ANOVA:\n")
model_neg <- aov(Negligent ~ Voice * Gender, data = driverdata)
summary(model_neg)


# ============================================
# 5. PENALTY
# ============================================

cat("\n\n========== PENALTY ==========\n")

cat("Means by condition:\n")
aggregate(Penalty ~ Voice + Gender, data = driverdata, FUN = mean)

cat("\nStandard deviations:\n")
aggregate(Penalty ~ Voice + Gender, data = driverdata, FUN = sd)

cat("\n2×2 ANOVA:\n")
model_pen <- aov(Penalty ~ Voice * Gender, data = driverdata)
summary(model_pen)


# ============================================
# SIMPLE EFFECTS (If you find significant interactions)
# ============================================

# For RESPONSIBILITY - if interaction is significant
# Test voice effect separately for each gender

cat("\n\n========== SIMPLE EFFECTS FOR RESPONSIBILITY ==========\n")

# For Female drivers only
female_data <- subset(driverdata, Gender == 'Female')
cat("\nFemale drivers - Active vs Passive:\n")
t.test(Responsible ~ Voice, data = female_data)

# For Male drivers only
male_data <- subset(driverdata, Gender == 'Male')
cat("\nMale drivers - Active vs Passive:\n")
t.test(Responsible ~ Voice, data = male_data)


# Repeat for PENALTY if needed
cat("\n\n========== SIMPLE EFFECTS FOR PENALTY ==========\n")

cat("\nFemale drivers - Active vs Passive:\n")
t.test(Penalty ~ Voice, data = female_data)

cat("\nMale drivers - Active vs Passive:\n")
t.test(Penalty ~ Voice, data = male_data)


################ GENDER IN PARTICIPANT GROUPS ###################
table(driverdata$Gender, driverdata$Group)

ggplot(driverdata, aes(x = Age, fill = Group)) +
  geom_bar() +
  scale_fill_manual() +
  labs(
    title = 'Distribution of Age by Participant Group', 
    x = 'Age Group',
    y = 'Frequency'
  ) +
  theme_minimal()

