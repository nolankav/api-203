# API 203: TF Session 5
# N.M. Kavanagh
# April 5, 2024

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Install packages
# install.packages("glmnet")
# install.packages("rsample")

# Load packages
library(here)         # Working directory
library(readstata13)  # Dataset tools
library(tidyverse)    # Analysis tools
library(rsample)      # Analysis tools
library(fixest)       # Modeling tools
library(glmnet)       # Modeling tools
library(ggplot2)      # Graphing tools
library(modelsummary) # Table tools

##############################################################################
# Dataset preparation
##############################################################################

# Read in 2019 CES dataset
df <- read.dta13("CCES19_Common_OUTPUT.dta")

# Age
df$age <- 2019 - df$birthyr

# Gender
df <- df %>% mutate(
  gender_cl = case_when(
    gender == "Male"   ~ "Man",
    gender == "Female" ~ "Woman"))

# Race/ethnicity
df <- df %>% mutate(
  race_eth = case_when(
    race %in% c("White")                            ~ "White",
    race %in% c("Black")                            ~ "Black",
    race %in% c("Hispanic")                         ~ "Hispanic/Latino",
    race %in% c("Asian")                            ~ "Asian",
    race %in% c("Native American")                  ~ "Native American",
    race %in% c("Mixed", "Other", "Middle Eastern") ~ "Other"))

# Educational attainment
df <- df %>% mutate(
  education = case_when(
    educ %in% c("No HS")                  ~ "Less than high school",
    educ %in% c("High school graduate")   ~ "High school graduate",
    educ %in% c("Some college", "2-year") ~ "Some college or 2-year degree",
    educ %in% c("4-year", "Post-grad")    ~ "College graduate or higher"))

# Marital status
# Note: Includes civil partnership with never married
# Consistent with coding of American Community Survey
df <- df %>% mutate(
  marital = case_when(
    marstat %in% c("Married")   ~ "Married",
    marstat %in% c("Separated") ~ "Separated",
    marstat %in% c("Divorced")  ~ "Divorced",
    marstat %in% c("Widowed")   ~ "Widowed",
    marstat %in% c("Never married", "Domestic / civil partnership") ~ "Never married"))

# Employment status
df <- df %>% mutate(
  employment = case_when(
    employ %in% c("Full-time", "Part-time")             ~ "Employed",
    employ %in% c("Temporarily laid off", "Unemployed") ~ "Unemployed",
    employ %in% c("Retired")                            ~ "Retired",
    employ %in% c("Permanently disabled")               ~ "Disabled",
    employ %in% c("Homemaker", "Student", "Other")      ~ "Other"
  ))

# Survey device
df <- df %>% mutate(
  device = case_when(
    comptype == "I am taking this survey on a smart phone (e.g., iPhone or Android phone)" ~ "Smartphone",
    comptype == "I am taking this survey on a tablet (e.g., iPad)" ~ "Tablet",
    comptype == "I am taking this survey on a desktop computer or laptop computer" ~ "Computer"))

# Party identification
df <- df %>% mutate(
  republican = case_when(
    pid3 == "Republican" ~ 1,
    !is.na(pid3)         ~ 0
  ))

# Public health insurance
df <- df %>% mutate(
  public_ins = case_when(
    healthins_2 == "selected" ~ 1,
    TRUE ~ 0))

# Support for public option in Medicare
df <- df %>% mutate(
  public_option = case_when(
    CC19_327b == "Support" ~ 1,
    CC19_327b == "Oppose"  ~ 0))

# Select variables of interest
df <- df %>%
  select(caseid, commonweight, age, gender, race_eth, education,
         marital, party_scale, public_ins, public_option)

# Eliminate cases with missing data
df <- df[complete.cases(df), ]

# Export dataframe
write.csv(df, "Sample dataset.csv")

##############################################################################
# Graph of party scale
##############################################################################

# Generate bar plot of parties
plot_1 <- ggplot(data=df, aes(x=party_scale)) +
  geom_bar(stat="count") +
  xlab("Party identification scale\n(Strong Rep. to strong Dem.)") +
  ylab("Count of respondents") +
  theme_light() +
  theme(text = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0,5000),
                     breaks = seq(0,5000,1000))

# Export figure
ggsave(plot=plot_1, file="Example graph.pdf",
       width=3, height=3.5, units='in', dpi=600)

##############################################################################
# OLS regression exploration
##############################################################################

# Designate training and test sets
set.seed(1234)
split    <- initial_split(df, 0.7)
df_train <- training(split)
df_test  <- testing(split)

# OLS regression
model_1 <- lm(party_scale ~ gender, data=df_train)
summary(model_1)

# In-sample prediction & MSE
df_train$predict <- predict(model_1, df_train)
df_train %>% summarise(mse = mean((party_scale - predict)^2))

# Out-of-sample prediction & MSE
df_test$predict <- predict(model_1, df_test)
df_test %>% summarise(mse = mean((party_scale - predict)^2))


# OLS regression
model_2 <- lm(party_scale ~ age + gender + race_eth + education, data=df_train)
summary(model_2)

# In-sample prediction & MSE
df_train$predict_2 <- predict(model_2, df_train)
df_train %>% summarise(mse = mean((party_scale - predict_2)^2))

# Out-of-sample prediction & MSE
df_test$predict_2 <- predict(model_2, df_test)
df_test %>% summarise(mse = mean((party_scale - predict_2)^2))


# OLS regression
model_3 <- lm(party_scale ~ age*gender*race_eth*education*marital*public_ins*public_option, data=df_train)
summary(model_3)

# In-sample prediction & MSE
df_train$predict_3 <- predict(model_3, df_train)
df_train %>% summarise(mse = mean((party_scale - predict_3)^2))

# Out-of-sample prediction & MSE
df_test$predict_3 <- predict(model_3, df_test)
df_test %>% summarise(mse = mean((party_scale - predict_3)^2))

##############################################################################
# LASSO regression
##############################################################################

# Set the variables that are "fair game"
Y <- df_train$party_scale
X <- data.matrix(df_train[, c("age", "gender", "race_eth", "education",
                              "marital", "public_ins", "public_option")])

# LASSO regression
lasso <- cv.glmnet(x=X, y=Y)
plot(lasso); log(lasso$lambda.min)

# In-sample prediction & MSE
df_train$predict_4 <- predict(lasso, newx=X, s="lambda.min")[, 1]
df_train %>% summarise(mse = mean((party_scale - predict_4)^2))

# Get X variables of test set
X_test <- data.matrix(df_test[, c("age", "gender", "race_eth", "education",
                                  "marital", "public_ins", "public_option")])

# Out-of-sample prediction & MSE
df_test$predict_4 <- predict(lasso, newx=X_test, s="lambda.min")[, 1]
df_test %>% summarise(mse = mean((party_scale - predict_4)^2))
