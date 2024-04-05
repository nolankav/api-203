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
library(pROC)         # ROC tools
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
  select(caseid, commonweight, age, gender, race_eth, education, marital,
         employment, device, republican, public_ins, public_option)

# Eliminate cases with missing data
df <- df[complete.cases(df), ]

# Export dataframe
write.csv(df, "Sample dataset.csv")

##############################################################################
# Graph of parties
##############################################################################

# Generate bar plot of parties
plot_1 <- ggplot(data=df, aes(x=republican)) +
  geom_bar(stat="count") +
  xlab("Identifies as Republican") +
  ylab("Count of respondents") +
  theme_light() +
  theme(text = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(-3, 3, 1)) +
  scale_y_continuous(limits = c(0,14000),
                     breaks = seq(0,15000,2000))

# Export figure
ggsave(plot=plot_1, file="Example graph 1.pdf",
       width=2.25, height=3.5, units='in', dpi=600)

##############################################################################
# GLM regression exploration
##############################################################################

# Designate training and test sets
set.seed(1234)
split    <- initial_split(df, 0.7)
df_train <- training(split)
df_test  <- testing(split)

# Logit regression
model_1 <- glm(republican ~ age + gender + race_eth + education + marital +
                 employment + device, data=df_train, family="binomial")
summary(model_1)

# In-sample prediction
df_train$predict <- predict(model_1, df_train, type="response")

# Histogram of predictions
plot_2 <- ggplot(df_train, aes(x=predict)) +
  geom_histogram() +
  theme_light() +
  theme(text = element_text(size = 10, face = "bold")) +
  xlab("Predicted probability for Republican") +
  ylab("Count")
print(plot_2)

# Export figure
ggsave(plot=plot_2, file="Example graph 2.pdf",
       width=4, height=3.5, units='in', dpi=600)

# Set potential thresholds
df_train <- df_train %>% mutate(
  threshold_1 = case_when(
    predict > 0.4 ~ 1, TRUE ~ 0),
  threshold_2 = case_when(
    predict > 0.2 ~ 1, TRUE ~ 0
  ))

# Evaluate thresholds
table(df_train$republican, df_train$threshold_1)
table(df_train$republican, df_train$threshold_2)

# Generate ROC curve for training set
roc_train <- roc(df_train$republican, df_train$predict)
roc_train$auc

# Plot ROC curve
plot_3 <- ggroc(roc_train) +
  theme_light() +
  theme(text = element_text(size = 10, face = "bold")) +
  geom_abline(slope=1, intercept=1, linetype="dashed") +
  xlab("Specificity") +
  ylab("Sensitivity")
print(plot_3)

# Export figure
ggsave(plot=plot_3, file="Example graph 3.pdf",
       width=4, height=4, units='in', dpi=600)

# Out-of-sample prediction
df_test$predict <- predict(model_1, df_test, type="response")

# Set potential thresholds
df_test <- df_test %>% mutate(
  threshold_1 = case_when(
    predict > 0.4 ~ 1, TRUE ~ 0),
  threshold_2 = case_when(
    predict > 0.2 ~ 1, TRUE ~ 0
  ))

# Evaluate thresholds
table(df_test$republican, df_test$threshold_1)
table(df_test$republican, df_test$threshold_2)

# Generate ROC curve for test set
roc_test <- roc(df_test$republican, df_test$predict)
roc_test$auc

# Plot ROC curve
plot_4 <- ggroc(roc_test) +
  theme_light() +
  theme(text = element_text(size = 10, face = "bold")) +
  geom_abline(slope=1, intercept=1, linetype="dashed") +
  xlab("Specificity") +
  ylab("Sensitivity")
print(plot_4)

# Export figure
ggsave(plot=plot_4, file="Example graph 4.pdf",
       width=4, height=4, units='in', dpi=600)
