# API 203: TF Session 4
# N.M. Kavanagh
# March 29, 2024

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Install packages
# install.packages("glmnet")

# Load packages
library(here)         # Working directory
library(readstata13)  # Dataset tools
library(tidyverse)    # Analysis tools
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

# Party identification scale
df <- df %>% mutate(
  party_scale = case_when(
    pid7 == "Strong Democrat"            ~  3,
    pid7 == "Not very strong Democrat"   ~  2,
    pid7 == "Lean Democrat"              ~  1,
    pid7 == "Independent"                ~  0,
    pid7 == "Lean Republican"            ~ -1,
    pid7 == "Not very strong Republican" ~ -2,
    pid7 == "Strong Republican"          ~ -3))

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
ggsave(plot=plot_1, file="Example graph 1.pdf",
       width=3, height=3.5, units='in', dpi=600)

##############################################################################
# Regression exploration
##############################################################################

# OLS regression
model_1 <- feols(party_scale ~ age + gender + race_eth + education, data=df)
summary(model_1)

# OLS regression with controls
model_2 <- feols(public_option ~ age_over65 + gender + race_eth +
                   education + marital | 0, data=df, vcov="HC1")
summary(model_2)

# Regression discontinuity: Manual, linear
model_3 <- feols(public_option ~ age_over65 + age_center +
                   age_over65*age_center | 0, data=df, vcov="HC1")
summary(model_3)

# Save predictions
df <- mutate(df, predictions = model_3$fitted.values)

# Plot predictions against true values
plot_rd_2 <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=65, linetype="dashed") +
  
  # Binned scatterplots for each side of 65
  stat_binmean(n=1000, data=subset(df, age < 65),
               aes(x=age, y=public_option), size=1) +
  stat_binmean(n=1000, data=subset(df, age > 65 & age <= 90),
               aes(x=age, y=public_option), size=1) +
  
  # Predicted support
  geom_point(data=df, aes(x=age, y=predictions), color="red", size=1) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Support for a public option in Medicare") +
  coord_cartesian(xlim=c(18,90), ylim=c(0,1)) +
  theme_light() +
  theme(text = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Export figure
ggsave(plot=plot_rd_2, file="Example graph 2.pdf",
       width=4, height=3.5, units='in', dpi=600)

##############################################################################
# Advanced regression discontinuity: Quadratic
##############################################################################

# Square centered age variable
df$age_center_sq <- (df$age_center)^2

# Regression discontinuity: Manual, quadratic
model_4 <- feols(public_option ~ age_over65 + age_center + age_center_sq +
                   age_over65*age_center + age_over65*age_center_sq |
                   0, data=df, vcov="HC1")
summary(model_4)

# Save predictions
df <- mutate(df, predictions_sq = model_4$fitted.values)

# Plot predictions against true values
plot_rd_3 <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=65, linetype="dashed") +
  
  # Binned scatterplots for each side of 65
  stat_binmean(n=1000, data=subset(df, age < 65),
               aes(x=age, y=public_option), size=1) +
  stat_binmean(n=1000, data=subset(df, age > 65 & age <= 90),
               aes(x=age, y=public_option), size=1) +
  
  # Predicted support
  geom_point(data=df, aes(x=age, y=predictions_sq),
             color="red", size=1) +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Support for a public option in Medicare") +
  coord_cartesian(xlim=c(18,90), ylim=c(0,1)) +
  theme_light() +
  theme(text = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Export figure
ggsave(plot=plot_rd_3, file="Example graph 3.pdf",
       width=4, height=3.5, units='in', dpi=600)

##############################################################################
# Advanced regression discontinuity: Local linear regression
##############################################################################

# Load library
library(rdrobust)

# Regression discontinuity: Data-driven bandwidths
model_5 <- rdrobust(df$public_option, df$age, c=65,
                    p=1, kernel="triangular")
summary(model_5)
