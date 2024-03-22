# API 203: TF Session 3
# N.M. Kavanagh
# March 22, 2024

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Load packages
library(here)         # Working directory
library(readstata13)  # Dataset tools
library(tidyverse)    # Analysis tools
library(fixest)       # Modeling tools
library(ggplot2)      # Graphing tools
library(statar)       # Graphing tools
library(modelsummary) # Table tools
library(tidycensus)   # Census tools

##############################################################################
# Dataset preparation
##############################################################################

# Read in 2019 CES dataset
df <- read.dta13("CCES19_Common_OUTPUT.dta")

# Age
df$age <- 2019 - df$birthyr

# Center age at 65
df$age_center <- df$age - 65

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

# Dichotomize age
df <- df %>% mutate(
  age_over65 = case_when(
    age > 65 ~ 1,
    age < 65 ~ 0))

# Select variables of interest
df <- df %>%
  select(caseid, commonweight, age, age_over65, gender, race_eth,
         education, marital, public_ins, public_option)

# Eliminate cases with missing data
df <- df[complete.cases(df), ]

# Export dataframe
write.csv(df, "Sample dataset.csv")

##############################################################################
# Graph of discontinuity
##############################################################################

# Generate RDD plot
plot_rd_1 <- ggplot() +
  
  # Threshold line
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
  # Binned scatterplots for each side of 65
  stat_binmean(n=1000, data=subset(df, age < 65),
               aes(x=age, y=public_option), size=1) +
  stat_binmean(n=1000, data=subset(df, age > 65 & age <= 90),
               aes(x=age, y=public_option), size=1) +
  
  # Global polynomial fit lines
  # Note: Limit to ≤90 years given sparse data over 90
  geom_smooth(data=subset(df, age < 65),
              aes(x=age, y=public_option), formula=y ~ poly(x, 4, raw=TRUE),
              method = "lm", se = F, color="red") +
  geom_smooth(data=subset(df, age > 65 & age <= 90),
              aes(x=age, y=public_option), formula=y ~ poly(x, 4, raw=TRUE),
              method = "lm", se = F, color="red") +
  
  # Cosmetic modifications
  xlab("Age") + ylab("Support for a public option in Medicare") +
  coord_cartesian(xlim=c(18,90), ylim=c(0,1)) +
  theme_light() +
  theme(text = element_text(size = 10, face = "bold")) +
  scale_x_continuous(breaks = seq(0, 100, 10)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2),
                     labels = function(x) paste0(x*100,"%"))

# Export figure
ggsave(plot=plot_rd_1, file="Example graph 1.pdf",
       width=4, height=3.5, units='in', dpi=600)

##############################################################################
# Regression exploration
##############################################################################

# OLS regression without controls
model_1 <- feols(public_option ~ age_over65 | 0, data=df, vcov="HC1")
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
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
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
  geom_vline(xintercept=THRESHOLD, linetype="dashed") +
  
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
