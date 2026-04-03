# API 203: TF Session 4
# N.M. Kavanagh
# April 3, 2026

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

# Read in sample dataset
df <- read.csv("Sample dataset.csv")

##############################################################################
# Graph of Republican distribution
##############################################################################

# Plot distribution of Republican support
plot_1 <- ggplot(data=df, aes(x=pc_trump)) +
  geom_histogram() +
  xlab("Share of votes for Trump in 2020") +
  ylab("Count of counties") +
  theme_light() +
  theme(text = element_text(size = 10, face = "bold")) +
  scale_x_continuous(labels = function(x) paste0(x,"%"))

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
model_1 <- lm(pc_trump ~ med_inc_000s, data=df_train)
summary(model_1)

# In-sample prediction & MSE
df_train$predict <- predict(model_1, df_train)
df_train %>% summarise(mse = mean((pc_trump - predict)^2))

# Out-of-sample prediction & MSE
df_test$predict <- predict(model_1, df_test)
df_test %>% summarise(mse = mean((pc_trump - predict)^2))


# OLS regression
model_2 <- lm(pc_trump ~ med_inc_000s + pc_hs_grad + pc_male + pc_latin, data=df_train)
summary(model_2)

# In-sample prediction & MSE
df_train$predict_2 <- predict(model_2, df_train)
df_train %>% summarise(mse = mean((pc_trump - predict_2)^2))

# Out-of-sample prediction & MSE
df_test$predict_2 <- predict(model_2, df_test)
df_test %>% summarise(mse = mean((pc_trump - predict_2)^2))


# OLS regression
model_3 <- lm(pc_trump ~ med_inc_000s*pc_hs_grad*unemploy_rate*pc_uninsured*pc_under_18*pc_over_65*pc_male*pc_black*pc_latin, data=df_train)
summary(model_3)

# In-sample prediction & MSE
df_train$predict_3 <- predict(model_3, df_train)
df_train %>% summarise(mse = mean((pc_trump - predict_3)^2))

# Out-of-sample prediction & MSE
df_test$predict_3 <- predict(model_3, df_test)
df_test %>% summarise(mse = mean((pc_trump - predict_3)^2))

##############################################################################
# LASSO regression
##############################################################################

# Set the variables that are "fair game"
Y <- df_train$pc_trump
X <- data.matrix(df_train[, c("med_inc_000s", "pc_hs_grad", "unemploy_rate", "pc_uninsured",
                              "pc_under_18", "pc_over_65", "pc_male", "pc_black", "pc_latin")])

# LASSO regression
lasso <- cv.glmnet(x=X, y=Y)
plot(lasso); log(lasso$lambda.min)

# In-sample prediction & MSE
df_train$predict_4 <- predict(lasso, newx=X, s="lambda.min")[, 1]
df_train %>% summarise(mse = mean((pc_trump - predict_4)^2))

# Get X variables of test set
X_test <- data.matrix(df_test[, c("med_inc_000s", "pc_hs_grad", "unemploy_rate", "pc_uninsured",
                                  "pc_under_18", "pc_over_65", "pc_male", "pc_black", "pc_latin")])

# Out-of-sample prediction & MSE
df_test$predict_4 <- predict(lasso, newx=X_test, s="lambda.min")[, 1]
df_test %>% summarise(mse = mean((pc_trump - predict_4)^2))
