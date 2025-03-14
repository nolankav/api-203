# API 203: TF Session 2
# N.M. Kavanagh
# March 14, 2025

# Please direct questions about this script file to nolankavanagh@fas.harvard.edu.

# Clear R environment
rm(list = ls())

# Load packages
library(here)         # Working directory
library(dplyr)        # Analysis tools
library(tidyverse)    # Analysis tools
library(fixest)       # Modeling tools
library(ggplot2)      # Graphing tools
library(cowplot)      # Graphing tools
library(modelsummary) # Table tools
library(tidycensus)   # Census tools

##############################################################################
# Dataset preparation
##############################################################################

# Read in election dataset
# Source: MIT Election Lab
elections <- read.csv("countypres_2000-2020.csv")

# Subset to desired years
elections <- subset(elections, year %in% c(2016))

# Subset to Republicans
elections <- subset(elections, party=="REPUBLICAN")

# Sum all forms of vote
# Take one value for total votes
republican <- elections %>%
  group_by(county_fips) %>%
  summarise(rep_votes = sum(candidatevotes),
            all_votes = first(totalvotes),
            state     = first(state_po))

# Compute percent support
republican$pc_rep <- republican$rep_votes/republican$all_votes*100

# Get ACS data on county-level characteristics
acs <- get_acs(geography = "county",
               variables = c("DP05_0022E", "DP03_0062",
                             "DP03_0099P", "DP03_0009P"),
               year      = 2016,
               survey    = "acs5")

# Treat FIPS as numeric
acs$county_fips <- as.numeric(acs$GEOID)

# Rename variables
acs <- acs %>% mutate(
  variable = case_when(
    variable == "DP05_0022"  ~ "pop_over_18",
    variable == "DP03_0062"  ~ "median_income",
    variable == "DP03_0099P" ~ "pc_uninsured",
    variable == "DP03_0009P" ~ "unemploy_rate"
  ))

# Convert to wide dataset
acs_wide <- acs %>% select(county_fips, variable, estimate) %>%
  pivot_wider(names_from = c(variable), values_from = estimate)

# Merge datasets
df <- left_join(republican, acs_wide, by="county_fips")

# Estimate turnout
df$turnout <- df$all_votes/df$pop_over_18*100

# Scale median income by $1000
df$med_inc_000s <- df$median_income/1000

# Read in precipitation dataset
# Source: National Oceanic and Atmospheric Administration
rainfall <- read.csv("precipitation_october_2016.csv")

# Rename columns of interest
rainfall$rain_oct_16     <- as.numeric(rainfall$Value)
rainfall$rain_historical <- as.numeric(rainfall$X1901.2000.Mean)

# Reformat county IDs
# Parse ID into county FIPs variable
rainfall$county_num <- sapply(strsplit(rainfall$ID, "-"), "[[", 2)

# Convert state to FIPs
rainfall$state_fips <- cdlTools::fips(rainfall$State, to="FIPS")

# Combine state and county FIPs
rainfall$county_fips <- as.numeric(paste0(rainfall$state_fips, rainfall$county_num))

# Select desired columns
rainfall <- rainfall %>% select(county_fips, rain_oct_16, rain_historical)

# Merge dataframes
df <- left_join(df, rainfall, by="county_fips")

# Simulate rain on election day
set.seed(1234)
df <- df %>% mutate(
  rain_election = ifelse(
    turnout < median(turnout, na.rm=T),
    sample(c(0, 1), n(), replace = TRUE, prob = c(0.50, 0.50)),
    sample(c(0, 1), n(), replace = TRUE, prob = c(0.95, 0.05))
  ))

# Eliminate counties with missing data
df <- df[complete.cases(df), ]

# Export dataframe
write.csv(df, "Sample dataset.csv")

##############################################################################
# Naive graph and regression
##############################################################################

# Graph turnout and percent Republican
plot_1 <- ggplot() +
  
  # Add scatterplot points
  geom_point(data=df, aes(x=turnout, y=pc_rep), alpha=0.15) +
  
  # Labels of axes
  xlab("County turnout") +
  ylab("County support for Republicans") +
  
  # Cosmetic changes
  theme_light() +
  theme(text = element_text(face="bold"),
        legend.position = "none") +
  scale_y_continuous(limits=c(0,100),
                     labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits=c(0,100),
                     labels = function(x) paste0(x,"%")) +
  
  # Add line of best fit
  geom_smooth(data=df, aes(x=turnout, y=pc_rep),
              method="lm", se=F, formula = y~x)

# Export figure
ggsave(plot=plot_1, file="Example graph.pdf",
       width=4, height=3.75, units='in', dpi=600)

# Estimate naive regressions
model_1 <- feols(pc_rep ~ turnout | 0, df)
summary(model_1)
model_2 <- feols(pc_rep ~ turnout + med_inc_000s + unemploy_rate | 0, df)
summary(model_2)
model_3 <- feols(pc_rep ~ turnout + med_inc_000s + unemploy_rate
                 | state, df, se="iid")
summary(model_3)

##############################################################################
# Instrumental variables approach
##############################################################################

# Estimate first stage
model_fs <- feols(turnout ~ rain_election + rain_historical | 0, data=df)
summary(model_fs)

# Save predicted turnout
df$turnout_pred <- predict(model_fs)

# Estimate second stage
model_ss <- feols(pc_rep ~ turnout_pred + rain_historical | 0, data=df)
summary(model_ss)

# Estimate all-in-one IV model
# This includes a degree of freedom correction
model_iv <- feols(pc_rep ~ rain_historical | 0 | turnout ~ rain_election, data=df)
summary(model_iv)

# Estimate reduced form
reg_rf <- feols(pc_rep ~ rain_election + rain_historical | 0, data=df)
summary(reg_rf)
