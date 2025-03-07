# API 203: TF Session 1
# N.M. Kavanagh
# March 7, 2025

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
elections <- subset(elections, year %in% c(2012, 2016, 2020))

# Subset to Republicans
elections <- subset(elections, party=="REPUBLICAN")

# Sum all forms of vote
# Take one value for total votes
republican <- elections %>%
  group_by(county_fips, year) %>%
  summarise(rep_votes = sum(candidatevotes),
            all_votes = first(totalvotes),
            state     = first(state_po))

# Compute percent support
republican$pc_rep <- republican$rep_votes/republican$all_votes*100

# Get ACS data on county-level characteristics
acs_12 <- get_acs(geography = "county",
                    variables = c("DP05_0022E", "DP03_0062",
                                  "DP03_0099P", "DP03_0009P"),
                    year      = 2012,
                    survey    = "acs5")
acs_16 <- get_acs(geography = "county",
                  variables = c("DP05_0022E", "DP03_0062",
                                "DP03_0099P", "DP03_0009P"),
                  year      = 2016,
                  survey    = "acs5")
acs_20 <- get_acs(geography = "county",
                  variables = c("DP05_0021E", "DP03_0062",
                                "DP03_0099P", "DP03_0009P"),
                  year      = 2020,
                  survey    = "acs5")

# Assign years
acs_12$year <- 2012
acs_16$year <- 2016
acs_20$year <- 2020

# Merge years together
acs <- rbind(acs_12, acs_16)
acs <- rbind(acs,    acs_20)

# Treat FIPS as numeric
acs$county_fips <- as.numeric(acs$GEOID)

# Rename variables
acs <- acs %>% mutate(
  variable = case_when(
    variable %in% c("DP05_0021", "DP05_0022") ~ "pop_over_18",
    variable == "DP03_0062"  ~ "median_income",
    variable == "DP03_0099P" ~ "pc_uninsured",
    variable == "DP03_0009P" ~ "unemploy_rate"
  ))

# Convert to wide dataset
acs_wide <- acs %>% select(county_fips, year, variable, estimate) %>%
  pivot_wider(names_from = c(variable), values_from = estimate)
View(acs_wide)

# Merge datasets
df <- left_join(republican, acs_wide, by=c("county_fips", "year"))

# Eliminate counties with missing data
df <- df[complete.cases(df), ]

# Estimate turnout
df$turnout <- df$all_votes/df$pop_over_18*100

# Scale median income by $1000
df$med_inc_000s <- df$median_income/1000

# Export dataframe
write.csv(df, "Sample dataset.csv")

##############################################################################
# Graphical exploration
##############################################################################

# Estimate between-county regressions
model_1 <- feols(pc_rep ~ turnout | 0, df)
summary(model_1)
model_2 <- feols(pc_rep ~ turnout + med_inc_000s + unemploy_rate | 0, df)
summary(model_2)

# Estimate within-county regressions
model_3 <- feols(pc_rep ~ turnout | county_fips, df)
summary(model_3)
model_4 <- feols(pc_rep ~ turnout + med_inc_000s + unemploy_rate | county_fips, df)
summary(model_4)

# Estimate residual regressions
resid_x <- feols(pc_rep  ~ 1 | county_fips, df)
resid_y <- feols(turnout ~ 1 | county_fips, df)
df$resid_x <- residuals(resid_x)
df$resid_y <- residuals(resid_y)

# Compile results into tables
modelsummary(list("Model 1" = model_1,
                  "Model 2" = model_2,
                  "Model 3" = model_3,
                  "Model 4" = model_4),
             gof_omit    = "Log*|AIC|BIC|F|RMSE|Std",
             coef_rename = c("turnout"       = "County turnout",
                             "med_inc_000s"  = "County median income ($1000s)",
                             "unemploy_rate" = "County unemployment rate"),
             statistic   = c("({std.error})"),
             stars       = TRUE,
             fmt         = fmt_statistic("estimate" = 2, "std.error" = 2))

# Graph pooled turnout and percent Republican
plot_1 <- ggplot() +
  
  # Add scatterplot points
  geom_point(data=df, aes(x=turnout, y=pc_rep), alpha=0.15) +
  
  # Labels of axes
  xlab("County turnout\n") +
  ylab("\nCounty support for Republicans") +
  
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
ggsave(plot=plot_1, file="Example graph 1.pdf",
       width=4, height=3.75, units='in', dpi=600)

# Graph residualized turnout and percent Republican
plot_2 <- ggplot() +
  
  # Add scatterplot points
  geom_point(data=df, aes(x=resid_x, y=resid_y), alpha=0.15) +
  
  # Labels of axes
  xlab("County turnout,\nwith county fixed effects") +
  ylab("County support for Republicans\nwith county fixed effects") +
  
  # Cosmetic changes
  theme_light() +
  theme(text = element_text(face="bold"),
        legend.position = "none") +
  scale_y_continuous(limits=c(-20,20), labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits=c(-20,20), labels = function(x) paste0(x,"%")) +
  
  # Add line of best fit
  geom_smooth(data=df, aes(x=resid_x, y=resid_y),
              method="lm", se=F, formula = y~x)

# Combine panels
plot_comb_1 <- plot_grid(plot_1, plot_2)

# Export figure
ggsave(plot=plot_comb_1, file="Example graph 1.pdf",
       width=7.5, height=3.5, units='in', dpi=600)

##############################################################################
# Residualized regressions
##############################################################################

# Subset to a few counties
sub <- subset(df, county_fips %in% c(1001:1013))
sub$county_fips <- as.factor(sub$county_fips)

# Graph pooled turnout and percent Republican
plot_3 <- ggplot() +
  
  # Add scatterplot points
  geom_point(data=sub, aes(x=turnout, y=pc_rep,
                           group=county_fips, color=county_fips)) +
  
  # Labels of axes
  xlab("County turnout\n") +
  ylab("\nCounty support for Republicans") +
  
  # Cosmetic changes
  theme_light() +
  theme(text = element_text(face="bold"),
        legend.position = "none") +
  scale_y_continuous(limits=c(0,100), labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits=c(20,80), breaks=c(25,50,75),
                     labels = function(x) paste0(x,"%")) +
  
  # Add lines of best fit
  geom_smooth(data=sub, aes(x=turnout, y=pc_rep,
                            group=county_fips, color=county_fips),
              method="lm", se=F, formula = y~x, alpha=0.25) +
  geom_smooth(data=sub, aes(x=turnout, y=pc_rep),
              method="lm", se=F, formula = y~x)

# Graph residualized turnout and percent Republican
plot_4 <- ggplot() +
  
  # Add scatterplot points
  geom_point(data=sub, aes(x=resid_x, y=resid_y,
                           group=county_fips, color=county_fips)) +
  
  # Labels of axes
  xlab("County turnout,\nwith county fixed effects") +
  ylab("County support for Republicans\nwith county fixed effects") +
  
  # Cosmetic changes
  theme_light() +
  theme(text = element_text(face="bold"),
        legend.position = "none") +
  scale_y_continuous(limits=c(-5,5), labels = function(x) paste0(x,"%")) +
  scale_x_continuous(limits=c(-5,5), labels = function(x) paste0(x,"%")) +
  
  # Add line of best fit
  geom_smooth(data=sub, aes(x=resid_x, y=resid_y),
              method="lm", se=F, formula = y~x)

# Combine panels
plot_comb_2 <- plot_grid(plot_3, plot_4)

# Export figure
ggsave(plot=plot_comb_2, file="Example graph 2.pdf",
       width=7.5, height=3.5, units='in', dpi=600)
