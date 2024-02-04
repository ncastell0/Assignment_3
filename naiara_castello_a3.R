library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(car)
library(psych)
library(ggthemes)
library(summarytools)
library(apaTables)
share_data <- read.csv("share_selected.csv")

# Explore relationships
summary(share_data)

freq(share_data$sex,
     report.nas = TRUE)

freq(share_data$lives_with_partner,
     report.nas = TRUE)

freq(share_data$retired,
     report.nas = TRUE)

freq(share_data$books_age_10,
     report.nas = TRUE)


## Numeric
correlations <- share_data |>
  select(where(is.numeric)) |>
  cor(use = "pairwise.complete.obs")

share_data |>
  select(where(is.numeric)) |>
  cor(use = "pairwise.complete.obs") |>
  ggcorrplot()

apa.cor.table(
  share_data,
  filename = "corr_table",
  show.conf.interval = TRUE,
  show.sig.stars = TRUE,
  landscape = TRUE
)

na_counts <- colSums(is.na(share_data))
na_counts

##Categorical
ggplot(share_data, aes(x = sex, y = household_income)) +
  geom_boxplot()

ggplot(share_data, aes(x = household_income, color = sex, fill = sex)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Household Income and Sex",
    x = "Household income (SEK/Month)", y = "Density")

ggplot(share_data, aes(x = household_income, color = lives_with_partner, fill = lives_with_partner)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Household Income and Living with Partner",
    x = "Household income (SEK/Month)", y = "Density")

ggplot(share_data, aes(x = household_income, color = retired, fill = retired)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Household Income and Retirement",
    x = "Household income (SEK/Month)", y = "Density")

ggplot(share_data, aes(x = household_income, color = books_age_10, fill = books_age_10)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Household Income and Books at Age 10",
    x = "Household income (SEK/Month)", y = "Density")

ggplot(share_data, aes(x = household_income, color = relative_math_ability_at_age_10, fill = relative_math_ability_at_age_10)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Household Income and Math Ability at Age 10",
    x = "Household income (SEK/Month)", y = "Density")

ggplot(share_data, aes(x = household_income, color = relative_language_ability_at_age_10, fill = relative_language_ability_at_age_10)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Household Income and Language Ability at Age 10",
    x = "Household income (SEK/Month)", y = "Density")

ggplot(share_data, aes(x = n_household, color = lives_with_partner, fill = lives_with_partner)) +
  geom_density(alpha = 0.5) +
  labs(
    title = "Number of People in the Household and Living with Partner",
    x = "Number of People in the Household", y = "Density")

ggplot(
  data = share_data, 
  mapping = aes(x = n_household, y = household_income)) +
  geom_point(mapping = aes(color = lives_with_partner)) +
  geom_smooth(method = "lm")

ggplot(
  data = share_data, 
  mapping = aes(x = age, y = household_income)) +
  geom_point(mapping = aes(color = retired)) +
  geom_smooth(method = "lm")

share_data <- share_data |> select( - lives_with_partner, - age_partner)

# Build a regression model using backward selection
## All predictors included
model_all <- share_data |>
  lm(formula = household_income ~ .)
summary(model_all)
vif(model_all)

## Select model
model_allminus1 <- share_data |>
  lm(formula = household_income ~ . - age)
summary(model_allminus1)

model_allminus2 <- share_data |>
  lm(formula = household_income ~ . - age - years_of_education)
summary(model_allminus2)

model_allminus3 <- share_data |>
  lm(formula = household_income ~ . - age - years_of_education - sex)
summary(model_allminus3)

model_allminus4 <- share_data |>
  lm(formula = household_income ~ . - age - years_of_education - sex - relative_math_ability_at_age_10)
summary(model_allminus4)

model_final <- share_data |>
  lm(formula = household_income ~ n_household + retired + books_age_10 + relative_language_ability_at_age_10)
summary(model_final)

vif_values <- vif(model_final)
vif_data <- data.frame(
  variable = names(vif_values),
  vif = vif_values)

ggplot(vif_data, aes(x = variable, y = vif)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  geom_hline(yintercept = 1.00, linetype = "dashed", color = "red") +
  labs(
    title = "Variance Inflation Factor (VIF)",
    x = "Predictor Variables",
    y = "VIF Values")
