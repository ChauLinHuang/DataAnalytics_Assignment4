# DA_Assignment_4.R
# 
# Charly Huang
# Oct 21, 2020

# Remove previous remnant outcomes
rm(list = ls())

# Import libraries
if (!require('readxl')) install.packages('readxl')
if (!require('nortest')) install.packages('nortest')
library(readxl)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(nortest)

# Import datasets
data_xls <- './datasets/rollingsales_manhattan_fixed.xls'
houseRent_df <- read_excel(data_xls)

# Reassign header to dataframe
colnames(houseRent_df) <- houseRent_df[4,]
houseRent_df <- houseRent_df[-c(1:4),]

# # Data cleaning. Drop columns: BOROUGH, BLOCK, LOT, EASEMENT, SALE DATE
houseRent_df.cleaned <- houseRent_df[, c(-1, -5, -6, -8, -21) ]
str(houseRent_df.cleaned)

houseRent_df.cleaned$`SALE PRICE` <- as.numeric(houseRent_df.cleaned$`SALE PRICE`)

# # EDA
# ## NEIGHBORHOOD vs SALE PRICE
neighborhood_salePrice_bar <- houseRent_df.cleaned %>% ggplot(aes(x = NEIGHBORHOOD, y = `SALE PRICE`)) +
  geom_bar(stat = 'identity')
neighborhood_salePrice_bar + ggtitle('Neighborhood vs Sale price')

## ADDRESS vs SALE PRICE
# houseRent_df.cleaned %>% ggplot(aes(x = ADDRESS, y = `SALE PRICE`)) + geom_point()

## YEAR BUILT vs SALE PRICE
builtYear_vs_SalePrice_plot <- houseRent_df %>% ggplot(aes(x = `YEAR BUILT`, y = `SALE PRICE`)) + geom_point(na.rm = TRUE)
builtYear_vs_SalePrice_plot + ggtitle('Year built vs Sale price')

## BUILDING CLASS CATEGORY vs SALE PRICE
buildClassCat_SalePrice_bar <- houseRent_df.cleaned %>% ggplot(aes(x = `BUILDING CLASS CATEGORY`, y = `SALE PRICE`)) + geom_bar( stat = 'identity'  )
buildClassCat_SalePrice_bar + ggtitle('Building class category vs Sales price')

## ADDRESS vs YEAR BUILT
## TODO

# Model fiting. SALE PRICE as dependent variable and have NEIGHBORHOOD, BUILD CLASS CATEGORY, YEAR BUILT
# as the independent variables.
## multivariate regression
houseRent_df.cleaned.lm <- lm( `SALE PRICE`~ NEIGHBORHOOD + `BUILDING CLASS CATEGORY` + `YEAR BUILT`, data = houseRent_df.cleaned)

summary(houseRent_df.cleaned.lm)


salePrice_lm_pred <- predict(houseRent_df.cleaned.lm, interval = 'confidence')

lm_pred_plot <- houseRent_df.cleaned %>% ggplot(aes(x = NEIGHBORHOOD, y = `SALE PRICE`)) +
  geom_bar(stat = 'identity') + stat_smooth(method = lm) 
lm_pred_plot + ggtitle('Prediction SALE PRICE ~ \nNEIGHBORHOOD, BUILDING\n CLASS CATEGORY, YEAR BUILT')

lm_pred_sale_price_on_year_plot <- houseRent_df.cleaned %>% ggplot(aes(x = `YEAR BUILT`, y = `SALE PRICE`)) +
    geom_point() + stat_smooth(method = lm) 
lm_pred_sale_price_on_year_plot + ggtitle('Prediction of sale price Year built')

# Significance test
houseRent_df.cleaned <- houseRent_df.cleaned %>% filter(`SALE PRICE` > 0)
adTest_sale_price <- ad.test(houseRent_df.cleaned$`SALE PRICE`)
adTest_sale_price


# 