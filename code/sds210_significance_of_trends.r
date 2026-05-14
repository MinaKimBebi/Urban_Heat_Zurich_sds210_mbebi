# set your workindirectors to the data folder
setwd("data")
#load required libraries
library(tidyverse)
library(ggfortify)

#load data
NL = read_csv("NDVI_LST.csv")

## exclude waterbodies from analysis:
filtered = filter(NL, NDVI >= 0)

# Analysis of significance for 2.1
## fit a simple linear model
fit = lm(LST ~ NDVI, data = filtered)

## control model assumptions
fit_assumptions = autoplot(fit)
show(fit_assumptions)#this can take a while
### we see some deviations from normal behaviour,which is not unexpected,
### because the pixels are not independant (common with spatial data)
### p-values should therefore be treated cautiously

## assess model fit
fit_summary = summary(fit)
fit_assessment = anova(fit)

print(paste("Coefficients: intercept:", round(fit$coefficients[1],1), "°C, slope:", round(fit$coefficients[2],1), "°C/ NDVI unit"))
print(paste("NDVI explains", round(fit_summary$r.squared * 100, 2), "% of the variation in LST"))
print(paste("the p-value is:", fit_assessment["NDVI", "Pr(>F)"]))# not trustworthy! 

# Analysis of significance of explainer "year" for 2.2

## fit a linear model, with the year and interaction included
fit_year = lm(LST ~ NDVI * year, data = filtered)

## control model assumptions
fit_year_assumptions = autoplot(fit_year)
show(fit_year_assumptions)# similar patterns as in analysis for 2.1

## assess model fit
fit_year_summary = summary(fit_year)
fit_year_assessment = anova(fit_year)

print(paste("the p-value for the year is:", fit_year_assessment["year", "Pr(>F)"]))
print(paste("the p-value for the interaction is:", fit_year_assessment["NDVI:year", "Pr(>F)"]))
#both not siginfcant(at all!)