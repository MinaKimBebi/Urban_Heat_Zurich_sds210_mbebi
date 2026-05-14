#load required libraries
library(tidyverse)
library(ggfortify)

# Analysis of significance for 2.1
##load data from 2024
NL_2024 = read_csv("data/NDVI_LST_2024.csv")

## exclude waterbodies from analysis:
filt_2024 = filter(NL_2024, NDVI >= 0)

## fit a simple linear model
fit_2024 = lm(LST ~ NDVI, data = filt_2024)
## control model assumptions
fit_2024_assumptions = autoplot(fit_2024)
show(fit_2024_assumptions)#this can take a while
### we see some deviations from normal behaviour,which is not unexpected,
### because the pixels are not independant (common with spatial data)
### however because of the high amount of data, the result is still interpretable

## assess model fit
fit_2024_summary = summary(fit_2024)
fit_2024_assessment = anova(fit_2024)

print(paste("Coefficients: intercept:", round(fit_2024$coefficients[1],1), "°C, slope:", round(fit_2024$coefficients[2],1), "°C/ NDVI unit"))
print(paste("NDVI explains", round(fit_2024_summary$r.squared * 100, 2), "% of the variation in LST"))
print(paste("the p-value is:", fit_2024_assessment["NDVI", "Pr(>F)"]))


# Analysis of significance of explainer "year" for 2.2
## load data from all years
NL_all = read_csv("data/NDVI_LST_all.csv")

## exclude waterbodies from analysis:
filt_all = filter(NL_all, NDVI >= 0)

## fit a simple linear model, with the year and interaction included
fit_year = lm(LST ~ NDVI * year, data = filt_all)

## control model assumptions
fit_year_assumptions = autoplot(fit_year)
show(fit_year_assumptions)# similar patterns as in analysis for 2.1


## assess model fit
fit_year_summary = summary(fit_year)
fit_year_assessment = anova(fit_year)

print(paste("the p-value for the year is:", fit_year_assessment["year", "Pr(>F)"]))
print(paste("the p-value for the interaction is:", fit_year_assessment["NDVI:year", "Pr(>F)"]))
#both not siginfcant(at all!)