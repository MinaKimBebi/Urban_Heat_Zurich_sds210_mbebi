#if not already done, set your working directory to the project folder
#setwd("xxx/Urban_Heat_Zurich_sds210_mbebi")
#load required libraries
library(tidyverse)
library(ggfortify)

#load data
NL = read_csv("data/processed/NDVI_LST.csv")

## exclude waterbodies from analysis:
filtered = filter(NL, NDVI >= 0)

# Analysis of significance for 2.1
## fit a simple linear model
fit = lm(LST ~ NDVI, data = filtered)

## control model assumptions
png(file="outputs/2_3_model_assumptions_simple_model.png")
autoplot(fit)
dev.off()
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
#center time
centered = filtered |>mutate(time_cent = time - mean(time))

## fit a linear model, with the year and interaction included
fit_year = lm(LST ~ NDVI * time, data = centered)

## control model assumptions
png(file="outputs/2_3_model_assumptions_with_time.png")
autoplot(fit_year)# similar patterns as in analysis for 2.1
dev.off()

## assess model fit
fit_year_summary = summary(fit_year)
fit_year_assessment = anova(fit_year)

print(paste("the p-value for the interaction is:", round(fit_year_assessment["NDVI:time", "Pr(>F)"], 2)))
print(paste("effect size of the interaction:", round(coef(fit_year)["NDVI:time"], 2),"°C / year"))
### there is siginficant interaction (but again p-value is to be used cautiosly), 
### --> time doesn't have the same effect at every NDVI value, 
### the effect is however very small (a shift of 0.01 in the slope). 

