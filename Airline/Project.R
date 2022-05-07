# X <- cbind(CASM_Pilot, CASM_Maintenance, CASM_Pilot2, CASM_Maintenance2, I(CASM_Pilot*CASM_Maintenance), CASM_Pilot_change, CASM_Maintenance_change)
rm(list = ls())
# Panel Data Models in R
options(scipen = 99)
library(plm)
library(car)

mydata<- read.csv("G://My Drive/Airline/df_covid.csv")
head(mydata, 3)
# Set data as panel data
pdata <- pdata.frame(mydata, index=c("airline","year"))

# Pooled OLS estimator
pooling <- plm(pc_delay ~ log(CASM_Pilot) + log(CASM_Maintenance), data=pdata, model= "pooling")
summary(pooling)


lmtest::bptest(pooling)
# Null - Same intercept. No heteroscedasticity
# Alternate - Difference intercept.heteroscedasticity present


# LM test for random effects versus OLS # 
plmtest(pooling)
# NUll - heterogeneity among individuals may not be significant
# Alternate - heterogeneity among individuals may be significant

# Random effects estimator
random <- plm(pc_delay ~ log(CASM_Pilot) + log(CASM_Maintenance), data=pdata, model= "random", random.method = "ht", inst.method = "baltagi")
summary(random)

# Fixed effects or within estimator
fixed <- plm(pc_delay ~ log(CASM_Pilot) + log(CASM_Maintenance), data=pdata, model= "within")
summary(fixed)

# Hausman test for fixed versus random effects model
phtest(random, fixed)
# NUll - RE provides consistent estimators, no correlation between unique errors and regressors
# Alter - Fixed provides consistent estimators, correlation between unique errors and regressors


# LM test for fixed effects versus OLS
pFtest(fixed, pooling)
# Null - No fixed effect. OLS is better
# Alternate - Fixed effect is better
# Reject null hypothesis that entity level effect are not all zero

###########################################
# Pooled OLS estimator
pooling <- plm(pc_delay ~ log(CASM_Pilot) + log(CASM_Maintenance) + CASM_Pilot2 + CASM_Maintenance2, data=pdata, model= "pooling")
summary(pooling)


lmtest::bptest(pooling)
# Null - Same intercept. No heteroscedasticity
# Alternate - Difference intercept.heteroscedasticity present


# LM test for random effects versus OLS # 
plmtest(pooling)
# NUll - heterogeneity among individuals may not be significant
# Alternate - heterogeneity among individuals may be significant

# Random effects estimator
random <- plm(pc_delay ~ log(CASM_Pilot) + log(CASM_Maintenance) + CASM_Pilot2 + CASM_Maintenance2, data=pdata, model= "random", random.method = "ht", inst.method = "baltagi")
summary(random)

# Fixed effects or within estimator
fixed <- plm(pc_delay ~ log(CASM_Pilot) + log(CASM_Maintenance) + CASM_Pilot2 + CASM_Maintenance2, data=pdata, model= "within")
summary(fixed)

# Hausman test for fixed versus random effects model
phtest(random, fixed)
# NUll - RE provides consistent estimators, no correlation between unique errors and regressors
# Alter - Fixed provides consistent estimators, correlation between unique errors and regressors


# LM test for fixed effects versus OLS
pFtest(fixed, pooling)
# Null - No fixed effect. OLS is better
# Alternate - Fixed effect is better
# Reject null hypothesis that entity level effect are not all zero



########################################################################
########################################################################
########################################################################
mydata<- read.csv("G://My Drive/Airline/df_pre_covid.csv")
head(mydata, 3)
# Set data as panel data
pdata <- pdata.frame(mydata, index=c("airline","year"))

# Pooled OLS estimator
pooling <- plm(pc_delay ~ log(CASM_Pilot) + log(CASM_Maintenance), data=pdata, model= "pooling")
summary(pooling)


lmtest::bptest(pooling)
# Null - Same intercept. No heteroscedasticity
# Alternate - Difference intercept.heteroscedasticity present


# LM test for random effects versus OLS # 
plmtest(pooling)
# NUll - heterogeneity among individuals may not be significant
# Alternate - heterogeneity among individuals may be significant

# Random effects estimator
random <- plm(pc_delay ~ CASM_Pilot + CASM_Maintenance, data=pdata, model= "random", random.method = "ht", inst.method = "baltagi")
summary(random)

# Fixed effects or within estimator
fixed <- plm(pc_delay ~ CASM_Pilot + CASM_Maintenance, data=pdata, model= "within")
summary(fixed)

# Hausman test for fixed versus random effects model
phtest(random, fixed)
# NUll - RE provides consistent estimators, no correlation between unique errors and regressors
# Alter - Fixed provides consistent estimators, correlation between unique errors and regressors


# LM test for fixed effects versus OLS
pFtest(fixed, pooling)
# Null - No fixed effect. OLS is better
# Alternate - Fixed effect is better
# Reject null hypothesis that entity level effect are not all zero

###########################################
# Pooled OLS estimator
pooling <- plm(pc_delay ~ CASM_Pilot + CASM_Maintenance + CASM_Pilot2 + CASM_Maintenance2, data=pdata, model= "pooling")
summary(pooling)


lmtest::bptest(pooling)
# Null - Same intercept. No heteroscedasticity
# Alternate - Difference intercept.heteroscedasticity present


# LM test for random effects versus OLS # 
plmtest(pooling)
# NUll - heterogeneity among individuals may not be significant
# Alternate - heterogeneity among individuals may be significant

# Random effects estimator
random <- plm(pc_delay ~ CASM_Pilot + CASM_Maintenance + CASM_Pilot2 + CASM_Maintenance2, data=pdata, model= "random", random.method = "ht", inst.method = "baltagi")
summary(random)

# Fixed effects or within estimator
fixed <- plm(pc_delay ~ CASM_Pilot + CASM_Maintenance + CASM_Pilot2 + CASM_Maintenance2, data=pdata, model= "within")
summary(fixed)

# Hausman test for fixed versus random effects model
phtest(random, fixed)
# NUll - RE provides consistent estimators, no correlation between unique errors and regressors
# Alter - Fixed provides consistent estimators, correlation between unique errors and regressors


# LM test for fixed effects versus OLS
pFtest(fixed, pooling)
# Null - No fixed effect. OLS is better
# Alternate - Fixed effect is better
# Reject null hypothesis that entity level effect are not all zero

plot(log(mydata$CASM_Pilot), random$residuals, main = "Residual Vs CASM_Pilot")
abline(0.2, -0.08)

