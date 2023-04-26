################################################################################
## Wellesley College Data Science Major Capstone
## Part 1: Fit a multiple linear regression model for wave 4 data
## Clara Kim '23
## Apr 26, 2023
################################################################################

### Libraries ##################################################################
library(car) # multicollinearity (VIF)
library(creditmodel) # multicollinearity (Cramer's V)
library(ggplot2)
library(MASS) # stepwise selection
################################################################################

setwd("/Users/clarakim/Desktop/spring23/data_science_capstone_spring23")
recoded <- read.csv("./from_stat318/data/recoded.csv")


###############################################################################
## Data Cleaning - Handle Missing Values
###############################################################################

# Check for missing data
apply(is.na(recoded), 2, which) # which rows are missing for each column
sum(is.na(recoded)) # 2055 total NA values in data frame
sum(!complete.cases(recoded)) # 335 rows with NA values
recoded$na_count <- apply(recoded, 1, function(x) sum(is.na(x))) # count NA values per row

# Delete rows missing response variable (13)
recoded <- recoded[!is.na(recoded$relationship),]

# Delete columns with too many missing values
recoded.clean <- subset(recoded, 
                        select=-c(gameplay1, gameplay2, game.effect, mood.score, 
                                  socanx.score, lone.score, stress.score, 
                                  bodimg.score, queer, household.effect, covid.cost, 
                                  parcheck))

# Remove the remaining 76 rows with NA values
sum(!complete.cases(recoded.clean)) # 76
recoded.clean <- recoded.clean[complete.cases(recoded.clean),]
recoded.clean$na_count <- apply(recoded.clean, 1, function(x) sum(is.na(x))) 
table(recoded.clean$na_count) # no missing values anymore!

# Remove column 'na_count'
recoded.clean <- recoded.clean[,c(-27)]
dim(recoded.clean) # 879 rows, 26 columns (1 response, 25 potential predictor variables)
wave4 <- recoded.clean

############## Summary ######################
# Initial data: 968 rows, 38 columns
# Final clean data: 879 rows, 26 columns
# 9.19% of total rows removed
#############################################

# explore 'rel.parents' as outcome variable
hist(wave4$rel.parents)


###############################################################################
## Multicollinearity
##
## 1. Quantitative Variables (VIF scores)
## 2. Categorical Variables (Cramer's V)
###############################################################################
attach(wave4)
## test for multicollinearity in quantitative variables using VIF scores (thresh=10)
lm.quant.full <- lm(rel.parents~grade+grades.rec+phone.use+parmon1+parmon2
                    +parmon3+parmon4+parmon5+parmon6+parmon7+parmon8, 
                    data=wave4)
vif(lm.quant.full,th=10)

## test for multicollinearity in 13 categorical variables using Cramer's V (thresh=3)
df.categorical <- cbind(iep, open.circle, relationship,home.lang,both.parents,
                        siblings,momed.hs,momed.college,sw.diffc,phone.own,
                        phone.limit1,phone.limit2,phone.teach)
varnames <- c("iep", "open.circle", "relationship","home.lang","both.parents",
              "siblings","momed.hs","momed.college","sw.diffc","phone.own",
              "phone.limit1","phone.limit2","phone.teach")

corr.mat <- cor(df.categorical)

# 2 pairs exceeding (thresh=0.3)
corr.mat[8,7] # momed.college & momed.hs
corr.mat[12,11] # phone.limit1 & phone.limit2

# eliminate momed.hs
cor(rel.parents, momed.college) # 0.12
cor(rel.parents, momed.hs) # 0.11

# eliminate phone.limit1
cor(rel.parents, phone.limit1) # 0.17
cor(rel.parents, phone.limit2) # 0.21

## test again with remaining 11 categorical variables
df.categorical2 <- cbind(iep, open.circle, relationship,home.lang,both.parents,
                        siblings,momed.college,sw.diffc,phone.own,phone.limit2,phone.teach)
varnames <- c("iep", "open.circle", "relationship","home.lang","both.parents",
              "siblings","momed.college","sw.diffc","phone.own","phone.limit1","phone.teach")
cor(df.categorical2)[cor(df.categorical2) > 0.3]

wave4 <- wave4[, -c(1,5,19)]


###############################################################################
## Variable Selection + Model Building
##
## Stepwise Selection based on AIC and BIC
###############################################################################

# Stepwise Selection: AIC and BIC
y <- rel.parents
X <- wave4[,-5]
stepwise.AIC <- lm(y ~ ., data = X, family = binomial)
stepwise.AIC <- stepAIC(stepwise.AIC, k = 2, trace = FALSE)
stepwise.BIC <- stepAIC(stepwise.AIC, k = log(dim(wave4)[1]), trace = FALSE)

# Summarize the two selected models
summary(stepwise.AIC)
summary(stepwise.BIC)

# General F-test for AIC model
stepwise.AIC <- lm(formula = y ~ phone.use + sw.diffc + parmon1 + parmon3 + parmon4 + 
                     parmon5 + phone.limit2 + grades.rec + grade + open.circle + 
                     home.lang + both.parents + siblings, data = X)
summary(stepwise.AIC)
1-pf(28.43, df1=13, df2=865) # p-val = 0

# General F-test for BIC model
stepwise.BIC <- lm(formula = y ~ phone.use + sw.diffc + parmon1 + parmon3 + parmon4 + 
                     parmon5 + phone.limit2 + grades.rec, data = X)
summary(stepwise.BIC)
1-pf(42.52, df1=8, df2=870) # p-val < 0.001


# Partial F-test comparing AIC vs. BIC model (p<0.001) 
anova(stepwise.AIC)
F.partial <- ((34.9+19.8+36.3+29.0+14.4)/5)/5.96
1-pf(F.partial, df1=5, df2=865) # p < 0.001, AIC is better than BIC model


# Refine AIC model
refine.AIC1 <- lm(formula = y ~ phone.use + sw.diffc + parmon1 + parmon3 + parmon4 + 
                parmon5 + phone.limit2 + grades.rec + grade + open.circle +
                home.lang + both.parents, data = X)


# Partial F-test comparing AIC vs. refine.AIC1 model (p = 0.12)
anova(stepwise.AIC)
F.partial2 <- (14.4/1)/5.96
1-pf(F.partial2, df1=1, df2=865) # p = 0.12, remove 'siblings'


# Partial F-test comparing refine.AIC1 vs. refine.AIC2 model (p = 0.027)
refine.AIC2 <- lm(formula = y ~ phone.use + sw.diffc + parmon1 + parmon3 + parmon4 + 
                    parmon5 + phone.limit2 + grades.rec + grade + open.circle + 
                    home.lang, data = X)
anova(refine.AIC1)
F.partial3 <- (29.0/1)/5.97
1-pf(F.partial3, df1=1, df2=866) # p = 0.027, can't remove 'both.parents'

# Final model after variable selection
final.model <- refine.AIC1
summary(final.model) # final model contains 12 predictors!

# lm(formula = y ~ phone.use + sw.diffc + parmon1 + parmon3 + parmon4 + 
#      parmon5 + phone.limit2 + grades.rec + grade + open.circle + 
#      home.lang + both.parents, data = X)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -9.6177 -1.4851  0.2492  1.6050  7.4255 
# 
# Coefficients:
#   Estimate Std. Error t value             Pr(>|t|)    
# (Intercept)  14.99922    0.79617  18.839 < 0.0000000000000002 ***
#   phone.use    -0.19293    0.03308  -5.833      0.0000000076831 ***
#   sw.diffc      0.55784    0.17686   3.154              0.00167 ** 
#   parmon1       0.46936    0.07787   6.027      0.0000000024632 ***
#   parmon3      -0.39099    0.08399  -4.655      0.0000037454278 ***
#   parmon4       0.45014    0.06731   6.687      0.0000000000407 ***
#   parmon5       0.23990    0.06466   3.710              0.00022 ***
#   phone.limit2  0.52571    0.18071   2.909              0.00372 ** 
#   grades.rec   -0.23386    0.07820  -2.990              0.00286 ** 
#   grade        -0.18119    0.06458  -2.806              0.00513 ** 
#   open.circle  -0.40844    0.17257  -2.367              0.01816 *  
#   home.lang     0.53363    0.21542   2.477              0.01343 *  
#   both.parents  0.45484    0.20635   2.204              0.02777 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.444 on 866 degrees of freedom
# Multiple R-squared:  0.2974,	Adjusted R-squared:  0.2876 
# F-statistic: 30.54 on 12 and 866 DF,  p-value: < 0.00000000000000022


###############################################################################
## Model Evaluation and Selection
##
## 1) Residual Analysis
## 2) Identify Outlying / Influential Observations
###############################################################################

## 1) Residual Analysis
par(mfrow=c(1,3))

# constant variance (residual plot: residual vs. fitted value)
plot(final.model$residuals~final.model$fitted.values,main="Reisduals vs. Fitted values", xlab="Fitted values", ylab="Residuals")
abline(10,-1, col="blue") 
abline(21,-1, col="blue") 

# independence (time sequence plot)
plot(final.model$residuals, main="Time sequence plot", ylab="Residuals")
abline(h=0, col="red")

# normality (qq plot)
qqnorm(y, main="Normal Q-Q Plot")
qqline(y, col="red")


## 2) Identify Outlying / Influential Observations
cooksD <- cooks.distance(final.model)
F_fifty.percentile <- qf(0.5, df1=12, df2=866)
influential <- cooksD[(cooksD > F_fifty.percentile)] # no outlying observations influential enough to remove
