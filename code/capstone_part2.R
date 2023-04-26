################################################################################
## Wellesley College Data Science Major Capstone
## Part 2: Explore the data across all 5 waves + fit a MLR for each wave
## Clara Kim '23
## Apr 26, 2023
################################################################################

### Libraries ##################################################################
library(car, verbose=FALSE) # multicollinearity (VIF)
library(creditmodel, verbose=FALSE) # multicollinearity (Cramer's V)
library(ggplot2, verbose=FALSE)
library(MASS, verbose=FALSE) # stepwise selection
library(randomcoloR, verbose=FALSE)
################################################################################

setwd("/Users/clarakim/Desktop/spring23/data_science_capstone_spring23")
all.waves <- read.csv("./mock_data.csv", header=TRUE)

###############################################################################
## Correlation Plots
###############################################################################
# in ShowMyData.org

###############################################################################
## Investigate all 5 waves: Why should we care?
###############################################################################
cp <- distinctColorPalette(k = 98, altCol = FALSE, runTsne = FALSE)

### split units into those displaying a upward vs. constant vs. downward trend in response variable - rel.parents
rel.parents <- all.waves[, c("w1rel.parents","w2rel.parents","w3rel.parents","w4rel.parents","w5rel.parents")]
names(rel.parents) <- c("one", "two", "three", "four", "five")

par(mfrow=c(1,3))
upward.trend <- rel.parents[rel.parents$five - rel.parents$one > 0,] # 20
plot(as.numeric(upward.trend[1,]), type="o", col = "darkgray", xlab = "Waves", 
     ylab = "Relationship with Parents", ylim=c(0,20),
     main = "Upward Trend (n=20)")
for (ii in 1:98){
  lines(jitter(as.numeric(upward.trend[ii,])), type = "o", col = "gray")
}
lines(colMeans(upward.trend), type="o", col="red", lwd=2)

same <- rel.parents[rel.parents$five - rel.parents$one == 0,] # 16
plot(as.numeric(same[1,]), type="o", col = "gray", xlab = "Waves", 
     ylab = "Relationship with Parents", ylim=c(0,20),
     main = "Same (n=16)")
for (ii in 1:98){
  lines(jitter(as.numeric(same[ii,])), type = "o", col = "gray")
}
lines(colMeans(same), type="o", col="red", lwd=2)

downward.trend <- rel.parents[rel.parents$five - rel.parents$one < 0,] # 62
plot(as.numeric(downward.trend[1,]), type="o", col = "gray", xlab = "Waves", 
     ylab = "Relationship with Parents", ylim=c(0,20),
     main = "Downward Trend (n=62)")
for (ii in 1:98){
  lines(jitter(as.numeric(downward.trend[ii,])), type = "o", col = "gray")
}
lines(colMeans(downward.trend), type="o", col="red", lwd=2)

#write.csv(rel.parents, "relparents.csv", quote=FALSE, row.names=FALSE, col.names=TRUE)



wave1var <- c("home.lang","both.parents","siblings","momed.hs","momed.college","rel.parents",
              "phone.use","phone.own","parmon1","parmon2","parmon3","parmon4","parmon5",
              "parmon6","parmon7","parmon8","phone.limit","phone.teach","gameplay1",
              "gameplay2","game.effect","grade","grades.rec","white")

wave2var <- c("w2home.lang","w2both.parents","w2siblings","w2momed.hs","w2momed.college","w2rel.parents",
              "w2phone.use","w2phone.own","w2parmon1","w2parmon2","w2parmon3","w2parmon4","w2parmon5",
              "w2parmon6","w2parmon7","w2parmon8","w2phone.limit","w2phone.teach","w2gameplay1",
              "w2gameplay2","w2game.effect","w2grade","w2grades.rec","w2white")

wave3var <- c("w3home.lang","w3both.parents","w3siblings","w3momed.hs","w3momed.college","w3rel.parents",
              "w3phone.use","w3phone.own","w3parmon1","w3parmon2","w3parmon3","w3parmon4","w3parmon5",
              "w3parmon6","w3parmon7","w3parmon8","w3phone.limit","w3phone.teach","w3gameplay1",
              "w3gameplay2","w3game.effect","w3grade","w3grades.rec","w3white")

wave4var <- c("w4home.lang","w4both.parents","w4siblings","w4momed.hs","w4momed.college","w4rel.parents",
              "w4phone.use","w4phone.own","w4parmon1","w4parmon2","w4parmon3","w4parmon4","w4parmon5",
              "w4parmon6","w4parmon7","w4parmon8","w4phone.limit","w4phone.teach","w4gameplay1",
              "w4gameplay2","w4game.effect","w4grade","w4grades.rec","w4white")

wave5var <- c("w5home.lang","w5both.parents","w5siblings","w5momed.hs","w5momed.college","w5rel.parents",
              "w5phone.use","w5phone.own","w5parmon1","w5parmon2","w5parmon3","w5parmon4","w5parmon5",
              "w5parmon6","w5parmon7","w5parmon8","w5phone.limit","w5phone.teach","w5gameplay1",
              "w5gameplay2","w5game.effect","w5grade","w5grades.rec","w5white")


###############################################################################
## Data Cleaning - Handle Missing Values
###############################################################################

# Check for missing data
apply(is.na(all.waves), 2, which) # which rows are missing for each column
sum(is.na(all.waves)) # 437 total NA values in data frame
nrow(all.waves) # 108 total rows
sum(!complete.cases(all.waves)) # 63 rows with NA values (45 complete rows)
all.waves$na_count <- apply(all.waves, 1, function(x) sum(is.na(x))) # count NA values per row
table(all.waves$na_count)

# Delete rows missing response variable 
all.waves <- all.waves[!is.na(all.waves$rel.parents) & !is.na(all.waves$w2rel.parents) & !is.na(all.waves$w3rel.parents) & !is.na(all.waves$w4rel.parents) & !is.na(all.waves$w5rel.parents),]
dim(all.waves) # 101 left


# Delete columns with too many missing values
all.waves <- subset(all.waves, select=-c(gameplay1, gameplay2, game.effect, 
                                         w2gameplay1, w2gameplay2, w2game.effect, 
                                         w3gameplay1, w3gameplay2, w3game.effect,
                                         w4gameplay1, w4gameplay2, w4game.effect,
                                         w5gameplay1, w5gameplay2, w5game.effect))
dim(all.waves)

# Remove the remaining 19 rows with NA values
sum(!complete.cases(all.waves)) # 19
all.waves <- all.waves[complete.cases(all.waves),]
all.waves$na_count <- apply(all.waves, 1, function(x) sum(is.na(x))) 
table(all.waves$na_count) # no missing values anymore!

# Remove column 'na_count'
all.waves <- subset(all.waves, select=-c(na_count))
dim(all.waves) # 82 rows, 105 columns 

############## Summary ######################
# Final clean data: 82 rows, 105 columns
#############################################


###############################################################################
## Check multicollinearity for each wave
##
## 1. Quantitative Variables (VIF scores)
## 2. Categorical Variables (Cramer's V)
###############################################################################

## test for multicollinearity in quantitative variables using VIF scores (thresh=10)
lm.quant1 <- lm(phone.use~parmon1+parmon2+parmon3+parmon4+parmon5+
                      parmon6+parmon7+parmon8+grade+grades.rec, 
                    data=all.waves)
vif(lm.quant1,th=10)

lm.quant2 <- lm(w2phone.use~w2parmon1+w2parmon2+w2parmon3+w2parmon4+w2parmon5+
                  w2parmon6+w2parmon7+w2parmon8+w2grade+w2grades.rec, 
                data=all.waves)
vif(lm.quant2,th=10)

lm.quant3 <- lm(w3phone.use~w3parmon1+w3parmon2+w3parmon3+w3parmon4+w3parmon5+
                  w3parmon6+w3parmon7+w3parmon8+w3grade+w3grades.rec, 
                data=all.waves)
vif(lm.quant3,th=10)

lm.quant4 <- lm(w4phone.use~w4parmon1+w4parmon2+w4parmon3+w4parmon4+w4parmon5+
                  w4parmon6+w4parmon7+w4parmon8+w4grade+w4grades.rec, 
                data=all.waves)
vif(lm.quant4,th=10)

lm.quant5 <- lm(w5phone.use~w5parmon1+w5parmon2+w5parmon3+w5parmon4+w5parmon5+
                  w5parmon6+w5parmon7+w5parmon8+w5grade+w5grades.rec, 
                data=all.waves)
vif(lm.quant5,th=10)

## test for multicollinearity in 13 categorical variables using Cramer's V (thresh=0.3)
attach(all.waves)
df.categorical1 <- cbind(both.parents,siblings,momed.hs,momed.college,
                        phone.own,phone.limit,phone.teach,white)
df.categorical2 <- cbind(w2both.parents,w2siblings,w2momed.hs,w2momed.college,
                         w2phone.own,w2phone.limit,w2phone.teach,w2white)
df.categorical3 <- cbind(w3both.parents,w3siblings,w3momed.hs,w3momed.college,
                         w3phone.own,w3phone.limit,w3phone.teach,w3white)
df.categorical4 <- cbind(w4both.parents,w4siblings,w4momed.hs,w4momed.college,
                         w4phone.own,w4phone.limit,w4phone.teach,w4white)
df.categorical5 <- cbind(w5both.parents,w5siblings,w5momed.hs,w5momed.college,
                         w5phone.own,w5phone.limit,w5phone.teach,w5white)

corr.mat1 <- cor(df.categorical1)
corr.mat2 <- cor(df.categorical2)
corr.mat3 <- cor(df.categorical3)
corr.mat4 <- cor(df.categorical4)
corr.mat5 <- cor(df.categorical5)


# both.parents & momed.college
corr.mat1[corr.mat1 > 0.3]
cor(rel.parents, momed.college) # 0.26
cor(rel.parents, both.parents) # 0.17 (REMOVE)

corr.mat2[corr.mat2 > 0.3]


# 2 pairs exceeding (thresh=0.3)
corr.mat3[corr.mat3 > 0.3]
# w3momed.hs, w3both.parents
# w3momed.hs, w3momed.college
# w3phone.teach, w3phone.limit

cor(w3rel.parents, w3momed.hs) # 0.35
cor(w3rel.parents, w3both.parents) # 0.32 (REMOVE)
cor(w3rel.parents, w3momed.college) # 0.30 (REMOVE)
cor(w3rel.parents, w3phone.teach) # 0.32
cor(w3rel.parents, w3phone.limit) # 0.25 (REMOVE)


corr.mat4[corr.mat4 > 0.3]
# w4momed.hs, w4momed.college
cor(w4rel.parents, w4momed.hs) # 0.37
cor(w4rel.parents, w4momed.college) # 0.36 (REMOVE)

corr.mat5[corr.mat5 > 0.3]
# home.lang & white
# momed.hs & momed.college
cor(w5rel.parents, w5home.lang) # 0.19
cor(w5rel.parents, w5white) # 0.26
cor(w5rel.parents, w5momed.hs) # 0.25 (REMOVE)
cor(w5rel.parents, w5momed.college) # 0.11

# eliminate home.lang
all.waves.temp <- subset(all.waves, select=-c(both.parents, w3both.parents, w3momed.college, 
                                              w3phone.limit,w4momed.college, w5momed.hs))
attach(all.waves)
df.categorical1 <- cbind(siblings,momed.hs,momed.college,
                         phone.own,phone.limit,phone.teach,white)
df.categorical2 <- cbind(w2both.parents,w2siblings,w2momed.hs,w2momed.college,
                         w2phone.own,w2phone.limit,w2phone.teach,w2white)
df.categorical3 <- cbind(w3siblings,w3momed.hs,w3phone.own,w3phone.teach,w3white)
df.categorical4 <- cbind(w4both.parents,w4siblings,w4momed.hs,
                         w4phone.own,w4phone.limit,w4phone.teach,w4white)
df.categorical5 <- cbind(w5both.parents,w5siblings,w5momed.college,
                         w5phone.own,w5phone.limit,w5phone.teach,w5white)



cor(df.categorical1)[cor(df.categorical1) > 0.3]
cor(df.categorical2)[cor(df.categorical2) > 0.3]
cor(df.categorical3)[cor(df.categorical3) > 0.3]
cor(df.categorical4)[cor(df.categorical4) > 0.3]
cor(df.categorical5)[cor(df.categorical5) > 0.3]
# no multicollinearity issue anymore!

all.waves <- all.waves.temp

###############################################################################
## Variable Selection + Model Building for each of the 5 waves
##
## Stepwise Selection based on AIC and BIC
###############################################################################

# Stepwise Selection using AIC and BIC criterion

### wave 1
wave1 <- subset(all.waves, select=names(all.waves)[1:20])
wave1.stepwise <- lm(rel.parents ~ ., data = wave1)
wave1.aic <- stepAIC(wave1.stepwise, k = 2, trace = FALSE)
wave1.bic <- stepAIC(wave1.stepwise, k = log(dim(wave1)[1]), trace = FALSE)

w1.model <- lm(formula = rel.parents ~ parmon1 + phone.limit + 
                 momed.college + momed.hs + parmon4 + home.lang, data = wave1)

w1.model.red <- lm(formula = rel.parents ~ parmon1 + phone.limit + 
                     momed.college + momed.hs + parmon4, data = wave1)

# Partial F-test comparing AIC vs. BIC model (p<0.001) 
anova(w1.model)
anova(w1.model.red)

f.partial <- ((13.39)/1) / 5.077
1-pf(f.partial, df1=1, df2=75) # p = 0.11, remove
w1.model <- w1.model.red

### wave 2
wave2 <- subset(all.waves, select=names(all.waves)[21:41])
wave2.stepwise <- lm(w2rel.parents ~ ., data = wave2)
wave2.aic <- stepAIC(wave2.stepwise, k = 2, trace = FALSE)
wave2.bic <- stepAIC(wave2.stepwise, k = log(dim(wave2)[1]), trace = FALSE)

w2.model <- lm(formula = w2rel.parents ~ w2parmon1 + w2phone.limit + w2white + w2phone.use +
                  + w2parmon7 + w2momed.college, data = wave2)
w2.model.red <- lm(formula = w2rel.parents ~ w2parmon1 + w2phone.limit + w2white + w2phone.use, 
                   data = wave2)

f.partial <- ((10.137+9.009)/2) / 4.135
1-pf(f.partial, df1=2, df2=75) # p = 0.11, remove both
w2.model <- w2.model.red

### wave 3
wave3 <- subset(all.waves, select=names(all.waves)[42:59])
wave3.stepwise <- lm(w3rel.parents ~ ., data = wave3)
wave3.aic <- stepAIC(wave3.stepwise, k = 2, trace = FALSE)
wave3.bic <- stepAIC(wave3.stepwise, k = log(dim(wave3)[1]), trace = FALSE)

w3.model <- lm(formula = w3rel.parents ~ w3phone.use + w3parmon2 + w3momed.hs + w3phone.teach + w3parmon1 + 
                 w3parmon6 + w3home.lang + w3siblings + w3grades.rec, data = wave3)
w3.model.red <- lm(formula = w3rel.parents ~ w3phone.use + w3parmon2 + w3momed.hs + w3phone.teach + w3parmon1 + 
                 w3parmon6 + w3home.lang + w3siblings, data = wave3)

f.partial <- ((7.511)/1) / 3.495
1-pf(f.partial, df1=1, df2=72) # p = 0.15, remove w3grades.rec
w3.model <- w3.model.red

### wave 4
wave4 <- subset(all.waves, select=names(all.waves)[60:79])
wave4.stepwise <- lm(w4rel.parents ~ ., data = wave4)
wave4.aic <- stepAIC(wave4.stepwise, k = 2, trace = FALSE)
wave4.bic <- stepAIC(wave4.stepwise, k = log(dim(wave4)[1]), trace = FALSE)

w4.model <- lm(formula = w4rel.parents ~ w4both.parents + w4momed.hs + 
                 w4parmon3 + w4parmon7 + w4phone.own + w4siblings + w4parmon8 + w4grade + w4phone.teach,
               data = wave4)
w4.model.red <- lm(formula = w4rel.parents ~ w4both.parents + w4momed.hs + 
                 w4parmon3 + w4parmon7 + w4phone.own + w4siblings + w4parmon8 + w4grade,
               data = wave4)

f.partial <- ((6.379 + 8.030 + 7.175 + 6.922)/4) / 3.437
1-pf(f.partial, df1=4, df2=72) # p = 0.15, remove w4siblings, w4parmon8, w4grade, w4phone.teach
w4.model <- w4.model.red

### wave 5
wave5 <- subset(all.waves, select=names(all.waves)[80:99])
wave5.stepwise <- lm(w5rel.parents ~ ., data = wave5)
wave5.aic <- stepAIC(wave5.stepwise, k = 2, trace = FALSE)
wave5.bic <- stepAIC(wave5.stepwise, k = log(dim(wave5)[1]), trace = FALSE)

w5.model <- lm(formula = w5rel.parents ~ w5phone.teach + w5parmon4 + w5home.lang + w5parmon5 + 
                 w5phone.limit, data = wave5)
w5.model.red <- lm(formula = w5rel.parents ~ w5phone.teach + w5parmon4 + w5home.lang + w5parmon5, data = wave5)

f.partial <- ((17.47 + 14.87)/2) / 6.649
1-pf(f.partial, df1=2, df2=76) # p = 0.09, remove w5parmon5, w5phone.limit
w5.model <- w5.model.red

# Summarize the selected models
w1.model
w2.model
w3.model
w4.model
w5.model

