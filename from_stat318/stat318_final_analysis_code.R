###############################################################################
## STAT 318 Final Project
##
## The Immediate Impact of the COVID-19 Pandemic on Young  
## Adolescent Relationships with Family Members
##
## Clara Kim, Alice Zhang
## Fall 2022
###############################################################################


### Libraries #################################################################
library(car) # multicollinearity (VIF)
library(creditmodel) # multicollinearity (Cramer's V)
library(ggplot2)
library(ggcorrplot) # plot correlation matrices
library(bestglm) # fit logistic regression model
library(MASS) # stepwise selection
library(pROC) # ROC curve and AUC
library(ROCR)
#library(caret)
#library(cvms)
#library(tibble) 
library(blorr) # delta deviance plot
###############################################################################


###############################################################################
## Data Cleaning 
##
## 1. Recode Variables (both response & predictors)
## 2. Handle Missing Values
###############################################################################

### 1. Recode Variables ***SKIP TO LINE 467 IF YOU WANT TO SKIP VARIABLE RECODING CODE***
data <- read.csv('/Users/alicezhang/Desktop/STAT318 Final Project/original_data.csv', head=TRUE)

### First half of the variables (1/2)
data$relationship <- with(data, ifelse(w4cq01 == 1 | w4cq01 == 2, 1, 0))
data$home.lang <- with(data, ifelse(w4homlang == 1, 1, 0))
data$both.parents <- with(data, ifelse(!is.na(w4hstruct_1) & !is.na(w4hstruct_2), 1, 0))
data$siblings <- with(data, ifelse(!is.na(w4hstruct_3) | !is.na(w4hstruct_4) | !is.na(w4hstruct_5) | !is.na(w4hstruct_6), 1, 0))
data$momed.hs <- with(data, ifelse(w4momed > 1, 1, 0))
data$momed.college <- with(data, ifelse(w4momed > 3, 1, 0))
data$rel.parents <- data$w4relpar_1 + data$w4relpar_2 + data$w4relpar_3 + data$w4relpar_4 
data$phone.use <- data$w4relpar_5 + data$w4relpar_6 + data$w4relpar_7
data$sw.diffc <- with(data, ifelse(w4remsch_6 == 1, 1, ifelse(w4remsch_6 == 0, 0, NA)))
data$phone.own <- with(data, ifelse(w4agesmart == 0, 0, 1))
data$parmon1 <- data$w4parmon_1
data$parmon2 <- data$w4parmon_2
data$parmon3 <- data$w4parmon_3
data$parmon4 <- data$w4parmon_4
data$parmon5 <- data$w4parmon_5
data$parmon6 <- data$w4parmon_6
data$parmon7 <- data$w4parmon_7
data$parmon8 <- data$w4parmon_8
data$phone.limit1 <- with(data, ifelse(!is.na(w4parmon2_8), 0, 1))
data$phone.limit2 <- with(data, ifelse(w4parmon3 == 8, 0, 1))
data$phone.teach <- with(data, ifelse(!is.na(w4smwuse_2) | !is.na(w4smwuse_3) | !is.na(w4smwuse_4), 1, 0))
data$gameplay1 <- data$w4gameplay_1
data$gameplay2 <- data$w4gameplay_2
data$game.effect <- data$w4gamebeh_2

clean1 <- data[,135:158]


### Second half of the variables (1/2)
df <- data[,68:134]
attach(df)

## mood.score
# nas in row
na.count.mood <- rep(NA, 968)
# ques left blank
na.count.10 <- rep(NA, 96) # i.10
na.count.9 <- rep(NA, 3) # i.9
na.count.8 <- rep(NA, 3) # i.8
na.count.7 <- rep(NA, 2) # i.7
# keep
na.count.0 <- rep(NA, 805) # i.0
i.10 <- 0
i.9 <- 0
i.8 <- 0
i.7 <- 0
i.0 <- 0

for(ii in 1:968) {
  na.count.mood[ii] <- sum(is.na(
    c(
      w4dep_1[ii], w4dep_2[ii], w4dep_3[ii], w4dep_4[ii], w4dep_5[ii],
      w4dep_6[ii], w4dep_7[ii], w4dep_8[ii], w4dep_9[ii], w4dep_10[ii]
    )
  ))
  if(na.count.mood[ii] == 10) { # 96 participants filled out nothing
    i.10 = i.10 + 1
    na.count.10[i.10] <- ii
  }
  if(na.count.mood[ii] == 9) {
    i.9 = i.9 + 1
    na.count.9[i.9] <- ii
  }
  if(na.count.mood[ii] == 8) {
    i.8 = i.8 + 1
    na.count.8[i.8] <- ii
  }
  if(na.count.mood[ii] == 7) {
    i.7 = i.7 + 1
    na.count.7[i.7] <- ii
  }
  if(na.count.mood[ii] == 0){
    i.0 = i.0 + 1
    na.count.0[i.0] <- ii
  }
}

mood.score <- w4dep_1 + w4dep_2 + w4dep_3 + w4dep_4 + w4dep_6 + w4dep_7 + w4dep_9 + w4dep_10
df['mood.score'] <- mood.score


## loneliness scale
sum(is.na(w4feel1))
sum(is.na(w4feel2))
sum(is.na(w4feel3))

# impute 0
row.gaps <- c(43, 623, 634, 184, 679, 734, 924)
for (item in row.gaps) {
  if (is.na(w4feel1[item])) {
    w4feel1[item] <- 0 }
  if (is.na(w4feel2[item])) {
    w4feel2[item] <- 0 }
  if (is.na(w4feel3[item])) {
    w4feel3[item] <- 0 }
}
lone.score <- w4feel1 + w4feel2 + w4feel3 # 102 ppl didn't fill it out
df['lone.score'] <- lone.score


## socanx.scale
df.subset <- subset(df, select = c('w4socanx_1', 'w4socanx_2', 'w4socanx_3', 
                                   'w4socanx_4', 'w4socanx_5', 'w4socanx_6' ))
row.sum <- rowSums(is.na(df.subset))
sum(row.sum[row.sum == 6]) # 594 skipped these questions

socanx.score <- w4socanx_1 + w4socanx_2 + w4socanx_3 + w4socanx_4 + w4socanx_5 + w4socanx_6
df['socanx.score'] <- socanx.score


## w4stress_2
na.w4stress_2 <- rep(NA, 129)


## bodimg.score
# recode positive question scale (flip)
for(ii in 1:dim(df)[1]) {
  if(is.na(w4bodimg_1[ii])) {
    w4bodimg_1[ii] <- w4bodimg_1[ii]
  } else if (w4bodimg_1[ii] == 1) {
    w4bodimg_1[ii] <- 5
  } else if (w4bodimg_1[ii] == 2) {
    w4bodimg_1[ii] <- 4
  } else if (w4bodimg_1[ii] == 4) {
    w4bodimg_1[ii] <- 2
  } else if (w4bodimg_1[ii] == 5) {
    w4bodimg_1[ii] <- 1
  }
  if(is.na(w4bodimg_3[ii])) {
    w4bodimg_3[ii] <- w4bodimg_3[ii]
  } else if (w4bodimg_3[ii] == 1) {
    w4bodimg_3[ii] <- 5
  } else if (w4bodimg_3[ii] == 2) {
    w4bodimg_3[ii] <- 4
  } else if (w4bodimg_3[ii] == 4) {
    w4bodimg_3[ii] <- 2
  } else if (w4bodimg_3[ii] == 5) {
    w4bodimg_3[ii] <- 1
  }
}

# nas in row
na.count.bodimg <- rep(NA, 968)
# keep
na.count.3 <- rep(NA, 9) # i.3
na.count.2 <- rep(NA, 8) # i.2
na.count.1 <- rep(NA, 4) # i.1
i.3 <- 0
i.2 <- 0
i.1 <- 0

for(ii in 1:968) {
  na.count.bodimg[ii] <- sum(is.na(c(w4bodimg_1[ii], w4bodimg_2[ii], 
                                     w4bodimg_3[ii], w4bodimg_4[ii]))) # 516 skipped these questions
  if(na.count.bodimg[ii] == 1) {
    i.1 = i.1 + 1
    na.count.1[i.1] <- ii }
  if(na.count.bodimg[ii] == 2) {
    i.2 = i.2 + 1
    na.count.2[i.2] <- ii }
  if(na.count.bodimg[ii] == 3) {
    i.3 = i.3 + 1
    na.count.3[i.3] <- ii }
}

# zero imputation
for(ii in 1:length(na.count.1)) {
  if(is.na(w4bodimg_1[ii])) {
    w4bodimg_1[ii] <- 0 }
  if(is.na(w4bodimg_2[ii])) {
    w4bodimg_2[ii] <- 0 }
  if(is.na(w4bodimg_3[ii])) {
    w4bodimg_3[ii] <- 0 }
  if(is.na(w4bodimg_4[ii])) {
    w4bodimg_4[ii] <- 0 }
}

for(ii in 1:length(na.count.2)) {
  if(is.na(w4bodimg_1[ii])) {
    w4bodimg_1[ii] <- 0 }
  if(is.na(w4bodimg_2[ii])) {
    w4bodimg_2[ii] <- 0 }
  if(is.na(w4bodimg_3[ii])) {
    w4bodimg_3[ii] <- 0 }
  if(is.na(w4bodimg_4[ii])) {
    w4bodimg_4[ii] <- 0 }
}

for(ii in 1:length(na.count.3)) {
  if(is.na(w4bodimg_1[ii])) {
    w4bodimg_1[ii] <- 0 }
  if(is.na(w4bodimg_2[ii])) {
    w4bodimg_2[ii] <- 0 }
  if(is.na(w4bodimg_3[ii])) {
    w4bodimg_3[ii] <- 0 }
  if(is.na(w4bodimg_4[ii])) {
    w4bodimg_4[ii] <- 0 }
}
bodimg.score <- w4bodimg_1 + w4bodimg_2 + w4bodimg_3 + w4bodimg_4
df['bodimg.score'] <- bodimg.score


## queer
queer.indic <- rep(NA, dim(df)[1])

for(ii in 1:dim(df)[1]) {
  if(is.na(w4sexor[ii])) {
    w4sexor[ii] <- w4sexor[ii]
  } else if (w4sexor[ii] == 1 ||
             w4sexor_5_TEXT[ii] == "no" ||
             w4sexor_5_TEXT[ii] == "I'm normal" ||
             w4sexor_5_TEXT[ii] == "i didn't understand this question" ||
             w4sexor_5_TEXT[ii] == "idk really. Opposite I guess? I don't like anyone yet" ||
             w4sexor_5_TEXT[ii] == "idk really. Opposite I guess? I don't like anyone yet" ||
             w4sexor_5_TEXT[ii] == "Women/girl" ||
             w4sexor_5_TEXT[ii] == "Males") {
    queer.indic[ii] <- 0
  } else {
    queer.indic[ii] <- 1
  }
  
  if (is.na(w4gender[ii])) {
    w4gender[ii] <- w4gender[ii]
  } else if (w4gender_3_TEXT[ii] == 'transgender' ||
             w4gender_3_TEXT[ii] == 'non binary (they/them)' ||
             w4gender_3_TEXT[ii] == 'non binary' ||
             w4gender_3_TEXT[ii] == 'them/they') {
    queer.indic[ii] <- 1
  }
}
df['queer.indic'] <- queer.indic


## covid.household.effect
# recode NA values
# for(ii in 1:dim(df)[1]) {
#   if(w4covidh_1[ii] == 999) {w4covidh_1[ii] <- NA}
#   if(w4covidh_2[ii] == 999) {w4covidh_2[ii] <- NA}
#   if(w4covidh_3[ii] == 999) {w4covidh_3[ii] <- NA}
#   if(w4covidh_4[ii] == 999) {w4covidh_4[ii] <- NA}
#   if(w4covidh_5[ii] == 999) {w4covidh_5[ii] <- NA}
#   if(w4covidh_6[ii] == 999) {w4covidh_6[ii] <- NA}
#   if(w4covidh_7[ii] == 999) {w4covidh_7[ii] <- NA}
#   if(w4covidh_10[ii] == 999) {w4covidh_10[ii] <- NA}
# }
sum(is.na(w4covidh_1))
sum(is.na(w4covidh_2))
sum(is.na(w4covidh_3))
sum(is.na(w4covidh_4))
sum(is.na(w4covidh_5))
sum(is.na(w4covidh_6))
sum(is.na(w4covidh_7))
sum(is.na(w4covidh_10))

covid.household.effect <- w4covidh_1 + w4covidh_2 + w4covidh_3 + w4covidh_4 + w4covidh_5 + w4covidh_6 + w4covidh_7 + w4covidh_10
df['covid.household.effect'] <- covid.household.effect


## covidc
# nas in row
na.count.cov <- rep(NA, 968)
# ques left blank
na.count.6 <- rep(NA, 89) # i.6
na.count.1 <- rep(NA, 3) # i.1
na.count.0 <- rep(NA, 861) # i.0
i.6 <- 0
i.1 <- 0
i.0 <- 0

for(ii in 1:968) {
  na.count.cov[ii] <- sum(is.na(c(w4covidc_1[ii], w4covidc_2[ii], w4covidc_3[ii], 
                                  w4covidc_4[ii], w4covidc_5[ii],w4covidc_6[ii])))
  if(na.count.cov[ii] == 6) {
    i.6 = i.6 + 1
    na.count.6[i.6] <- ii }
  if(na.count.cov[ii] == 1) {
    i.1 = i.1 + 1
    na.count.1[i.1] <- ii }
  if(na.count.cov[ii] == 0){
    i.0 = i.0 + 1
    na.count.0[i.0] <- ii }
}

# impute 0
for(ii in 1:length(na.count.1)) {
  if(is.na(w4covidc_1[na.count.1[ii]])) {
    w4covidc_1[na.count.1[ii]] <- 0 }
  if(is.na(w4covidc_2[na.count.1[ii]])) {
    w4covidc_2[na.count.1[ii]] <- 0 }
  if(is.na(w4covidc_3[na.count.1[ii]])) {
    w4covidc_3[na.count.1[ii]] <- 0 }
  if(is.na(w4covidc_4[na.count.1[ii]])) {
    w4covidc_4[na.count.1[ii]] <- 0 }
  if(is.na(w4covidc_5[na.count.1[ii]])) {
    w4dep_5[na.count.1[ii]] <- 0 }
  if(is.na(w4covidc_6[na.count.1[ii]])) {
    w4covidc_6[na.count.1[ii]] <- 0 }
}
cov.score <- w4covidc_1 + w4covidc_2 + w4covidc_3 + w4covidc_4 + w4covidc_5 + w4covidc_6
df['cov.score'] <- cov.score


## w4iep
# get all that are no iep = 2
idx <- rep(NA, 968)
for(ii in 1:968) {
  if(is.na(w4iep[ii])) {
    idx[ii] <- NA } 
  else if(w4iep[ii] == 2) {
    idx[ii] <- ii }
}
idx.noNA <- na.omit(idx)
length(idx.noNA)
for(item in idx.noNA) {
  if (w4iep_TEXT[967] != " ") {
    print(TRUE)
  }
}

# recode yes or no
for(ii in 1:968) {
  if(is.na(w4iep[ii])) {
    w4iep[ii] <- 0
  } else if(w4iep[ii] == 2) {
    w4iep[ii] <- 0
  }
}

## w4oc
#recode yes or no
for(ii in 1:968) {
  if(is.na(w4oc[ii])) {
    w4oc[ii] <- 0
  } else if(w4oc[ii] == 2) {
    w4oc[ii] <- 0
  }
}


## w4hisp
for(ii in 1:968) {
  if(is.na(w4hisp[ii])) {
    w4hisp[ii] <- 0
  }
}


## race
asian.idx <- rep(NA, sum(!is.na(w4race_4))) # 68
black.idx <- rep(NA, sum(!is.na(w4race_2))) # 124
nativeamer.idx <- rep(NA, sum(!is.na(w4race_5))) # 44
white.idx <- rep(NA, sum(!is.na(w4race_1))) # 539
latamer.idx <- rep(NA, sum(!is.na(w4race_7))) # 144
mideast.isx <- rep(NA, sum(!is.na(w4race_8))) # 41
other.isx <- rep(NA, sum(!is.na(w4race_6))) # 146

df['mood.score'] <- mood.score
df['lone.score'] <- lone.score
df['socanx.score'] <- socanx.score
df['stress.score'] <- w4stress_2
df['bodimg.score'] <- bodimg.score
df['queer'] <- queer.indic
df['household.effect'] <- covid.household.effect
df['covid.cost'] <- cov.score
df['parcheck'] <- w4cparcheck
df['grade'] <- w4grade
df['grades.rec'] <- w4schgra
df['iep'] <- w4iep
df['open.circle'] <- w4oc
df['hisp'] <- w4hisp

attach(df)
clean2 <- data.frame(mood.score,socanx.score,lone.score,stress.score,
                     bodimg.score,queer,household.effect,covid.cost,parcheck,
                     grade,grades.rec,iep,open.circle,hisp)


### 2. Handle Missing Values
#recoded <- read.csv("/Users/clarakim/Desktop/fall22/STAT 318/FinalProject/recoded.csv", header=TRUE)
recoded <- cbind(clean1, clean2)
dim(recoded) # 968 rows, 38 columns

# Check for missing data
apply(is.na(recoded), 2, which) # which rows are missing for each column
sum(is.na(recoded)) # 2055 total NA values in data frame
sum(!complete.cases(recoded)) # 335 rows with NA values
recoded$na_count <- apply(recoded, 1, function(x) sum(is.na(x))) # count NA values per row

# Delete rows missing response variable (13)
recoded <- recoded[!is.na(recoded$relationship),]

# Delete columns with too many missing values
sum(is.na(recoded$gameplay1)) # 234
sum(is.na(recoded$gameplay2)) # 236
sum(is.na(recoded$game.effect)) # 251
sum(is.na(recoded$mood.score)) # 104
sum(is.na(recoded$socanx.score)) # 99
sum(is.na(recoded$lone.score)) # 102
sum(is.na(recoded$stress.score)) # 129
sum(is.na(recoded$bodimg.score)) # 137
sum(is.na(recoded$queer)) # 131
sum(is.na(recoded$household.effect)) # 85
sum(is.na(recoded$covid.cost)) # 92
sum(is.na(recoded$parcheck)) # 90
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

############## Summary ######################
# Initial data: 968 rows, 38 columns
# Final clean data: 879 rows, 26 columns
# 9.19% of total rows removed
#############################################

# Write clean data into csv file
#write.csv(recoded.clean, "/Users/alicezhang/Desktop/STAT318 Final Project/recoded_clean.csv", col.names=TRUE, row.names=FALSE)
write.csv(recoded.clean, "/Users/alicezhang/Desktop/STAT318 Final Project/recoded_clean.csv", row.names=TRUE)

# Pie Chart: Imbalance in Response Variable
slices <- c(83.8, 16.2)
lbls <- paste(slices,"%",sep="") # add % to labels
colors <- c("#e3e8ea","#707a7e")
pie(slices,labels = lbls, col=colors, main="Imbalance in Response Variable")
legend(-0.6, -0.86, 
       c("Better/Same Relationship with Family (0)","Worse Relationship with Family (1)"), 
       cex = 0.7, fill=colors)


###############################################################################
## Multicollinearity
##
## 1. Quantitative Variables (VIF scores)
## 2. Categorical Variables (Cramer's V)
###############################################################################

full.data <- recoded.clean
attach(full.data)

## test for multicollinearity in quantitative variables using VIF scores (thresh=10)
lm.quant.full <- lm(relationship~grade+grades.rec+rel.parents+phone.use+parmon1
                    +parmon2+parmon3+parmon4+parmon5+parmon6+parmon7+parmon8, 
                    data=full.data)
vif(lm.quant.full,th=10)

## test for multicollinearity in 13 categorical variables using Cramer's V (thresh=3)
df.categorical <- cbind(iep, open.circle, relationship,home.lang,both.parents,
                        siblings,momed.hs,momed.college,sw.diffc,phone.own,
                        phone.limit1,phone.limit2,phone.teach)
varnames <- c("iep", "open.circle", "relationship","home.lang","both.parents",
              "siblings","momed.hs","momed.college","sw.diffc","phone.own",
              "phone.limit1","phone.limit2","phone.teach")

# 13 x 13 matrix
cor.mat <- matrix(NA, nrow = length(varnames), ncol = length(varnames))
row.idx <- 1
for (name in varnames){
  temp <- char_cor_vars(df.categorical, x = name)
  for(ii in 1:length(temp)) {
    cor.mat[row.idx, ii] <- temp[ii]
  }
  row.idx = row.idx + 1
}
cor.mat[1,]
for(ii in cor.mat) {
  if(ii > 0.3){
    print(ii)
  }
}

# 2 pairs exceeding (thresh=3)
cor.mat[8,] # momed.college & momed.hs
cor.mat[12,] # phone.limit1 & phone.limit2

# eliminate momed.hs
char_cor_vars(relationship,momed.college)
char_cor_vars(relationship,momed.hs)

# eliminate phone.limit2
char_cor_vars(relationship,phone.limit1)
char_cor_vars(relationship,phone.limit2)

## test again with remaining 11 categorical variables
df.categorical <- cbind(iep, open.circle, relationship,home.lang,both.parents,
                        siblings,momed.college,sw.diffc,phone.own,phone.limit1,phone.teach)
varnames <- c("iep", "open.circle", "relationship","home.lang","both.parents",
              "siblings","momed.college","sw.diffc","phone.own","phone.limit1","phone.teach")
length(varnames)

# 11 x 11 matrix
cor.mat <- matrix(NA, nrow = length(varnames), ncol = length(varnames))
row.idx <- 1
for (name in varnames){
  temp <- char_cor_vars(df.categorical, x = name)
  for(ii in 1:length(temp)) {
    cor.mat[row.idx, ii] <- temp[ii]
  }
  row.idx = row.idx + 1
}

# No more multicollinearity issue!
full.data <- full.data[,-c(5,20)]
write.csv(full.data, "/Users/alicezhang/Desktop/STAT318 Final Project/trimmed_data.csv", row.names=TRUE)
attach(full.data)

## Plotting Correlation Matrices
# quantitative variables
df.quant <- cbind(grade, grades.rec, rel.parents, phone.use, parmon1, parmon2, 
                  parmon3, parmon4, parmon5, parmon6, parmon7, parmon8)
r <- cor(df.quant, use="complete.obs")
round(r,2)
ggcorrplot(r)

# categorical variables
rownames(cor.mat) <- c("iep", "open.circle", "relationship","home.lang",
                       "both.parents","siblings","momed.college","sw.diffc",
                       "phone.own","phone.limit1","phone.teach")
colnames(cor.mat) <- c("iep", "open.circle", "relationship","home.lang",
                       "both.parents","siblings","momed.college","sw.diffc",
                       "phone.own","phone.limit1","phone.teach")
round(cor.mat,2)
ggcorrplot(cor.mat)

# Plotting Response Graphic
worse.count <- 0
better.count <- 0
for(ii in 1:length(relationship)) {
  if(relationship[ii] == 0) {
    worse.count = worse.count + 1
  } else {
    better.count = better.count + 1
  }
}
pie(x = c(worse.count, better.count), labels = c('', ''), col = c('#eda986', "#dbfffd"))


###############################################################################
## Variable Selection + Model Building
##
## Stepwise Selection based on AIC and BIC
###############################################################################

# Stepwise Selection: AIC and BIC
y <- relationship
X <- full.data[,-1]
stepwise.model.AIC <- glm(y ~., data = X, family = binomial)
stepwise.model.AIC <- stepAIC(stepwise.model.AIC, k = 2, trace = FALSE)
stepwise.model.BIC <- stepAIC(stepwise.model.AIC, k = log(dim(full.data)[1]), trace = FALSE)
stepwise.model.BIC

# Summarize the two selected models
summary(stepwise.model.AIC)
summary(stepwise.model.BIC)


###############################################################################
## Model Evaluation and Selection
##
## Based on:
## 1) Graphical evaluation using ROC curve and AUC
## 2) Performance metrics (accuracy, sensitivity, specificity, precision)
## 3) AIC and BIC
###############################################################################


########## Model 1: Stepwise AIC ##########
stepwiseAIC <- glm(formula = relationship ~ home.lang + both.parents + rel.parents 
                   + phone.own + parmon3 + phone.teach + iep + hisp, 
                   family = binomial(link = 'logit'), data = full.data)

## 1. Graphical Evaluation (ROC curve and AUC)
pi.hat.stepAIC <- predict(stepwiseAIC, type="response") #predicted prob
plot(roc(relationship, pi.hat.stepAIC))
auc(relationship, pi.hat.stepAIC) # Area under the curve: 0.7816


## 2. Performance Metrics (accuracy, sensitivity, specificity, precision)
# Partition the data into k equal subsets
set.seed(1212)
n <- dim(full.data)[1] # sample size
K <- 10 # 10-fold CV
n.fold <- floor(n/K)
n.shuffle <- sample(1:n, n, replace = FALSE) # shuffle the n indices
index.fold <- list() 

for (i in 1:K){
  if (i < K){
    index.fold[[i]] <- n.shuffle[((i-1) * n.fold + 1):(i * n.fold)]
  } 
  else{
    index.fold[[i]] <- n.shuffle[((K-1) * n.fold + 1):n]
  }
}

# CV to find the optimized threshold value
c <- seq(from=0.1, to=0.5, by=0.05)
CV_accuracy_AIC <- rep(0, length(c))
CV_sensitivity_AIC <- rep(0, length(c))
CV_specificity_AIC <- rep(0, length(c))
CV_precision_AIC <- rep(0, length(c))

for(i in 1:length(c)) {
  for(kval in 1:K) { #suppose you consider K-fold CV
    fit <- glm(formula = relationship ~ home.lang + both.parents + rel.parents 
               + phone.own + parmon3 + phone.teach + iep + hisp, 
               family = binomial(link = 'logit'), 
               data = full.data[-index.fold[[kval]],])
    
    p.hat <- predict(fit,  newdata = full.data[index.fold[[kval]],], type = "response")
    Y.hat <- ifelse(p.hat > c[i], 1, 0)
    conf.matrix <- table(Y.hat, full.data[index.fold[[kval]],]$relationship)
    CV_accuracy_AIC[i] <- 
      CV_accuracy_AIC[i] + (1/K) * (conf.matrix[1,1] + conf.matrix[2,2]) / sum(conf.matrix)
    CV_sensitivity_AIC[i] <-
      CV_sensitivity_AIC[i] + (1/K) * conf.matrix[2,2] / (conf.matrix[1,2] + conf.matrix[2,2])
    CV_specificity_AIC[i] <-
      CV_specificity_AIC[i] + (1/K) *  conf.matrix[1,1]/ (conf.matrix[1,1] + conf.matrix[2,1])
    CV_precision_AIC[i] <-
      CV_precision_AIC[i] + (1/K) * conf.matrix[2,2] / (conf.matrix[2, 1] + conf.matrix[2,2])
  }
}
perf.metrics_AIC <- data.frame(cbind(c, CV_accuracy_AIC, CV_sensitivity_AIC, 
                                     CV_specificity_AIC, CV_precision_AIC))
perf.metrics_AIC$F.measure <- 2 * CV_precision_AIC * CV_sensitivity_AIC / (CV_precision_AIC + CV_sensitivity_AIC)
perf.metrics_AIC # best cut-off = 0.25

# Final confusion matrix
Y.hat = ifelse(pi.hat.stepAIC > 0.25, 1, 0) #dichotomize using 0.1
AIC.step.confusian.table <- table(Y.hat,relationship)
AIC.step.confusian.table

Actual <- factor(c(0, 0, 1, 1))
Predicted <- factor(c(0, 1, 0, 1))
Y <- c(635, 102, 67, 75)
df <- data.frame(Actual, Predicted, Y)

ggplot(data =  df, mapping = aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#eda986", high = "light blue") +
  theme_bw() + theme(legend.position = "none")

## 3. AIC and BIC 
AIC(stepwiseAIC) # 665.9954
BIC(stepwiseAIC) # 709.0045


########## Model 2: Stepwise BIC ##########
stepwiseBIC <- glm(formula = relationship ~ rel.parents + phone.own + parmon3, 
                   family = binomial(link = 'logit'), data = full.data)

## 1. Graphical Evaluation (ROC curve and AUC)
pi.hat.stepBIC = predict(stepwiseBIC,type="response") #predicted prob
plot(roc(relationship,pi.hat.stepBIC))
auc(relationship,pi.hat.stepBIC) # Area under the curve: 0.7692

## 2. Performance Metrics (accuracy, sensitivity, specificity, precision)
# CV to find the optimized cutoff value
CV_accuracy_BIC <- rep(0, length(c))
CV_sensitivity_BIC <- rep(0, length(c))
CV_specificity_BIC <- rep(0, length(c))
CV_precision_BIC <- rep(0, length(c))

for(i in 1:length(c)) 
{
  for(kval in 1:K) #suppose you consider K-fold CV
  {
    fit <- glm(formula = relationship ~ rel.parents + phone.own + parmon3,
               family = binomial(link = 'logit'), 
               data = full.data[-index.fold[[kval]],])
    
    p.hat <- predict(fit,  newdata = full.data[index.fold[[kval]],], type = "response")
    Y.hat <- ifelse(p.hat > c[i], 1, 0)
    mytable <- table(Y.hat, full.data[index.fold[[kval]],]$relationship)
    CV_accuracy_BIC[i] <- 
      CV_accuracy_BIC[i] + (1/K) * (mytable[1,1] + mytable[2,2]) / sum(mytable)
    CV_sensitivity_BIC[i] <-
      CV_sensitivity_BIC[i] + (1/K) * mytable[2,2] / (mytable[1,2] + mytable[2,2])
    CV_specificity_BIC[i] <-
      CV_specificity_BIC[i] + (1/K) *  mytable[1,1]/ (mytable[1,1] + mytable[2,1])
    CV_precision_BIC[i] <-
      CV_precision_BIC[i] + (1/K) * mytable[2,2] / (mytable[2, 1] + mytable[2,2])
  }
}
perf.metrics_BIC <- data.frame(cbind(c, CV_accuracy_BIC, CV_sensitivity_BIC, 
                                     CV_specificity_BIC, CV_precision_BIC))
perf.metrics_BIC$F.measure <- 2 * CV_precision_BIC*CV_sensitivity_BIC / (CV_precision_BIC+CV_sensitivity_BIC)
perf.metrics_BIC # best cut-off = 0.20

# Final confusion matrix
Y.hat = ifelse(pi.hat.stepBIC > 0.2, 1, 0) #dichomotize using 0.1
BIC.step.confusian.table <- table(Y.hat,relationship)
BIC.step.confusian.table

Actual <- factor(c(0, 0, 1, 1))
Predicted <- factor(c(0, 1, 0, 1))
Y <- c(584, 153, 58, 84)
df <- data.frame(Actual, Predicted, Y)

ggplot(data =  df, mapping = aes(x = Actual, y = Predicted)) +
  geom_tile(aes(fill = Y), colour = "white") +
  geom_text(aes(label = sprintf("%1.0f", Y)), vjust = 1) +
  scale_fill_gradient(low = "#eda986", high = "light blue") +
  theme_bw() + theme(legend.position = "none")

## 3. AIC and BIC
AIC(stepwiseBIC) # 677.3265
BIC(stepwiseBIC) # 696.4416


### LR test with the saturated model
g.AIC <- summary(stepwiseAIC)$deviance 
1-pchisq(g.AIC, df=n-(length(stepwiseAIC$coefficients)-1)) 
# 1, AIC model fits the data adequately well

g.BIC <- summary(stepwiseBIC)$deviance 
1-pchisq(g.BIC, df=n-(length(stepwiseBIC$coefficients)-1)) 
# 1, BIC model fits the data adequately well


### LR test to compare nested models AIC vs. BIC
gdiff <- summary(stepwiseBIC)$deviance - summary(stepwiseAIC)$deviance
1-pchisq(gdiff, df=length(stepwiseAIC$coefficients) - length(stepwiseBIC$coefficients)) 
# 0.0007012981, AIC model fits the data significantly better than BIC model

# Final model is stepwiseAIC!
final.model <- stepwiseAIC



###############################################################################
## Regression Diagnostics
##
## 1. Residual Plots (outlying observations)
## 2. Delta Deviance Plot (influential observations)
###############################################################################

# Residual plots for final model
par(mfrow=c(2,2))
plot(final.model) 

# Examine outlying observations: 517, 630, 635, 656, 683
full.data[c(517, 630, 635, 656, 683), c('relationship', 'home.lang', 'both.parents', 
                                        'rel.parents', 'phone.own', 'parmon3', 
                                        'phone.teach', 'iep', 'hisp')]

# Are there influential observations? (delta deviance plot)
blr_plot_diag_difdev(
  final.model,
  point_color = "blue",
  title = "Delta Deviance Plot",
  xaxis_title = "id",
  yaxis_title = "Delta Deviance"
)

# No influential observations!
