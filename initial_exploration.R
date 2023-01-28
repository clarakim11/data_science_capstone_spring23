
library(car)
library(MASS)

original <- read.csv("/Users/clarakim/Desktop/spring23/data_science_capstone_spring23/data/original_data.csv")
recoded <- read.csv("/Users/clarakim/Desktop/spring23/data_science_capstone_spring23/data/recoded.csv")
length(recoded$bodimg.score) # 968 entries
table(recoded$bodimg.score) 
hist(recoded$bodimg.score, main="Histogram of Response Variable (Body Image Score)")
sum(is.na(recoded$bodimg.score)) # 137 NA values

# drop rows that are missing the outcome variable
recoded <- recoded[!is.na(recoded$bodimg.score), ]
full.model <- lm(bodimg.score ~ ., data=recoded)

# check for multicollinearity issue
vif(full.model,th=10)

stepwise.model.AIC <- stepAIC(full.model, k = 2, trace = FALSE)
stepwise.model.BIC <- stepAIC(full.model, k = log(dim(recoded)[1]), trace = FALSE)
