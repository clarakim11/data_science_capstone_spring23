########################################################################
## Data Science Capstone - Data Cleaning (5 waves)
## Clara Kim
## Mar 10, 2023
########################################################################
library(stringr)
library(ggplot2)

setwd("/Users/clarakim/Desktop/spring23/data_science_capstone_spring23")
og.data <- read.csv("final_data_t12345.csv")
table(data$finalwaves)

data <- og.data[og.data$finalwaves == 12345,] # 108 rows
n <- nrow(data)
clean <- data.frame(matrix(ncol = 0, nrow = 108))

# wave 1
clean$w1home.lang <- with(data, ifelse(homlang == 1, 1, 0))
clean$w1both.parents <- with(data, ifelse(!is.na(hstruct_1) & !is.na(hstruct_2), 1, 0))
clean$w1rel.parents <- data$relpar_1 + data$relpar_2 + data$relpar_3 + data$relpar_4 
clean$w1phone.own <- with(data, ifelse(agesmart == 0, 0, 1))
clean$w1parmon3 <- data$parmon_3
clean$w1phone.teach <- with(data, ifelse(!is.na(smwuse_2) | !is.na(smwuse_3) | !is.na(smwuse_4), 1, 0))
clean$w1iep <- rep("na", n)
clean$w1hisp <- with(data, ifelse(race_3 == 0 | is.na(race_3), 0, 1))

# wave 2
clean$w2home.lang <- with(data, ifelse(w2homlang == 1, 1, 0))
clean$w2both.parents <- with(data, ifelse(!is.na(w2hstruct_1) & !is.na(w2hstruct_2), 1, 0))
clean$w2rel.parents <- data$w2relpar_1 + data$w2relpar_2 + data$w2relpar_3 + data$w2relpar_4 
clean$w2phone.own <- with(data, ifelse(w2agesmart == 0, 0, 1))
clean$w2parmon3 <- data$w2parmon_3
clean$w2phone.teach <- with(data, ifelse(!is.na(w2smwuse_2) | !is.na(w2smwuse_3) | !is.na(w2smwuse_4), 1, 0))
clean$w2iep <- rep("na", n)
clean$w2hisp <- with(data, ifelse(w2hisp == 0 | is.na(w2hisp), 0, 1))

# wave 3
clean$w3home.lang <- with(data, ifelse(w3homlang == 1, 1, 0))
clean$w3both.parents <- with(data, ifelse(!is.na(w3hstruct_1) & !is.na(w3hstruct_2), 1, 0))
clean$w3rel.parents <- data$w3relpar_1 + data$w3relpar_2 + data$w3relpar_3 + data$w3relpar_4 
clean$w3phone.own <- with(data, ifelse(w3agesmart == 0, 0, 1))
clean$w3parmon3 <- data$w3parmon_3
clean$w3phone.teach <- with(data, ifelse(!is.na(w3smwuse_2) | !is.na(w3smwuse_3) | !is.na(w3smwuse_4), 1, 0))
clean$w3iep <- with(data, ifelse(w3iep == 2 | is.na(w3iep), 0, 1))
clean$w3hisp <- with(data, ifelse(w3hisp == 0 | is.na(w3hisp), 0, 1))

# wave 4
#clean$w4relationship <- with(data, ifelse(w4cq01 == 1 | w4cq01 == 2, 1, 0))
clean$w4home.lang <- with(data, ifelse(w4homlang == 1, 1, 0))
clean$w4both.parents <- with(data, ifelse(!is.na(w4hstruct_1) & !is.na(w4hstruct_2), 1, 0))
clean$w4rel.parents <- data$w4relpar_1 + data$w4relpar_2 + data$w4relpar_3 + data$w4relpar_4 
clean$w4phone.own <- with(data, ifelse(w4agesmart == 0, 0, 1))
clean$w4parmon3 <- data$w4parmon_3
clean$w4phone.teach <- with(data, ifelse(!is.na(w4smwuse_2) | !is.na(w4smwuse_3) | !is.na(w4smwuse_4), 1, 0))
clean$w4iep <- with(data, ifelse(w4iep == 2 | is.na(w4iep), 0, 1))
clean$w4hisp <- with(data, ifelse(w4hisp == 0 | is.na(w4hisp), 0, 1))

# wave 5
clean$w5home.lang <- with(data, ifelse(w5homlang == 1, 1, 0))
clean$w5both.parents <- with(data, ifelse(!is.na(w5hstruct_1) & !is.na(w5hstruct_2), 1, 0))
clean$w5rel.parents <- data$w5relpar_1 + data$w5relpar_2 + data$w5relpar_3 + data$w5relpar_4 
clean$w5phone.own <- with(data, ifelse(w5agesmart == 0, 0, 1))
clean$w5parmon3 <- data$w5parmon_3
clean$w5phone.teach <- with(data, ifelse(!is.na(w5smwuse_2) | !is.na(w5smwuse_3) | !is.na(w5smwuse_4), 1, 0))
clean$w5iep <- with(data, ifelse(w5iep == 2 | is.na(w5iep), 0, 1))
clean$w5hisp <- with(data, ifelse(w5hisp == 0 | is.na(w5hisp), 0, 1))

# combine hisp into one data
data[, c("race_3","w2hisp","w3hisp","w4hisp","w5hisp")]
clean$hisp <- data$w2hisp + clean$w3hisp + clean$w4hisp + clean$w5hisp
clean$hisp <- with(data, ifelse(w5hisp == 0 | is.na(w5hisp), 0, 1))

test <- rep(NA, 180)
for(ii in 1:length(colnames(data))){
  test[ii] = str_detect(names(data[ii]), "hisp")
}
df <- data.frame(tf = test, names=colnames(data))
df[df$tf==TRUE,]


# remove 10 rows with missing data
sum(is.na(clean))
apply(is.na(clean), 2, which) 
sum(!complete.cases(clean)) # 10 rows with NA values
clean <-clean[complete.cases(clean),]

# write the clean data into a .csv file
write.csv(clean, file="clean_wave12345.csv", row.names=FALSE)

