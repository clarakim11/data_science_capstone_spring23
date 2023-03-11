########################################################################
## Data Science Capstone - Data Analysis (all 5 waves)
## Clara Kim
## Mar 10, 2023
########################################################################

library(randomcoloR)
setwd("/Users/clarakim/Desktop/spring23/data_science_capstone_spring23")

clean <- read.csv("clean_wave12345.csv")
cp <- distinctColorPalette(k = 98, altCol = FALSE, runTsne = FALSE)

## rel.parents: potential outcome variable
rel.parents <- clean[, c("w1rel.parents","w2rel.parents","w3rel.parents","w4rel.parents","w5rel.parents")]
plot(as.numeric(rel.parents[1,]), type="o", col = "red", xlab = "Waves", 
     ylab = "Relationship with Parents", ylim=c(5,22),
     main = "Relationship with Parents across Waves")

for (ii in 1:98){
  lines(jitter(as.numeric(rel.parents[ii,])), type = "o", col = cp[ii])
}

# phone.own
phone.own <- clean[, c("w1phone.own","w2phone.own","w3phone.own","w4phone.own","w5phone.own")]
plot(as.numeric(phone.own[1,]), type="o", col = "red", xlab = "Waves", 
     ylab = "phone.own", ylim=c(-0.5,1.5),
     main = "Does the teenager own a phone?")

for (ii in 1:98){
  lines(jitter(as.numeric(phone.own[ii,])), type = "o", col = cp[ii])
}

# home.lang
home.lang <- clean[, c("w1home.lang","w2home.lang","w3home.lang","w4home.lang","w5home.lang")]
plot(as.numeric(home.lang[1,]), type="o", col = "red", xlab = "Waves", 
     ylab = "home.language", ylim=c(-0.5,1.5),
     main = "What is the language spoken at home?")

for (ii in 1:98){
  lines(jitter(as.numeric(home.lang[ii,])), type = "o", col = cp[ii])
}

# parmon3
parmon3 <- clean[, c("w1parmon3","w2parmon3","w3parmon3","w4parmon3","w5parmon3")]
plot(as.numeric(parmon3[1,]), type="o", col = "red", xlab = "Waves", 
     ylab = "Parent Monitoring", ylim=c(0,8),
     main = "Parents have taken away phone or internet use")

for (ii in 1:98){
  lines(jitter(as.numeric(parmon3[ii,])), type = "o", col = cp[ii])
}

# both.parents
both.parents <- clean[, c("w1both.parents","w2both.parents","w3both.parents","w4both.parents","w5both.parents")]
plot(as.numeric(both.parents[1,]), type="o", col = "red", xlab = "Waves", 
     ylab = "both.parents", ylim=c(-0.5,1.5),
     main = "Do both parents live in the same home?")

for (ii in 1:98){
  lines(jitter(as.numeric(both.parents[ii,])), type = "o", col = cp[ii])
}

# phone.teach
phone.teach <- clean[, c("w1phone.teach","w2phone.teach","w3phone.teach","w4phone.teach","w5phone.teach")]
plot(as.numeric(phone.teach[1,]), type="o", col = "red", xlab = "Waves", 
     ylab = "phone.teach", ylim=c(-0.5,1.5),
     main = "Did the teenager learn how to use social media from parents?")

for (ii in 1:98){
  lines(jitter(as.numeric(phone.teach[ii,])), type = "o", col = cp[ii])
}




clean[, c("w1home.lang","w2home.lang","w3home.lang","w4home.lang","w5home.lang")]
home.lang <- clean[, c("w1home.lang","w2home.lang","w3home.lang","w4home.lang","w5home.lang")]

ggplot(home.lang, aes(x=hom, y=len)) + 
  geom_violin(trim=FALSE)

