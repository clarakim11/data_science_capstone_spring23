########################################################################
## Data Science Capstone - Data Cleaning (5 waves)
## Clara Kim
## Apr 10, 2023
########################################################################
library(stringr, verbose=FALSE)
library(ggplot2, verbose=FALSE)

setwd("/Users/clarakim/Desktop/spring23/data_science_capstone_spring23")
og.data <- read.csv("data/final_data_t12345.csv")
table(og.data$finalwaves)

data <- og.data[og.data$finalwaves == 12345,] # 108 rows
n <- nrow(data)
clean <- data.frame(matrix(ncol = 0, nrow = 108))

# wave1var <- data[,3:577]
# wave2var <- data[,578:1441]
# wave3var <- data[,1442:1923]
# wave4var <- data[,1924:2503]
# wave5var <- data[,2734:3458]


wave1var <- data[,c("homlang","hstruct_1","hstruct_2","hstruct_3","hstruct_4","hstruct_5",
                 "hstruct_6","hstruct_7","hstruct_8","hstruct_9","hstruct_10","momed",
                 "relpar_1","relpar_2","relpar_3","relpar_4","relpar_5","relpar_6",
                 "relpar_7","sch_1","sch_2","sch_3","agesmart","parmon_1","parmon_2",
                 "parmon_3","parmon_4","parmon_5","parmon_6","parmon_7","parmon_8",
                 "parmon3","smwuse_1","smwuse_2","smwuse_3","smwuse_4","smwuse_5",
                 "smwuse_6","smwuse_7","smwuse_8","smwuse_9","smwuse_10","smwuse_11",
                 "smwuse_12","gameplay_1","gameplay_2","gamebeh_2","grade","schgra","race_1")]

wave2var <- data[,c("w2homlang","w2hstruct_1","w2hstruct_2","w2hstruct_3","w2hstruct_4","w2hstruct_5",
                        "w2hstruct_6","w2hstruct_7","w2hstruct_8","w2hstruct_9","w2hstruct_10","w2momed",
                        "w2relpar_1","w2relpar_2","w2relpar_3","w2relpar_4","w2relpar_5","w2relpar_6",
                        "w2relpar_7","w2sch_1","w2sch_2","w2sch_3","w2agesmart","w2parmon_1","w2parmon_2",
                        "w2parmon_3","w2parmon_4","w2parmon_5","w2parmon_6","w2parmon_7","w2parmon_8",
                        "w2parmon3","w2smwuse_1","w2smwuse_2","w2smwuse_3","w2smwuse_4","w2smwuse_5",
                        "w2smwuse_6","w2smwuse_7","w2smwuse_8","w2smwuse_9","w2smwuse_10","w2smwuse_11",
                        "w2smwuse_12","w2gameplay_1","w2gameplay_2","w2gamebeh_2","w2grade","w2schgra","w2race_1")]

wave3var <- data[,c("w3homlang","w3hstruct_1","w3hstruct_2","w3hstruct_3","w3hstruct_4","w3hstruct_5",
                        "w3hstruct_6","w3hstruct_7","w3hstruct_8","w3hstruct_9","w3hstruct_10","w3momed",
                        "w3relpar_1","w3relpar_2","w3relpar_3","w3relpar_4","w3relpar_5","w3relpar_6",
                        "w3relpar_7","w3sch_1","w3sch_2","w3sch_3","w3agesmart","w3parmon_1","w3parmon_2",
                        "w3parmon_3","w3parmon_4","w3parmon_5","w3parmon_6","w3parmon_7","w3parmon_8",
                        "w3parmon3","w3smwuse_1","w3smwuse_2","w3smwuse_3","w3smwuse_4","w3smwuse_5",
                        "w3smwuse_6","w3smwuse_7","w3smwuse_8","w3smwuse_9","w3smwuse_10","w3smwuse_11",
                        "w3smwuse_12","w3gameplay_1","w3gameplay_2","w3gamebeh_2","w3grade","w3schgra","w3race_1")]

wave4var <- data[,c("w4homlang","w4hstruct_1","w4hstruct_2","w4hstruct_3","w4hstruct_4","w4hstruct_5",
                        "w4hstruct_6","w4hstruct_7","w4hstruct_8","w4hstruct_9","w4hstruct_10","w4momed",
                        "w4relpar_1","w4relpar_2","w4relpar_3","w4relpar_4","w4relpar_5","w4relpar_6",
                        "w4relpar_7","w4sch_1","w4sch_2","w4sch_3","w4agesmart","w4parmon_1","w4parmon_2",
                        "w4parmon_3","w4parmon_4","w4parmon_5","w4parmon_6","w4parmon_7","w4parmon_8",
                        "w4parmon3","w4smwuse_1","w4smwuse_2","w4smwuse_3","w4smwuse_4","w4smwuse_5",
                        "w4smwuse_6","w4smwuse_7","w4smwuse_8","w4smwuse_9","w4smwuse_10","w4smwuse_11",
                        "w4smwuse_12","w4gameplay_1","w4gameplay_2","w4gamebeh_2","w4grade","w4schgra","w4race_1")]

wave5var <- data[,c("w5homlang","w5hstruct_1","w5hstruct_2","w5hstruct_3","w5hstruct_4","w5hstruct_5",
                        "w5hstruct_6","w5hstruct_7","w5hstruct_8","w5hstruct_9","w5hstruct_10","w5momed",
                        "w5relpar_1","w5relpar_2","w5relpar_3","w5relpar_4","w5relpar_5","w5relpar_6",
                        "w5relpar_7","w5sch_1","w5sch_2","w5sch_3","w5agesmart","w5parmon_1","w5parmon_2",
                        "w5parmon_3","w5parmon_4","w5parmon_5","w5parmon_6","w5parmon_7","w5parmon_8",
                        "w5parmon3","w5smwuse_1","w5smwuse_2","w5smwuse_3","w5smwuse_4","w5smwuse_5",
                        "w5smwuse_6","w5smwuse_7","w5smwuse_8","w5smwuse_9","w5smwuse_10","w5smwuse_11",
                        "w5smwuse_12","w5gameplay_1","w5gameplay_2","w5gamebeh_2","w5grade","w5schgra","w5race_1")]

# wave 1
clean$home.lang <- with(data, ifelse(homlang == 1, 1, 0))
clean$both.parents <- with(data, ifelse(!is.na(hstruct_1) & !is.na(hstruct_2), 1, 0))
clean$siblings <- with(data, ifelse(!is.na(hstruct_3) | !is.na(hstruct_4) | !is.na(hstruct_5) | !is.na(hstruct_6), 1, 0))
clean$momed.hs <- with(data, ifelse(momed > 1, 1, 0))
clean$momed.college <- with(data, ifelse(momed > 3, 1, 0))
clean$rel.parents <- data$relpar_1 + data$relpar_2 + data$relpar_3 + data$relpar_4 
clean$phone.use <- data$relpar_5 + data$relpar_6 + data$relpar_7
clean$phone.own <- with(data, ifelse(agesmart == 0, 0, 1))
clean$parmon1 <- data$parmon_1
clean$parmon2 <- data$parmon_2
clean$parmon3 <- data$parmon_3
clean$parmon4 <- data$parmon_4
clean$parmon5 <- data$parmon_5
clean$parmon6 <- data$parmon_6
clean$parmon7 <- data$parmon_7
clean$parmon8 <- data$parmon_8
clean$phone.limit <- with(data, ifelse(parmon3 == 8, 0, 1))
clean$phone.teach <- with(data, ifelse(!is.na(smwuse_2) | !is.na(smwuse_3) | !is.na(smwuse_4), 1, 0))
clean$gameplay1 <- data$gameplay_1
clean$gameplay2 <- data$gameplay_2
clean$game.effect <- data$gamebeh_2
clean$grade <- data$grade
clean$grades.rec <- data$schgra
clean$white <- with(data, ifelse(!is.na(race_1), 1, 0))

# wave 2
clean$w2home.lang <- with(data, ifelse(w2homlang == 1, 1, 0))
clean$w2both.parents <- with(data, ifelse(!is.na(w2hstruct_1) & !is.na(w2hstruct_2), 1, 0))
clean$w2siblings <- with(data, ifelse(!is.na(w2hstruct_3) | !is.na(w2hstruct_4) | !is.na(w2hstruct_5) | !is.na(w4hstruct_6), 1, 0))
clean$w2momed.hs <- with(data, ifelse(w2momed > 1, 1, 0))
clean$w2momed.college <- with(data, ifelse(w2momed > 3, 1, 0))
clean$w2rel.parents <- data$w2relpar_1 + data$w2relpar_2 + data$w2relpar_3 + data$w2relpar_4 
clean$w2phone.use <- data$w2relpar_5 + data$w2relpar_6 + data$w2relpar_7
clean$w2phone.own <- with(data, ifelse(w2agesmart == 0, 0, 1))
clean$w2parmon1 <- data$w2parmon_1
clean$w2parmon2 <- data$w2parmon_2
clean$w2parmon3 <- data$w2parmon_3
clean$w2parmon4 <- data$w2parmon_4
clean$w2parmon5 <- data$w2parmon_5
clean$w2parmon6 <- data$w2parmon_6
clean$w2parmon7 <- data$w2parmon_7
clean$w2parmon8 <- data$w2parmon_8
clean$w2phone.limit <- with(data, ifelse(w2parmon3 == 8, 0, 1))
clean$w2phone.teach <- with(data, ifelse(!is.na(w2smwuse_2) | !is.na(w2smwuse_3) | !is.na(w2smwuse_4), 1, 0))
clean$w2gameplay1 <- data$w2gameplay_1
clean$w2gameplay2 <- data$w2gameplay_2
clean$w2game.effect <- data$w2gamebeh_2
clean$w2grade <- data$w2grade
clean$w2grades.rec <- data$w2schgra
clean$w2white <- with(data, ifelse(!is.na(w2race_1), 1, 0))

# wave 3 
clean$w3home.lang <- with(data, ifelse(w3homlang == 1, 1, 0))
clean$w3both.parents <- with(data, ifelse(!is.na(w3hstruct_1) & !is.na(w3hstruct_2), 1, 0))
clean$w3siblings <- with(data, ifelse(!is.na(w3hstruct_3) | !is.na(w3hstruct_4) | !is.na(w3hstruct_5) | !is.na(w3hstruct_6), 1, 0))
clean$w3momed.hs <- with(data, ifelse(w3momed > 1, 1, 0))
clean$w3momed.college <- with(data, ifelse(w3momed > 3, 1, 0))
clean$w3rel.parents <- data$w3relpar_1 + data$w3relpar_2 + data$w3relpar_3 + data$w3relpar_4 
clean$w3phone.use <- data$w3relpar_5 + data$w3relpar_6 + data$w3relpar_7
clean$w3phone.own <- with(data, ifelse(w3agesmart == 0, 0, 1))
clean$w3parmon1 <- data$w3parmon_1
clean$w3parmon2 <- data$w3parmon_2
clean$w3parmon3 <- data$w3parmon_3
clean$w3parmon4 <- data$w3parmon_4
clean$w3parmon5 <- data$w3parmon_5
clean$w3parmon6 <- data$w3parmon_6
clean$w3parmon7 <- data$w3parmon_7
clean$w3parmon8 <- data$w3parmon_8
clean$w3phone.limit <- with(data, ifelse(w3parmon3 == 8, 0, 1))
clean$w3phone.teach <- with(data, ifelse(!is.na(w3smwuse_2) | !is.na(w3smwuse_3) | !is.na(w3smwuse_4), 1, 0))
clean$w3gameplay1 <- data$w3gameplay_1
clean$w3gameplay2 <- data$w3gameplay_2
clean$w3game.effect <- data$w3gamebeh_2
clean$w3grade <- data$w3grade
clean$w3grades.rec <- data$w3schgra
clean$w3white <- with(data, ifelse(!is.na(w3race_1), 1, 0))

# wave 4
clean$w4home.lang <- with(data, ifelse(w4homlang == 1, 1, 0))
clean$w4both.parents <- with(data, ifelse(!is.na(w4hstruct_1) & !is.na(w4hstruct_2), 1, 0))
clean$w4siblings <- with(data, ifelse(!is.na(w4hstruct_3) | !is.na(w4hstruct_4) | !is.na(w4hstruct_5) | !is.na(w4hstruct_6), 1, 0))
clean$w4momed.hs <- with(data, ifelse(w4momed > 1, 1, 0))
clean$w4momed.college <- with(data, ifelse(w4momed > 3, 1, 0))
clean$w4rel.parents <- data$w4relpar_1 + data$w4relpar_2 + data$w4relpar_3 + data$w4relpar_4 
clean$w4phone.use <- data$w4relpar_5 + data$w4relpar_6 + data$w4relpar_7
clean$w4phone.own <- with(data, ifelse(w4agesmart == 0, 0, 1))
clean$w4parmon1 <- data$w4parmon_1
clean$w4parmon2 <- data$w4parmon_2
clean$w4parmon3 <- data$w4parmon_3
clean$w4parmon4 <- data$w4parmon_4
clean$w4parmon5 <- data$w4parmon_5
clean$w4parmon6 <- data$w4parmon_6
clean$w4parmon7 <- data$w4parmon_7
clean$w4parmon8 <- data$w4parmon_8
clean$w4phone.limit <- with(data, ifelse(w4parmon3 == 8, 0, 1))
clean$w4phone.teach <- with(data, ifelse(!is.na(w4smwuse_2) | !is.na(w4smwuse_3) | !is.na(w4smwuse_4), 1, 0))
clean$w4gameplay1 <- data$w4gameplay_1
clean$w4gameplay2 <- data$w4gameplay_2
clean$w4game.effect <- data$w4gamebeh_2
clean$w4grade <- data$w4grade
clean$w4grades.rec <- data$w4schgra
clean$w4white <- with(data, ifelse(!is.na(w4race_1), 1, 0))

# wave 5
clean$w5home.lang <- with(data, ifelse(w5homlang == 1, 1, 0))
clean$w5both.parents <- with(data, ifelse(!is.na(w5hstruct_1) & !is.na(w5hstruct_2), 1, 0))
clean$w5siblings <- with(data, ifelse(!is.na(w5hstruct_3) | !is.na(w5hstruct_4) | !is.na(w5hstruct_5) | !is.na(w5hstruct_6), 1, 0))
clean$w5momed.hs <- with(data, ifelse(w5momed > 1, 1, 0))
clean$w5momed.college <- with(data, ifelse(w5momed > 3, 1, 0))
clean$w5rel.parents <- data$w5relpar_1 + data$w5relpar_2 + data$w5relpar_3 + data$w5relpar_4 
clean$w5phone.use <- data$w5relpar_5 + data$w5relpar_6 + data$w5relpar_7
clean$w5phone.own <- with(data, ifelse(w5agesmart == 0, 0, 1))
clean$w5parmon1 <- data$w5parmon_1
clean$w5parmon2 <- data$w5parmon_2
clean$w5parmon3 <- data$w5parmon_3
clean$w5parmon4 <- data$w5parmon_4
clean$w5parmon5 <- data$w5parmon_5
clean$w5parmon6 <- data$w5parmon_6
clean$w5parmon7 <- data$w5parmon_7
clean$w5parmon8 <- data$w5parmon_8
clean$w5phone.limit <- with(data, ifelse(w5parmon3 == 8, 0, 1))
clean$w5phone.teach <- with(data, ifelse(!is.na(w5smwuse_2) | !is.na(w5smwuse_3) | !is.na(w5smwuse_4), 1, 0))
clean$w5gameplay1 <- data$w5gameplay_1
clean$w5gameplay2 <- data$w5gameplay_2
clean$w5game.effect <- data$w5gamebeh_2
clean$w5grade <- data$w5grade
clean$w5grades.rec <- data$w5schgra
clean$w5white <- with(data, ifelse(!is.na(w5race_1), 1, 0))

write.csv(clean, "clean_wave12345_allvar.csv", quote=FALSE, row.names=FALSE)

