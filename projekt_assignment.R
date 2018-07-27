library(dplyr); library(ggplot2); library(caret)

#main <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
#                     stringsAsFactors = F,
#                     na.strings = c("NA", ""))
#save(main, file = "C:/Kurser/Machine_Learning/main.Rdata")
load("C:/Kurser/Machine_Learning/main.Rdata")
quiz_set <- read.csv("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                     stringsAsFactors = F)

manyNAs <- unname(which(sapply(main, function(x) sum(is.na(x))) >= 0.95 * nrow(main)))
main <- main[,-manyNAs]

main2 <- main %>%
  select(-X, -cvtd_timestamp) %>%
  mutate(user_name = as.factor(user_name),
         classe = as.factor(classe),
         new_window = as.factor(new_window),
         #raw_timestamp_part_2 = as.character(raw_timestamp_part_2),
         raw_timestamp_part_1 = substr(raw_timestamp_part_1, 7, 10),
         raw_timestamp_part_2 = ifelse(nchar(raw_timestamp_part_2) < 6, 
                                       paste0(do.call(paste0, as.list(rep("0", 3))), raw_timestamp_part_2),
                                       raw_timestamp_part_2),
         raw_timestamp_part_2 = substr(raw_timestamp_part_2, 
                                       nchar(raw_timestamp_part_2)-5, 
                                       nchar(raw_timestamp_part_2)),
         time_stamp = paste(raw_timestamp_part_1, substr(raw_timestamp_part_2, 1, 5), sep = "."),
         time_stamp = as.numeric(time_stamp)) %>%
  select(-raw_timestamp_part_1, -raw_timestamp_part_2) %>%
  group_by(user_name, classe) %>%
  mutate(time_stamp = time_stamp - min(time_stamp)) %>%
  ungroup()

set.seed(26062018)
inTrain <- createDataPartition(main$classe, p = 0.80, list = F)

training <- main2[inTrain,]
pcaObj <- preProcess(x = training[-c(1:3, 56:57)], method = "pca")

trainingPca <- predict(pcaObj, training) %>%
  select(-(user_name:num_window), -time_stamp)

testing <- main2[-inTrain,]

testingPca <- predict(pcaObj, testing) %>%
  select(-(user_name:num_window), -time_stamp)

#fit1 <- train(classe ~ ., data = trainingPca, method = "rf", trControl = trainControl(method = "cv", number = 10))
#save(fit1, file = "C:/Kurser/Machine_Learning/fit1.Rdata")
load("C:/Kurser/Machine_Learning/fit1.Rdata")

conf_test <- confusionMatrix(testingPca$classe, predict(fit1, testingPca))

knitr::kable(as.matrix(conf_test)/apply(conf_test$table, 2, sum))


