----#fun modeling steps----
suppressMessages(library(funModeling))

my_data_status=df_status(abril)

# Removing variables with 100% of zero values
vars_to_remove=subset(my_data_status, my_data_status$p_zeros ==100)
vars_to_remove["variable"]

abril <- abril[,!(names(abril) %in% vars_to_remove[,"variable"])]
abril$participa <- factor(abril$participa)
abril$problemafin <- factor(abril$problemafin)

correlation_table(data=abril, str_target="clase_binaria1")

----#imbalanced classes-----

library(caret)

table(abril_dataset_training$clase_binaria1)

set.seed(9560)

up_train <- upSample(x = abril_dataset_training[, !(names(abril_dataset_training) %in% c("clase_binaria1"))],
                     y = abril_dataset_training$clase_binaria1)                         
table(up_train$Class) 

abril_dataset_training[is.na(abril_dataset_training)] <- -999999.0
vars_to_remove=subset(my_data_status, my_data_status$unique ==1)
abril <- abril[,!(names(abril) %in% vars_to_remove[,"variable"])]
abril[which(abril==Inf)] <- 0
set.seed( 9560 )
abril_inTraining <- createDataPartition( abril$clase_binaria1, p = .70, list = FALSE)
abril_dataset_training <- abril[ abril_inTraining,]
abril_dataset_testing <- abril[-abril_inTraining,]


library(DMwR)

set.seed(9560)
smote_train <- SMOTE(clase_binaria1 ~ ., data  = abril_dataset_training)                         
table(smote_train$clase_binaria1) 


library(ROSE)

set.seed(9560)
rose_train <- ROSE(clase_binaria1 ~ ., data  = abril_dataset_training)$data                         
table(rose_train$clase_binaria1)