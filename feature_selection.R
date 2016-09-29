----#librerias----

# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)

----#Identify highly correlated features in caret r package-----
filtro <- my_data_status$variable[my_data_status$p_zeros<100 & my_data_status$type %in% c("numeric","integer") & !(my_data_status$variable %in% c("foto_mes","tpaquete1"))]

  #calculate correlation matrix
  correlationMatrix <- cor(subset(abril_dataset,select = filtro),use="pairwise.complete.obs")
  # summarize the correlation matrix
  print(correlationMatrix)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated <- findCorrelation(correlationMatrix[,colSums(is.na(correlationMatrix))==0], cutoff=0.5)
  # print indexes of highly correlated attributes
  print(highlyCorrelated)

  
-----#Rank features by importance using the caret r package-----
  
  # prepare training scheme
  control <- trainControl(method="repeatedcv", number=10, repeats=3)
  
  # train the model
  model <- train(clase~., data=abril_dataset_training, method="lvq",na.action=na.omit, preProcess="scale", trControl=control)
  # estimate variable importance
  importance <- varImp(model, scale=FALSE)
  # summarize importance
  print(importance)
  # plot importance
  plot(importance)
  
  -----#Automatically select features using Caret R Package----
  
  # ensure the results are repeatable
  set.seed(7)
  # define the control using a random forest selection function
  control <- rfeControl(functions=rfFuncs, method="cv", number=10)
  rfe()
  # run the RFE algorithm
  results <- rfe(as.data.frame(abril_dataset_training)[,2:124], as.data.frame(abril_dataset_training)[,169], sizes=c(1:50),na.action=na.omit, rfeControl=control)
  # summarize the results
  print(results)
  # list the chosen features
  predictors(results)
  # plot the results
  plot(results, type=c("g", "o"))
  