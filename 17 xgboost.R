----#library----

library(xgboost)

drat:::addRepo("dmlc")
library(caret)
library(xgboost)
library(readr)
library(dplyr)
library(tidyr)

---#xgboost test----

loglossobj <- function(preds, dtrain) {
  # dtrain is the internal format of the training data
  # We extract the labels from the training data
  labels <- getinfo(dtrain, "label")
  # We compute the 1st and 2nd gradient, as grad and hess
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  # Return the result as a list
  return(list(grad = grad, hess = hess))
}


class(abril)
clase <- as.numeric(ifelse(abril_dataset_training$clase_binaria1=="POS",1,0))
data_xg <- data.matrix(abril_dataset_training[,-which(names(abril_dataset_training) %in% c("clase_binaria1"))])
data[is.nan(data)] <- NA
head(clase)
# xgboost fitting with arbitrary parameters
xgb_params_1 = list(
  objective = "binary:logistic",                                               # binary classification
  eta = 0.01,                                                                  # learning rate
  max.depth = 3,                                                               # max tree depth
  eval_metric = "auc"                                                          # evaluation/loss metric
)



# fit the model with the arbitrary parameters specified above
xgb_1 = xgboost(data = data_xg,
                label = clase,
                params = xgb_params_1,
                nrounds = 100,                                                 # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 10                                         # stop if no improvement within 10 trees
)



# xgboost fitting with arbitrary parameters
xgb_params_2 = list(
  objective = "multi:softprob",                                               # binary classification                                                                 # learning rate                                                               # max tree depth                                                          # evaluation/loss metric
  num_class=2,eta = 0.01, 
  subsample = 0.7, 
  colsample_bytree = 0.4, 
  min_child_weight = vmin_child_weight, 
  max_depth = vmax_depth,
  alpha = 0, lambda = 0.1, gamma = 0.01,
  nround= vnround, 
  eval_metric = "merror",
  num_class = 2,
  objective = "multi:softprob"  )

xgb_1 = xgboost(data = data_xg,
                label = clase,
                params = xgb_params_2,
                nrounds = 100,                                                 # max number of trees to build
                verbose = TRUE,                                         
                print.every.n = 1,
                early.stop.round = 10                                        # stop if no improvement within 10 trees
                ,num_class = 2,,missing = NaN)


# cross-validate xgboost to get the accurate measure of error
xgb_cv_1 = xgb.cv(params = xgb_params_1,
                  data = data_xg,
                  label = clase,
                  nrounds = 100, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10,missing = NaN
)

# plot the AUC for the training and testing samples
xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()

xgb_cv_1$dt %>%
  select(-contains("std")) %>%
  mutate(IterationNum = 1:n()) %>%
  gather(TestOrTrain, AUC, -IterationNum) %>%
  ggplot(aes(x = IterationNum, y = AUC, group = TestOrTrain, color = TestOrTrain)) + 
  geom_line() + 
  theme_bw()

#entrenar

xgb_cv_1 = xgboost(params = xgb_params_1,
                  data = data_xg,
                  label = clase,
                  nrounds = 100, 
                  nfold = 5,                                                   # number of folds in K-fold
                  prediction = TRUE,                                           # return the prediction using the final model 
                  showsd = TRUE,                                               # standard deviation of loss across folds
                  stratified = TRUE,                                           # sample is unbalanced; use stratified sampling
                  verbose = TRUE,
                  print.every.n = 1, 
                  early.stop.round = 10,missing = NaN
)




#predict
clasep <- as.numeric(ifelse(abril_dataset_testing$clase_binaria1=="POS",1,0))
data_xgp <- data.matrix(abril_dataset_testing[,-which(names(abril_dataset_testing) %in% c("clase_binaria1"))])
class(data_xgp)
data_xgp[is.nan(data_xgp)] <- -9999999.0

xg_predict <- predict(xgb_cv_1,new=data_xgp,missing= NaN)
predict_xgbooxt <- cbind.data.frame(clase_binaria1=ifelse(xg_predict>0.3,"POS","NEG"),porcentaje=xg_predict)
table(predict_xgbooxt$clase_binaria1,abril_dataset_testing$clase_binaria1)

class(xgb_cv_1)
data_xgp_m <- xgb.DMatrix(data_xg,missing = NaN)
# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid(
  nrounds = 100,
  eta = c(0.01, 0.001),
  max_depth = c(6, 8, 10),
  gamma = 1,
  colsample_bytree=c(0.4,0.7),
  min_child_weight=c(0.5,1,1.5)
)


data_xg[is.nan(data_xg)] <- 0


head(is.nan(data_xg)==TRUE)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 5,
  verboseIter = TRUE,                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             returnData = FALSE,
  returnResamp = "all",                                                        # save losses across all models
  classProbs = TRUE,                                                           # set to TRUE for AUC to be computed
  summaryFunction = twoClassSummary,
  allowParallel = TRUE
)

clase_xg <- factor(ifelse(clase==1,"POS","NEG"))
# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb_train_1 = train(
  x = data_xg,
  y = clase_xg,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree"
)

# scatter plot of the AUC against max_depth and eta
ggplot(xgb_train_1$results, aes(x = as.factor(eta), y = max_depth, size = ROC, color = ROC)) + 
  geom_point() + 
  theme_bw() + 
  scale_size_continuous(guide = "none")

ganancia(xg_predict,abril_dataset_testing$clase_binaria1,0.3) 


-----#intento 3----

outcome <- c("clase_binaria1")
predictors <- names(abril)[!names(abril)%in% outcome]

#take 10% of data

trainPortion <- floor(nrow(abril)*0.1)
abril_dataset_training <- abril[1:floor(trainPortion/2),]
abril_dataset_testing <- abril[(floor(trainPortion/2)+1):trainPortion,]

dim(abril_dataset_training)

smallestError <- 100
for (depth in seq(1,10,1)) {
  for(rounds in seq(1,20,1)){
  #train
  bst <- xgboost(data = as.matrix(abril_dataset_training[,predictors]),
                 label = abril_dataset_training[,outcome],
                 max.depth=depth,
                 nrounds = rounds,
                 objective=""
                 )
  }
}

---#iteraciones modificadas----


levels(abril$clase_binaria1) <- c("POS","NEG")

#definicion  funcion ganancia para nuestro problema

ganancia = function( probs, clases, prob )
{
  suma = 0 ;
  largo = length( clases ) ;
  
  for( i in 1:largo )
  {
    if( probs[ 2*i ]  > prob   ){ suma <- suma + if( clases[i]=="POS" ) { 7750 } else { -250 }  
    } ;
  }
  
  return( suma )
}



archivo_entrada <- "abril_binario1.txt"
archivo_salida  <- "salida_xgboost_one_modificado.txt"



#escribo los  titulos  del archivo salida
if( !file.exists( archivo_salida) )
{
  cat( "fecha", "archivo", "algoritmo", "nulos", "vmax_depth", "vmin_child_weight", "vnround",  "ganancia_promedio", "tiempo_promedio", "ganancias" ,  "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1



abril_dataset <- read.table( archivo_entrada, header=TRUE, sep="\t", row.names="numero_de_cliente")


#quito campos que son constantes
drops <- c("foto_mes","tpaquete1", "tpaquete3", "tpaquete5", "tpaquete8", "tautoservicio", "tcajas_consultas", "participa" )
abril_dataset <- abril_dataset[ , !(names(abril_dataset) %in% drops)]


#imputo los nulos, ya que xgboost  no acepta nulos
abril[ is.na(abril)] <- 0
vmetodo_imputacion <- "ceros"
abril$problemafin <- factor(abril$problemafin)

semilla <- c( 102191, 200177, 410551, 552581, 892237 )

linea <- 1


for(  vmax_depth  in  c( 3, 4) )
{
  for(  vmin_child_weight  in  c( 1, 5, 10) )
  {
    
    
    ganancias <- c() 
    tiempos   <- c()
    
    vnround <- 1000	
    
    
    
    if( linea > lineas_archivo  )
    {
      for( s in  1:5 )
      {
        # Genero training y testing con 70% , 30%
        set.seed( semilla[s] )
        abril_inTraining <- createDataPartition( abril$clase_binaria1, p = .70, list = FALSE)
        abril_dataset_training <- abril[ abril_inTraining,]
        abril_dataset_testing  <- abril[-abril_inTraining,]
        
        
        # generacion del modelo sobre los datos de training
        t0 =  Sys.time()
        
        abril_dataset_training_sinclase <-   abril_dataset_training[ , !(names(abril_dataset_training) %in% c("clase_binaria1") ) ] 
        abril_modelo   <- xgboost( 
          data = data.matrix( abril_dataset_training_sinclase ), 
          label = data.matrix( as.numeric(  (abril_dataset_training$clase_binaria1)=="POS" )  ), 
          eta = 0.01, 
          subsample = 0.7, 
          colsample_bytree = 0.4, 
          min_child_weight = vmin_child_weight, 
          max_depth = vmax_depth,
          alpha = 0, lambda = 0.1, gamma = 0.01,
          nround= vnround, 
          eval_metric = "auc",
          num_class = 2,
          objective='multi:softprob' 
          ,scale_pos_weight = 31
        )
        
        t1 = Sys.time()
        tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")
        
        # calculo la ganancia normalizada  en testing
        
        abril_dataset_testing_sinclase <-   abril_dataset_testing[ , !(names(abril_dataset_testing) %in% c("clase_binaria1") ) ] 
        am <-  data.matrix( abril_dataset_testing_sinclase  )
        
        for( i in 1:50 )
        {
          abril_testing_prediccion  = predict(  abril_modelo,  am ,  ntreelimit= i*20 )
          ganancias[ i*5 + s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase_binaria1,  0.03125  ) / 0.30
          
        }
        
      }
      
      for( i in 1:50 )
      {
        cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "xgboost", vmetodo_imputacion,  vmax_depth, vmin_child_weight, i*20, mean( c(ganancias[i*5+1], ganancias[i*5+2], ganancias[i*5+3], ganancias[i*5+4], ganancias[i*5+5] )), mean(tiempos), ganancias[i*5+1], ganancias[i*5+2], ganancias[i*5+3], ganancias[i*5+4], ganancias[i*5+5], "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )
      }
      
      
    }
    linea <-  linea + 50
  }
}