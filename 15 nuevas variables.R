----#library----

library(lubridate)
library(dplyr)
library(caret)
library(ranger)

abril$clase_binaria1 <- factor(ifelse(abril$clase=="BAJA+2","BAJA+2","CONTINUA"))
abril$clase <- NULL

-----#operacion sobre fecha----

last_day <- function(date) {
  ceiling_date(date, "month") - days(1)
}

abril$cant_inicio_mora <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$visa_finiciomora),format="%Y%m%d"))
abril$visa_finiciomora <- NULL

abril$cant_inicio_mora_master <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$master_finiciomora),format="%Y%m%d"))
abril$master_finiciomora <- NULL

abril$cant_master_fvencimiento <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$master_fvencimiento),format="%Y%m%d"))
abril$master_fvencimiento <- NULL

abril$cant_visa_fechaalta <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$visa_fechaalta),format="%Y%m%d"))
abril$visa_fechaalta <- NULL

abril$cant_visa_fvencimiento <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$visa_fvencimiento),format="%Y%m%d"))
abril$visa_fvencimiento <- NULL

abril$cant_visa_fultimo_cierre <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$visa_fultimo_cierre),format="%Y%m%d"))
abril$visa_fultimo_cierre <- NULL

abril$cant_master_fultimo_cierre <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$master_fultimo_cierre),format="%Y%m%d"))
abril$master_fultimo_cierre <- NULL

abril$cant_master_fechaalta <- as.integer(last_day(as.Date(as.character(abril$foto_mes*100+01),format="%Y%m%d")) - as.Date(as.character(abril$master_fechaalta),format="%Y%m%d"))
abril$master_fechaalta <- NULL


  abril$visa_cuenta_estado.Visa_mpagospesos <- abril$visa_cuenta_estado*abril$visa_mpagospesos
  abril$visa_marca_atraso.ttarjeta_visa <- abril$visa_marca_atraso*abril$ttarjeta_visa
  abril$visa_marca_atraso.cant_inicio_mora_master <- abril$visa_marca_atraso*abril$cant_inicio_mora_master
  abril$visa_marca_atraso.tcallcenter <- abril$visa_marca_atraso*abril$tcallcenter
  abril$visa_marca_atraso.tmovimientos_ultimos90dias <- abril$visa_marca_atraso*abril$tmovimientos_ultimos90dias
  abril$visa_marca_atraso.tcajas <- abril$visa_marca_atraso*abril$tcajas
  abril$ttarjeta_visa.mcuenta_corriente_paquete <- abril$ttarjeta_visa*abril$mcuenta_corriente_paquete
  abril$ttarjeta_visa.visa_mpagospesos <- abril$ttarjeta_visa*abril$visa_mpagospesos
  abril$ttarjeta_visa.cant_inicio_mora_master <- abril$ttarjeta_visa*abril$cant_inicio_mora_master
  abril$ttarjeta_visa.tmovimientos_ultimos90dias <- abril$ttarjeta_visa*abril$tmovimientos_ultimos90dias
  abril$mcuenta_corriente_paquete.chomebanking_transacciones <- abril$mcuenta_corriente_paquete*abril$chomebanking_transacciones
  abril$mactivos_margen.cant_inicio_mora_master <- abril$mactivos_margen*abril$cant_inicio_mora_master
  abril$mactivos_margen.mcaja_ahorro_paquete <- abril$mactivos_margen*abril$mcaja_ahorro_paquete
  abril$mactivos_margen.chomebanking_transaccioneschomebanking_transacciones <- abril$mactivos_margen*abril$chomebanking_transacciones
  abril$visa_mpagospesos.mcaja_ahorro_paquete <- abril$visa_mpagospesos*abril$mcaja_ahorro_paquete
  abril$visa_mpagospesos.tmovimientos_ultimos90dias <- abril$visa_mpagospesos*abril$tmovimientos_ultimos90dias
  abril$cant_inicio_mora_master.tmovimientos_ultimos90dias <- abril$cant_inicio_mora_master*abril$tmovimientos_ultimos90dias
  abril$tcallcenter.tmovimientos_ultimos90dias <- abril$tcallcenter*abril$tmovimientos_ultimos90dias
  abril$mtarjeta_visa_consumo.tmovimientos_ultimos90dias <- abril$mtarjeta_visa_consumo*abril$tmovimientos_ultimos90dias


----#fun modeling steps----
suppressMessages(library(funModeling))

my_data_status=df_status(abril)

# Removing variables with 100% of zero values
vars_to_remove=subset(my_data_status, my_data_status$p_zeros ==100)
vars_to_remove["variable"]

abril <- abril[,!(names(abril) %in% vars_to_remove[,"variable"])]
abril$participa <- factor(abril$participa)
abril$problemafin <- factor(abril$problemafin)

abril[is.na(abril)] <- -9999999.0
abril$problemafin <- ifelse(is.na(abril$problemafin)==TRUE,"NO","SI")
abril_dataset_training[is.na(abril_dataset_training)] <- -9999999.0
for( s in 1:4 )
{
  # Genero training y testing con 70% , 30%
  set.seed( semilla[s] )
  abril_inTraining <- createDataPartition( abril$clase_binaria1, p = .70, list = FALSE)
  abril_dataset_training <- abril[ abril_inTraining,]
  abril_dataset_testing <- abril[-abril_inTraining,]
  
  abril_modelo <- ranger(clase_binaria1 ~ ., data = abril_dataset_training , num.trees=500, min.node.size=450, probability=TRUE,write.forest = TRUE )
  
  
  # calculo la ganancia normalizada en testing
  abril_testing_prediccion = predict( abril_modelo, abril_dataset_testing )
  ganancias[s] = ganancia( abril_testing_prediccion$predictions, abril_dataset_testing$clase_binaria1, 0.032 ) / 0.30
  
  
  
}

mean(ganancias)


grid <-  expand.grid(mtry = c(20,22,24,26,28,30,35,40))

fitControl <- trainControl(method = "CV",
                           number = 5,
                           verboseIter = TRUE,
                           classProbs = TRUE
                           n.thread)

fit = train(
  x = abril_dataset_training[ , names(abril_dataset_training) != 'clase_binaria1'],
  y = abril_dataset_training[ , names(abril_dataset_training) == 'clase_binaria1'],
  method = 'ranger',
  num.trees = 500,
  trControl = fitControl
)
print(fit)
print(proc.time() - ptm) # ~2.4 seconds


abril_modelo <- csrf(clase_binaria1 ~ .,training_data = abril_dataset_training,test_data = abril_dataset_testing,params1 = list(num.trees=300,mtry=20),params2 = list(num.trees=5))
-----#prueba de optimizar----

 set.seed(949)
 mod0 <- train(x = abril_dataset_training[ , names(abril_dataset_training) != 'clase_binaria1'],
               y = abril_dataset_training[ , names(abril_dataset_training) == 'clase_binaria1'],
                            method = "ranger",
                           metric = "ROC",
                                         trControl = trainControl(method = "cv",
                                                   classProbs = TRUE
                                                   ))
getTrainPerf(mod0)



-----#prueba weka---------
library(RWeka)
library(RWekajars)
                                                                                                          10, 0, 0, 0, 0, 0, 10, 0), ncol = 3), W = "weka.classifiers.trees.J48", 
-----#random forest-----

library(randomForest)
library(mlr)
library(caret)
abril_inTraining <- createDataPartition( abril$clase_binaria1, p = .70, list = FALSE)
abril_dataset_training    <- abril[ abril_inTraining,]
abril_dataset_testing  <- abril[-abril_inTraining,]

abril[is.na(abril)] <- -99999999.0

rf <- randomForest(data=abril_dataset_training, clase_binaria1 ~  ., ntree = 501, cutoff=c(0.004884868,(1-0.004884868)))

summary(abril$clase_binaria1)

df = iris
cost = matrix(runif(150 * 3, 0, 2000), 150) * (1 - diag(3))[df$Species,] + runif(150, 0, 10)

colnames(cost) = levels(iris$Species)
rownames(cost) = rownames(iris)
df$Species = NULL

costsens.task = makeCostSensTask(id = "iris", data = df, cost = cost)
costsens.task

-----#TEST ROSE-----
library(funModeling)
library(ROSE)
library(rpart)
library(ranger)
df <- df_status(abril_dataset_training)
abril_dataset_training$problemafin <- factor(abril_dataset_training$problemafin)
abril_dataset_training$cant_inicio_mora <- as.integer(abril_dataset_training$cant_inicio_mora)
abril_dataset_training$cant_inicio_mora_master <- as.integer(abril_dataset_training$cant_inicio_mora_master)
abril_dataset_training$cant_master_fvencimiento <- as.integer(abril_dataset_training$cant_master_fvencimiento)
abril_dataset_training$cant_visa_fechaalta <- as.integer(abril_dataset_training$cant_visa_fechaalta)
abril_dataset_training$cant_visa_fvencimiento <- as.integer(abril_dataset_training$cant_visa_fvencimiento)
abril_dataset_training$cant_visa_fultimo_cierre <- as.integer(abril_dataset_training$cant_visa_fultimo_cierre)
abril_dataset_training$cant_master_fultimo_cierre <- as.integer(abril_dataset_training$cant_master_fultimo_cierre)
abril_dataset_training$cant_master_fechaalta <- as.integer(abril_dataset_training$cant_master_fechaalta)
abril_dataset_training$visa_marca_atraso.cant_inicio_mora_master <- as.integer(abril_dataset_training$visa_marca_atraso.cant_inicio_mora_master)
abril_dataset_training$ttarjeta_visa.cant_inicio_mora_master <- as.integer(abril_dataset_training$ttarjeta_visa.cant_inicio_mora_master)
abril_dataset_training$mactivos_margen.cant_inicio_mora_master <- as.integer(abril_dataset_training$mactivos_margen.cant_inicio_mora_master)
abril_dataset_training$cant_inicio_mora_master.tmovimientos_ultimos90dias <- as.integer(abril_dataset_training$cant_inicio_mora_master.tmovimientos_ultimos90dias)

abril_dataset_testing$problemafin <- factor(abril_dataset_testing$problemafin)
abril_dataset_testing$cant_inicio_mora <- as.integer(abril_dataset_testing$cant_inicio_mora)
abril_dataset_testing$cant_inicio_mora_master <- as.integer(abril_dataset_testing$cant_inicio_mora_master)
abril_dataset_testing$cant_master_fvencimiento <- as.integer(abril_dataset_testing$cant_master_fvencimiento)
abril_dataset_testing$cant_visa_fechaalta <- as.integer(abril_dataset_testing$cant_visa_fechaalta)
abril_dataset_testing$cant_visa_fvencimiento <- as.integer(abril_dataset_testing$cant_visa_fvencimiento)
abril_dataset_testing$cant_visa_fultimo_cierre <- as.integer(abril_dataset_testing$cant_visa_fultimo_cierre)
abril_dataset_testing$cant_master_fultimo_cierre <- as.integer(abril_dataset_testing$cant_master_fultimo_cierre)
abril_dataset_testing$cant_master_fechaalta <- as.integer(abril_dataset_testing$cant_master_fechaalta)
abril_dataset_testing$visa_marca_atraso.cant_inicio_mora_master <- as.integer(abril_dataset_testing$visa_marca_atraso.cant_inicio_mora_master)
abril_dataset_testing$ttarjeta_visa.cant_inicio_mora_master <- as.integer(abril_dataset_testing$ttarjeta_visa.cant_inicio_mora_master)
abril_dataset_testing$mactivos_margen.cant_inicio_mora_master <- as.integer(abril_dataset_testing$mactivos_margen.cant_inicio_mora_master)
abril_dataset_testing$cant_inicio_mora_master.tmovimientos_ultimos90dias <- as.integer(abril_dataset_testing$cant_inicio_mora_master.tmovimientos_ultimos90dias)




data.rose <- ROSE(clase_binaria1 ~ ., data = abril_dataset_training, seed = 1)$data
table(data.rose$clase_binaria1)
tree.rose <- rpart(clase_binaria1 ~ ., data = data.rose)
pred.tree.rose <- predict(tree.rose, newdata = abril_dataset_testing)

roc.curve(abril_dataset_testing$clase_binaria1, pred.tree.rose[,2])

ROSE.holdout <- ROSE.eval(clase_binaria1 ~ ., data = abril_dataset_training, learner = rpart, method.assess = "holdout", extr.pred = function(obj)obj[,2], seed = 1)
ROSE.holdout

-----#c5 cost matrix-----

library(C50)

control_c5 <- C5.0Control(subset = TRUE,
                          bands = 0,
                          winnow = FALSE,
                          noGlobalPruning = FALSE,
                          CF = 0.01,
                          minCases = 20,
                          fuzzyThreshold = FALSE,
                          sample = 0,
                          seed = sample.int(4096, size = 1) - 1L,
                          earlyStopping = TRUE,
                          label = "outcome")

matriz_costo <- matrix(c(0,250,8000,0),nrow=2,ncol=2)
matriz_costo
rownames(matriz_costo) <- levels(abril_dataset_training$clase_binaria1)
colnames(matriz_costo) <- levels(abril_dataset_training$clase_binaria1)
matriz_costo

c5_train <- C5.0(clase_binaria1 ~ ., data = abril_dataset_training,rules=TRUE,costs=matriz_costo)
c5_train

pred.c5 <- predict(c5_train, newdata = abril_dataset_testing,type="prob")
roc.curve(abril_dataset_testing$clase_binaria1, pred.c5[,2])