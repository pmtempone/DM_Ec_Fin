-----#librarias----


library(lubridate)
library(dplyr)
library(caret)
library(ranger)
library(RPostgreSQL)
library(DBI)
library(rpart)
library(arules)
library(zoo)
library(randomForest)
library(foreach)
suppressMessages(library(funModeling))
library(xgboost)
options(scipen = 99999)

----#binaria2----

abril_b2 <- abril
abril_b2$clase_binaria2 <- factor(ifelse(abril_b2$clase %in% c("BAJA+2","BAJA+1"),"POS","NEG"))
abril_b2$clase <- NULL

etiqueta <- ifelse(abril_b2$clase_binaria2=="POS",1,0)

vmax_depth         <- 4
vmin_child_weight  <- 5
vnround            <- 1000
dtrain_sinpeso = xgb.DMatrix(data = data.matrix(abril_dataset_sinclase),label =etiqueta,missing = NA )

#multi:softprob  devuelve vector doble con la probabilidad que la clase sea  0 , y la probabilidad de 1
#sp =  SIN PESO,  por eso el punto de corte es 0.03125
fganancia_multi.sp.b2 <- function(preds, dtrain) 
{
  labels <- getinfo(dtrain, "label")
  
  suma = 0 ;
  largo = length( labels ) ;
  
  for( i in 1:largo )
  {
    if( preds[ 2*i ]  > 0.0568  ){ suma <- suma + if( labels[i]==1 ) { 7750 } else { -250 }  
    } ;
  }
  
  return(list(metric = "ganancia", value = suma))
}


# Corro modelo con multi:softprob  SIN PESO

set.seed( 102191  )

t6 =  Sys.time()
cv.multi.sp = xgb.cv( 
  data = dtrain_sinpeso ,          missing = 0 ,
  stratified = TRUE,       nfold = 5 ,
  eta = 0.01, 
  subsample = 1.0, 
  colsample_bytree = 0.7, 
  min_child_weight = vmin_child_weight, 
  max_depth = 4,
  alpha = 0, lambda = 0.1, gamma = 0.01,
  nround= vnround, 
  objective="multi:softprob",         num_class=2,
  feval = fganancia_multi.sp.b2,            maximize =TRUE
)

t7 = Sys.time()



#Comparo

as.numeric(  t1 - t0, units = "secs")
max( cv.logistic.cp[ , test.ganancia.mean] ) / (1/5) 
which.max(  cv.logistic.cp[ , test.ganancia.mean] )

as.numeric(  t3 - t2, units = "secs")
max( cv.multi.cp[ , test.ganancia.mean] ) / (1/5) 
which.max(  cv.multi.cp[ , test.ganancia.mean] )

as.numeric(  t5 - t4, units = "secs")
max(  cv.logistic.sp[ 310:1000, test.ganancia.mean] ) / (1/5) 
which.max(  cv.logistic.sp[  310:1000, test.ganancia.mean] )

as.numeric(  t7 - t6, units = "secs")
max( cv.multi.sp[310:1000 , test.ganancia.mean] ) / (1/5) 
which.max(  cv.multi.sp[ 310:1000, test.ganancia.mean] )

x <- c( 350:1000 )

plot(   x, cv.multi.sp[  350:1000,test.ganancia.mean ] / (1/5),  col="orange",  type="l" )
lines(  x,  cv.logistic.cp[  350:1000,test.ganancia.mean ] / (1/5) , col="blue" )
lines(  x, cv.multi.cp[  350:1000,test.ganancia.mean ] / (1/5),  col="green" )
lines(  x, cv.logistic.sp[  350:1000,test.ganancia.mean ] / (1/5),  col="red" )

-----#ranger binaria2----

ganancia.rf = function( probs, clases, prob )
{
  suma = 0 ;
  largo = length( clases ) ;
  #largo = length( abril_dataset_validation$clase_binaria1 )
  for( i in 1:largo )
  {
    if( probs[ i, "POS"]  > prob   ){ suma <- suma + if( clases[i]=="POS" ) { 7750 } else { -250 }  
    } ;
  }
  
  return( suma )
}


abril_b2[is.na(abril_b2)] <- -99999999.0


for( s in  1:5 )
{
  # Genero training y testing con 70% , 30%
  set.seed( semilla[s] )
  abril_inTraining <- createDataPartition( abril_b2$clase_binaria2, p = .70, list = FALSE)
  abril_dataset_training    <- abril_b2[ abril_inTraining,]
  abril_dataset_testing  <- abril_b2[-abril_inTraining,]
  abril_t_prueba <- abril[-abril_inTraining,]
  abril_t_prueba[is.na(abril_t_prueba)] <- -99999999.0
  abril_t_prueba$clase <- factor(ifelse(abril_t_prueba$clase %in% c("BAJA+2"),"POS","NEG"))
  abril_modelo_ranger   <- ranger(clase_binaria2 ~ ., data = abril_dataset_training , num.trees=500,  min.node.size=20, probability=TRUE )	
  
  
  # calculo la ganancia normalizada  en testing
  abril_testing_prediccion  = predict(  abril_modelo_ranger,  abril_dataset_testing )
  ganancias[s] = ganancia.rf( abril_testing_prediccion$predictions,  abril_t_prueba$clase,  0.085  ) / 0.30
  
  
}

mean(ganancias) #promedio 1523667

abril_testing_prediccion  = predict(  abril_modelo,  abril_dataset_testing )
ganancia( abril_testing_prediccion,  abril_t_prueba$clase,  0.085  ) / 0.30


----#for de cv boost----
abril_b1 <- abril
abril_b1$clase_binaria1 <- factor(ifelse(abril_b1$clase=="BAJA+2","POS","NEG"))
abril_b1$clase <- NULL


fganancia_multi.sp.b2 <- function(preds, dtrain) 
{
  labels <- getinfo(dtrain, "label")
  
  suma = 0 ;
  largo = length( labels ) ;
  
  for( i in 1:largo )
  {
    if( preds[ 2*i ]  > 0.021  ){ suma <- suma + if( labels[i]==1 ) { 7750 } else { -250 }  
    } ;
  }
  
  return(list(metric = "ganancia", value = suma))
}


abril_dataset_sinclase <-   abril_b1[ , !(names(abril_b1) %in% c("clase_binaria1") ) ] 
dtrain_sinpeso = xgb.DMatrix(data = data.matrix(abril_dataset_sinclase),label =etiqueta,missing = NA )

semilla <- c( 102191, 200177, 410551, 552581, 892237 )

archivo_entrada_xg <- "xg_for.txt"
archivo_salida <- "test_peso.txt"
if( !file.exists( archivo_salida) )
{
  cat( "fecha", "archivo", "algoritmo", "nulos", "vmax_depth", "vmin_child_weight", "vnround",  "ganancia_promedio", "tiempo_promedio", "ganancias" ,  "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1

linea <- 1

for(  vmax_depth  in  c( 2 ) )
{
  for(  vmin_child_weight  in  c( 5,10,15 ) )
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
        abril_inTraining <- createDataPartition( abril_b1$clase_binaria1, p = .70, list = FALSE)
        abril_dataset_training <- abril_b1[ abril_inTraining,]
        abril_dataset_testing  <- abril_b1[-abril_inTraining,]
        
        
        # generacion del modelo sobre los datos de training
        t0 =  Sys.time()
        etiqueta <- ifelse(abril_dataset_training$clase_binaria1=="POS",1,0)
        abril_dataset_training_sinclase <-   abril_dataset_training[ , !(names(abril_dataset_training) %in% c("clase_binaria1") ) ] 
        dtrain_sinpeso = xgb.DMatrix(data = data.matrix(abril_dataset_training_sinclase),label =etiqueta,missing = NA )
        
        abril_modelo   <- xgboost( 
          dtrain_sinpeso, 
          eta = 0.01, 
          subsample = 0.7, 
          colsample_bytree = 0.6, 
          min_child_weight = vmin_child_weight, 
          max_depth = vmax_depth,
          alpha = 0, lambda = 0.1, gamma = 0.01,
          nround= vnround, 
          eval_metric = "merror",feval=fganancia_multi.sp.b2,
          num_class = 2,
          objective='multi:softprob',missing = 0
          #scale_pos_weight = 31
        )
        
        t1 = Sys.time()
        tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")
        
        # calculo la ganancia normalizada  en testing
        
        abril_dataset_testing_sinclase <-   abril_dataset_testing[ , !(names(abril_dataset_testing) %in% c("clase_binaria1") ) ] 
        am <-  data.matrix( abril_dataset_testing_sinclase  )
        
        for( i in 1:50 )
        {
          abril_testing_prediccion  = predict(  abril_modelo,  am ,missing=NA,  ntreelimit= i*20 )
          ganancias[ i*5 + s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase_binaria1,  0.021  ) / 0.30
          
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
