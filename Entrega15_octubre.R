------#libreria-----

library(rpart)
library(caret)
library(doParallel)
library(RPostgreSQL)

options(scipen = 9999)

---#cliente a indice----
rownames(abril) <- abril$numero_de_cliente
abril$numero_de_cliente <- NULL

----#funcion de ganancia---
  
  
  ganancia = function( probs, clases ){
    suma = 0 ;
    largo = length( clases ) ;
    
    for( i in 1:largo )
    {
      if( probs[ i, "BAJA+2"]  > ( 250/8000)   ){ suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
      } ;
    }
    
    return( suma )
  }


----#variables base----

#Nuevos parametros segun analisis de corridas
semillas <- c(101987,226283,342191,417379)#,557309,649277)

vcp <- c(0,0.0001,0.001) ;
vminsplit <- c(20) ;
vminbucket <- c(2,3,4,5) ; #cambiar por un valor en el for
vmaxdepth <- c(11:15)
----#Iniciacion de los archivos-----


----#modelos----


for (s in 1:4){
  #genero training y test
  set.seed( semilla[s] )
  abril_inTraining <- createDataPartition( abril$clase, p = .70, list = FALSE)
  abril_dataset_training <- abril[ abril_inTraining,]
  abril_dataset_testing  <- abril[-abril_inTraining,]
  for (vcp_c in 1:length(vcp)){
    for (vminsplit_c in 1:length(vminsplit)) {
      for (vminbucket_c in 1:length(vminbucket)) {
        for (vmaxdepth_c in 1:length(vmaxdepth)) {
          
          # generacion del modelo sobre los datos de training
          t0 =  Sys.time()
          abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset_training,xval=0, cp=vcp[vcp_c], minsplit=vminsplit[vminsplit_c], minbucket=vminsplit[vminsplit_c]/vminbucket[vminbucket_c], maxdepth=vmaxdepth[vmaxdepth_c])  
          t1 = Sys.time()
          tiempos <-  as.numeric(  t1 - t0, units = "secs" )
          
          
          abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")
          
          #grabar en base
          
          dbWriteTable(con, "pred_15_octubre_v1", value = cbind(semillas[s],as.data.frame(abril_testing_prediccion)), append = TRUE)
          
          # calculo la ganancia normalizada  en testing
          ganancias = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
          
          #grabar en base
          
          dbWriteTable(con, "ganancias_modelos_15_octubre_v1", value = data.frame(date=format(Sys.time(), "%Y%m%d %H%M%S"),duracion= tiempos,seed=semillas[s],algoritm='rpart',cp=vcp[vcp_c], minsplit=vminsplit[vminsplit_c], minbucket=vminbucket[vminbucket_c], maxdepth=vmaxdepth[vmaxdepth_c],ganancias), append = TRUE,row.names=FALSE)
        }
      }
    }
    
  }
}


ganancias
