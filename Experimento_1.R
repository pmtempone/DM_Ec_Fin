----#librarias----

library(rpart)
library(caret)

---#separacion test y train----
semillas <- c(101987,226283,342191,417379)#,557309,649277)
# Genero training y testing con 70% , 30%
set.seed( semilla[s] )
abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
abril_dataset_training <- abril_dataset[ abril_inTraining,]
abril_dataset_testing  <- abril_dataset[-abril_inTraining,]

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

---#preparacion archivo-----

cat( "fecha", "archivo", "algoritmo", "cp" , "minsplit", "minbucket", "maxdepth", "ganancias" , "\n", sep="\t", file="salida_exp_1.txt", fill=FALSE, append=FALSE )


----#variables base----

vcp <- c(0,0.0001,0.001,0.005,0.01,0.05,0.1) ;
vminsplit <- c(20,50,100,200,300,400,500,600,700,800,900,1000,1500,2000,2500,3000) ;
vminbucket <- c(2,3,4,5) ; #cambiar por un valor en el for
vmaxdepth <- c(3:20)

#test
abril_modelo  <- rpart( clase ~ . ,data = abril_dataset_training,xval=0, cp=vcp[4], minsplit=vminsplit[1], minbucket= 10, maxdepth=vmaxdepth[1])  


---#loop rpart exp 1----
exp_1_result <- data.frame()

for (s in 1:4){
  #genero training y test
  set.seed( semilla[s] )
  abril_inTraining <- createDataPartition( abril$clase, p = .70, list = FALSE)
  abril_dataset_training <- abril[ abril_inTraining,1:170]
  abril_dataset_testing  <- abril[-abril_inTraining,1:170]
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
          
          
          # calculo la ganancia normalizada  en testing
          ganancias = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
          
          exp_1_result <<- rbind(exp_1_result,data.frame(date=format(Sys.time(), "%Y%m%d %H%M%S"),duracion= tiempos,seed=semillas[s],algoritm='rpart',cp=vcp[vcp_c], minsplit=vminsplit[vminsplit_c], minbucket=vminbucket[vminbucket_c], maxdepth=vmaxdepth[vmaxdepth_c],ganancias))
          
          cat( format(Sys.time(), "%Y%m%d %H%M%S"),'abril', "rpart", vcp[vcp_c] , vminsplit[vminsplit_c], vminbucket[vminbucket_c], vmaxdepth[vmaxdepth_c], ganancias, "\n", sep="\t", file="salida_exp_1.txt", fill=FALSE, append=TRUE )
          
        }
      }
    }
    
  }
}



for(  vmaxdepth  in  3:20 )
{
  ganancias <- c() 
  tiempos <- c()
  
  for( s in  1:6 )
  {
    # Genero training y testing con 70% , 30%
    set.seed( semilla[s] )
    abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
    abril_dataset_training <- abril_dataset[ abril_inTraining,1:170]
    abril_dataset_testing  <- abril_dataset[-abril_inTraining,1:170]
    
    
    # generacion del modelo sobre los datos de training
    t0 =  Sys.time()
    abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset_training,   cp=vcp, minsplit=vminsplit, minbucket=vminbucket, maxdepth=vmaxdepth )  
    t1 = Sys.time()
    tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )
    
    
    abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")
    
    
    # calculo la ganancia normalizada  en testing
    ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
    
    
  }
  
  
  cat( format(Sys.time(), "%Y%m%d %H%M%S"), "201604.txt", "rpart", vcp , vminsplit, vminbucket, vmaxdepth, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file="salida.txt", fill=FALSE, append=TRUE )
  
}

