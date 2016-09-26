library(doParallel)
library(rpart)
library(caret)

registerDoParallel(cores=4)
cat( "fecha", "archivo", "algoritmo", "cp" , "minsplit", "minbucket", "maxdepth", "ganancia_promedio", "tiempo_promedio", "ganancias" , "\n", sep="\t", file="salida_exp_1.txt", fill=FALSE, append=FALSE )

foreach (s =1:6) %dopar% {
  #genero training y test
  set.seed( semillas[s] )
  abril_inTraining <- createDataPartition( abril_dataset$clase, p = .70, list = FALSE)
  abril_dataset_training <- abril_dataset[ abril_inTraining,1:170]
  abril_dataset_testing  <- abril_dataset[-abril_inTraining,1:170]
  foreach (vcp_c = 1:length(vcp)) %do%{
    foreach (vminsplit_c = 1:length(vminsplit)) %do% {
      foreach (vminbucket_c = 1:length(vminbucket)) %do% {
        foreach (vmaxdepth_c = 1:length(vmaxdepth)) %do% {
          
          # generacion del modelo sobre los datos de training
          t0 =  Sys.time()
          abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset_training,   cp=vcp[vcp_c], minsplit=vminsplit[vminsplit_c], minbucket=vminsplit[vminsplit_c]/vminbucket[vminbucket_c], maxdepth=vmaxdepth[vmaxdepth_c])  
          t1 = Sys.time()
          tiempos <-  as.numeric(  t1 - t0, units = "secs" )
          
          
          abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")
          
          
          # calculo la ganancia normalizada  en testing
          ganancias = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30
          
          exp_1_result <<- rbind(data.frame(date=format(Sys.time(), "%Y%m%d %H%M%S"),duracion= tiempos,seed=semillas[s],algoritm='rpart',cp=vcp[vcp_c], minsplit=vminsplit[vminsplit_c], minbucket=vminbucket[vminbucket_c], maxdepth=vmaxdepth[vmaxdepth_c],ganancias))
          cat( format(Sys.time(), "%Y%m%d %H%M%S"), "201604.txt", "rpart", vcp[vcp_c] , vminsplit[vminsplit_c], vminbucket[vminbucket_c], vmaxdepth[vmaxdepth_c], mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file="salida_exp_1.txt", fill=FALSE, append=TRUE )          
        }
      }
    }
    
  }
}
