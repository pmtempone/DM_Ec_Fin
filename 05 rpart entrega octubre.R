

# MODELO BAGGING


#-------------------------------------------------------------------
# Carga Package y datos
library(C50);
library(rpart)
library(foreach)
library(caret)
library(doParallel)
library(partykit)

set.seed(557309)

abril_inTraining <- createDataPartition( abril$clase, p = .70, list = FALSE)

abril_dataset_training <- abril[ abril_inTraining,]
abril_dataset_testing  <- abril[-abril_inTraining,]

Clase       <-unique(abril_dataset_training$clase)
Iteraciones <- 45

ganancia = function( probs, clases, prob ){
  suma = 0 ;
  largo = length( clases ) ;
  
  for( i in 1:largo )
  {
    if( probs[ i, "BAJA+2"]  > prob   ){ suma <- suma + if( clases[i]=="BAJA+2" ) { 7750 } else { -250 }  
    } ;
  }
  
  return( suma )
}


#------------------------------------------------------------------
# Crea el modelo Bagging con multiples arboles de decision
modelo<- foreach(i=1:Iteraciones) %do% {  
  muestra   <- sample(nrow(abril_dataset_training), size=floor((nrow(abril_dataset_training)*.5)))  
  rpart(clase ~ .,data=abril_dataset_training[muestra,],xval=0, minsplit=20,maxdepth=11,cp=0)   
}  

#----------------------------------------------------------------- 
# Acumula la prediccion de cada arbol en una tabla
# Usando el modelo ya creado y guardado previamente
Prediccion <-as.data.frame(
  foreach(i=1:Iteraciones,.combine=cbind) %do% 
  {predict(modelo[[i]], abril_dataset_testing)}
)

#grabar salida en las base
pw <- {
"..."
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password

#crear tabla con predicciones

pred_bagging <- cbind("BAJA+1"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+1'],1,mean)),"BAJA+2"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+2'],1,mean)),"CONTINUA"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='CONTINUA'],1,mean)))
colnames(pred_bagging) <- c("BAJA+1","BAJA+2","CONTINUA")

ganancias = ganancia(pred_bagging,  abril_dataset_testing$clase,(250/8000) ) / 0.30

dbWriteTable(con,"pred_bagging",pred_bagging)


#------------------------------------------------------------------
# Crea el modelo Bagging con multiples arboles de decision con peso! Baja ganancia

#Asigno pesos <7750, 250>  es equivalente a  <31, 1>  
vweights <- ifelse(abril_dataset_training$clase=='BAJA+2', 31, 1)

modelo_w<- foreach(i=1:Iteraciones) %do% {  
  muestra   <- sample(nrow(abril_dataset_training), size=floor((nrow(abril_dataset_training)*.5)))  
  
  #Asigno pesos <7750, 250>  es equivalente a  <31, 1>  
  vweights <- ifelse(abril_dataset_training$clase[muestra]=='BAJA+2', 31, 1) 


  rpart(clase ~ .,data=abril_dataset_training[muestra,],xval=0, minsplit=20,maxdepth=11,cp=0,weights=vweights)   
}  

#----------------------------------------------------------------- 
# Acumula la prediccion de cada arbol en una tabla
# Usando el modelo ya creado y guardado previamente
Prediccion <-as.data.frame(
  foreach(i=1:Iteraciones,.combine=cbind) %do% 
  {predict(modelo_w[[i]], abril_dataset_testing)}
)

#grabar salida en las base
pw <- {
  "Belsaga305"
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the passwor

#crear tabla con predicciones

pred_bagging_w <- cbind("BAJA+1"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+1'],1,mean)),"BAJA+2"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+2'],1,mean)),"CONTINUA"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='CONTINUA'],1,mean)))
colnames(pred_bagging_w) <- c("BAJA+1","BAJA+2","CONTINUA")

ganancias = ganancia(pred_bagging_w,  abril_dataset_testing$clase,0.6 ) / 0.30

dbWriteTable(con,"pred_bagging",pred_bagging)

ganancias
