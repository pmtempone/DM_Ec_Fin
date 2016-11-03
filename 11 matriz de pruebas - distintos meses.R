----#librerias----
library(RPostgreSQL)
library(DBI)
library(dplyr)
library(ranger)
library(rpart)
library(arules)
library(caret)
library(lubridate)
library(zoo)

-----#conexion base----
# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)

----#loop para matriz de ganancia----
matriz_meses <- matrix(nrow = 6,ncol = 6)
lista_meses <-  c(noviembre=201511,diciembre=201512,enero=201601,febrero=201602,marzo=201603,abril=201604)

for (i in 1:6){
  query_a <- paste("select * from fct_prod_premium_v2 where foto_mes = ",lista_meses[i])
  training <- dbGetQuery(con, query_a)
  training[is.na(training)] <- -99999999.0
  
  for (e in 1:6)
    if (i==e) {
      set.seed( 154515 )
      
      abril_inTraining <- createDataPartition( training$clase_binaria1, p = .70, list = FALSE)
      abril_dataset_training <- training[ abril_inTraining,]
      abril_dataset_testing  <- training[-abril_inTraining,]
      
      modelo <- ranger(clase_binaria1 ~ ., data = abril_dataset_training , num.trees=500,  min.node.size=300, probability=TRUE )	
      abril_testing_prediccion  <-  predict(  modelo,  abril_dataset_testing )
      
      matriz_meses[i,e] <- ganancia( abril_testing_prediccion$predictions,  abril_dataset_testing$clase_binaria1,  250/8000  ) / 0.30
    }
    else  {
      set.seed( 154515 )
      query_b <- paste("select * from fct_prod_premium_v2 where foto_mes = ",lista_meses[e])
      testing <- dbGetQuery(con, query_b)
      testing[is.na(testing)] <- -99999999.0
      modelo <- ranger(clase_binaria1 ~ ., data = training , num.trees=500,  min.node.size=300, probability=TRUE )	
      matriz_meses[i,e] <- ganancia( abril_testing_prediccion$predictions,  abril_dataset_testing$clase_binaria1,  250/8000  ) / 0.30
      
    }
}


mat_cor <- cor(abril_dataset_training[,4:169])

136003*0.001
