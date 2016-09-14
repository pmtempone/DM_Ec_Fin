----#librarias----

library(rpart)
library(caret)

---#separacion test y train----
semillas <- c(101987,226283,342191,417379,557309,649277)
# Genero training y testing con 70% , 30%
set.seed( semilla[s] )
abril_inTraining <- createDataPartition( abril$clase, p = .70, list = FALSE)
abril_dataset_training <- abril[ abril_inTraining,]
abril_dataset_testing  <- abril[-abril_inTraining,]

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