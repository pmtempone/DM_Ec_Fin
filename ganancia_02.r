#Arbol con libreria  rpart

library(rpart)
library(caret)




#definicion  funcion ganancia para nuestro problema

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



abril_dataset <- read.table("d:\\uba\\a2016\\producto_premium_201604.txt", header=TRUE, sep="\t", row.names="numero_de_cliente")

semilla <- c( 102191, 200177, 410551, 552581, 892237 )
ganancias <- c() 
tiempos <- c()

for( s in  1:5 )
{
	# Genero training y testing con 70% , 30%
	set.seed( semilla[s] )
	abril_inTraining <- createDataPartition( abril$clase, p = .70, list = FALSE)
	abril_dataset_training <- abril[ abril_inTraining,]
	abril_dataset_testing  <- abril[-abril_inTraining,]


	# generacion del modelo sobre los datos de training
	t0 =  Sys.time()
	abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset_training,   cp = 0.00044 )  # 0.0004401162  #1179667
	t1 = Sys.time()
	tiempos[s] <-  as.numeric(  t1 - t0 )


	abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")


	# calculo la ganancia normalizada  en testing
	ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30

	
}

ganancias
mean( ganancias )
