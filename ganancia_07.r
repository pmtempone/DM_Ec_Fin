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



cat( "fecha", "archivo", "algoritmo", "cp" , "minsplit", "minbucket", "maxdepth", "ganancia_promedio", "tiempo_promedio", "ganancias" , "\n", sep="\t", file="salida.txt", fill=FALSE, append=FALSE )


abril_dataset <- read.table("d:\\uba\\a2016\\producto_premium_201604.txt", header=TRUE, sep="\t", row.names="numero_de_cliente")

vcp <- 0 ;
vminsplit <- 50 ;
vminbucket <- 8 ;
vmaxdepth <- 8


for(  vmaxdepth  in  3:12 )
{
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
		abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset_training,   cp=vcp, minsplit=vminsplit, minbucket=vminbucket, maxdepth=vmaxdepth )  
		t1 = Sys.time()
		tiempos[s] <-  as.numeric(  t1 - t0, units = "secs" )


		abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")


		# calculo la ganancia normalizada  en testing
		ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase ) / 0.30

	
	}


	cat( format(Sys.time(), "%Y%m%d %H%M%S"), "201604.txt", "rpart", vcp , vminsplit, vminbucket, vmaxdepth, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file="salida.txt", fill=FALSE, append=TRUE )

}

