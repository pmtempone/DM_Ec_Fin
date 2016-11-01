
#Arbol con libreria  rpart
# source( "d:\\uba\\a2016\\codigoR\\rpart_binaria1.r" )

library(rpart)
library(caret)




#definicion  funcion ganancia para nuestro problema

ganancia = function( probs, clases, prob ){
  suma = 0 ;
  largo = length( clases ) ;

  for( i in 1:largo )
  {
    if( probs[ i, "POS"]  > prob   ){ suma <- suma + if( clases[i]=="POS" ) { 7750 } else { -250 }  
     } ;
  }

  return( suma )
}


archivo_entrada <- "d:\\uba\\a2016\\abril_binario1.txt"
archivo_salida  <- "d:\\uba\\a2016\\salida_binaria1.txt"

#escribo los  titulos  del archivo salida
cat( "fecha", "archivo", "algoritmo", "peso", "cp" , "minsplit", "minbucket", "maxdepth", "ganancia_promedio", "tiempo_promedio", "ganancias" , "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )


abril_dataset <- read.table( archivo_entrada, header=TRUE, sep="\t", row.names="numero_de_cliente")


semilla <- c( 102191, 200177, 410551, 552581, 892237 )


for(  vcp  in  c( 0, 0.0005,  0.001, 0.005 ) )
{
for( vminsplit  in  c(  20, 50, 100, 200, 300, 400, 500, 600, 700, 800 )  )
{
for( vminbucket  in  c( trunc(vminsplit/4), trunc(vminsplit/3) )  )
{
for(  vmaxdepth  in  c(  5, 6, 7,8,9,10, 11, 12 ) )
{

	ganancias <- c() 
	tiempos <- c()

	for( s in  1:5 )
	{
		# Genero training y testing con 70% , 30%
		set.seed( semilla[s] )
		abril_inTraining <- createDataPartition( abril_dataset$clase_binaria1, p = .70, list = FALSE)
		abril_dataset_training <- abril_dataset[ abril_inTraining,]
		abril_dataset_testing  <- abril_dataset[-abril_inTraining,]


		# generacion del modelo sobre los datos de training
		t0 =  Sys.time()
		abril_modelo  <- rpart( clase_binaria1 ~ .   ,   data = abril_dataset_training,  method="class", xval=0, maxsurrogate=1, surrogatestyle=1,   cp=vcp, minsplit=vminsplit, minbucket=vminbucket, maxdepth=vmaxdepth )  
		t1 = Sys.time()
		tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")


		abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")


		# calculo la ganancia normalizada  en testing
		ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase_binaria1,  (250/8000)  ) / 0.30

	
	}

	cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "rpart", "sin_peso", vcp , vminsplit, vminbucket, vmaxdepth, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )





	#ahora calculo CON pesos

	ganancias <- c() 
	tiempos <- c()

	for( s in  1:5 )
	{
		# Genero training y testing con 70% , 30%
		set.seed( semilla[s] )
		abril_inTraining <- createDataPartition( abril_dataset$clase_binaria1, p = .70, list = FALSE)
		abril_dataset_training <- abril_dataset[ abril_inTraining,]
		abril_dataset_testing  <- abril_dataset[-abril_inTraining,]

		#Asigno pesos <7750, 250>  es equivalente a  <31, 1>  
		vweights <- ifelse( abril_dataset_training$clase_binaria1=='POS', 31, 1 )

		# generacion del modelo sobre los datos de training
		t0 =  Sys.time()
		abril_modelo  <- rpart( clase_binaria1 ~ .   ,   data = abril_dataset_training,  weights=vweights,  method="class", xval=0, maxsurrogate=1, surrogatestyle=1,   cp=vcp, minsplit=vminsplit, minbucket=vminbucket, maxdepth=vmaxdepth )  
		t1 = Sys.time()
		tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")


		abril_testing_prediccion  = predict(  abril_modelo, abril_dataset_testing , type = "prob")


		# calculo la ganancia normalizada  en testing
		ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing$clase_binaria1,  0.5 ) / 0.30

	
	}

	cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "rpart", "con_peso", vcp , vminsplit, vminbucket, vmaxdepth, mean(ganancias), mean(tiempos), ganancias , "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )


}
}
}
}



