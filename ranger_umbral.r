
#Random Forest con libreria  ranger  que es MULTICORE  !
# source( "d:\\uba\\a2016\\codigoR\\ranger_umbral.r" )

library(ranger)
library(caret)

library(randomForest)  #solo se usa para imputar nulos




#definicion  funcion ganancia para nuestro problema

ganancia = function( probs, clases, prob )
{
  suma = 0 ;
  largo = length( clases ) ;

  for( i in 1:largo )
  {
    if( probs[ i, "POS"]  > prob   ){ suma <- suma + if( clases[i]=="POS" ) { 7750 } else { -250 }  
     } ;
  }

  return( suma )
}


#Busca el punto de corte optimo, desde 0.02 a 0.10 
umbral_ganancia_optimo = function( probs, clases)
{

  vgan_maxima = -9999999.0 ;
  vumbral =  0 ;


  #itero de 0.02 a 0.10  en incrementos de 0.01
  for( i in 0:80 ) 
  {
    vgan = ganancia(  probs, clases, 0.020 + i/1000 )

    if( vgan > vgan_maxima )
    {
	vgan_maxima =  vgan ;
	vumbral =  0.020 +  i/1000 ;
    }

  }

  return( vumbral )
}


archivo_entrada <- "d:\\uba\\a2016\\abril_binario1.txt"
archivo_salida  <- "d:\\uba\\a2016\\salida_ranger_umbral.txt"



#escribo los  titulos  del archivo salida
if( !file.exists( archivo_salida) )
{
	cat( "fecha", "archivo", "algoritmo", "nulos", "tipo_umbral", "num.trees", "vmin.node.size", "umbral_promedio", "ganancia_promedio", "tiempo_promedio", "ganancias" , "umbrales",  "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1



abril_dataset <- read.table( archivo_entrada, header=TRUE, sep="\t", row.names="numero_de_cliente")

#imputo los nulos, ya que ranger no acepta nulos
abril_dataset.imputed <-  na.roughfix( abril_dataset )
metodo_imputacion  <-  "na.roughfix"

#Otra opcion de imputacion, comentada
#abril_dataset.imputed[is.na(abril_dataset)] <- -99999999.0
#metodo_imputacion  <-  "menos_nueves"



semilla <- c( 102191, 200177, 410551, 552581, 892237 )

linea <- 1


for(  vnum.trees  in  c( 5, 10, 20, 50, 100, 200, 500, 800, 1000, 1500, 2000, 5000) )
{
for(  vmin.node.size  in  c( 10000, 5000, 3000, 2000, 1000, 800, 700, 600, 500, 300, 200, 100, 50, 20) )
{

	# El umbral  es  0.03125   250/8000
	ganancias <- c() 
	tiempos   <- c()

	if( linea > lineas_archivo  )
	{
		for( s in  1:5 )
		{
			# Genero training y testing con 70% , 30%
			set.seed( semilla[s] )
			abril_inTraining <- createDataPartition( abril_dataset.imputed$clase_binaria1, p = .70, list = FALSE)
			abril_dataset_training <- abril_dataset.imputed[ abril_inTraining,]
			abril_dataset_testing  <- abril_dataset.imputed[-abril_inTraining,]


			# generacion del modelo sobre los datos de training
			t0 =  Sys.time()
			abril_modelo   <- ranger(clase_binaria1 ~ ., data = abril_dataset_training , num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE )	
			t1 = Sys.time()
			tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")


			# calculo la ganancia normalizada  en testing con probabilidad corte  250/8000
			abril_testing_prediccion  = predict(  abril_modelo,  abril_dataset_testing )
			ganancias[s] = ganancia( abril_testing_prediccion$predictions,  abril_dataset_testing$clase_binaria1,  250/8000  ) / 0.30
	
	
		}

		cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "ranger", metodo_imputacion, "umbral_fijo",  vnum.trees, vmin.node.size, 250/8000, mean(ganancias), mean(tiempos), ganancias, "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )


	}
	linea <-  linea + 1




	# Se optimizan el  umbral de la probabilidad en   validation
	umbrales  <- c()

	if( linea > lineas_archivo  )
	{
		for( s in  1:5 )
		{
			# Genero training y testing con 70% , 30%
			set.seed( semilla[s] )
			abril_inTraining <- createDataPartition( abril_dataset.imputed$clase_binaria1, p = .70, list = FALSE)
			abril_dataset_train    <- abril_dataset.imputed[ abril_inTraining,]
			abril_dataset_testing  <- abril_dataset.imputed[-abril_inTraining,]

			abril_inValidation <- createDataPartition( abril_dataset_train$clase_binaria1, p = .70, list = FALSE)
			abril_dataset_training    <- abril_dataset_train[  abril_inValidation, ]
			abril_dataset_validation  <- abril_dataset_train[ -abril_inValidation, ]


			# generacion del modelo sobre los datos de training del 49%
			abril_modelo   <- ranger(clase_binaria1 ~ ., data = abril_dataset_training , num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE )	


			# determino el umbral optimo
			abril_validation_prediccion  = predict(  abril_modelo,  abril_dataset_validation )
			umbrales[s]  <-  umbral_ganancia_optimo( abril_validation_prediccion$predictions,  abril_dataset_validation$clase_binaria1 ) 

			
			t0 =  Sys.time()
			abril_modelo   <- ranger(clase_binaria1 ~ ., data = abril_dataset_train , num.trees=vnum.trees,  min.node.size=vmin.node.size, probability=TRUE )	
			t1 = Sys.time()
			tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")



			# calculo la ganancia normalizada  en testing
			abril_testing_prediccion  = predict(  abril_modelo,  abril_dataset_testing )
			ganancias[s] = ganancia( abril_testing_prediccion$predictions,  abril_dataset_testing$clase_binaria1,  umbrales[s]  ) / 0.30
	

	
		}

		cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "ranger", metodo_imputacion, "umbral_estimado",  vnum.trees, vmin.node.size, mean(umbrales), mean(ganancias), mean(tiempos), ganancias, umbrales, "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )


	}
	linea <-  linea + 1
}
}



