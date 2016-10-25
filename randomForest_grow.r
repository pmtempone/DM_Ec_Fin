
#Random Forest con libreria  randomForest
# source( "c:\\uba\\a2016\\codigoR\\randomForest_grow.r" )

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


archivo_entrada <- "c:\\uba\\a2016\\abril_binario1.txt"
archivo_salida  <- "c:\\uba\\a2016\\salida_randomForest_umbral.txt"



#escribo los  titulos  del archivo salida
if( !file.exists( archivo_salida) )
{
	cat( "fecha", "archivo", "algoritmo", "nulos", "tipo_umbral", "ntree", "nodesize", "umbral_promedio", "ganancia_promedio", "tiempo_promedio", "ganancias" , "umbrales",  "\n", sep="\t", file=archivo_salida, fill=FALSE, append=FALSE )
}

lineas_archivo <-  length( readLines(archivo_salida) )  - 1



abril_dataset <- read.table( archivo_entrada, header=TRUE, sep="\t", row.names="numero_de_cliente")

#imputo los nulos, ya que randomForest no acepta nulos
abril_dataset.imputed <-  na.roughfix( abril_dataset )
metodo_imputacion  <-  "na.roughfix"

#Otra opcion de imputacion, comentada
#abril_dataset.imputed[is.na(abril_dataset)] <- -99999999.0
#metodo_imputacion  <-  "menos_nueves"



semilla <- c( 102191, 200177, 410551, 552581, 892237 )

abril_dataset_training  <- list()
abril_dataset_testing   <- list()

for( s in  1:5 )
{
	set.seed( semilla[s] )
	abril_inTraining <- createDataPartition( abril_dataset.imputed$clase_binaria1, p = .70, list = FALSE)
	abril_dataset_training[[s]] <- abril_dataset.imputed[ abril_inTraining,]
	abril_dataset_testing[[s]]  <- abril_dataset.imputed[-abril_inTraining,]
}


linea <- 1



for(  vnodesize  in  c( 1, 5, 10, 20, 50, 100, 200, 500 )  )
{

	varboles <- 10 ;

	# El umbral  es  0.03125   250/8000
	ganancias <- c() 
	tiempos   <- c()
	abril_modelo  <- list()

		
	for( s in  1:5 )
	{	
		t0 =  Sys.time()
		abril_modelo[[s]]   <- randomForest(clase_binaria1 ~ ., data = abril_dataset_training[[s]] , ntree=varboles,  nodesize=vnodesize, probability=TRUE )	
                t1 = Sys.time()
		tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")

		# calculo la ganancia normalizada  en testing con probabilidad corte  250/8000
		abril_testing_prediccion  =  predict(  abril_modelo[[s]],  abril_dataset_testing[[s]], type="prob" )
		ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing[[s]]$clase_binaria1,  250/8000  ) / 0.30
	}

	cat( "\n", "vnodesize ", vnodesize, "\n" )
	cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "randomForest", metodo_imputacion, "umbral_fijo",  varboles, vnodesize, 250/8000, mean(ganancias), mean(tiempos), ganancias, "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )


	vstep <- 10 

	while(  varboles < 5000 )
	{

		varboles <-  varboles + vstep ;


		for( s in  1:5 )
		{
			
			# generacion del modelo sobre los datos de training
			t0 =  Sys.time()
			abril_modelo[[s]]   <- grow( abril_modelo[[s]] ,  how.many= vstep  )	
			t1 = Sys.time()
			tiempos[s] <-  as.numeric(  t1 - t0, units = "secs")


			# calculo la ganancia normalizada  en testing con probabilidad corte  250/8000
			abril_testing_prediccion  = predict(  abril_modelo[[s]],  abril_dataset_testing[[s]], type="prob" )
			ganancias[s] = ganancia( abril_testing_prediccion,  abril_dataset_testing[[s]]$clase_binaria1,  250/8000  ) / 0.30
	
	
		}

		cat( format(Sys.time(), "%Y%m%d %H%M%S"), archivo_entrada, "randomForest", metodo_imputacion, "umbral_fijo",  varboles, vnodesize, 250/8000, mean(ganancias), mean(tiempos), ganancias, "\n", sep="\t", file=archivo_salida, fill=FALSE, append=TRUE )

		if( varboles / 10 == vstep )   vstep <- vstep * 10 

		

	}
}
