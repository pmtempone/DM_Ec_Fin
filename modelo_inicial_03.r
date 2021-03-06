#Arbol con libreria  rpart

library(rpart)
library(rattle)


abril_dataset <- read.table("d:\\uba\\a2016\\producto_premium_201604.txt", header=TRUE, sep="\t", row.names="numero_de_cliente")


# generacion del modelo
t0 =  Sys.time()
<<<<<<< HEAD
abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset )
=======
abril_modelo  <- rpart( clase ~ .   ,   data = abril )
>>>>>>> 219e4e78cbe0f32bf6e53d649dcfebc745206a14
t1 = Sys.time()
as.numeric(  t1 - t0 )




# Aplico el modelo a los datos de junio y genero un archivo con la prediccion

junio_dataset <- read.table("d:\\uba\\a2016\\producto_premium_201606.txt", header=TRUE, sep="\t", row.names="numero_de_cliente")

junio_prediccion  <- predict(  abril_modelo, junio_dataset, type = "prob")

<<<<<<< HEAD
write.table(  junio_prediccion, file="d:\\uba\\a2016\\junio_prediccion.txt", row.names=TRUE)
=======
write.table( sort( junio_prediccion, 2 ), file="d:\\uba\\a2016\\junio_prediccion.txt", row.names=TRUE)
>>>>>>> 219e4e78cbe0f32bf6e53d649dcfebc745206a14

