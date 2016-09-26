#Arbol con libreria  rpart

library(rpart)

abril_dataset <- read.table("d:\\uba\\a2016\\producto_premium_201604.txt", header=TRUE, sep="\t", row.names="numero_de_cliente")


# generacion del modelo
<<<<<<< HEAD
abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset )
=======
abril_modelo  <- rpart( clase ~ .   ,   data = abril )
>>>>>>> 219e4e78cbe0f32bf6e53d649dcfebc745206a14


# impresion basica del arbol
plot( abril_modelo, uniform=TRUE, main="Arbol para Abril")
text( abril_modelo, use.n=TRUE, all=TRUE, cex=.8, digits=10)

<<<<<<< HEAD

=======
>>>>>>> 219e4e78cbe0f32bf6e53d649dcfebc745206a14
