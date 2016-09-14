#Arbol con libreria  rpart

library(rpart)

abril_dataset <- read.table("d:\\uba\\a2016\\producto_premium_201604.txt", header=TRUE, sep="\t", row.names="numero_de_cliente")


# generacion del modelo
abril_modelo  <- rpart( clase ~ .   ,   data = abril_dataset )


# impresion basica del arbol
plot( abril_modelo, uniform=TRUE, main="Arbol para Abril")
text( abril_modelo, use.n=TRUE, all=TRUE, cex=.8, digits=10)


