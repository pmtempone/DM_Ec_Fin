----#librerias----

library(data.table)
library(ggplot2)
library(caret)
library(h2o)
library(lattice)

options(scipen=999)
---#carga----

producto_premium_201604 <- fread("E:/GitHub/DM_Ec_Fin/producto_premium_201604.txt")
id_clientes <- producto_premium_201604$numero_de_cliente
rownames(producto_premium_201604) <- id_clientes
producto_premium_201604$numero_de_cliente <- NULL

#No. of rows and columns in Train
dim(producto_premium_201604)

str(producto_premium_201604)

#analyzing variable
producto_premium_201604[,prop.table(table(clase))]

'     BAJA+1      BAJA+2    CONTINUA 
0.003721261 0.003175682 0.993103058 '

producto_premium_201604[,prop.table(table(participa))]

#unique values in ID variables
length(unique(producto_premium_201604$cliente_sucursal))

#missing values
colSums(is.na(producto_premium_201604))


----#exploracion grafica----

#marketing vs clase
ggplot(producto_premium_201604, aes(marketing_activo_ultimos90dias, fill = clase)) + geom_bar()

#cliente_vip vs clase
ggplot(producto_premium_201604, aes(cliente_vip, fill = clase)) + geom_bar()

#cliente_sucursal vs clase
ggplot(producto_premium_201604, aes(cliente_sucursal, fill = clase)) + geom_bar()

#cliente_edad vs clase
ggplot(producto_premium_201604, aes(cliente_edad, fill = clase)) + geom_bar()
stem(producto_premium_201604$cliente_edad)
#cprestamos_hipotecarios vs clase

producto_premium_201604[,prop.table(table(producto_premium_201604$cprestamos_hipotecarios,producto_premium_201604$clase))]

ggplot(producto_premium_201604, aes(cprestamos_hipotecarios, fill = clase)) + geom_bar()

#check classes of all variables
sapply(producto_premium_201604, class)

----#creacion variables----

substr(as.character(unique(test$foto_mes)),5,6)

producto_premium_201604[,Mes:= substr(as.character(unique(foto_mes)),5,6)]
unique(cut(producto_premium_201604$cliente_edad,breaks=10))
producto_premium_201604[,Edad_bins:=cut(cliente_edad,breaks=10)]
unique(producto_premium_201604$Edad_bins)
ggplot(producto_premium_201604, aes(Edad_bins, fill = clase)) + geom_bar()



----#test y train----
producto_premium_201604$clase <- factor(producto_premium_201604$clase)
producto_premium_201604$participa <- factor(producto_premium_201604$participa)

set.seed(123)
intrain <- createDataPartition(y=producto_premium_201604$clase,p=0.80,list = FALSE)
train <- producto_premium_201604[intrain,]
test <- producto_premium_201604[-intrain,]


---#h20----

localH2O <- h2o.init(nthreads = -1)
h2o.init()
