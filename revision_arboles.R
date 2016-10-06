----#librerias----

library(rpart)
library(rpart.plot)

----#revision arboles----

modelo_1 <- rpart(clase ~ ., data=abril_dataset_training,xval=0, cp=0, minsplit=20, minbucket=20/2)
plot(modelo_1)
text(modelo_1)
rpart.plot(modelo_1,cex = .5)

summary(modelo_1)


modelo_2 <- rpart(clase ~ ., data=abril_dataset_training,xval=0, cp=0.0001, minsplit=20, minbucket=20/2)
plot(modelo_2)
text(modelo_2)
rpart.plot(modelo_2,cex = .5)

modelo_3 <- rpart(clase ~ ., data=abril_dataset_training,xval=0, cp=0.0001, minsplit=20, minbucket=20/4)
plot(modelo_3)
text(modelo_3,cex=.3)
rpart.plot(modelo_3,cex = .3)
modelo_3
