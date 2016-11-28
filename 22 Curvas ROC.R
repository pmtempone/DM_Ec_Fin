####Roc#####

---#librerias----

library(pROC)
library(xgboost)
library(caret)
library(dplyr)


----#curvas----

febrero_prediccion_roc <- c()
for (i in 1:nrow(febrero)){
  febrero_prediccion_roc[i] <- febrero_prediccion1[i*2]
}

g <- roc(clase_binaria1 ~ febrero_prediccion_roc, data = febrero)
plot(g, col="red")

febrero_prediccion_roc <- c()
for (i in 1:nrow(febrero)){
  febrero_prediccion_roc[i] <- febrero_prediccion2[i*2]
}

g.2 <- roc(clase_binaria1 ~ febrero_prediccion_roc, data = febrero)
lines(g.2, col="blue")

febrero_prediccion_roc <- c()
for (i in 1:nrow(febrero)){
  febrero_prediccion_roc[i] <- febrero_prediccion3[i*2]
}

g.3 <- roc(clase_binaria1 ~ febrero_prediccion_roc, data = febrero)
lines(g.3, col="green")

febrero_prediccion_roc <- c()
for (i in 1:nrow(febrero)){
  febrero_prediccion_roc[i] <- febrero_prediccion4[i*2]
}

g.4 <- roc(clase_binaria1 ~ febrero_prediccion_roc, data = febrero)
lines(g.4, col="yellow")

legend("bottomright", c("modelo4","modelo3","modelo2","modelo1"), col = c("yellow","green","blue", "red"), lty = 1 )

g
g.2
g.3

plot(g.4$thresholds,g.4$sensitivities)
unique(g.4$sensitivities)

plot(g.4$cases>0.021)

sum(g.4$cases>0.015)

summary(febrero$clase_binaria1)
summary(factor(abril$clase))
