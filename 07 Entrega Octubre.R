----#librerias---
  
library(rpart)
library(foreach)

----#prediccion---

  Prediccion <-as.data.frame(
    foreach(i=1:Iteraciones,.combine=cbind) %do% 
    {predict(modelo[[i]], entrega_octubre)}
  )  

#salvar prediccion en archivo
  
  pred_bagging <- cbind("BAJA+1"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+1'],1,mean)),"BAJA+2"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='BAJA+2'],1,mean)),"CONTINUA"=as.data.frame(apply(Prediccion[,colnames(Prediccion)=='CONTINUA'],1,mean)))
  colnames(pred_bagging) <- c("BAJA+1","BAJA+2","CONTINUA")
  
  remove(Prediccion)
  
  pred_bagging$clase <- factor(ifelse(pred_bagging[,"BAJA+2"]>(250/8000),"BAJA+2","CONTINUA"))
  