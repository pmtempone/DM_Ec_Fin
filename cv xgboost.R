
# XGBOOST  que es MULTICORE  !
# source( "c:\\uba\\a2016\\codigoR\\xgboost_01.r" )

library(xgboost)



#binary:logistic devuelve un solo vector con la probabilidad que la clase sea  1
#cp =  CON PESO,  por eso el punto de corte es 0.5
fganancia_logistic.cp <- function(preds, dtrain) 
{
  labels <- getinfo(dtrain, "label")
  
  suma = 0 ;
  largo = length( labels ) ;
  
  for( i in 1:largo )
  {
    if( preds[ i ]  > 0.5  ){ suma <- suma + if( labels[i]==1 ) { 7750 } else { -250 }  
    } ;
  }
  
  return(list(metric = "ganancia", value = suma))
}

#binary:logistic devuelve un solo vector con la probabilidad que la clase sea  1
#sp =  SIN PESO,  por eso el punto de corte es 0.03125
fganancia_logistic.sp <- function(preds, dtrain) 
{
  labels <- getinfo(dtrain, "label")
  
  suma = 0 ;
  largo = length( labels ) ;
  
  for( i in 1:largo )
  {
    if( preds[ i ]  > 0.03125  ){ suma <- suma + if( labels[i]==1 ) { 7750 } else { -250 }  
    } ;
  }
  
  return(list(metric = "ganancia", value = suma))
}


#multi:softprob  devuelve vector doble con la probabilidad que la clase sea  0 , y la probabilidad de 1
#cp =  CON PESO,  por eso el punto de corte es 0.5
fganancia_multi.cp <- function(preds, dtrain) 
{
  labels <- getinfo(dtrain, "label")
  
  suma = 0 ;
  largo = length( labels ) ;
  
  for( i in 1:largo )
  {
    if( preds[ 2*i ]  > 0.5  ){ suma <- suma + if( labels[i]==1 ) { 7750 } else { -250 }  
    } ;
  }
  
  return(list(metric = "ganancia", value = suma))
}


#multi:softprob  devuelve vector doble con la probabilidad que la clase sea  0 , y la probabilidad de 1
#sp =  SIN PESO,  por eso el punto de corte es 0.03125
fganancia_multi.sp <- function(preds, dtrain) 
{
  labels <- getinfo(dtrain, "label")
  
  suma = 0 ;
  largo = length( labels ) ;
  
  for( i in 1:largo )
  {
    if( preds[ 2*i ]  > 0.021  ){ suma <- suma + if( labels[i]==1 ) { 7750 } else { -250 }  
    } ;
  }
  
  return(list(metric = "ganancia", value = suma))
}


fganancia_multi.sp <- function(preds, dtrain) 
{
  labels <- getinfo(dtrain, "label")
  
  suma = 0 ;
  largo = length( labels ) ;
  
  for( i in 1:largo )
  {
    if( preds[ 2*i ]  > 0.021  ){ suma <- suma + if( labels[i]==1 ) { 7750 } else { -250 }  
    } ;
  }
  
  return(list(metric = "ganancia", value = suma))
}




archivo_entrada <- "c:\\uba\\a2016\\abril_01.txt"

abril_dataset <- read.table( archivo_entrada, header=TRUE, sep="\t", row.names="numero_de_cliente")


abril_dataset_sinclase <-   abril[ , !(names(abril) %in% c("clase") ) ] 


#agrego PESOS
vweights <- ifelse( abril_dataset$clase==1, 31, 1 )
dtrain_conpeso = xgb.DMatrix(data = data.matrix(abril_dataset_sinclase),label = abril_dataset$clase,missing = NA,  weight=vweights)

etiqueta <- ifelse(abril$clase=="BAJA+2",1,0)

vmax_depth         <- 4
vmin_child_weight  <- 5
vnround            <- 1000



# Corro modelo con  binary:logistic

set.seed( 102191  )

t0 =  Sys.time()

cv.logistic.cp = xgb.cv( 
  data = dtrain_conpeso ,        missing = 0 ,
  stratified = TRUE,     nfold = 5 ,
  eta = 0.01, 
  subsample = 1.0, 
  colsample_bytree = 0.6, 
  min_child_weight = vmin_child_weight, 
  max_depth = vmax_depth,
  alpha = 0, lambda = 0.1, gamma = 0.01,
  nround= vnround, 
  objective="binary:logistic",
  feval = fganancia_logistic.cp,     maximize =TRUE
)

t1 = Sys.time()
as.numeric(  t1 - t0, units = "secs")



# Corro modelo con multi:softprob

set.seed( 102191  )

t2 =  Sys.time()
cv.multi.cp = xgb.cv( 
  data = dtrain_conpeso ,          missing = 0 ,
  stratified = TRUE,       nfold = 5 ,
  eta = 0.01, 
  subsample = 1.0, 
  colsample_bytree = 0.6, 
  min_child_weight = vmin_child_weight, 
  max_depth = vmax_depth,
  alpha = 0, lambda = 0.1, gamma = 0.01,
  nround= vnround, 
  objective="multi:softprob",         num_class=2,
  feval = fganancia_multi.cp,            maximize =TRUE
)

t3 = Sys.time()





#SIN PESO
abril_dataset_sinclase <-   abril_seis[ , !(names(abril_seis) %in% c("clase") ) ] 

dtrain_sinpeso = xgb.DMatrix(data = data.matrix(abril_dataset_sinclase),label =etiqueta,missing = NA )
etiqueta <- ifelse(abril$clase=="BAJA+2",1,0)



# Corro modelo con  binary:logistic  SIN PESO

set.seed( 102191  )

t4 =  Sys.time()

cv.logistic.sp = xgb.cv( 
  data = dtrain_sinpeso ,        missing = 0 ,
  stratified = TRUE,     nfold = 5 ,
  eta = 0.01, 
  subsample = 1.0, 
  colsample_bytree = 0.6, 
  min_child_weight = vmin_child_weight, 
  max_depth = vmax_depth,
  alpha = 0, lambda = 0.1, gamma = 0.01,
  nround= vnround, 
  objective="binary:logistic",
  feval = fganancia_logistic.sp,     maximize =TRUE
)

t5 = Sys.time()
as.numeric(  t1 - t0, units = "secs")




# Corro modelo con multi:softprob  SIN PESO

set.seed( 102191  )

t6 =  Sys.time()
cv.multi.sp = xgb.cv( 
  data = dtrain_sinpeso ,          missing = 0 ,
  stratified = TRUE,       nfold = 5 ,
  eta = 0.01, 
  subsample = 1.0, 
  colsample_bytree = 0.6, 
  min_child_weight = 5, 
  max_depth = 4,
  alpha = 0, lambda = 0.1, gamma = 0.01,
  nround= vnround, 
  objective="multi:softprob",         num_class=2,
  feval = fganancia_multi.sp,            maximize =TRUE
)

t7 = Sys.time()



#Comparo

as.numeric(  t1 - t0, units = "secs")
max( cv.logistic.cp[ , test.ganancia.mean] ) / (1/5) 
which.max(  cv.logistic.cp[ , test.ganancia.mean] )

as.numeric(  t3 - t2, units = "secs")
max( cv.multi.cp[ , test.ganancia.mean] ) / (1/5) 
which.max(  cv.multi.cp[ , test.ganancia.mean] )

as.numeric(  t5 - t4, units = "secs")
max(  cv.logistic.sp[ 310:1000, test.ganancia.mean] ) / (1/5) 
which.max(  cv.logistic.sp[  310:1000, test.ganancia.mean] )

as.numeric(  t7 - t6, units = "secs")
max( cv.multi.sp[310:1000 , test.ganancia.mean] ) / (1/5) 
which.max(  cv.multi.sp[ 310:1000, test.ganancia.mean] )

x <- c( 350:1000 )
write.table(cv.multi.sp[  350:1000,test.ganancia.mean ],"multi_sp_1.txt",row.names = FALSE,quote=FALSE)
write.table(cv.multi.sp.2[  350:1000,test.ganancia.mean ],"multi_sp_2.txt",row.names = FALSE,quote=FALSE)

plot(   x, cv.multi.sp[  350:1000,test.ganancia.mean ] / (1/5),  col="orange",  type="l" )
lines(  x,  cv.logistic.cp[  350:1000,test.ganancia.mean ] / (1/5) , col="blue" )
lines(  x, cv.multi.cp[  350:1000,test.ganancia.mean ] / (1/5),  col="green" )
lines(  x, cv.logistic.sp[  350:1000,test.ganancia.mean ] / (1/5),  col="red" )



"mejor con 11, nround 449"

set.seed( 102191  )

t6 =  Sys.time()
cv.multi.sp.2 = xgb.cv( 
  data = dtrain_sinpeso ,          missing = 0 ,
  stratified = TRUE,       nfold = 5 ,
  eta = 0.01, 
  subsample = 1.0, 
  colsample_bytree = 0.4, 
  min_child_weight = vmin_child_weight, 
  max_depth = 4,
  alpha = 0, lambda = 0.1, gamma = 0.01,
  nround= vnround, 
  objective="multi:softprob",         num_class=2,
  feval = fganancia_multi.sp,            maximize =TRUE
)

t7 = Sys.time()