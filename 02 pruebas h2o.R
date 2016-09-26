-----#carga de librerias#-------

library(data.table)
library(h2o)

----#carga de set de datos grande#----
#no correr porque llena memoria
prod_premium <- fread("/home/pablo/Documentos/TP_DM_Ec_Fin/producto_premium_2016.txt", stringsAsFactors = T)


