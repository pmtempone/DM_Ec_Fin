----#carga de librerias----

library(dplyr)
library(reshape)
library(ggplot2)
library(FactoMineR)
library(funModeling)

---#revision de datos----

my_data_status=df_status(producto_premium_201604)

my_data_status[order(-my_data_status$p_zeros), c('variable', 'p_zeros')]
