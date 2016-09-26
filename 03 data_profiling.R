----#carga de librerias----

library(dplyr)
library(reshape)
library(ggplot2)
library(FactoMineR)
library(funModeling)

---#revision de datos----

my_data_status=df_status(abril)

my_data_status[order(-my_data_status$p_zeros), c('variable', 'p_zeros')]

write.csv(my_data_status,file = "data_profiling.csv")
