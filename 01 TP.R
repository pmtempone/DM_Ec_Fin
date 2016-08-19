-----#Carga de librerias#-----

library(dplyr)


----#Abrir archivo#------

producto_premium_201604 <- read.delim("E:/GitHub/DM_Ec_Fin/producto_premium_201604.txt")

---#crear la base de datos#-----

my_db <- src_sqlite("my_db.sqlite3", create = T)
premium_sqlite <- copy_to(my_db, producto_premium_201604, temporary = FALSE, indexes = list(
  c("numero_de_cliente", "foto_mes")))

select(premium_sqlite, numero_de_cliente)
