----#librerias----

library(RPostgreSQL)
library(dplyr)

---#preparacion archivo----
`201604_checkpoint` <- read.delim("~/Documentos/TP_DM_Ec_Fin/201604_checkpoint.txt")
colnames(`201604_checkpoint`) <- tolower(colnames(`201604_checkpoint`))
dbWriteTable(con,"checkpoint_octubre",`201604_checkpoint`,row.names=FALSE)

#despues se corren scripts checkpoint_octubre_v2/v3 y groups_by_cliente_checkpoint

#grabar salida en las base
pw <- {
  "..."
}

# loads the PostgreSQL driver
drv <- dbDriver("PostgreSQL")
# creates a connection to the postgres database
# note that "con" will be used later in each connection to the database
con <- dbConnect(drv, dbname = "dm_db",
                 host = "localhost", port = 5432,
                 user = "postgres", password = pw)
rm(pw) # removes the password

entrega_octubre <- dbReadTable(con,"checkpoint_octubre_v3")

#cambio los rownames
rownames(entrega_octubre) <- entrega_octubre$numero_de_cliente
entrega_octubre$numero_de_cliente <- NULL
