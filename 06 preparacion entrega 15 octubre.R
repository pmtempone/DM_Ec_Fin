----#librerias----

library(RPostgreSQL)
library(dplyr)

---#preparacion archivo----

rownames(`201604_checkpoint`) <- `201604_checkpoint`$numero_de_cliente
`201604_checkpoint`$numero_de_cliente <- NULL

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

colnames(`201604_checkpoint`) <- tolower(colnames(`201604_checkpoint`))

dbWriteTable(con,"checkpoint_octubre",`201604_checkpoint`)
