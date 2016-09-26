-----#Carga de librerias#-----
library(RPostgreSQL)
library(dplyr)

----#clase 2-----
lift_dm <- function (pos,tot) {
  (pos/tot)/(617/194289)
}

#P1 antiguedad < 12

(52/9529)/(617/194289) #lift alto

#ganancia

52*8000-250*9529

#P not 1
(565/(670+183525))/(617/194289)

#visa cuenta estado >10

(79/558)/(617/194289) #lift 44

lift_dm(79,558)

#ganacias

79*8000-250*558

#edad >35

----#arbol basico----

visa_cuenta_estado <- dbGetQuery(con,"select visa_cuenta_estado,clase,count(*)
from fct_prod_premium_201604
group by visa_cuenta_estado,clase
order by 1,2;")

#reemplazo dataset abril (archivo mal subido)

producto_premium_201604 <-dbGetQuery(con,"SELECT *
  FROM public.fct_prod_premium_201604;") 

