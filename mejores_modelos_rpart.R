library(dplyr)
library(sqldf)
options(scipen = 9999)

----#mejores parametros----

rpart_mejores <- gcias_exp1 %>% ungroup() %>% arrange(gcia_prom,desc(gcia_prom)) %>% filter(gcia_prom > quantile(gcia_prom, .9))
order

abril %>% select(clase) %>% group_by(clase) %>% count(clase)


617/(192949+723)

1300/192949

250/8000*2
