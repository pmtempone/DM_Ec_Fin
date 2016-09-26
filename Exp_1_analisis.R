----#libreria----

library(dplyr)

---#mejor modelo----

gcias_exp1 <- exp_1_result %>% select(cp,minsplit,minbucket,maxdepth,ganancias) %>% group_by(cp,minsplit,minbucket,maxdepth) %>% summarise(gcia_prom=mean(ganancias))

t <- gcias_exp1 %>% ungroup %>% filter(gcia_prom==max(gcia_prom))
