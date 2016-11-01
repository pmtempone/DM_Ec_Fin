----#librerias----

library(FactoMineR)
library(dplyr)

----#componentes principales dataset training----
head(abril_dataset_training  %>% select(5:100))

res.pca = PCA((abril_dataset_training  %>% select(5:167)), scale.unit=TRUE, ncp=5, graph=T) 


