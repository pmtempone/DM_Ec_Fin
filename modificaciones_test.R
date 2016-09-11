library(funModeling)
df_status(producto_premium_201604)

abril <- producto_premium_201604
abril$clase <- factor(abril$clase)
abril$participa <- factor(abril$participa)
rownames(abril) <- abril$numero_de_cliente
abril$numero_de_cliente <- NULL
