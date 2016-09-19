-----#librerias-----


----#repetir clase minoritaria (oversampling)------

df.over <- abril_dataset[abril_dataset$clase=='BAJA+2',1:170] 
df.over <- df.over[rep(seq_len(nrow(df.over)), each=300),]
test.rm <- df.over[which(grepl("\\.",rownames(df.over))),]
abril.over <- rbind(abril_dataset[,1:170],test.rm)

