n<-1000000 #primos hasta

primoshallados<-c(1,2)

for (i in 1:n){
  primo<-1
  final<-sqrt(i)
  for(j in 2:final){
    if (floor(i/j) == i/j){  
      primo<-0
      break
    }
  }
  
  if(primo==1){
    primoshallados<-c(primoshallados,i)
    primo<-0
  }
  
}
primoshallados<-data.frame(primoshallados)

primoshallados