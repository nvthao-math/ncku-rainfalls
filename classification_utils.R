library(binaryLogic)

for(i in c(1:2^9-1)){
  x.bi <- as.binary(i, n=9)
  print(x.bi)
}



