### Programming and Geostatistics
### First session 22.10.19

## Introduction

# R as calculator

5+6
5*5^4-88*15
1/0

result1<- 5+6
result1

vector2<- seq(1,9,by=2)

vector1<- c(4,67,22,56,37,2,43,68,84,23,73,58,34,20,27,83,47,15)
max(vector1)
min(vector1)
mean(vector1)

plot(vector1, col="red", pch=19, xlab= "X-Achse", ylab="Werte")

vector1log<- log(vector1)
plot(vector1log, col="red", pch=19, xlab=" X-Achse", ylab="Log-Werte", cex=1.5)
boxplot(vector1,col="orange")

sum(vector1)
summary(vector1)

# combining data 

union(vector1,vector2)
