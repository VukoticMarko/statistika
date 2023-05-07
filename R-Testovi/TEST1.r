set.seed(5623)
uzorak<-round(runif(25,1,50),0)
x<-uzorak

tx<-table(uzorak)
mo <- as.numeric(names(tx)[tx==max(tx)])
mo

xn<-mean(x)
n<-length(x)
sn2 <- sd(x)^2*(n-1)/n
round(sn2,3)

#koef asim
mi3 <- mean((x-mean(x))^3)
mi2 <- mean((x-mean(x))^2)

kasim <- mi3/mi2^(3/2)
round(kasim, 3)

library(e1071)
moment(x, order=2)
mi2obican <- mean(x^2)

#6. decil
quantile(x, 0.60)

#empf 23.25
ecdf(x)(23.25)


#3333333333333333333333333333333333333333333

qt(0.85, 15)
round(qt(0.85, 15),3)

pbinom(24.7, 100, 0.2)
round(pbinom(24.7, 100, 0.2), 3)


dexp(0.32, 2.85)
round(dexp(0.32, 2.85),3)


rez <- pnorm(9, 6, 1) - pnorm(4.5, 6, 1)
round(rez,3)

round(pnorm(9, 6, 1), 3) - round(pnorm(4.5, 6, 1), 3)




pnorm(-1.565)
round(pnorm(-1.565),3)

qnorm(0.492)
round(qnorm(0.492),3)

pnorm(1.068)
round(pnorm(1.068),3)
