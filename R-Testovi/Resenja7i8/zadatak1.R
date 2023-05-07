x <- c(87, 103, 130, 160, 180, 195, 132, 145, 211, 105, 145, 153, 152, 138, 87, 99, 93, 119, 129, 145)

#a) modus i medijana
tx <- table(x)
Mo <- as.numeric(names(tx)[tx==max(tx)])

Me <- median(x)

#b) aritmeticka sredina i standardna devijacija
xn <- mean(x)

n <- length(x) 
sn <- sqrt(sd(x)^2 * (n - 1) / n)

#c) kurtosis i skewness
#1. nacin:
mi4 <- mean((x - xn)^4)
mi3 <- mean((x - xn)^3)
mi2 <- mean((x - xn)^2)

gamma2 <- mi4 / mi2^2
gamma1 <- mi3 / mi2^(3 / 2)

#2. nacin
library(e1071)

gamma2 <- kurtosis(x, type = 1) + 3
gamma1 <- skewness(x, type = 1)

#d) uzoracka funkcija raspodele
plot.ecdf(x)

#e) histogram i poligon
mi <- c(80, 100, 130, 170, 220)
h <- hist(x, breaks = mi, freq = T)
sirine <- mi[2:5] - mi[1:4]
h$counts <- h$counts / sirine
plot(h, freq = T)

#poligon rucno docrtamo

#f) kvartili, Q-Q plot i Boxplot
summary(x)
IQR(x)

qqnorm(x) 
qqline(x) 

boxplot(x)