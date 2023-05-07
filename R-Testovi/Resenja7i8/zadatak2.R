#a) uzoracka funkcija raspodele, histogram i poligon
xi <- c(0.5, 1.5, 2.5, 4, 7.5, 15) #sredine intervala
fi <- c(15, 11, 7, 7, 6, 4) #frekvencije
x <- rep(xi, fi)

plot.ecdf(x)

mi <- c(80, 100, 130, 170, 220)
h <- hist(x, breaks = mi, freq = T)
sirine <- mi[2:5] - mi[1:4]
h$counts <- h$counts / sirine
plot(h, freq = T)

#b) modus i medijana - rucno po formulama

#c) aritmeticka sredina i standardna devijacija
xn <- mean(x)

n <- length(x)
sn <- sqrt(sd(x)^2 * (n - 1) / n)

#d) kurtosis i skewness
library(e1071)

gamma1 <- kurtosis(x, type = 1) + 3
gamma2 <- skewness(x, type = 1)

#e) kvartili, Q-Q plot i Boxplot
summary(x)
IQR(x)

qqnorm(x) 
qqline(x) 

boxplot(x)