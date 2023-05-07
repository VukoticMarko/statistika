#NEPARAMETARSKI TESTOVI

#Pirsonov 𝜒2-test
chisq.test(c(35, 35, 18, 12), p = c(0.4375, 0.3125, 0.1875, 0.0625))

#Tabele kontingencĳe
x <- matrix(c(16, 39, 15, 30), ncol = 2) 
chisq.test(x, correct = FALSE)

#𝜆-test Kolmogorov-Smirnov
x <- c(0.0790, 0.2672, 0.1474, 0.3613, 0.0933, 0.0784, 0.1522, 0.0810, 0.3003, 0.0705, 0.0072, 0.0066, 0.0084, 0.1250, 0.2174, 0.0108) 
ks.test(x, "pexp", 5)

#Testovi normalnosti
xr <- c(...) 
ks.test(xr, "pnorm", mean = 1.5, sd = 2.5)
shapiro.test(xr)

#T-TESTOVI PAROVA

#T-test na nezavisnim uzorcima sa N(m1, sigm1) i N(m2, sigm2) raspodelama
x <- c(...); y <- c(...)
t.test(x, y, var.equal = ...) #var.equal na osnovu var.test - TRUE ili FALSE

Df <- data.frame(Vrednosti = x, Faktori = y)
t.test(Vrednosti~Faktori , data = Df, var.equal = ...)

t.test(Vrednosti~Faktori , alt = "greater", var.equal = ...) #alternativna hipoteza H1(m1 > m2)

#T-test na uparenim uzorcima sa N(m1, sigm1) i N(m2, sigm2) raspodelama
D <- data.frame(Vrednosti = x.df, Faktori = y.df) 
t.test(Vrednosti~Faktori, data = D, var.equal = ..., paired = T)

#TWO-SAMPLE PERMUTATION TEST

x.M <- c(178, 167, 172, 176, 190, 169, 176, 180, 185, 172, 176, 190, 183, 190, 176) 
x.F <- c(171, 159, 162, 166, 162, 155, 158, 165, 170, 170, 168, 166, 172, 176, 169) 
originalna.razlika <- mean(x.M) - mean(x.F) 

x <- c(x.M, x.F) 
N <- 9999 
razlike <- numeric(N) 
for (i in 1 : N) { 
  indeksi <- sample(30, size = 15, replace = F) 
  razlike[i] <- mean(x[indeksi]) - mean(x[-indeksi]) 
} 

razlike[N + 1] <- originalna.razlika 
p.vrednost <- (sum(razlike >= originalna.razlika)) / (N + 1)