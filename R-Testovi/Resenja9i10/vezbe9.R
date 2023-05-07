#Test nepoznatog ocekivanja m obelezja normalne (m, sigma) raspodele, sigma poznato
xn <- mean(x) 
n <- length(x) 
beta <- 0.95 
m0 <- 15 #pretpostavljena vrednost za m 
sigma <- 3.5 #poznata standardna devijacija 
z <- abs(xn-m0)*sqrt(n)/sigma #vrednost test statistike 
p <- 2*pnorm(-z)

#Test nepoznatog ocekivanja m obelezja normalne (m, sigma) raspodele, sigma nepoznato
#174. zbirka
xi <- c(1, 2, 3, 4, 5)
fi <- c(5, 9, 4, 5, 5)
x <- rep(xi, fi) 
t.test(x, mu = 3.5)

#Test nepoznate proporc??e p obelezja sa binomnom raspodelom
#172. zbirka
prop.test(200, 1000, p=1/6, conf.level = 0.99, correct = FALSE)

binom.test(200, 1000, p=1/6, conf.level = 0.99) #preciznije

#Test jednakosti var??anse - F-test
x1 <- c(1, 2, 3, 4, 5)
x2 <- c(5, 9, 4, 5, 5)
var.test(x1, x2, conf.level = 0.99) 

Dfr <- data.frame(Brojevi = x1, Faktori = x2)
var.test(Brojevi~Faktori, data = Dfr)