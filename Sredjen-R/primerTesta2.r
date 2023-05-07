
#1. uzorak######################################################

mi <- c(4,9,13,17,21,25,30)
fi <- c(3,7,9,5,4,2)

ml <- mi[1:length(mi)-1] #leve granice
md <- mi[2:length(mi)] #desne
xi<-(ml+md)/2 #nasi predstavnici intervala - sredine intervala
xi

x<-rep(xi,fi) #nas aproksimativni uzorak
x

#OBIM POPULACIJE------------------------
n<-length(x)
n<-sum(fi)

#MAKSIMUM APROKS UZORKA-----------------
maxU <- max(x)

#Medijalni interval--------------------
#onaj sa > n/2 kum sume
cumsum(fi)
n/2
#to je treci interval - (13,17]

#modus aproksimativnog uzorka-----------
tx<-table(x)
modus <- as.numeric(names(tx)[tx==max(tx)])

#aritmeticka sredina uzorka-------------
xn <- mean(x)

#standardna devijacija uzorka-----------
sn<-sqrt(sd(x)^2*(n-1)/n)

#koef. asimetrije - skewness----------
mi3 <- mean((x-xn)^3)
mi2 <- mean((x-xn)^2)
koefasim <- mi3/mi2^(3/2)


#treci decil-----------------------
tdecil <- quantile(x, probs = 0.30)


#realizovanu vrednost empirijske funkcije raspodele fn(19.7):
funk <- ecdf(x)
rez <- funk(19.7)

#drugi nacin
cumsum(fi)/n #gledamo cetvrtu vrednost

#QQPLOT-------------------------
qqnorm(x)
qqline(x)


#2. zadatak##############################################################

#kvantil reda 0.35 Pirsonove hi kvadrat, step slob 10
qchisq(0.35, 10)

#kvantil reda 0.6 Studentove raspodele t10:
qt(p=0.6, df=10)

#treci kvartil Gausove raspodele N(4, 2):
qnorm(0.30, 4, 2)

#vrednost funkcije raspodele Uniformne raspodele U(1; 5) u x = 3.5
punif(3.5, 1, 5)

#vrednost funkcije raspodele Binomne raspodele B(50; 0:75) u x = 30:
pbinom(30, 50, 0.75)

#verovatnocu P(X > 12:5); gde X : N(15; 3):
1-pnorm(12.5, 15, 3)
pnorm(12.5,15,3, lower.tail = FALSE)


#4. zadatak-------------------------

pnorm(1.282)
pnorm(1.645)
pnorm(1.960)
pnorm(2.326)
pnorm(2.576)
pnorm(3.090)
pnorm(3.291)
pnorm(3.891)
pnorm(4.417)


#########PRIMERI SA SLEKA######################

#1. zadatak##############################################################

#Prvi kvartil N(1,2)
qnorm(0.25, 1, 2)

#Kvantil reda 0.25 hikv 4 rasp
qchisq(0.25, 4)

#Kvantil reda 0.45 B(12, 0.1)
qbinom(0.45,12,0.1)

#vrednost funkcije gustine U(0.1, 0.9)
dunif(0.75, 0.1, 0.9)

#vrednost fje raspodele E(0.1) rasp u 10
pexp(10, 0.1)


#verovatnoca P(Z<=0.25) N(0, 2.5)
pnorm(0.25, 0, 2.5)
1-pnorm(0.25, 0, 2.5)



#2. zadatak##############################################################

x=c(1,1,2,2,3,3,1,1,2,2,4,4,1,1,2,2,3,3)
table(x)

n<-length(x) #obim
n

#sedmi element varijacionog niza
sort(x)[7]

#modus
tx<-table(x)
modus <- as.numeric(names(tx)[tx==max(tx)])
modus

#000000000000000000000000000000000

# *nekorigovana uzoracka disperzija 
    sn2 <- sd(x)^2*(n-1)/n
    sn2 <- sn^2 #preko standardne devijacije ako je imamo
    
# *nekorigovana standardna devijacija:
    sn <- sqrt(sd(x)^2*(n-1)/n) #ovako smo na vezbama
    sn <- sqrt(sn2)


# **korigovana uzoracka disperzija
    sn2korig <- sd(x)^2
    sn2korig <- var(x)

# **korigovana standardna devijacija 
    snkorig <- sd(x)

#000000000000000000000000000000000000000
    
#aritm sredina
xn <- mean(x)

#kurtosis - spljostenost
mi4 <- mean((x-xn)^4)
mi2 <- mean((x-xn)^2)

koefsplj <- mi4/mi2^2
koefsplj

#drugi kvartil
quantile(x, 0.50) #==median
median(x)

#kvanil reda 0.87
quantile(x, 0.87)

#vrednost realizovane fje raspodele f*N(1.999)
ecdf(x)(1.999) #mrsuljin
cumsum(table(x))/length(x)


#3. zadatak##############################################################

set.seed(2019)
x<-round(rexp(250, rate=0.2), digits=2)
y<-round(rexp(250, rate=0.4), digits=2)


#a) frekvencija elementa 3.5
table(x) #nula

#b) kovarijansa
cov(x,y)

#c) naci nekorigovanu standardnu devijaciju razlike ovih uzoraka
z<- x-y
nz<-length(z)
snz<-sqrt(sd(z)^2*(nz-1)/nz)
snz

#d) koliko ima autlajera (outliers) u x i y?
boxplot(x)
boxplot(y)

length(boxplot(x)$out)
manjiX <- x[x<quantile(x,0.25) - 1.5*IQR(x)]
length(manjiX)

veciX <- x[x>quantile(x,0.75) + 1.5*IQR(x)]
length(veciX)



manjiY <- y[y<=quantile(y,0.25) - 1.5*IQR(y)]
length(manjiY)

veciY <- y[y>=quantile(y,0.75) + 1.5*IQR(y)]
length(veciY)

hist(x = rf(1000,10,10), probability = TRUE)
hist(x=rnorm(1000), probability = TRUE)

