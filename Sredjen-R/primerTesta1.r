#1. ZADATAK -------------------------------------------------------------------
#Cemu sluzi i sta radi komanda plot.ecdf()? 

#Komanda plot.ecdf() daje graficki prikaz empirijske (uzoracke) kumulativne funkcije raspodele. 
#Uzoracka fja raspodele nekog obelezja je definisana za svako x na sledeci nacin: F*n = Nx/n
#gde je Nx broj elemenata uzorka koji su <= x, a n je obim realizovanog uzorka

x <- rnorm(1000)
plot(density(x))
plot.ecdf(x)

#2. ZADATAK -------------------------------------------------------------------
#Prvi kvartil N(0,2.5) raspodele
qnorm(0.25, 0, 2.5)

#Kvantil reda 0.25 hi kvadrat raspodele, stepen slobode 4
qchisq(0.25, df = 4)

#Kvantil reda 0.35 t4 raspodele
qt(0.35, df = 4)

#Kvantil reda 0.45 B(12, 0.1) raspodele
qbinom(0.45, 12, 0.1)

#Vrednost funkcije gustine uniformne U(0.1, 0.9) raspodele u x = 0.75
dunif(0.75, 0.1, 0.9) #rez: 1.25 = 1/(0.9-0.1) == 1/(b-a)
plot(density(runif(100000, min=0.1, max=0.9)))

#Vrednost funkcije raspodele eksponencijalne E(0.1) raspodele u x=10
#funkcija raspodele je u sustini P(X<x) ?
pexp(10, rate=0.1)

#Verovatnocu P(Z<=0.25) , Z:N(0,2.5)
plot(density(rnorm(100000,0,2.5)))
pnorm(0.25, mean=0, sd=2.5)


#3. ZADATAK -------------------------------------------------------------------
# Napisati sta ne valja:
rnorm(12, 2, -3) #ne moze standardna devijacija da bude negativna
qnorm(1.2, 2, 3) #kvantili moraju biti u opsegu [0,1]
rlnorm(100, df1=5, df2=2) #nevalidni argumenti, treba proslediti meanlog i sdlog
dgeom(4, 1.1) #ne moze verovatnoca p da bude van [0,1] opsega - ovde je 1.1
pbinom(2, 100, 1.5) #ne moze verovatnoca p da bude van [0,1] opsega - ovde je 1.5



#------------------------------------------------------2.TEST

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

donjaGranica = max(min(x), quantile(x,0.25)-1.5*IQR(x))
manji = x[x<donjaGranica]

gornjaGranica = min(max(x), quantile(x, 0.75)+1.5*IQR(x))
veci = x[x>gornjaGranica]

outlieriX <- length(manji) + length(veci) #isto i za y radis

## ecdf plot dat, obim je 10, napraviti uzorak

# 10*0.2 = 2 broja <= 1 [1,1]
# 10*0.5 = 5 brojeva <= 2 ---------- bez ona prva dva keca to je TRI DVOJKE[1,1,2,2,2]
# 10*0.9 = 9 brojeva <= 3 ----------- bez onih prvih 5 to je CETIRI TROJKE[1,1,2,2,2,3,3,3,3]
# 10*1 = 10 brojeva <= 4 ----------- bez onih prvih 9 to je JEDNA CETVORKA [1,1,2,2,2,3,3,3,3,4]

uzorak <- c(1,1,2,2,2,3,3,3,3,4)
plot.ecdf(uzorak)

