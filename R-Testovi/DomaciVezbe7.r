
#1. zadatak

#Na slucajan nacin je izabrano 150 prirodnih brojeva manjih od 1000.
#a) Formirati raspodelu frekvencija sa 10 klasa iste sirine i rezultate predstaviti
#tabelarno.

#nas uzorak
set.seed(1111)
x <- round(runif(n=150, min=1, max=999), 0)

#trazimo granice od 10 intervala - ne znam da li moze ovako
mi<-c()
for (i in seq(1, 1001, 100)) #korak je 100 jer nam treba 10 intervala a imamo 1000 elem
  mi<-c(mi,i)

#trazimo frekvencije
h <- hist(x,probability = TRUE, breaks=mi)
fi <- h$count

#prikazujemo rezultat tabelarno? Ne znam kako je ovo trebalo?

# [1,101] [101,201] [201,301] [301,401] [401,501] [501,601] [601,701] [701,801] [801,901] [901,1001]
#    13       14        14        15        16        16        13        15        12        22

#b) Odrediti modalni i medijalni interval iz a).
tx<-table(x)
modus <- as.numeric(names(tx)[tx==max(tx)])

#modus je 986
# => modalni interval je onaj koji sadrzi modus -> [901,1001] NEEEEEEEEEE
# => MODALNI INTERVAL JE ONAJ KOJI IMA NAJVECU FREKVENCIJU! - [901,1001]

#medijalni je onaj cija kumulativna suma prelazi n/2 = 150/2 = 75
cumsum(fi)

#kumulativne sume:
# [1,101] [101,201] [201,301] [301,401] [401,501] [501,601] [601,701] [701,801] [801,901] [901,1001]
#    13       27        41        56        72        88        101       116        128        150

#medijalni interval -> [501,601] (Jer je tu prva vrednost kum sume veca od 75)


#c) Odrediti aritmeticku sredinu i uzoracku disperziju uzorka.
xn <- mean(x)
sn2 <- sd(x)^2 *(n-1)/n

#d) Odrediti koeficijent asimetrije (skewness) i sedmi centralni momenat uzorka.
mi3 <- mean((x-xn)^3)
mi2 <- mean((x-xn)^2)

koefasimetr <- mi3/mi2^(3/2)

mi7 <- mean((x-xn)^7)

#e) Izracunati realizovanu vrednost empirijske funkcije raspodele fn*(615.5)
func <- ecdf(x)
func(615.5) #ovde se dobije 0.5933333
plot.ecdf(x)

#provera  - starim nacinom sa pdf-a
#ne znam koji radimo od ova dva ispod?
vrednostiEmpirijskeFje = cumsum(fi)/150 #ako se ovde gleda dobije se 0.586666667 - NE OVO
vrednostiEmpirijskeFje2 = cumsum(table(x))/150 #0.5933 - pod brojem 612, jer se trazi X <= x, znaci prvi manji od 615.5 je u tabeli 612 ? #OVO A NE ONO IZNAD
# u tabeli imamo vrednosti sa 612 i 616,  nama treba 615.5 - tj verovatnoca da je X <= 615.5, znaci uzimas iz tabele vrednst pod 612

#f) Nactrati histogram i pie chart koristeci intervale iz a).
h <- hist(x, breaks=mi, probability = TRUE)
pie(fi) #ovde sam koristila fi, ne znam da li tako treba - ne, bolje uradi cut pa onda (veybe 7)


############################ 2. zadatak #######################################

FlightDelays <- read.csv("FlightDelays.csv")
edit(FlightDelays)

#a) Nacrtati Box plot za dužinu leta (FlightLength) po mesecu.
boxplot(FlightLength~Month, data=FlightDelays)

#b) Odrediti koliko su u proseku kasnili letovi (Delay) za Denver (Destination: DEN).
DenverFlights <- FlightDelays[FlightDelays$Destination == "DEN",]
edit(DenverFlights)
meanDelayDenver <- mean(DenverFlights$Delay)
meanDelayDenver


#c) Da li je standardna devijacija duzine leta veca kod aviona koji su poleteli izmedju
#4 i 8 casova (DepartTime: 4-8am) ili kod onih koji su poleteli izmedu 16 i 20
#casova (DepartTime: 4-8pm)?

AMFlights <- FlightDelays[FlightDelays$DepartTime == "4-8am",]
PMFlights <- FlightDelays[FlightDelays$DepartTime == "4-8pm",]

AM_n <- length(AMFlights)
PM_n <- length(PMFlights)

AM_sn <- sqrt(sd(AMFlights$FlightLength)^2*(AM_n-1)/AM_n)
PM_sn <- sqrt(sd(PMFlights$FlightLength)^2*(PM_n-1)/PM_n)

#Odgovor: veca je standard dev kod popodnevnih letova


#d) Naci treci decil za 
#B(10,3/4)
rezB2 <- qbinom(0.30, 10, 3/4)

#G(1/10)
rezG2 <- qgeom(0.30, 1/10)


#t10 - Ovde ne dobijam bas isto
rezT2 <- qt(0.30, 10)

#chisq, stepen slobode 10 - ni ovde nije bas isto
rezChisq2 <- qchisq(0.30, 10)

#N(15,3) - 
rezNorm2 <- qnorm(0.30, 15, 3)



#e) Naci ecdf(7.5) ako x ima 
#B(10,3/4)
xB <- rbinom(100, 10, 3/4)
funcB <- ecdf(xB)
ecdfB <- funcB(7.5)

#G(1/10)
xG <- rgeom(100, 1/10)
funcG <- ecdf(xG)
ecdfG <- funcG(7.5)

#t10
xT <- rt(100,10)
funcT <- ecdf(xT)
ecdfT <- funcT(7.5)

#Chisq, stepen slob 10
xChisq <-rchisq(100,10)
funcChisq <- ecdf(xChisq)
ecdfChisq <- funcChisq(7.5)

#N(15,3)
xN <- rnorm(100,15,3)
funcN <- ecdf(xN)
ecdfN <- funcN(7.5)


#Izracunati Gama(-3/2).
gamma(-3/2)
