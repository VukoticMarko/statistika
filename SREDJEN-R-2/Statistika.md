Statistika - Vezbe 9

PAPIR 	- 0 - 1:10:00
R	- 1:10:00 - kraj

52:20 	- 6. zadatak
1:07:05 - 1. zadatak

1:15:00 - Intervali poverenja

1:56:30 - P-vrednost, a*

2:25:40 - Neparametarski testovi

Tipican zadatak za 2. klk:
	- Oceni parametar
	- Nadji disperziju
	- Odredi efikasniju ako imam 2
	- Ispitaj postojanost

===================================================================

INTERVALI POVERENJA

	Interval poverenja za m gde X : N(m, sigma), sigma poznato

		x <- rnorm(100, sd=2)
		beta = 0.95		# Ako se ne naglasi, po defaultu je 0.95
		z <- qnorm((1+beta)/2)
		sigma = 2
		n <- length(x)
	
		levaGranica 	<- mean(x)-z*sigma/sqrt(n)
		desnaGranica 	<- mean(x)+z*sigma/sqrt(n)

	===================================================================

	Interval poverenja za m gde X : N(m, sigma), sigma nepoznato

		x <- dato mi je
		beta = 0.95		# Ako se ne naglasi, po defaultu je 0.95
		n <- length(x)
		t <- qt((1+beta)/2, df=n-1)
		sigma <- sd(x)		# Standardna devijacija od uzorka

		levaGranica 	<- mean(x)-t*sigma/sqrt(n)
		desnaGranica 	<- mean(x)+t*sigma/sqrt(n)

===================================================================

P-VREDNOST, alfa*

	Svaki staticki test ima svoju nultu (H0) i alternativnu (H1) hipotezu
	H0 - ne postoji staticki znacajan efekat
	H1 - suprotno od H0

	Dva nacina testiranja:	- (ne)pridanje pretpostavljene vrednosti parametra kriticnoj oblasti
				- uporedjivanje p-vrednosti alfa* sa pragom znacajnosti alfa=1-beta

	prag znacajnosti - alfa=1-beta (0.05)
		poredim p-vrednost sa pragom znacajnosti i ako je p-vrednost manja onda postoje statisticki znacajne razlike ne prihvata se nulta hipoteza
							   ako je p-vrednost veca onda ne postoje staticki znacajne razlike i prihvata se nulta hipoteza

PARAMETARSKI TESTOVI

	Test nepoznatog ocekivanja (Ako predpostavljamo recimo srednju ocenu, preko t.test, prosledjenog uzorka i prepostavljene vrednosti dobijamo p-value koju poredimo sa alfa, i dobijamo interval preko koga mozemo da preoverimo da li predpostavljena vrednost pripada tom intervalu)
	t.test(x, mu=3.5)	# Prosledjujem uzorak i vrednost parametra za koji pretpostavljam da je tacno
				# t.test vraca interval i vraca prosecnu vrednost
	
	Test nepoznate proporcije
		#aproksimacije
		prop.test(200, 1000, p=1/6, conf.level=0.99, correct=FALSE)	# Sa binomnom raspodelom
		prva vrednost (200)	- broj uspesnih realizacija
		druga vrednost (1000)	- ukupan broj eksperimenata
		treci (p=1/6)		- povoljni/moguci
		cetvrti (conf.level=0.99) - nivo poverenja (beta = 1-alfa)
		peti (correct=FALLSE)	- 

		#tacno
		binom.test(200, 1000, p=1/6, conf.level=0.99)

		prag znacajnosti a 	- po defaultu uzima vrednost 0.05

	Test jednakosti varijanse
		Utvrdjuje da li su dva uzorka dobijena uzorkovanjem normalne raspodele sa istom varijansom
		Uzorci ne moraju biti istog obima, niti imati isto m

		x1<-c(...);	x2<-c(...)
		var.test(x1,x2, conf.level=0.99)

		# ili preko data frame-a
		Dfr<-data.frame(Brojevi=x, Faktori=y))
		var.test(Brojevi~Faktori, data=Dfr) 

	
Verovatno na drugom racunarskom:
	- Izracunati ne samo intervale poverenja, ali takodje i p vrednost
	- 3 ista pitanja:
		1. Da li pretpostavljene vrednosti pripadaju intervalu poverenja
		2. Da li je p vrednost manja od praga znacajnosti
		3. Da li na onsovu uzorka imam razlog da prihvatim nultu hipotezu


===================================================================
===================================================================
Statistika - Vezbe 10

17:00	- Pocetak

 
NEPARAMETARSKI TESTOVI

	Pirsonov Hi kvadrat test
		Testira se hipoteza da prosecna vrednost dobijena anketom ima raspodelu datom funkcijom raspodele

		Napraviti intervalne uzorke koji moraju imati vise od 5 elemenata, ako ima manje od 5 onda ga spajamo sa nekim od susednih
		Z = suma((anketom - izracunati)^2 / izracunati)
		poredim Z sa trazenim kvantilom		Hi^2 1-alfa(0.05), k - 1 - s
									k - broj intervala koji imaju vise od 5
									s - broj nepoznatih
		izracunati - P(X pripada [0,0.5)) = Fx(0.5) - Fx(0)

		kada poredim Z i Hi^2 ako je Z manje onda ne odbacujem hipotezu

		chisq.test(vektorIzmerenih, p=vektorVerovatnoca)

	===================================================================

	Test tabele kontingencije
		Proverava se da li su neka obelezja zavisna ili nezavisna
		- Nulta hipoteza: obelezja su nezavisna			Z < hi^2
		- Alternativna hipoteza: obelezja su zavisna

		Z = suma((anketom - izracunati)^2 / izracunati)
		poredim Z sa trazenim kvantilom		Hi^2 1-alfa(0.05), (k-1)x(r-1)     k - broj kolona   r - broj vrsta
		ako je manja ne prihvatam, ako je veca prihvatam

		izracunato - suma po kolonama * suma po vrstama / ukupno
					
		x<-matrix(c(a,c,b,d), ncol=2) #po kolonama
		chisq.test(x, correct=FALSE)

	===================================================================

	Lambda-test Kolmogorov-Smirnov
		Proveravam da li je uzorak saglasan sa eksponencijalnom raspodelom E(x)

		- Sortiramo uzorak
		- Nadjemo empirijsku funkciju raspodele fn*(x) = 0, x<=x1
								1/n, x1<x<=x2
								...
								1, x>xn
		- 

		Dn - maksimalno odstupanje ECDF-a i predpostavljene teorijske raspodele
		Dn*sqrt(obima) i poredim za lambda koje je dato u folijama u tabeli 4.4

	Testovi normalnosti
		Kolmogorov-Smirnov
			ks.test(x, "pexp", 5)  #uzorak, sa kojom funkcijom poredi, i sa kojim parametrima funkcije rasporedjuje)


		Shapiro-Wilk test
			xr <- c(...)	#vektor uzorka
			shapiro.test(xr)

===================================================================

T-test na nezavisnim uzorcima

	x<-c(...); y<-c(...);
	t.test(x, y, var.equal=TRUE/FALSE)	#na osnovu var.test-a


	Df<-data.frame(Vrednosti=x, Faktori=y)
	t.test(Vrednosti~Faktori, data=Df, var.equal=TRUE/FALSE)

	Ako se kaze da alternativa nije jednako nego recimo m1>m2 onda ide:
	t.test(Vrednosti~Faktori, alt="greater", var.equal=TRUE/FALSE)


T-test na uparenim uzorcima
	D<-data.frame(Vrednosti=x.df, Faktori=y.df)
	t.test(Vrednosti~Faktori, data=D, var.equal=TRUE/FALSE, paired=T/F)

===================================================================

Two sample permutation test
	# Primer sa visinama muskaraca i zena

	Ako su ekstremne razlike retke a originalna razlika je ekstremna to znaci da originalna razlika nije slucajna


Analiza Varijanse, one-way ANOVA
	# Da li postoji par gde se prosecne vrednosti znacajno razlikuju odnostno
	# Da ne postoje statisticki znacajne razlike u pojedinacnim prosecima izmedju parova

	Ubacim podatke u matricu i izracunam srednje vrednosti po grupama, izracunam aritmeticku sredinu svih vrednosti (Grand mean odnosno Y nadvuceno ..)


	Proverim da li svi imaju normalnu raspodelu (Shapiro-Wilk test) i tek ako imaju mogu da koristim anovu
	oneway.test(Brojevi~Slova, data=Df)	# nejednake varijanse
	oneway.test(Brojevi~Slova, data=Df, var.equal=TRUE)	#klasicna oneway ANOVA:

Regresija
	Rezedual - rastojanje izmedju dobijene tacke i izracunate

	plot(x,y)	# zavisnost x i y
	b<-sum((x-mean(x))*(y-mean(y)))/sum((x-mean(x))^2)
	a<-mean(y)-b*mean(x)
	OVO SVE SE MOZE URADITI U R

	lm(y~x)		# u resenju - prvi element je presek sa y osom, a drugi je koeficijent pravca
	abline(lm(y~x))	# prava koja ima presek sa y osom i koef pravca

	fitted(lm(y~x))		# pokaze kako izgledaju bele tacke (y kordinate belih tacaka)
	residuals(lm(y~x)) 	# visine stubica
	

3.2 Teorija ocena	
	3.2.1 Tackaste ocene
	3.2.2 Intervalne ocene
3.3 Statisticki testovi
	3.3.1 Parametarski testovi
	3.3.2 Neparametarski testovi
3.4 Uzoracka korelacija i regresija
	
