# FOLIJE https://docs.google.com/document/d/1uiaw3jKkAdddjQO8q2LqI5NXXlMbtgBhZODp2mfkyy8/edit?usp=sharing <br />
# R-FOLIJE https://docs.google.com/document/d/1J4m4OXj1qA7aod3dAtkl5G-LSdqIA3Rb7ctqertfaAo/edit?usp=sharing

## d-rasp - GUSTINA, q-rasp - KVANTIL, p-rasp - VREDNOST, r-rasp RANDOM-Raspodela | Kucas distribution u help za imena raspodela u R.

### DomaciVezbe7.r <br />
**1)** <ins>slucajan izbor 150 prirodnih brojeva</ins>, granice 10 intervala, frekvencije, modalni i medijalni interval, arit sredina, uzoracka disperzija, empirijska raspodela, histogram i pie plot. <br />
**2)** box plot, mean, standardna devijacija, treci decil za: bin, geo, t10, chisq, norm; ecdf za: bin, geo, t10, chisq, norm; gama

### primerTesta1.r
**Test1 (2019_prvi_Rac_Kol.pdf (GRP.E))**: <br />
**1)** cemu sluzi plot.ecdf. <br />
**2)** prvi kvartil n-rasp, kvantil hi-kvadrat(qchisq), kvantil t4-rasp(qt), gustina uniformne rasp(dunif), vrednost f exp-rasp(pexp), verovatnoca. <br />
**3)** sta ne valja tip zadatka

**Test2 (Test1-primer.pdf (GRP.B))**: <br />
**1)** za <ins>intervalni uzorak</ins> odrediti: obim, max aprox uzorka, medijalni interval, modus aprox uzorka, aritmeticku sredinu uzorka, standardnu devijaciju uzorka, skewness, treci decil 0.3, realizovanu vrednost empirijske funkcije raspodele fn, qqplot <br />
**2)** kvantil reda pirsonove hi kvadrat(qchisq), kvantil reda studentove t10(qt), vrednost uniformne(punif), vrednost binomne(pbinom), verovatnoca; <br />
**4)** napraviti tablicu z->fi(z): koristi se pnorm().


### primerTesta2.r
**Test1** isto sve ko Test2 iznad.

**Test2 (2019_prvi_Rac_Kol.pdf (GRP.F))**: <br />
**1)** 1. kvantil za n(qnorm), kvantil reda za b(qbinom), vr. f gustine u(dunif), vr. f rasporele e(pexp), verovatnoca; <br />
**2) ovo je 1. na pdf** <ins>vektor vrednosti</ins>, obim(length), i-ti element var niza(sort(x(imevektora)[i]), modus, nekorigovana i korigovana uzoracka disp, arit sredina(mean), kurtozis, 2. kvartil, kvantil reda, vrednost fje; <br />
**3) ovo je 2. na pdf** set.seed, frekvencija elementa(table), kovarijansa(cov), nekorigovana standardna dev., outlieri <br />

## zadatak1.R (vezbe1.png)
**1)** <ins>rucni unos uzoraka sa papira(c)</ins>, modus i medijana za jednostavan uzorak, aritmetiska sredina i standardna devijacija, kurtozis i skewness (2. nacina), uzoracka funkcija raspodele,  histogram i poligon, kvartili, qqplot i boxplot

### Napraviti tablicu: z->fi(z): pnorm ||||| fi(z)->z: qnorm
