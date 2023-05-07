## d-rasp - GUSTINA, q-rasp - KVANTIL, p-rasp - VREDNOST, r-rasp RANDOM-Raspodela

### DomaciVezbe7.r
1) slucajan izbor 150 prirodnih brojeva, granice 10 intervala, frekvencije, modalni i medijalni interval, arit sredina, uzoracka disperzija, empirijska raspodela, histogram i pie plot. 2) box plot, mean, standardna devijacija, treci decil za: bin, geo, t10, chisq, norm; ecdf za: bin, geo, t10, chisq, norm; gama

### primerTesta1.r
**Test1 (2019_prvi_Rac_Kol.pdf (GRP.E))**: 1) cemu sluzi plot.ecdf. 2) prvi kvartil n-rasp, kvantil hi-kvadrat(qchisq), kvantil t4-rasp(qt), gustina uniformne rasp(dunif), vrednost f exp-rasp(pexp), verovatnoca. 3) sta ne valja tip zadatka

**Test2 (Test1-primer.pdf (GRP.B))**: 1) za intervalni uzorak odrediti: obim, max aprox uzorka, medijalni interval, modus aprox uzorka, aritmeticku sredinu uzorka, standardnu devijaciju uzorka, skewness, treci decil 0.3, realizovanu vrednost empirijske funkcije raspodele fn, qqplot 2) kvantil reda pirsonove hi kvadrat(qchisq), kvantil reda studentove t10(qt), vrednost uniformne(punif), vrednost binomne(pbinom), verovatnoca; 4) napraviti tablicu z->fi(z): koristi se pnorm().


### primerTesta2.r
**Test1** isto sve ko Test2 iznad. 
**Test2** (2019_prvi_Rac_Kol.pdf (GRP.E)**: 1) 1. kvantil za n(qnorm), kvantil reda za b(qbinom), vr. f gustine u(dunif), vr. f rasporele e(pexp), verovatnoca; **2) ovo je 1. na pdf** vektor vrednosti, obim(length), i-ti element var niza(sort(x(imevektora)[i]), modus, nekorigovana i korigovana uzoracka disp, arit sredina(mean), kurtozis, 2. kvartil, kvantil reda, vrednost fje; **3) ovo je 2. na pdf** set.seed, frekvencija elementa(table), kovarijansa(cov), nekorigovana standardna dev., outlieri

### Napraviti tablicu: z->fi(z): pnorm ||||| fi(z)->z: qnorm
