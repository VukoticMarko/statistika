# Test 2, Gr.C

#Ucitati fajl tips.csv.
#1. Testirati hipotezu da je srednja vrednost racuna tokom vikenda veca nego radnim danima,
#   pod pretpostavkom da su varijanse jednake.
#a) Srednja vrednost iznosa racuna tokom vikenda je:
#b) Srednja vrednost iznosa racuna radnim danima je:
#c) Test koji se koristi za uporedivanje srednjih vrednosti racuna tokom vikenda i radnim danima zove se, i spada
#   u grupu parametarskih / neparametarskih testova (zaokruziti).
#d) Nulta hipoteza ovog testa glasi: a alternativna hipoteza je:
#e) Realizovana vrednost test statistike iznosi:
#f) p-vrednost dobijena testom iznosi: , odakle zakljucujemo da je opravdano prihvatiti / odbaciti
#   hipotezu da je srednja vrednost racuna tokom vikenda veca nego radnim danima (zaokruziti).

#1. RESENJE
#a) 20.92569
#b) 17.41716
#c) t-test, parametarski
#d) sr.vrednost racuna tokom vikenda=sr.vrednost racuna radnim danima, m1>m2
#e) 2.793889 
#f) p-vrednost: 0.002812375, odbacujemo

#prosek racuna vikendom
racuni_vikend <- tips$total_bill[tips$day %in% c("Sat", "Sun")]
mean(racuni_vikend)

#prosek racuna radnim danima
racuni_radni_dani <- tips$total_bill[tips$day %in% c("Thur", "Fri")]
mean_radni_dani <- mean(racuni_radni_dani)

#test
t_test <- t.test(racuni_vikend, racuni_radni_dani, alternative = "greater", var.equal = TRUE)

#realizovana vrednost test statistike
t_test$statistic

#2. χ2 -testom ispitati saglasnost visine napojnica sa normalnom raspodelom N (3, 1.4). Uzorak
#   podeliti na pet intervala koristeci deobne taˇke 1.5,2.5,3.5 i 5.
#a) Nacrtati tabelu frekvencija za date intervale.
#b) Kolika je verovatnoca da iznos napojnice pripada intervalu (2.5, 3.5]? .
#c) χ2-test spada u grupu parametarskih / neparametarskih testova (zaokruziti).
#d) Nulta hipoteza ovog testa glasi:
#e) Realizovana vrednost test statistike iznosi:
#f) Sa kvantilom koje raspodele (i sa koliko stepeni slobode!) se ova vrednost uporeduje?:
#g) p-vrednost dobijena testom iznosi: , sto znaci da se H0 prihvata / ne prihvata (zaokruziti).


#2. RESENJE
#a) nacrtas iz tabele frekvencije
#b) 0.365
#c) neparametarski
#d) Nulta hipoteza glasi da visina napojnica ima normalnu raspodelou
#e) 36.34
#f) Pirsonova x^2
#g) 2.4519, ne prihvata

tip<-tips$tip
intervali <- c(-Inf, 1.5, 2.5, 3.5, 5, Inf)
frekvencije <- table(cut(tip, intervali))
#a)
tabela_frekvencija <- data.frame(Interval = names(frekvencije), Frekvencija = as.vector(frekvencije))
#b)
sum(frekvencije["(2.5,3.5]"]) / sum(frekvencije)
#e)
ocekivane_frekvencije <- diff(pnorm(intervali, mean = 3, sd = 1.4)) * sum(frekvencije)
matrica_kontingencije <- rbind(frekvencije, ocekivane_frekvencije)
chisq.test(matrica_kontingencije)$statistic

#3. Naci koeficijent korelacije visine napojnice u zavisnosti od iznosa racuna. Ako racun iznosi
#   40$, kolika ocekujemo da ce biti napojnica?
#a) Jednacina linearne regresije je:
#b) Koeficijent korelacije iznosi:
#c) Procena iz zadatka iznosi:

#3. RESENJE
#a)
summary(lm(napojnice ~ racun, data = tips))
#b) generalna korelacija napojnice i visine racuna (0.6757341)
#c) 5.12125 
koef_korelacije <- cor(tips$tip, tips$total_bill)
procena <- predict(reg_model, newdata = data.frame(racun = 40))
