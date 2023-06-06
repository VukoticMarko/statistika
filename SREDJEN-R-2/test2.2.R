# Test 2, Gr.B

#Ucitati fajl tips.csv.
#1. Testirati hipotezu da je srednja vrednost napojnica koje su muskarci ostavljali veca od 3.5$.
#a) U zadatku se testira hipoteza o:
#b) Srednja vrednost visine napojnica koje su muskarci ostavljali iznosi:
#c) Nulta hipoteza ovog testa glasi:
#d) Test koji se koristi spada u grupu parametarskih / neparametarskih statistickih testova (zaokruziti).
#e) p-vrednost dobijena testom iznosi: , odakle zakljucujemo da je opravdano prihvatiti / odbaciti
#   hipotezu da je srednja vrednost napojnica koje su muskarci ostavljali ve´ca od 3.5$ (zaokruziti).

#1 RESENJE
#a) U zadatku se testira hipoteza o srednjoj vrednosti napojnica koje su muškarci ostavljali.
#b) 3.089618
#c) Nulta hipoteza ovog testa glasi da je srednja vrednost napojnica koje su muškarci ostavljali manja ili jednaka 3.5 dolara.
#d) parametarskih
#e) 0.9996, prihvatiti(valjda)

tips <- read.csv("tips.csv")
nap <- tips$tip[tips$sex == "Male"]
mean_nap <- mean(nap)
t.test(nap, mu = 3.5, alternative = "greater")

#2. Kolmogorov-Smirnov testom testirati hipotezu o saglasnosti iznosa racuna sa normalnom
#   raspodelom N (20, 9).
#a) Kolmogorov-Smirnov test spada u grupu parametarskih / neparametarskih testova (zaokruziti).
#b) Nulta hipoteza ovog testa glasi:
#c) Vrednost dn iznosi:
#d) Realizovana vrednost test statistike iznosi i ona se uporeduje sa kvantilom λ0.95 = _____
#e) p-vrednost dobijena testom iznosi: , sto znaci da se H0 prihvata / ne prihvata (zaokruziti).

#2. RESENJE
#a) neparametarski
#b) Empirijska distribucija podataka odgovara normalnoj raspodeli N(20, 9)
#c) 1.994
#d) 0.1276, 1.36 ( ako je lambda 0.99 onda je 1.63)
#e) 0.00070, ne prihvata (P<0.05)

racun<-tips$total_bill
ks_test <- ks.test(racun, "pnorm", mean = 20, sd = 9)
c) dn<-ks_test$statistic * sqrt(length(racun))
d) realizovana_vrednost <- ks_test$statistic
e) p_vrednost <- ks_test$p.value

#3. Testirati hipotezu o jednakosti srednje vrednosti visine napojnica po danima (ANOVA).
#a) Nulta hipoteza ovog testa glasi:
#b) Nivoi faktora po kom se radi ANOVA su:
#c) Test statistika ima raspodelu sa stepeni slobode:
#d) Realizovana vrednost test statistike iznosi:
#e) p-vrednost dobijena testom iznosi: , sto znaci da postoje / ne postoje (zaokruziti) statisticki
#   znacajne razlike izmedu nivoa.

#3. RESENJE
#a) H0(m1=m2=m3=m4) - srednje vrednosti visine napojnica po danima su jednake
#b) Nivoi faktora po kojima se radi ANOVA su dani (day - Sun, Sat, Thur, Fri).
#c) F (Fiserova), 3
#d) 1.6724
#e) 0.1736, ne postoje (DA LI JE NULTA HIP. PRIHVACENA(P>0.05) -> JESTE ONDA NE POSTOJE RAZLIKE)


tip <- tips$tip
boxplot(tip~day, data = tips)
anova(lm(tip~day, data = tips))
